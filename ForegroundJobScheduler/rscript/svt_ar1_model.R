library("forecast")
library("mvtnorm")
library("dict")
library("dplyr")
library("xlsx")

source("C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//rscript//helper_functions.R")

calculate_var_cov_matrix_ar1 <-function(var, l, phi) {
  #### input var: A vector from var(an+l) to var(an+1) of length l
  #### input l: number of prediction
  #### input phi: coeff of AR1 model
  dm=abs(outer(1:l,1:l,"-"))
  var_cov <- matrix(var[outer(1:l,1:l,"pmin")],l,l)*phi^dm
  return(var_cov)
}

do_prediction <- function(last_obs, phi, mean, variance, predict_size, level) {
  # Construct mean
  mu <- rep(last_obs, predict_size)
  mu <- mu * phi^(1:predict_size) + (1 - phi^(1:predict_size)) * mean
  # Construct Var-cov matrix
  var <- cumsum((phi^2)^(0:(predict_size-1)))*variance
  varcov <- calculate_var_cov_matrix_ar1(var, predict_size, phi)
  # caclulate probability
  up_bound <- rep(level, predict_size)
  lower_bound <- rep(0, predict_size)
  prob <- 1 - pmvnorm(upper = up_bound, lower = lower_bound, mean = mu, sigma = varcov)
  result <- list('prob' = as.numeric(prob), 'mu' = mu, 'varcov'=varcov)
  return(result)
}

svt_stationary_model <- function(dataset, initial_train_size, window_size, job_length, cpu_required, prob_cut_off, mode, granularity) {
  #### input dataset: N by M matrix, N being number of observations, M being number of time series
  #### input initial_train_size: The number of first observations used to train the model
  #### input window_size: The number of observations used to train and predict
  #### input job_length: The number of windows that the foreground job will be runing
  #### input cpu_required: A vector, the cpu that the foreground job requires in percentage
  #### input prob_cut_off: If the probability of background job exceeding 100-cpu_required is smaller than prob_cut_off, then schedule it. Otherwise, don't.

  if (granularity > 0) {
    cpu_required <- sapply(cpu_required, round_to_nearest, granularity, FALSE)
  }
  
  ## N by M dataframe
  probability <- data.frame(row.names = 1)
  pi_upper_bounds <- data.frame(row.names = 1)
  ## N by M dataframe
  avg_usage <- data.frame(row.names = 1)
  job_survival <- data.frame(row.names = 1)
  ## N by M dataframe
  predict_result <- data.frame(row.names = 1)
  ## N by M dataframe
  actual_result <- data.frame(row.names = 1)
  ## 4 by M dataframe
  scheduling_summary <- data.frame(matrix(nrow = 4, ncol = ncol(dataset)))
  
  ## Dictionaries
  end_time_testing_queue <- numvecdict()
  pi_up_bounds <- dict()
  
  ## Summary Counts
  scheduled_num <- rep(0, ncol(dataset))
  unscheduled_num <- rep(0, ncol(dataset))
  falsely_scheduled_num <- rep(0, ncol(dataset))
  falsely_unscheduled_num <- rep(0, ncol(dataset))
  
  ## Split in Training and Testing Set
  train_dataset <- dataset[1:initial_train_size, 1:ncol(dataset)]
  test_dataset <- dataset[(initial_train_size+1):nrow(dataset), 1:ncol(dataset)]
  
  ## Convert Frequency
  new_trainset <- matrix(nrow = floor(nrow(train_dataset) / window_size), ncol = 0)
  for (ts_num in 1:ncol(dataset)) {
    converted_data <- convert_frequency_dataset(train_dataset[1:nrow(train_dataset), ts_num], window_size, mode)
    new_trainset <- cbind(new_trainset, converted_data)
  }
  rownames(new_trainset) <- seq(1, 1 + window_size * (nrow(new_trainset) - 1), window_size)
  colnames(new_trainset) <- colnames(train_dataset)
  
  ## Train Model
  coeffs <- rep(NA, ncol(new_trainset))
  means <- rep(NA, ncol(new_trainset))
  vars <- rep(NA, ncol(new_trainset))
  train_percent <- 0.00
  for (ts_num in 1:ncol(new_trainset)) {
    if (round(ts_num / ncol(new_trainset), 2) != train_percent) {
      print(paste("Training", train_percent))
      train_percent <- round(ts_num / ncol(new_trainset), 2)
    }
    ts_model <- tryCatch({
      arima(x=new_trainset[, ts_num], order = c(1,0,0), include.mean = TRUE, method = "CSS-ML", optim.control = list(maxit=2000))
    }, error = function(cond) {
      return(arima(x=new_trainset[, ts_num], order = c(1,0,0), include.mean = TRUE, method = "ML", optim.control = list(maxit=2000)))
    })
    coeffs[ts_num] <- as.numeric(ts_model$coef[1])
    means[ts_num] <- as.numeric(ts_model$coef[2])
    vars[ts_num] <- ts_model$sigma2
  }
  
  ## Test Model
  current_end <- 2
  current_percent <- 0.00
  
  schedule_time <- NA
  update_rule <- 1
  while (current_end <= nrow(test_dataset)) {
    
    ## Initialize Model 
    prob_vector <- c()
    pi_up <- c()
    prediction <- c()
    
    for (ts_num in 1:ncol(test_dataset)) {
      ## Schedule the job
      if (current_end <= (nrow(test_dataset) - job_length * window_size + 1)) {
        new_test_dataset <- convert_frequency_dataset(test_dataset[1:nrow(test_dataset), ts_num], window_size, mode)
        ## Do these in the unit of window_size
        last_obs <- new_test_dataset[(current_end-1), ts_num]
        prediction_result <- do_prediction(last_obs = last_obs, phi = coeffs[ts_num], mean = means[ts_num], variance = vars[ts_num], predict_size=job_length, level=(100-cpu_required[ts_num]))
        prob_vector[ts_num] <- prediction_result$prob
        if (prob_vector[ts_num] < prob_cut_off) {
          prediction[ts_num] <- 1
          scheduled_num[ts_num] <- scheduled_num[ts_num] + 1
        } else {
          prediction[ts_num] <- 0
          unscheduled_num[ts_num] <- unscheduled_num[ts_num] + 1
        }
        pi_up_bounds[[paste(ts_num, ",", current_end, sep = "")]] <- compute_pi_up(mu=prediction_result$mu, varcov=prediction_result$varcov, predict_size=job_length, prob_cutoff=prob_cut_off, granularity=granularity)
        if (job_length == 1) {
          pi_up[ts_num] <- pi_up_bounds[[paste(ts_num, ",", current_end, sep = "")]]
        }
      }
    }
    schedule_time <- current_end + window_size
    
    ## Store Probability
    probability <- rbind(probability, prob_vector)
    rownames(probability)[nrow(probability)] <- initial_train_size + 1 + (current_end - 1) * window_size
    
    ## Store Prediction Upper Bounds
    pi_upper_bounds <- rbind(pi_upper_bounds, pi_up)
    rownames(pi_upper_bounds)[nrow(pi_upper_bounds)] <- initial_train_size + 1 + (current_end - 1) * window_size
    
    ## Store Prediction
    predict_result <- rbind(predict_result, prediction)
    rownames(predict_result)[nrow(predict_result)] <- initial_train_size + 1 + (current_end - 1) * window_size
    
    ## Queue the jobs to check their correctness later
    closest_update <- current_end + job_length * window_size - 1
    end_time_testing_queue$append_number(closest_update, nrow(predict_result))
    
    actual <- c()
    avg_cycle_used <- c()
    survival <- c()
    
    for (ts_num in 1:ncol(test_dataset)) {
      ## Check correctness of previous schedulings
      if (length(end_time_testing_queue[[current_end]]) != 0) {
        new_test_dataset <- convert_frequency_dataset(test_dataset[1:nrow(test_dataset), ts_num], window_size, mode)
        ## Do these in the unit of window_size
        row_num <- end_time_testing_queue[[current_end]]
        end_time <- current_end
        start_time <- end_time - job_length + 1
        
        position_vec <- new_test_dataset[start_time:end_time, ts_num]
        if (all(position_vec < (100 - cpu_required[ts_num]))) {
          actual[ts_num] <- 1
          if (predict_result[row_num, ts_num] == 0) {
            falsely_unscheduled_num[ts_num] <- falsely_unscheduled_num[ts_num] + 1
          }
        } else {
          actual[ts_num] <- 0
          if (predict_result[row_num, ts_num] == 1) {
            falsely_scheduled_num[ts_num] <- falsely_scheduled_num[ts_num] + 1
          }
        } 
        
        # Evaluation of previous scheduling according to predictions
        if (current_end == schedule_time) {
          pi_up <- pi_up_bounds[[paste(ts_num, ",", start_time, sep = "")]]
          evalulation <- find_evaluation(pi_up=pi_up, actual_obs=position_vec, granularity=granularity)
          avg_cycle_used[ts_num] <- evalulation$avg_usage
          survival[ts_num] <- evalulation$survival
          
          if (!is.na(survival[ts_num]) & survival[ts_num] == 0) {
            update_rule = 1
          } else if (!is.na(survival[ts_num]) & survival[ts_num] == 1) {
            update_rule = window_size
          }
        }
      }
    }
    
    ## Store Actual
    actual_result <- rbind(actual_result, actual)
    rownames(actual_result)[nrow(actual_result)] <- initial_train_size + 1 + (current_end - 1) * window_size
    
    ## Store Evaluations
    avg_usage <- rbind(avg_usage, avg_cycle_used)
    job_survival <- rbind(job_survival, survival)
    
    ## Counter
    current_percent <- counter(current_percent, current_end, nrow(test_dataset)-1, "Testing:")
    
    ## Update current_end
    current_end <- current_end + update_rule
  }
  
  ## Change column and row names, N by M
  colnames(probability) <- colnames(test_dataset)
  colnames(pi_upper_bounds) <- colnames(test_dataset)
  colnames(avg_usage) <- colnames(test_dataset)
  colnames(job_survival) <- colnames(test_dataset)
  colnames(predict_result) <- colnames(test_dataset)
  colnames(actual_result) <- colnames(test_dataset)
  colnames(scheduling_summary) <- colnames(test_dataset)
  scheduling_summary[1,] <- scheduled_num
  scheduling_summary[2,] <- unscheduled_num
  scheduling_summary[3,] <- falsely_scheduled_num
  scheduling_summary[4,] <- falsely_unscheduled_num
  rownames(scheduling_summary) <- c('Scheduled_Num', 'Unscheduled_Num', 'Falsly_scheduled_Num', 'Falsely_unscheduled_Num')
  
  result <- list('prob' = probability, 'pi_up' = pi_upper_bounds, 'avg_usage'=avg_usage, 'job_survival'=job_survival, 'predict' = predict_result, 'actual' = actual_result, 'scheduling_summary' = scheduling_summary)
  return(result)
}


wrapper.epoche <- function(parameter, dataset, cpu_required, initial_train_size, bad.seq.adj, output_dp) {
  
  window_size <- as.numeric(parameter[1])
  prob_cut_off <- as.numeric(parameter[2])
  granularity <- as.numeric(parameter[3])
  
  output <- svt_stationary_model(datase =dataset, window_size=window_size, job_length=1, cpu_required=cpu_required, prob_cut_off=prob_cut_off, initial_train_size=initial_train_size, mode="max", granularity=granularity)
  overall_evaluation <- find_overall_evaluation(output$avg_usage, output$job_survival, bad.seq.adj)
  avg_utilization <- overall_evaluation$avg_utilization
  survival <- overall_evaluation$survival
  
  scheduled_num <- sum(output$scheduling_summary[1,])
  unscheduled_num <- sum(output$scheduling_summary[2,])
  correct_scheduled_num <- scheduled_num - sum(output$scheduling_summary[3,])
  correct_unscheduled_num <- unscheduled_num - sum(output$scheduling_summary[4,])
  correct_scheduled_rate <- correct_scheduled_num / scheduled_num
  correct_unscheduled_rate <- correct_unscheduled_num / unscheduled_num
  
  print(paste("Avg cycle used:", "job length", window_size, avg_utilization))
  print(paste("Job survival rate:", "job length", window_size, survival))
  print(paste("Scheduling summary:", "Correct scheduled rate:", correct_scheduled_rate, "Correct unscheduled rate:", correct_unscheduled_rate))
  
  write.csv(output$pi_up, file = paste("AR1",window_size, sample_size, prob_cut_off, granularity, "pi_upper.csv"))
  write.csv(output$scheduling_summary, file = paste("AR1", window_size, sample_size, prob_cut_off, granularity, "scheduling_sum.csv"))
  write.csv(output$avg_usage, file = paste("AR1", window_size, sample_size, prob_cut_off, granularity, "avg_usage.csv"))
  write.csv(output$job_survival, file = paste("AR1", window_size, sample_size, prob_cut_off, granularity, "job_survival.csv"))
  
  result_path.xlsx <- read.xlsx(output_dp, sheetIndex = 1)
  result_path.xlsx <- update.xlsx.df(result_path.xlsx, "AR1", prob_cut_off, NA, sample_size, window_size, granularity, avg_utilization, survival, correct_scheduled_rate, correct_unscheduled_rate)
  write.xlsx(result_path.xlsx, showNA = FALSE, file = output_dp, row.names = FALSE)
}

## Read back ground job pool

sample_size <- 100
cpu_usage <- 3
total_trace_length <- 8000
initial_train_size <- 6000
bad.seq.adj <- FALSE

window_sizes <- c(12, 36)
prob_cut_offs <- c(0.005, 0.01, 0.02, 0.1)
granularity <- c(10, 100/32, 100/64, 100/128, 0)

bg_jobs_path = "C://Users//carlo//Documents//sample background jobs//"
bg_job_pool <- NULL
if (sample_size == 100 ) {
  bg_job_pool <- read.csv("C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//pythonscripts//list of sampled 100 background jobs.csv")[,1]
  bg_job_pool <- sub(".pd", "", bg_job_pool)
} else {
  bg_job_pool <- read.csv("C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//pythonscripts//list of sampled background jobs.csv")[,1]
  bg_job_pool <- sub(".pd", "", bg_job_pool)
}

data_matrix <- matrix(nrow = total_trace_length, ncol = 0)
for (job_num in bg_job_pool) {
  bg_job <- read.csv(paste(bg_jobs_path, job_num, ".csv", sep = ""))
  data_matrix <- cbind(data_matrix, bg_job$max_cpu[1:total_trace_length])
}
rownames(data_matrix) <- seq(1, nrow(data_matrix) ,1)
colnames(data_matrix) <- bg_job_pool

cpu_required <- rep(0, ncol(data_matrix))
for (j in 1:ncol(data_matrix)) {
  cpu_required[j] <- as.numeric(quantile(data_matrix[,j], c(0.15, 0.5, 0.85), type = 4)[cpu_usage])
}

output_dp <- NULL
if (bad.seq.adj) {
  #output_dp <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//Nonoverlapping windows//summary (windows) max post adj.xlsx"
  output_dp <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//Nonoverlapping windows//summary (windows,granularity) post adj.xlsx"
} else {
  #output_dp <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//Nonoverlapping windows//summary (windows) max.xlsx"
  output_dp <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//Nonoverlapping windows//summary (windows,granularity).xlsx"
}

parameter.df <- expand.grid(window_sizes, prob_cut_offs, granularity)
colnames(parameter.df) <- c("window_size", "prob_cut_off", "granularity")
parameter.df <- parameter.df %>%
  arrange(window_size)
slt <- apply(parameter.df, 1, wrapper.epoche, dataset=data_matrix, cpu_required=(100-cpu_required), initial_train_size=initial_train_size, bad.seq.adj=bad.seq.adj, output_dp=output_dp)
