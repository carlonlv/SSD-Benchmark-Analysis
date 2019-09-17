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
  return(as.numeric(prob))
}


scheduling_foreground <- function(ts_num, test_dataset, coeffs, means, vars, window_size, job_length, prob_cut_off, cpu_required, granularity, mode, seek_length=NULL, last_time_schedule=NULL) {
  seek_length <- ifelse(mode == "dynamic", window_size * job_length, seek_length)
  last_time_schedule <- ifelse(mode == "dynamic", length(test_dataset) - window_size * job_length + 1, last_time_schedule)
  update_policy <- 1

  scheduled_num <- 0
  unscheduled_num <- 0
  correct_scheduled_num <- 0
  correct_unscheduled_num <- 0
  
  current_end <- 2
  while (current_end <= last_time_schedule) {
    ## Schedule based on model predictions
    last_obs <- test_dataset[(current_end-1), ts_num]
    prediction_result <- do_prediction(last_obs=last_obs, phi=coeffs[ts_num], mean=means[ts_num], variance=vars[ts_num], predict_size=job_length, level=(100-cpu_required[ts_num]))
    prediction <- ifelse(prediction_result <= prob_cut_off, 1, 0)
    scheduled_num <- ifelse(prediction == 1, scheduled_num + 1, scheduled_num)
    unscheduled_num <- ifelse(prediction == 1, unscheduled_num, unscheduled_num + 1)
    
    ## Evalute schedulings based on prediction
    start_time <- current_end
    end_time <- current_end + seek_length - 1
    position_vec <- test_dataset[start_time:end_time, ts_num]
    actual <- ifelse(all(position_vec <= (100 - cpu_required[ts_num])), 1, 0)
    correct_scheduled_num <- ifelse(prediction == 1 & actual == 1, correct_scheduled_num + 1, correct_scheduled_num)
    correct_unscheduled_num <- ifelse(prediction == 0 & actual == 0, correct_unscheduled_num + 1, correct_unscheduled_num)
    
    update_policy <- ifelse(mode == "dynamic", ifelse(is.na(survival), update_policy, ifelse(survival == 1, window_size * job_length, 1)), 1)
    current_end <- current_end + update_policy
  }
  
  return(list("scheduled_num"=scheduled_num, "unscheduled_num"=unscheduled_num, "correct_scheduled_num"=correct_scheduled_num, "correct_unscheduled_num"=correct_unscheduled_num))
}


scheduling_model <- function(ts_num, test_dataset, means, vars, window_size, prob_cut_off, granularity, mode, seek_length=NULL, last_time_schedule=NULL) {
  utilization <- c()
  survival <- c()
  runs <- rep(0, 5)
  run_counter <- 0
  run_switch <- FALSE
  
  current_end <- 2

  seek_length <- ifelse(mode == "dynamic", window_size, seek_length)
  last_time_schedule <- ifelse(mode == "dynamic", length(test_dataset) - window_size + 1, last_time_schedule)
  update_policy <- 1

  while (current_end <= last_time_schedule) {
    ## Schedule based on model predictions
    pi_up <- compute_pi_up(mu=as.vector(means[ts_num]), varcov=as.matrix(vars[ts_num]), predict_size=1, prob_cutoff=prob_cut_off, granularity=granularity)
    
    ## Evalute schedulings based on prediction
    start_time <- current_end
    end_time <- current_end + seek_length - 1
    position_vec <- test_dataset[start_time:end_time, ts_num]
    evalulation <- find_evaluation(pi_up=pi_up, actual_obs=position_vec, granularity=granularity)
    utilization <- c(utilization, evalulation$usage)
    survival <- c(survival, evalulation$survival)
    
    if (mode == "dynamic") {
      if (!is.na(survival) & survival == 1) {
        update_policy <- window_size
        if (run_switch) {
          idx <- ifelse(run_counter > 5, 5, run_counter)
          runs[idx] <- runs[idx] + run_counter
          run_counter <- 0
          run_switch <- FALSE
        }
      } else if (!is.na(survival) & survival == 0) {
        update_policy <- 1
        if (!run_switch) {
          run_switch <- TRUE
        }
        run_counter <- run_counter + 1
      }
    }
    current_end <- current_end + update_policy
  }
  
  overall_rate <- find_overall_evaluation(utilization, survival)
  return(list("utilization"=overall_rate$utilization_rate, "survival"=overall_rate$survival_rate, "one"=runs[1], "two"=runs[2], "thr"=runs[3], "fou"=runs[4], "fiv"=runs[5]))
}


svt_stationary_model <- function(dataset, initial_train_size, window_size, job_length, cpu_required, prob_cut_off, mode, granularity, schedule_policy="disjoint") {
  #### input dataset: N by M matrix, N being number of observations, M being number of time series
  #### input initial_train_size: The number of first observations used to train the model
  #### input window_size: The number of observations used to train and predict
  #### input job_length: The number of windows that the foreground job will be runing
  #### input cpu_required: A vector, the cpu that the foreground job requires in percentage
  #### input prob_cut_off: If the probability of background job exceeding 100-cpu_required is smaller than prob_cut_off, then schedule it. Otherwise, don't.
  #### input mode:
  #### input granularity:
  #### input schedule_policy: real, disjoint, overlap
  
  if (granularity > 0) {
    cpu_required <- sapply(cpu_required, round_to_nearest, granularity, FALSE)
  }
  
  scheduled_num <- data.frame(matrix(nrow=ncol(dataset), ncol=0))
  unscheduled_num <- data.frame(matrix(nrow=ncol(dataset), ncol=0))
  correct_scheduled_num <- data.frame(matrix(nrow=ncol(dataset), ncol=0))
  correct_unscheduled_num <- data.frame(matrix(nrow=ncol(dataset), ncol=0))
  
  avg_usage <- data.frame(matrix(nrow=ncol(dataset), ncol=0))
  job_survival <- data.frame(matrix(nrow=ncol(dataset), ncol=0))
  overall_runs <- data.frame(matrix(nrow=ncol(dataset), ncol = 5))
  
  ## Split in Training and Testing Set
  train_dataset <- dataset[1:initial_train_size, 1:ncol(dataset)]
  test_dataset <- dataset[(initial_train_size+1):nrow(dataset), 1:ncol(dataset)]
  
  ## Convert Frequency
  new_trainset <- apply(train_dataset, 2, convert_frequency_dataset, new_freq=window_size, mode=mode)
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
  if (schedule_policy != "real") {
    
    new_testset <- NULL
    last_time_schedule <- NULL
    seek_length <- NULL
    
    if (schedule_policy == "disjoint") {
      new_testset <- apply(test_dataset, 2, convert_frequency_dataset, new_freq=window_size, mode=mode)
      rownames(new_trainset) <- seq(1, 1 + window_size * (nrow(new_trainset) - 1), window_size)
      colnames(new_trainset) <- colnames(train_dataset)
      
      last_time_schedule <- nrow(new_testset) - job_length + 1
      seek_length <- job_length
    } else {
      new_testset <- test_dataset
      rownames(new_testset) <- rownames(test_dataset)
      colnames(new_testset) <- colnames(test_dataset)
      
      last_time_schedule <- nrow(new_testset) - job_length * window_size + 1
      seek_length <- job_length * window_size
    }
    
    result_foreground <- sapply(1:ncol(new_testset), scheduling_foreground, test_dataset=new_testset, coeffs=coeffs, means=means, vars=vars, window_size=window_size, job_length=job_length, prob_cut_off=prob_cut_off, cpu_required=cpu_required, granularity=granularity, mode="fixed", seek_length=seek_length, last_time_schedule=last_time_schedule)
    result_model <- sapply(1:ncol(new_testset), scheduling_model, test_dataset=new_testset, means=means, vars=vars, window_size=window_size, prob_cut_off=prob_cut_off, granularity=granularity, mode="fixed", seek_length=seek_length, last_time_schedule=last_time_schedule)
  } else {
    
    new_testset <- test_dataset
    rownames(new_testset) <- rownames(test_dataset)
    colnames(new_testset) <- colnames(test_dataset)
    
    result_foreground <- sapply(1:ncol(new_testset), scheduling_foreground, test_dataset=new_testset, coeffs=coeffs, means=means, vars=vars, window_size=window_size, job_length=job_length, prob_cut_off=prob_cut_off, cpu_required=cpu_required, granularity=granularity, mode="dynamic")
    result_model <- sapply(1:ncol(new_testset), scheduling_model, test_dataset=new_testset, means=means, vars=vars, window_size=window_size, prob_cut_off=prob_cut_off, granularity=granularity, mode="dynamic")
    
    for (i in 1:5) {
      overall_runs <- cbind(overall_runs, unlist(result_model_fixed[2+i,]))
    }
  }
  scheduled_num <- cbind(scheduled_num, unlist(result_foreground[1,]))
  unscheduled_num <- cbind(unscheduled_num, unlist(result_foreground[2,]))
  correct_scheduled_num <- cbind(correct_scheduled_num, unlist(result_foreground[3,]))
  correct_unscheduled_num <- cbind(correct_unscheduled_num, unlist(result_foreground[4,]))
  
  avg_usage <- cbind(avg_usage, unlist(result_model[1,]))
  job_survival <- cbind(job_survival, unlist(result_model[2,]))
  
  ## Change column and row names, N by M
  rownames(scheduled_num) <- colnames(test_dataset)
  colnames(scheduled_num) <- "scheduled_num"
  rownames(unscheduled_num) <- colnames(test_dataset)
  colnames(unscheduled_num) <- "unscheduled_num"
  rownames(correct_scheduled_num) <- colnames(test_dataset)
  colnames(correct_scheduled_num) <- "correct_scheduled_num"
  rownames(correct_unscheduled_num) <- colnames(test_dataset)
  colnames(correct_unscheduled_num) <- "correct_unscheduled_num"
  rownames(avg_usage) <- colnames(test_dataset)
  colnames(avg_usage) <- "avg_usage"
  rownames(job_survival) <- colnames(test_dataset)
  colnames(job_survival) <- "survival"
  rownames(overall_runs) <- colnames(test_dataset)
  colnames(overall_runs) <- sapply(1:5, function(i) as.character(i))
  
  result <- list('avg_usage'=avg_usage, 'job_survival'=job_survival, 'scheduled_num'=scheduled_num, "unscheduled_num"=unscheduled_num, "correct_scheduled_num"=correct_scheduled_num, "correct_unscheduled_num"=correct_unscheduled_num, "overall_runs"=overall_runs)
  return(result)
}


wrapper.epoche <- function(parameter, dataset, cpu_required, initial_train_size, output_dp, schedule_policy) {
  
  window_size <- as.numeric(parameter[1])
  prob_cut_off <- as.numeric(parameter[2])
  granularity <- as.numeric(parameter[3])
  
  output <- svt_stationary_model(datase =dataset, window_size=window_size, job_length=1, cpu_required=cpu_required, prob_cut_off=prob_cut_off, initial_train_size=initial_train_size, mode="max", granularity=granularity, schedule_policy=schedule_policy)
  overall_evaluation <- find_overall_evaluation(output$avg_usage[,1], output$job_survival[,1])
  
  utilization_rate <- overall_evaluation$utilization_rate
  survival_rate <- overall_evaluation$survival_rate
  
  scheduled_num <- sum(output$scheduled_num[,1])
  unscheduled_num <- sum(output$unscheduled_num[,1])
  correct_scheduled_num <- sum(output$correct_scheduled_num[,1])
  correct_unscheduled_num <- mean(output$correct_unscheduled_num[,1])
  
  correct_scheduled_rate <- correct_scheduled_num / scheduled_num
  correct_unscheduled_rate <- correct_unscheduled_num / correct_unscheduled_num
  
  print(paste("Avg cycle used:", "job length", window_size, utilization_rate))
  print(paste("Job survival rate:", "job length", window_size, survival_rate))
  print(paste("Scheduling summary:", "Correct scheduled rate:", correct_scheduled_rate, "Correct unscheduled rate:", correct_unscheduled_rate))
  
  result_path.xlsx <- read.xlsx(output_dp, sheetIndex = 1)
  result_path.xlsx <- update.xlsx.df(result_path.xlsx, "AR1", prob_cut_off, NA, sample_size, window_size, granularity, utilization_rate, survival_rate, correct_scheduled_rate, correct_unscheduled_rate)
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

schedule_policy <- "disjoint"

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
apply(parameter.df, 1, wrapper.epoche, dataset=data_matrix, cpu_required=(100-cpu_required), initial_train_size=initial_train_size, output_dp=output_dp, schedule_policy=schedule_policy)
