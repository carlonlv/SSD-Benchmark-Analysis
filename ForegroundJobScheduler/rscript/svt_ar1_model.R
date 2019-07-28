library("ggplot2")
library("ggfortify")
library("dplyr")
library("forecast")
library("mvtnorm")
library("dict")

convert_frequency_dataset <- function(dataset, new_freq, mode) {
  new_avg_cpu <- c()
  window_num <- NULL
  window_num <- floor(length(dataset) / new_freq)
  for (i in 1:window_num) {
    from <- (i - 1) * new_freq + 1
    to <- i * new_freq
    new_val <- NULL
    if (mode == 'max') {
      new_val <- max(dataset[from:to], na.rm = TRUE)
    } else {
      new_val <- mean(dataset[from:to], na.rm = TRUE)
    }
    new_avg_cpu <- c(new_avg_cpu, new_val)
  }
  return(new_avg_cpu)
}

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

find_pi_upperbound <- function(model_selection='AR1', last_obs, phi, mean, variance, predict_size, prob_cutoff) {
  # Construct mean
  mu <- rep(last_obs, predict_size)
  mu <- mu * phi^(1:predict_size) + (1 - phi^(1:predict_size)) * mean
  var <- rep(variance, predict_size)
  var_cov <- calculate_var_cov_matrix_ar1(var, predict_size, phi)
  upper_bounds <- rep(NA, predict_size)
  for (i in 1:length(upper_bounds)) {
    upper_bounds[i] <- min(mu[i] + qnorm((1-prob_cutoff), 0, 1) * sqrt(var_cov[i,i]), 100)
  }
  return(max(upper_bounds))
}


compute_pi_up <- function(mu, varcov, predict_size, prob_cutoff) {
  upper_bounds <- rep(NA, predict_size)
  for (i in 1:predict_size) {
    upper_bounds[i] <- min(mu[i] + qnorm((1-prob_cutoff), 0, 1) * sqrt(varcov[i,i]), 100)
  }
  return(upper_bounds)
}

find_evaluation <- function(pi_up, actual_obs, predict_size) {
  usage <- (100 - pi_up) / (100 - actual_obs)
  if (all(actual_obs <= pi_up)) {
    survival = 1
  } else {
    survival = 0
  }
  avg_usage <- mean(usage[!is.infinite(usage) & !is.na(usage) & usage <= 1])
  result <- list('avg_usage' = avg_usage, 'survival'= survival)
  return(result)
}

svt_stationary_model <- function(dataset, initial_train_size, window_size, job_length=5, cpu_required, prob_cut_off=0.01, update_freq=1) {
  #### input dataset: N by M matrix, N being number of observations, M being number of time series
  #### input initial_train_size: The number of first observations used to train the model
  #### input job_length: The time that the foreground job will be runing
  #### input cpu_required: A vector, the cpu that the foreground job requires in percentage
  #### input prob_cut_off: If the probability of background job exceeding 100-cpu_required is smaller than prob_cut_off, then schedule it. Otherwise, don't.
  #### input update_freq: The number of observations for each update of the model, and do the prediction
  
  ## N by M dataframe
  probability <- data.frame(row.names = 1)
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
  
  ## Convert Frequency
  new_dat <- matrix(nrow = floor(initial_train_size / window_size), ncol = 0)
  for (ts_num in 1:ncol(dataset)) {
    converted_data <- convert_frequency_dataset(dataset[1:initial_train_size, ts_num], window_size, 'max')
    new_dat <- cbind(new_dat, converted_data)
  }
  rownames(new_dat) <- seq(1, 1 + (window_size * 5) * (nrow(new_dat) - 1), (window_size * 5))
  colnames(new_dat) <- colnames(dataset)
  
  ## Train Model
  coeffs <- rep(NA, ncol(new_dat))
  means <- rep(NA, ncol(new_dat))
  vars <- rep(NA, ncol(new_dat))
  train_percent <- 0.00
  for (ts_num in 1:ncol(new_dat)) {
    if (round(ts_num / ncol(new_dat), 2) != train_percent) {
      print(paste("Training", train_percent))
      train_percent <- round(ts_num / ncol(new_dat), 2)
    }
    ts_model <- arima(new_dat[1:floor(initial_train_size / window_size), ts_num], order = c(1,0,0), include.mean = TRUE)
    coeffs[ts_num] <- as.numeric(ts_model$coef[1])
    means[ts_num] <- as.numeric(ts_model$coef[2])
    vars[ts_num] <- ts_model$sigma2
  }
  
  ## Test Model
  current_end <- initial_train_size
  current_percent <- 0.00
  while (current_end <= nrow(dataset)) {
    
    ## Initialize Model 
    prob_vector <- c()
    avg_cycle_used <- c()
    survival <- c()
    prediction <- c()
    actual <- c()
    avg_cycle_used <- c()
    survival <- c()
    
    for (ts_num in 1:ncol(dataset)) {
      
      ## Schedule the job
      if (current_end < nrow(dataset)) {
        last_obs <- dataset[current_end, ts_num]
        prediction_result <- do_prediction(last_obs = last_obs, phi = coeffs[ts_num], mean = means[ts_num], variance = vars[ts_num],predict_size = job_length, level = (100 - cpu_required[ts_num]))
        prob_vector[ts_num] <- prediction_result$prob
        if (prob_vector[ts_num] < prob_cut_off) {
          prediction[ts_num] <- 1
          scheduled_num[ts_num] <- scheduled_num[ts_num] + 1
        } else {
          prediction[ts_num] <- 0
          unscheduled_num[ts_num] <- unscheduled_num[ts_num] + 1
        }
        pi_up_bounds[[paste(ts_num, ",", (current_end + 1), sep = "")]] <- compute_pi_up(mu=prediction_result$mu, varcov=prediction_result$varcov, predict_size=job_length, prob_cutoff=prob_cut_off)
      }
      
      ## Check correctness of previous schedulings
      if (length(end_time_testing_queue[[current_end]]) != 0) {
        info_lst <- end_time_testing_queue[[current_end]]
        lst_len <- length(info_lst)
        for (i in seq(1, lst_len, 3)) {
          start_time <- info_lst[i]
          end_time <- info_lst[i+1]
          row_num <- info_lst[i+2]
          
          position_vec <- dataset[start_time:end_time, ts_num]
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
        }
        pi_up <- pi_up_bounds[[paste(ts_num, ",", start_time, sep = "")]]
        evalulation <- find_evaluation(pi_up=pi_up, actual_obs=position_vec, predict_size=job_length)
        avg_cycle_used[ts_num] <- evalulation$avg_usage
        survival[ts_num] <- evalulation$survival
      }
    }
    
    ## Store probability
    probability <- rbind(probability, prob_vector)
    ## Store Evaluations
    avg_usage <- rbind(avg_usage, avg_cycle_used)
    job_survival <- rbind(job_survival, survival)
    ## Store Prediction
    predict_result <- rbind(predict_result, prediction)
    ## Store Actual
    actual_result <- rbind(actual_result, actual)
    
    ## Queue the jobs to check their correctness later
    closest_update <- current_end + ceiling(job_length / update_freq) * update_freq
    end_time_testing_queue$append_number(closest_update, current_end + 1)
    end_time_testing_queue$append_number(closest_update, current_end + job_length)
    end_time_testing_queue$append_number(closest_update, nrow(predict_result))
    
    ## Update current_end
    current_end <- current_end + update_freq
    if (current_percent != round((current_end - initial_train_size) / (nrow(dataset) - initial_train_size), digits = 2)) {
      print(paste("Testing", current_percent))
      current_percent <- round((current_end - initial_train_size) / (nrow(dataset) - initial_train_size), digits = 2)
    }
  }
  
  ## Change column and row names, N by M
  colnames(probability) <- colnames(dataset)
  rownames(probability) <- seq(initial_train_size + 1, initial_train_size + 1 + update_freq * (nrow(probability) - 1), update_freq)
  
  colnames(avg_usage) <- colnames(dataset)
  rownames(avg_usage) <- seq(initial_train_size + 1, initial_train_size + 1 + update_freq * (nrow(avg_usage) - 1), update_freq)
  
  colnames(job_survival) <- colnames(dataset)
  rownames(job_survival) <- seq(initial_train_size + 1, initial_train_size + 1 + update_freq * (nrow(job_survival) - 1), update_freq)
  
  colnames(predict_result) <- colnames(dataset)
  rownames(predict_result) <- seq(initial_train_size + 1, initial_train_size + 1 + update_freq * (nrow(predict_result) - 1), update_freq)
  
  colnames(actual_result) <- colnames(dataset)
  rownames(actual_result) <- seq(initial_train_size + 1, initial_train_size + 1 + update_freq * (nrow(actual_result) - 1), update_freq)
  
  colnames(scheduling_summary) <- colnames(dataset)
  scheduling_summary[1,] <- scheduled_num
  scheduling_summary[2,] <- unscheduled_num
  scheduling_summary[3,] <- falsely_scheduled_num
  scheduling_summary[4,] <- falsely_unscheduled_num
  rownames(scheduling_summary) <- c('Scheduled_Num', 'Unscheduled_Num', 'Falsly_scheduled_Num', 'Falsely_unscheduled_Num')
  
  result <- list('prob' = probability, 'predict' = predict_result, 'avg_usage'=avg_usage, "job_survival"=job_survival, 'actual' = actual_result, 'scheduling_summary' = scheduling_summary)
  
  return(result)
}


## Read back ground job pool

bg_job_pool <- read.csv("C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//pythonscripts//list of sampled 100 bg jobs.csv")[,2]
bg_jobs_path = "C://Users//carlo//Documents//sample background jobs//"

data_matrix <- matrix(nrow = 4000, ncol = 0)
for (job_num in bg_job_pool) {
  bg_job <- read.csv(paste(bg_jobs_path, job_num, ".csv", sep = ""))
  data_matrix <- cbind(data_matrix, bg_job$avg_cpu[4033:8032])
}
rownames(data_matrix) <- seq(1, 1 + 5 * (nrow(data_matrix) - 1),5)
colnames(data_matrix) <- bg_job_pool

cpu_required <- rep(0, ncol(data_matrix))
for (j in 1:ncol(data_matrix)) {
  cpu_required[j] <- as.numeric(quantile(data_matrix[,j], c(0.15, 0.5, 0.85), type = 4)[3])
}

for (job_length in c(2)) {
  print(paste("Job_length", job_length))
  
  output <- svt_stationary_model(dataset=data_matrix, job_length=job_length, window_size = 6, cpu_required=(100-cpu_required), prob_cut_off=0.01, initial_train_size = 2000, update_freq=1)
  write.csv(output$avg_usage, file = paste("avgs AR1(6)", job_length, "100", 0.01, "avg_usage.csv"))
  print(paste("Avg cycle used:", "job length", job_length, mean(as.matrix(output$avg_usage), na.rm = TRUE)))
  write.csv(output$job_survival, file = paste("avgs AR1(6)", job_length, "100", 0.01,"job_survival.csv"))
  print(paste("Job survival rate:", "job length", job_length, sum(as.matrix(output$job_survival)) / (length(as.matrix(output$job_survival)))))
  write.csv(output$scheduling_summary, file = paste("avgs AR1(6)", job_length, "100", 0.01, "scheduling_sum.csv"))
  scheduled_num <- sum(output$scheduling_summary[1,])
  unscheduled_num <- sum(output$scheduling_summary[2,])
  correct_scheduled_num <- scheduled_num - sum(output$scheduling_summary[3,])
  correct_unscheduled_num <- unscheduled_num - sum(output$scheduling_summary[4,])
  print(paste("Scheduling summary:", "Correct scheduled rate:", correct_scheduled_num / scheduled_num, "Correct unscheduled rate:", correct_unscheduled_num / unscheduled_num))
}