library("ggplot2")
library("ggfortify")
library("dplyr")
library("forecast")
library("mvtnorm")
library("dict")


cov_cal <- function(variance, l, k, phi) {
  #### input variance: A vector of length min(l,k), var(an+min(l,k)) to var(an+1)
  #### input l: row number of var-cov matrix, minimal value 1
  #### input k: column number of var-cov matrix, minimal value 1
  #### input phi: coeff of AR1 model
  
  phi_mul <- phi^(seq((l+k-2*min(l,k)), l+k-2, by=2))
  result <- sum(phi_mul*variance)
  return(result)
}

calculate_var_cov_matrix_ar1 <-function(var, l, phi) {
  #### input var: A vector from var(an+l) to var(an+1) of length l
  #### input l: number of prediction
  #### input phi: coeff of AR1 model
  var_cov <- matrix(nrow = l, ncol = l)
  for (i in 1:l) {
    for (j in 1:l) {
      variance <- var[seq(l-min(i,j)+1, l, by=1)]
      var_cov[i, j] <- cov_cal(variance, i, j, phi)
    }
  }
  return(var_cov)
}

do_prediction <- function(model_selection="AR1", last_obs, phi, mean, variance, predict_size, level) {
  if (model_selection == "AR1") {
    # Construct mean
    mu <- rep(last_obs, predict_size)
    mu <- mu * phi^(1:predict_size) + (1 - phi^(1:predict_size)) * mean
    # Construct Var-cov matrix
    var <- rep(variance, predict_size)
    var_cov <- calculate_var_cov_matrix_ar1(var, predict_size, phi)
    # caclulate probability
    up_bound <- rep(level, predict_size)
    lower_bound <- rep(0, predict_size)
    prob <- 1 - pmvnorm(upper = up_bound, lower = lower_bound, mean = mu, sigma = var_cov)
    return(prob)
    
  } else {
    return(NULL)
  }
}

find_pi_upperbound <- function(model_selection='AR1', last_obs, phi, mean, variance, predict_size, prob_cutoff) {
  if (model_selection == "AR1") {
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
    
  } else {
    return(NULL)
  }
}

mvt_stationary_model <- function(dataset, initial_train_size = round(length(dataset) / 4, 1),job_length=5, cpu_required=rep(10, ncol(dataset)), prob_cut_off=0.01, update_freq=1) {
  #### input dataset: N by M matrix, N being number of observations, M being number of time series
  #### input initial_train_size: The number of first observations used to train the model
  #### input job_length: The time that the foreground job will be runing
  #### input cpu_required: A vector, the cpu that the foreground job requires in percentage
  #### input prob_cut_off: If the probability of background job exceeding 100-cpu_required is smaller than prob_cut_off, then schedule it. Otherwise, don't.
  #### input update_freq: The number of observations for each update of the model, and do the prediction
  
  ## N by M dataframe
  probability <- data.frame(row.names = 1)
  ## N by M dataframe
  pi_upper_bound <- data.frame(row.names = 1)
  ep_upper_bound <- data.frame(row.names = 1)
  ## N by M dataframe
  predict_result <- data.frame(row.names = 1)
  ## N by M dataframe
  actual_result <- data.frame(row.names = 1)
  ## 4 by M dataframe
  scheduling_summary <- data.frame(matrix(nrow = 4, ncol = ncol(dataset)))
  
  ## Dictionaries
  end_time_testing_queue <- numvecdict()
  
  ## Summary Counts
  scheduled_num <- rep(0, ncol(dataset))
  unscheduled_num <- rep(0, ncol(dataset))
  falsely_scheduled_num <- rep(0, ncol(dataset))
  falsely_unscheduled_num <- rep(0, ncol(dataset))
  
  
  ## Train Model
  coeffs <- rep(NA, ncol(dataset))
  means <- rep(NA, ncol(dataset))
  vars <- rep(NA, ncol(dataset))
  for (ts_num in 1:ncol(dataset)) {
    ts_model <- arima(dataset[1:initial_train_size, ts_num], order = c(1,0,0), include.mean = TRUE)
    coeffs[ts_num] <- as.numeric(ts_model$coef[1])
    means[ts_num] <- as.numeric(ts_model$coef[2])
    vars[ts_num] <- ts_model$sigma2
  }
  current_end <- initial_train_size
  current_percent <- 0.00
  while (current_end < nrow(dataset)) {
    
    ## Initialize Model 
    prob_vector <- c()
    pi_upper <- c()
    ep_up <- c()
    prediction <- c()
    actual <- c()
    
    for (ts_num in 1:ncol(dataset)) {
      
      ## Schedule the job
      last_obs <- dataset[current_end, ts_num]
      prob_vector[ts_num] <- do_prediction(last_obs = last_obs, phi = coeffs[ts_num], mean = means[ts_num], variance = vars[ts_num],predict_size = job_length, level = (100 - cpu_required[ts_num]))
      upper_bound <- find_pi_upperbound(last_obs = last_obs, phi = coeffs[ts_num], mean = means[ts_num], variance = vars[ts_num], predict_size = job_length, prob_cutoff = prob_cut_off)
      pi_upper[ts_num] <- upper_bound
      current_dataset <- dataset[,ts_num]
      ep_up[ts_num] <- round(length(current_dataset[current_dataset <= upper_bound]) / length(current_dataset), 3)
      if (prob_vector[ts_num] < prob_cut_off) {
        prediction[ts_num] <- 1
        scheduled_num[ts_num] <- scheduled_num[ts_num] + 1
      } else {
        prediction[ts_num] <- 0
        unscheduled_num[ts_num] <- unscheduled_num[ts_num] + 1
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
      }
    }
    
    ## Store probability
    probability <- rbind(probability, prob_vector)
    ## Store empirical probability of upper bound
    pi_upper_bound <- rbind(pi_upper_bound, pi_upper)
    ep_upper_bound <- rbind(ep_upper_bound, ep_up)
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
      print(current_percent)
      current_percent <- round((current_end - initial_train_size) / (nrow(dataset) - initial_train_size), digits = 2)
    }
  }
  
  ## Change column and row names, N by M
  colnames(probability) <- colnames(dataset)
  rownames(probability) <- seq(initial_train_size + 1, initial_train_size + 1 + update_freq * (nrow(probability) - 1), update_freq)
  
  colnames(pi_upper_bound) <- colnames(dataset)
  rownames(ep_upper_bound) <- seq(initial_train_size + 1, initial_train_size + 1 + update_freq * (nrow(pi_upper_bound) - 1), update_freq)
  
  colnames(ep_upper_bound) <- colnames(dataset)
  rownames(ep_upper_bound) <- seq(initial_train_size + 1, initial_train_size + 1 + update_freq * (nrow(ep_upper_bound) - 1), update_freq)
  
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
  
  result <- list('prob' = probability, 'predict' = predict_result, 'piup'=pi_upper_bound ,'epup'= ep_upper_bound, 'actual' = actual_result, 'scheduling_summary' = scheduling_summary)
  
  return(result)
}


## Read back ground job pool

bg_job_pool_names <- read.csv("C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//pythonscripts//list of sampled background jobs.csv")[,1]
bg_job_pool <- sub(".pd", "", bg_job_pool_names)
bg_jobs_path = "C://Users//carlo//Documents//datasets//csvalldata//background_jobs//"

data_matrix <- matrix(nrow = 4000, ncol = 0)
for (job_num in bg_job_pool) {
  bg_job <- read.csv(paste(bg_jobs_path, job_num, ".csv", sep = ""))
  data_matrix <- cbind(data_matrix, bg_job$max_cpu[4033:8032])
}
rownames(data_matrix) <- seq(0, 5 * (nrow(data_matrix) - 1),5)
colnames(data_matrix) <- bg_job_pool

result <- dict()
total_scheduled_jobs_count <- rep(0,3)
total_unscheduled_jobs_count <- rep(0,3)
total_falsely_scheduled_count <- rep(0,3)
total_falsely_unscheduled_count <- rep(0,3)
for (job_length in c(1)) {
  print(paste("Job_length", job_length))
  
  ## cpu_required = 100 - 85th quantile
  cpu_required1 <- rep(0, length(bg_job_pool))
  ## cpu_required = 100 - median
  cpu_required2 <- rep(0, length(bg_job_pool))
  ## cpu_required = 100 - 15th quantile
  cpu_required3 <- rep(0, length(bg_job_pool))
  for (j in 1:ncol(data_matrix)) {
    cpu_required1[j] <- as.numeric(quantile(data_matrix[,j], c(0.15, 0.5, 0.85), type = 4)[1])
    cpu_required2[j] <- as.numeric(quantile(data_matrix[,j], c(0.15, 0.5, 0.85), type = 4)[2])
    cpu_required3[j] <- as.numeric(quantile(data_matrix[,j], c(0.15, 0.5, 0.85), type = 4)[3])
  }
  output1 <- mvt_stationary_model(dataset=data_matrix, job_length=job_length, cpu_required=100-cpu_required1, prob_cut_off=0.01, initial_train_size = 2000, update_freq=1)
  result[[as.integer(paste(job_length, 1, sep = ""))]] <- output1
  total_scheduled_jobs_count[1] <- total_scheduled_jobs_count[1] + sum(output1$scheduling_summary[1,])
  total_unscheduled_jobs_count[1] <- total_unscheduled_jobs_count[1] + sum(output1$scheduling_summary[2,])
  total_falsely_scheduled_count[1] <- total_falsely_scheduled_count[1] + sum(output1$scheduling_summary[3,])
  total_falsely_unscheduled_count[1] <- total_falsely_unscheduled_count[1] + sum(output1$scheduling_summary[4,])
  write.csv(output1$scheduling_summary, file = paste(job_length, 1, "summary.csv", sep = ""))
  write.csv(output1$epup, file = paste(job_length, 1, "empiricalprob.csv", sep = ""))
  write.csv(output1$piup, file = paste(job_length, 1, "piupperbound.csv", sep = ""))
  
  output2 <- mvt_stationary_model(dataset=data_matrix, job_length=job_length, cpu_required=100-cpu_required2, prob_cut_off=0.01, initial_train_size = 2000, update_freq=1)
  result[[as.integer(paste(job_length, 2, sep = ""))]] <- output2
  total_scheduled_jobs_count[1] <- total_scheduled_jobs_count[1] + sum(output2$scheduling_summary[1,])
  total_unscheduled_jobs_count[1] <- total_unscheduled_jobs_count[1] + sum(output2$scheduling_summary[2,])
  total_falsely_scheduled_count[1] <- total_falsely_scheduled_count[1] + sum(output2$scheduling_summary[3,])
  total_falsely_unscheduled_count[1] <- total_falsely_unscheduled_count[1] + sum(output2$scheduling_summary[4,])
  write.csv(output2$scheduling_summary, file = paste(job_length, 2, "summary.csv", sep = ""))
  write.csv(output2$epup, file = paste(job_length, 2, "empiricalprob.csv", sep = ""))
  write.csv(output2$piup, file = paste(job_length, 2, "piupperbound.csv", sep = ""))
  
  output3 <- mvt_stationary_model(dataset=data_matrix, job_length=job_length, cpu_required=100-cpu_required3, prob_cut_off=0.01, initial_train_size = 2000, update_freq=1)
  result[[as.integer(paste(job_length, 3, sep = ""))]] <- output3
  total_scheduled_jobs_count[1] <- total_scheduled_jobs_count[1] + sum(output3$scheduling_summary[1,])
  total_unscheduled_jobs_count[1] <- total_unscheduled_jobs_count[1] + sum(output3$scheduling_summary[2,])
  total_falsely_scheduled_count[1] <- total_falsely_scheduled_count[1] + sum(output3$scheduling_summary[3,])
  total_falsely_unscheduled_count[1] <- total_falsely_unscheduled_count[1] + sum(output3$scheduling_summary[4,])
  write.csv(output3$scheduling_summary, file = paste(job_length, 3, "summary.csv", sep = ""))
  write.csv(output3$epup, file = paste(job_length, 3, "empiricalprob.csv", sep = ""))
  write.csv(output3$piup, file = paste(job_length, 3, "piupperbound.csv", sep = ""))
}