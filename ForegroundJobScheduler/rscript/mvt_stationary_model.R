library("ggplot2")
library("ggfortify")
library("dplyr")
library("forecast")
library("mvtnorm")
library("dict")
library("MTS")


initialize_coefficient_matrix <- function(ma_coef, q, predict_size, current_err) {
  initial <- matrix(0, nrow = 2, ncol = 2*(predict_size + q))
  pre_ma <- predict_size - current_err
  initial[1:2, (1+2*pre_ma):(2*(pre_ma+1))] <- diag(nrow = 2, ncol = 2)
  if (q != 0) {
    initial[1:2, (1+2*(pre_ma+1)):(2*q+2*(pre_ma+1))] <- ma_coef
  }
  return(initial)
}

update_dict_matrices <- function(prev, ar_coef) {
  num_small_matrices <- ncol(prev) / 2
  result <- matrix(0, nrow = 2, ncol = ncol(prev))
  for (i in 1:num_small_matrices) {
    multi <- prev[,(1+2*(i-1)):(2*i)]
    result[,(1+2*(i-1)):(2*i)] <- ar_coef %*% multi
  }
  return(result)
}


calculate_var_cov_matrix <-function(p, q, var, predict_size, ar_coef, ma_coef) {
  forecast_var <- dict()
  forecast_var_max <- numvecdict()
  forecast_var_avg <- numvecdict()
  for (i in 1:predict_size) {
    initial <- initialize_coefficient_matrix(ma_coef, q, predict_size, i)
    if (p != 0) {
      for (k in 1:p) {
        if (i > k) {
          ar_multiplier <- ar_coef[,(1+2*(k-1)):(2*k)]
          initial <- initial + update_dict_matrices(forecast_var[[i-k]], ar_multiplier)
        }
      }
    }
    forecast_var[[i]] <- initial
    for (m in 1:(ncol(initial)/2)) {
      forecast_var_max$append_number(i, initial[1,(1+2*(m-1))])
      forecast_var_avg$append_number(i, initial[1,(2*m)])
    }
  }

  var_cov <- matrix(nrow = predict_size, ncol = predict_size)
  var_max <- var[1,1]
  var_avg <- var[2,2]
  cov_max_avg <- var[1,2]
  for (row in 1:predict_size) {
    for (col in 1:predict_size) {
      if (row > col){
        var_cov[row, col] <- var_cov[col, row]
      } else {
        max_coef_row <- forecast_var_max[[row]]
        avg_coef_row <- forecast_var_avg[[row]]
        
        max_coef_col <- forecast_var_max[[col]]
        avg_coef_col <- forecast_var_avg[[col]]
        
        var_cov[row, col] <- sum(max_coef_row * max_coef_col) * var_max + sum(avg_coef_row * avg_coef_col) * var_max + 
          sum(max_coef_row * avg_coef_col) * cov_max_avg + sum(avg_coef_row * max_coef_col) * cov_max_avg
      }
    }
  }
  return(var_cov)
}

calculate_estimates <- function(p, ar_coef, last_obs, predict_size, intercept) {
  estimate <- matrix(nrow = 2, ncol = predict_size)
  if (p == 0) {
    estimate[1,] <- rep(intercept[1,1], predict_size)
    estimate[2,] <- rep(intercept[2,1], predict_size)
    return(estimate)
  } else {
    last_obs <- last_obs
    intercept_extended <- NULL
    for (l in 1:ncol(last_obs)) {
      intercept_extended <- cbind(intercept_extended, intercept)
    }
    last_obs <- last_obs - intercept_extended
    for (i in 1:predict_size) {
      last_ob <- matrix(c(0,0), nrow = 2, ncol = 1)
      for (j in 1:p) {
        ar_coef_matrix <- matrix(nrow = 2, ncol = 2)
        ar_coef_matrix[1,1] <- ar_coef[1,(1+2*(j-1))]
        ar_coef_matrix[1,2] <- ar_coef[1,(2*j)]
        ar_coef_matrix[2,1] <- ar_coef[2,(1+2*(j-1))]
        ar_coef_matrix[2,2] <- ar_coef[2,(2*j)]
        last_ob <- last_ob + ar_coef_matrix %*% last_obs[,j]
      }
      last_obs <- cbind(last_ob, last_obs)
    }
    intercept_extended <- NULL
    for (l in 1:ncol(last_obs)) {
      intercept_extended <- cbind(intercept_extended, intercept)
    }
    last_obs <- last_obs + intercept_extended
    return(last_obs[1,1:predict_size])
  }
}

do_prediction <- function(last_obs, ts_model, predict_size=1, level) {
  p <- ts_model$ARorder
  q <- ts_model$MAorder
  intercept <- matrix(nrow = 2, ncol = 1)
  intercept[1,1] <- as.numeric(ts_model$coef[1,1])
  intercept[2,1] <- as.numeric(ts_model$coef[2,1])
  ar_coef <- matrix(nrow = 2, ncol = 2 * p)
  ma_coef <- matrix(nrow = 2, ncol = 2 * q)
  if (p != 0) {
    for (i in 1:p) {
      ar_coef[1,(1+2*(i-1))] <- as.numeric(ts_model$coef[(i*2),1])
      ar_coef[1,(2*i)] <- as.numeric(ts_model$coef[(i*2+1),1])
      ar_coef[2,(1+2*(i-1))] <- as.numeric(ts_model$coef[(i*2),2])
      ar_coef[2,(2*i)] <- as.numeric(ts_model$coef[(i*2+1),2])
    }
  } else {
    ar_coef = NULL
  }
  if (q != 0) {
    for (j in 1:q) {
      ma_coef[1,(1+2*(j-1))] <- as.numeric(ts_model$coef[((j+p)*2),1])
      ma_coef[1,(2*j)] <- as.numeric(ts_model$coef[((j+p)*2+1),1])
      ma_coef[2,(1+2*(j-1))] <- as.numeric(ts_model$coef[((j+p)*2),2])
      ma_coef[2, (2*j)] <- as.numeric(ts_model$coef[((j+p)*2+1),2])
    }
  } else {
    ma_coef = NULL
  }
  sample_var <- ts_model$Sigma
  mu <- calculate_estimates(p, ar_coef, last_obs, predict_size, intercept)
  varcov <- calculate_var_cov_matrix(p, q, sample_var, predict_size, ar_coef, ma_coef)
  prob <- 1 - pmvnorm(lower = rep(0, predict_size), upper = rep(level, predict_size), mean = mu, sigma = varcov)
  
  result <- list('prob' = as.numeric(prob), 'mu' = mu, 'varcov'=varcov)
  return(result)
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

mvt_stationary_model <- function(dataset_avg, dataset_max, initial_train_size, p, q, job_length=5, cpu_required, prob_cut_off=0.01, update_freq=1, ts_models_import = NULL) {
  #### input dataset_avg: N by M matrix, N being number of observations, M being number of time series
  #### input dataset_max: N by M matrix, N being number of observations, M being number of time series
  #### input initial_train_size: The number of first observations used to train the model'
  #### input p: max order of p
  #### input q: max order of q
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
  scheduling_summary <- data.frame(matrix(nrow = 4, ncol = ncol(dataset_avg)))
  
  ## Dictionaries
  end_time_testing_queue <- numvecdict()
  ts_models <- dict()
  pi_up_bounds <- dict()
  
  ## Summary Counts
  scheduled_num <- rep(0, ncol(dataset_avg))
  unscheduled_num <- rep(0, ncol(dataset_avg))
  falsely_scheduled_num <- rep(0, ncol(dataset_avg))
  falsely_unscheduled_num <- rep(0, ncol(dataset_avg))
  
  ## Train Model
  train_percent <- 0.00
  for (ts_num in 1:ncol(dataset_avg)) {
    if (is.null(ts_models_import)) {
      if (round(ts_num / ncol(dataset_avg), 2) != train_percent) {
        print(paste("Training", train_percent))
        train_percent <- round(ts_num / ncol(dataset_avg), 2)
      }
      uni_data_avg <- dataset_avg[1:initial_train_size, ts_num]
      uni_data_max <- dataset_max[1:initial_train_size, ts_num]
      uni_data_matrix <- matrix(nrow = nrow(dataset_avg), ncol = 2)
      uni_data_matrix[,1] <- uni_data_max
      uni_data_matrix[,2] <- uni_data_avg
      ts_model <- VARMACpp(uni_data_matrix, p, q, include.mean = TRUE, details = FALSE)
      ts_models[[ts_num]] <- ts_model
    } else {
      ts_models[[ts_num]] <- ts_models_import[[ts_num]]
    }
  }
  
  current_end <- initial_train_size
  current_percent <- 0.00
  while (current_end <= nrow(dataset_avg)) {
    
    ## Initialize Model 
    prob_vector <- c()
    prediction <- c()
    actual <- c()
    avg_cycle_used <- c()
    survival <- c()
    
    for (ts_num in 1:ncol(dataset_avg)) {
      
      ## Schedule the job
      if (current_end <= nrow(dataset_avg) - job_length) {
        ts_model <- ts_models[[ts_num]]
        last_obs_max <- dataset_max[(current_end-p+1):current_end, ts_num]
        last_obs_avg <- dataset_avg[(current_end-p+1):current_end, ts_num]
        last_obs <- matrix(nrow = 2, ncol = length(last_obs_max))
        last_obs[1,] <- last_obs_max
        last_obs[2,] <- last_obs_avg
        prediction_result <- do_prediction(last_obs = last_obs, ts_model = ts_model, predict_size = job_length, level = (100 - cpu_required[ts_num]))
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
          
          position_vec <- dataset_avg[start_time:end_time, ts_num]
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
          pi_up <- pi_up_bounds[[paste(ts_num, ",", start_time, sep = "")]]
          evalulation <- find_evaluation(pi_up=pi_up, actual_obs=position_vec, predict_size=job_length)
          avg_cycle_used[ts_num] <- evalulation$avg_usage
          survival[ts_num] <- evalulation$survival
        }
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
    if (current_percent != round((current_end - initial_train_size) / (nrow(dataset_avg) - initial_train_size), digits = 2)) {
      print(paste("Testing", current_percent))
      current_percent <- round((current_end - initial_train_size) / (nrow(dataset_avg) - initial_train_size), digits = 2)
    }
  }
  
  ## Change column and row names, N by M
  colnames(probability) <- colnames(dataset_avg)
  rownames(probability) <- seq(initial_train_size + 1, initial_train_size + 1 + update_freq * (nrow(probability) - 1), update_freq)
  
  colnames(avg_usage) <- colnames(dataset_avg)
  rownames(avg_usage) <- seq(initial_train_size + 1, initial_train_size + 1 + update_freq * (nrow(avg_usage) - 1), update_freq)
  
  colnames(job_survival) <- colnames(dataset_avg)
  rownames(job_survival) <- seq(initial_train_size + 1, initial_train_size + 1 + update_freq * (nrow(job_survival) - 1), update_freq)
  
  colnames(predict_result) <- colnames(dataset_avg)
  rownames(predict_result) <- seq(initial_train_size + 1, initial_train_size + 1 + update_freq * (nrow(predict_result) - 1), update_freq)
  
  colnames(actual_result) <- colnames(dataset_avg)
  rownames(actual_result) <- seq(initial_train_size + 1, initial_train_size + 1 + update_freq * (nrow(actual_result) - 1), update_freq)
  
  colnames(scheduling_summary) <- colnames(dataset_avg)
  scheduling_summary[1,] <- scheduled_num
  scheduling_summary[2,] <- unscheduled_num
  scheduling_summary[3,] <- falsely_scheduled_num
  scheduling_summary[4,] <- falsely_unscheduled_num
  rownames(scheduling_summary) <- c('Scheduled_Num', 'Unscheduled_Num', 'Falsly_scheduled_Num', 'Falsely_unscheduled_Num')
  
  result <- list('prob' = probability, 'predict' = predict_result, 'avg_usage'=avg_usage, 'job_survival'=job_survival, 'actual' = actual_result, 'scheduling_summary' = scheduling_summary, 'ts_models' = ts_models)
  
  return(result)
}

## Read back ground job pool

bg_job_pool_names <- read.csv("C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//pythonscripts//list of sampled background jobs.csv")[,1]
bg_job_pool <- sub(".pd", "", bg_job_pool_names)
bg_jobs_path = "C://Users//carlo//Documents//sample background jobs//"

data_matrix_avg <- matrix(nrow = 4000, ncol = 0)
data_matrix_max <- matrix(nrow = 4000, ncol = 0)
for (job_num in bg_job_pool) {
  bg_job <- read.csv(paste(bg_jobs_path, job_num, ".csv", sep = ""))
  data_matrix_avg <- cbind(data_matrix_avg, bg_job$avg_cpu[4033:8032])
  data_matrix_max <- cbind(data_matrix_max, bg_job$max_cpu[4033:8032])
}
rownames(data_matrix_avg) <- seq(0, 5 * (nrow(data_matrix_avg) - 1),5)
rownames(data_matrix_max) <- seq(0, 5 * (nrow(data_matrix_max) - 1),5)
colnames(data_matrix_avg) <- bg_job_pool
colnames(data_matrix_max) <- bg_job_pool

cpu_required <- rep(0, ncol(data_matrix_max))
for (j in 1:ncol(data_matrix_max)) {
  cpu_required[j] <- as.numeric(quantile(data_matrix_max[,j], c(0.15, 0.5, 0.85), type = 4)[3])
}

for (job_length in c(12)) {
  print(paste("Job_length", job_length))
  
  output <- mvt_stationary_model(dataset_avg=data_matrix_avg, dataset_max = data_matrix_max, p=1, q=0,job_length=job_length, cpu_required=(100-cpu_required), prob_cut_off=0.01, initial_train_size = 2000, update_freq=1)
  write.csv(output$avg_usage, file = paste("VARMA",job_length, "100", 0.01, "avg_usage.csv"))
  print(paste("Avg cycle used:", "job length", job_length, mean(as.matrix(output$avg_usage), na.rm = TRUE)))
  write.csv(output$job_survival, file = paste("VARMA",job_length, "100", 0.01,"job_survival.csv"))
  print(paste("Job survival rate:", "job length", job_length, sum(as.matrix(output$job_survival)) / (length(as.matrix(output$job_survival)))))
  write.csv(output$scheduling_summary, file = paste("VARMA", job_length, "100", 0.01, "scheduling_sum.csv"))
  scheduled_num <- sum(output$scheduling_summary[1,])
  unscheduled_num <- sum(output$scheduling_summary[2,])
  correct_scheduled_num <- scheduled_num - sum(output$scheduling_summary[3,])
  correct_unscheduled_num <- unscheduled_num - sum(output$scheduling_summary[4,])
  print(paste("Scheduling summary:", "Correct scheduled rate:", correct_scheduled_num / scheduled_num, "Correct unscheduled rate:", correct_unscheduled_num / unscheduled_num))
}