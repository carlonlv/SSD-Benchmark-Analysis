library("mvtnorm")
library("dict")
library("MTS")

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

find_evaluation <- function(pi_up, actual_obs) {
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

mvt_stationary_model <- function(dataset_avg, dataset_max, initial_train_size, p, q, job_length=5, cpu_required, prob_cut_off=0.01, update_freq=1, ts_models_import = NULL, mode = "max") {
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
  
  ## Split in Training and Testing Set
  train_dataset_max <- dataset_max[1:initial_train_size,]
  test_dataset_max <- dataset_max[(initial_train_size+1):nrow(dataset_max),]
  train_dataset_avg <- dataset_avg[1:initial_train_size,]
  test_dataset_avg <- dataset_avg[(initial_train_size+1):nrow(dataset_avg),]
  
  ## Convert Frequency
  new_trainset_max <- matrix(nrow = floor(nrow(train_dataset_max) / window_size), ncol = 0)
  new_testset_max <- matrix(nrow = floor(nrow(test_dataset_max) / window_size), ncol = 0)
  new_trainset_avg <- matrix(nrow = floor(nrow(train_dataset_avg) / window_size), ncol = 0)
  new_testset_avg <- matrix(nrow = floor(nrow(train_dataset_avg) / window_size), ncol = 0)
  for (ts_num in 1:ncol(train_dataset_max)) {
    converted_data <- convert_frequency_dataset(train_dataset_max[, ts_num], window_size, "max")
    new_trainset_max <- cbind(new_trainset_max, converted_data)
    converted_data <- convert_frequency_dataset(test_dataset_max[, ts_num], window_size, "max")
    new_testset_max <- cbind(new_testset_max, converted_data)
    
    converted_data <- convert_frequency_dataset(train_dataset_avg[, ts_num], window_size, "avg")
    new_trainset_avg <- cbind(new_trainset_avg, converted_data)
    converted_data <- convert_frequency_dataset(test_dataset_avg[, ts_num], window_size, "avg")
    new_testset_avg <- cbind(new_testset_avg, converted_data)
  }
  rownames(new_trainset_max) <- seq(1, 1 + window_size * (nrow(new_trainset_max) - 1), window_size)
  colnames(new_trainset_max) <- colnames(train_dataset_max)
  rownames(new_testset_max) <- seq(initial_train_size + 1, initial_train_size + 1 + (nrow(new_testset_max) - 1), 1)
  colnames(new_testset_max) <- colnames(test_dataset_max)
  rownames(new_trainset_avg) <- seq(1, 1 + window_size * (nrow(new_trainset_avg) - 1), window_size)
  colnames(new_trainset_avg) <- colnames(train_dataset_avg)
  rownames(new_testset_avg) <- seq(initial_train_size + 1, initial_train_size + 1 + (nrow(new_testset_avg) - 1), 1)
  colnames(new_testset_avg) <- colnames(test_dataset_avg)
  
  ## Train Model
  train_percent <- 0.00
  for (ts_num in 1:ncol(new_trainset_max)) {
    if (is.null(ts_models_import)) {
      if (round(ts_num / ncol(new_trainset_max), 2) != train_percent) {
        print(paste("Training", train_percent))
        train_percent <- round(ts_num / ncol(new_trainset_max), 2)
      }
      uni_data_max <- new_trainset_max[, ts_num]
      uni_data_avg <- new_trainset_avg[, ts_num]
      uni_data_matrix <- matrix(nrow = nrow(new_trainset_max), ncol = 2)
      
      if (mode == "max") {
        uni_data_matrix[,1] <- uni_data_max
        uni_data_matrix[,2] <- uni_data_avg
      } else {
        uni_data_matrix[,1] <- uni_data_avg
        uni_data_matrix[,2] <- uni_data_max
      }
      ts_model <- VARMACpp(uni_data_matrix, p=p, q=0,include.mean = TRUE)
      ts_models[[ts_num]] <- ts_model
    } else {
      ts_models[[ts_num]] <- ts_models_import[[ts_num]]
    }
  }
  
  current_end <- p + 1
  current_percent <- 0.00
  while (current_end <= nrow(new_testset_max)) {
    
    ## Initialize Model 
    prob_vector <- c()
    prediction <- c()
    
    for (ts_num in 1:ncol(new_testset_max)) {
      
      ## Schedule the job
      if (current_end <= (nrow(new_testset_max) - job_length + 1)) {
        ts_model <- ts_models[[ts_num]]
        last_obs_max <- new_testset_max[(current_end-p):(current_end-1), ts_num]
        last_obs_avg <- new_testset_avg[(current_end-p):(current_end-1), ts_num]
        last_obs <- matrix(nrow = 2, ncol = length(last_obs_max))
        
        if (mode == "max") {
          last_obs[1,] <- last_obs_max
          last_obs[2,] <- last_obs_avg
        } else {
          last_obs[1,] <- last_obs_avg
          last_obs[2,] <- last_obs_max
        }
        
        prediction_result <- do_prediction(last_obs = last_obs, ts_model = ts_model, predict_size = job_length, level = (100 - cpu_required[ts_num]))
        prob_vector[ts_num] <- prediction_result$prob
        if (prob_vector[ts_num] < prob_cut_off) {
          prediction[ts_num] <- 1
          scheduled_num[ts_num] <- scheduled_num[ts_num] + 1
        } else {
          prediction[ts_num] <- 0
          unscheduled_num[ts_num] <- unscheduled_num[ts_num] + 1
        }
        pi_up_bounds[[paste(ts_num, ",", current_end, sep = "")]] <- compute_pi_up(mu=prediction_result$mu, varcov=prediction_result$varcov, predict_size=job_length, prob_cutoff=prob_cut_off)
      }
    }
    
    ## Store probability
    probability <- rbind(probability, prob_vector)
    ## Store Prediction
    predict_result <- rbind(predict_result, prediction)
    
    ## Queue the jobs to check their correctness later
    closest_update <- NA
    if (job_length == 1) {
      closest_update <- current_end
    } else {
      closest_update <- current_end + ceiling(job_length / update_freq) * update_freq
    }
    end_time_testing_queue$append_number(closest_update, current_end)
    end_time_testing_queue$append_number(closest_update, current_end + job_length - 1)
    end_time_testing_queue$append_number(closest_update, nrow(predict_result))
    
    actual <- c()
    avg_cycle_used <- c()
    survival <- c()
    
    ## Check correctness of previous schedulings
    for (ts_num in 1:ncol(new_testset_max)) {
      if (length(end_time_testing_queue[[current_end]]) != 0) {
        info_lst <- end_time_testing_queue[[current_end]]
        lst_len <- length(info_lst)
        for (i in seq(1, lst_len, 3)) {
          start_time <- info_lst[i]
          end_time <- info_lst[i+1]
          row_num <- info_lst[i+2]
          
          position_vec <- NULL
          if (mode == "max") {
            position_vec <- new_testset_max[start_time:end_time, ts_num]
          } else {
            position_vec <- new_testset_avg[start_time:end_time, ts_num]
          }
          
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
          evalulation <- find_evaluation(pi_up=pi_up, actual_obs=position_vec)
          avg_cycle_used[ts_num] <- evalulation$avg_usage
          survival[ts_num] <- evalulation$survival
        }
      }
    }
    
    ## Store Actual
    actual_result <- rbind(actual_result, actual)
    
    ## Store Evaluations
    avg_usage <- rbind(avg_usage, avg_cycle_used)
    job_survival <- rbind(job_survival, survival)
    
    ## Update current_end
    current_end <- current_end + update_freq
    if (current_percent != round((current_end - p - 1) / (nrow(new_testset_max) - job_length + 1 - p), digits = 2)) {
      print(paste("Testing", current_percent))
      current_percent <- round((current_end - p - 1) / (nrow(new_testset_max) - job_length + 1 - p), digits = 2)
    }
  }
  
  ## Change column and row names, N by M
  colnames(probability) <- colnames(new_testset_max)
  rownames(probability) <- seq(initial_train_size + p *window_size + 1, initial_train_size + p *window_size + 1 + (update_freq * window_size) * (nrow(probability) - 1), update_freq * window_size)
  
  colnames(avg_usage) <- colnames(new_testset_max)
  rownames(avg_usage) <- seq(initial_train_size + p *window_size + 1, initial_train_size + p *window_size + 1 + (update_freq * window_size) * (nrow(avg_usage) - 1), update_freq * window_size)
  
  colnames(job_survival) <- colnames(new_testset_max)
  rownames(job_survival) <- seq(initial_train_size + p *window_size + 1, initial_train_size + p *window_size + 1 + (update_freq * window_size) * (nrow(job_survival) - 1), update_freq * window_size)
  
  colnames(predict_result) <- colnames(new_testset_max)
  rownames(predict_result) <- seq(initial_train_size + p *window_size + 1, initial_train_size + p *window_size + 1 + (update_freq * window_size) * (nrow(predict_result) - 1), update_freq * window_size)
  
  colnames(actual_result) <- colnames(new_testset_max)
  rownames(actual_result) <- seq(initial_train_size + p *window_size + 1, initial_train_size + p *window_size + 1 + (update_freq * window_size) * (nrow(actual_result) - 1), update_freq * window_size)
  
  colnames(scheduling_summary) <- colnames(new_testset_max)
  scheduling_summary[1,] <- scheduled_num
  scheduling_summary[2,] <- unscheduled_num
  scheduling_summary[3,] <- falsely_scheduled_num
  scheduling_summary[4,] <- falsely_unscheduled_num
  rownames(scheduling_summary) <- c('Scheduled_Num', 'Unscheduled_Num', 'Falsly_scheduled_Num', 'Falsely_unscheduled_Num')
  
  result <- list('prob' = probability, 'predict' = predict_result, 'avg_usage'=avg_usage, 'job_survival'=job_survival, 'actual' = actual_result, 'scheduling_summary' = scheduling_summary, 'ts_models' = ts_models)
  
  return(result)
}

## Read back ground job pool

arg <- commandArgs(trailingOnly = TRUE)
sample_size <- 3000
window_size <- 12
job_length <- 1
cpu_usage <- 3
prob_cut_off <- 0.1
total_trace_length <- 8000
initial_train_size <- 6000
mode <- 'max'

cat(arg, sep = "\n")

bg_jobs_path = "C://Users//carlo//Documents//sample background jobs//"
bg_job_pool <- NULL
if (sample_size == 100 ) {
  bg_job_pool <- read.csv("C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//pythonscripts//list of sampled 100 bg jobs.csv")[,2]
} else {
  bg_job_pool <- read.csv("C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//pythonscripts//list of sampled background jobs.csv")[,1]
  bg_job_pool <- sub(".pd", "", bg_job_pool)
}

data_matrix_avg <- matrix(nrow = total_trace_length, ncol = 0)
data_matrix_max <- matrix(nrow = total_trace_length, ncol = 0)
for (job_num in bg_job_pool) {
  bg_job <- read.csv(paste(bg_jobs_path, job_num, ".csv", sep = ""))
  data_matrix_avg <- cbind(data_matrix_avg, bg_job$avg_cpu[1:total_trace_length])
  data_matrix_max <- cbind(data_matrix_max, bg_job$max_cpu[1:total_trace_length])
}
rownames(data_matrix_avg) <- seq(1, nrow(data_matrix_avg) ,1)
rownames(data_matrix_max) <- seq(1, nrow(data_matrix_max) ,1)
colnames(data_matrix_avg) <- bg_job_pool
colnames(data_matrix_max) <- bg_job_pool

cpu_required <- rep(0, ncol(data_matrix_max))
for (j in 1:ncol(data_matrix_max)) {
  cpu_required[j] <- as.numeric(quantile(data_matrix_max[,j], c(0.15, 0.5, 0.85), type = 4)[cpu_usage])
}

output <- mvt_stationary_model(dataset_avg=data_matrix_avg, dataset_max = data_matrix_max, p=1, q=0,job_length=job_length, cpu_required=(100-cpu_required), prob_cut_off=prob_cut_off, initial_train_size = initial_train_size, update_freq=1, mode = mode)
write.csv(output$avg_usage, file = paste("VARMA",window_size, sample_size, prob_cut_off, "avg_usage.csv"))
print(paste("Avg cycle used:", "job length", window_size, mean(as.matrix(output$avg_usage), na.rm = TRUE)))
write.csv(output$job_survival, file = paste("VARMA",window_size, sample_size, prob_cut_off,"job_survival.csv"))
print(paste("Job survival rate:", "job length", window_size, sum(as.matrix(output$job_survival)) / (length(as.matrix(output$job_survival)))))
write.csv(output$scheduling_summary, file = paste("VARMA", window_size, sample_size, prob_cut_off, "scheduling_sum.csv"))
scheduled_num <- sum(output$scheduling_summary[1,])
unscheduled_num <- sum(output$scheduling_summary[2,])
correct_scheduled_num <- scheduled_num - sum(output$scheduling_summary[3,])
correct_unscheduled_num <- unscheduled_num - sum(output$scheduling_summary[4,])
print(paste("Scheduling summary:", "Correct scheduled rate:", correct_scheduled_num / scheduled_num, "Correct unscheduled rate:", correct_unscheduled_num / unscheduled_num))