library("mvtnorm")
library("dict")
library("MTS")
library("dplyr")
library("xlsx")

source("C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//rscript//helper_functions.R")


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


do_prediction <- function(last_obs, ts_model, predict_size=1, level=NULL) {
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
  
  prob <- NULL
  if (!is.null(level)) {
    prob <- 1 - pmvnorm(lower = rep(0, predict_size), upper = rep(level, predict_size), mean = mu, sigma = varcov)
  }
  result <- list('prob' = as.numeric(prob), 'mu' = mu, 'varcov'=varcov)
  return(result)
}


scheduling_foreground <- function(ts_num, test_dataset_max, test_dataset_avg, ts_models, window_size, job_length, prob_cut_off, cpu_required, granularity, mode, schedule_policy) {
  scheduled_num <- 0
  unscheduled_num <- 0
  correct_scheduled_num <- 0
  correct_unscheduled_num <- 0
  
  seek_length <- window_size * job_length
  last_time_schedule <- nrow(test_dataset_max) - window_size * job_length + 1
  
  ts_model <- ts_models[[ts_num]]
  p <- ts_model$ARorder
  
  update_policy = ifelse(schedule_policy == "disjoint", window_size, 1)
  current_end <- p * window_size + 1
  while (current_end <= last_time_schedule) {
    ## Schedule based on model predictions
    last_obs_max <- convert_frequency_dataset(test_dataset_max[(current_end-p*window_size):(current_end-1), ts_num], window_size, mode=mode)
    last_obs_avg <- convert_frequency_dataset(test_dataset_avg[(current_end-p*window_size):(current_end-1), ts_num], window_size, mode=mode)
    last_obs <- matrix(nrow = 2, ncol = length(last_obs_max))
    if (mode == "max") {
      last_obs[1,] <- last_obs_max
      last_obs[2,] <- last_obs_avg
    } else {
      last_obs[1,] <- last_obs_avg
      last_obs[2,] <- last_obs_max
    }
    
    prediction_result <- do_prediction(last_obs=last_obs, ts_model=ts_model, predict_size=job_length, level=(100-cpu_required[ts_num]))
    prediction <- ifelse(prediction_result$prob <= prob_cut_off, 1, 0)
    scheduled_num <- ifelse(prediction == 1, scheduled_num + 1, scheduled_num)
    unscheduled_num <- ifelse(prediction == 1, unscheduled_num, unscheduled_num + 1)
    
    ## Evalute schedulings based on prediction
    start_time <- current_end
    end_time <- current_end + seek_length - 1
    if (mode == "max") {
      position_vec <- convert_frequency_dataset(test_dataset_max[start_time:end_time, ts_num], window_size, mode)
    } else {
      position_vec <- convert_frequency_dataset(test_dataset_avg[start_time:end_time, ts_num], window_size, mode)
    }
    actual <- ifelse(all(position_vec <= (100-cpu_required[ts_num])), 1, 0)
    correct_scheduled_num <- ifelse(prediction == 1 & actual == 1, correct_scheduled_num + 1, correct_scheduled_num)
    correct_unscheduled_num <- ifelse(prediction == 0 & actual == 0, correct_unscheduled_num + 1, correct_unscheduled_num)
    
    if (schedule_policy == "dynamic") {
      if (prediction == 1) {
        update_policy = ifelse(actual == 1, window_size * job_length, 1)
      } else {
        update_policy = 1
      }
    }
    current_end <- current_end + update_policy
  }
  
  return(list("scheduled_num"=scheduled_num, "unscheduled_num"=unscheduled_num, "correct_scheduled_num"=correct_scheduled_num, "correct_unscheduled_num"=correct_unscheduled_num))
}


scheduling_model <- function(ts_num, test_dataset_max, test_dataset_avg, ts_models, window_size, prob_cut_off, granularity, mode, schedule_policy) {
  utilization <- c()
  survival <- c()
  runs <- rep(0, 5)
  run_counter <- 0
  run_switch <- FALSE
  
  seek_length <- window_size
  last_time_schedule <- nrow(test_dataset_max) - window_size + 1
  
  ts_model=ts_models[[ts_num]]
  p = ts_model$ARorder
  
  current_end <- window_size + 1
  update_policy <- ifelse(schedule_policy == "disjoint", window_size, 1)
  while (current_end <= last_time_schedule) {
    ## Schedule based on model predictions
    last_obs_max <- convert_frequency_dataset(test_dataset_max[(current_end-p*window_size):(current_end-1), ts_num], window_size, mode=mode)
    last_obs_avg <- convert_frequency_dataset(test_dataset_avg[(current_end-p*window_size):(current_end-1), ts_num], window_size, mode=mode)
    last_obs <- matrix(nrow = 2, ncol = length(last_obs_max))
    if (mode == "max") {
      last_obs[1,] <- last_obs_max
      last_obs[2,] <- last_obs_avg
    } else {
      last_obs[1,] <- last_obs_avg
      last_obs[2,] <- last_obs_max
    }
    
    prediction_result <- do_prediction(last_obs=last_obs, ts_model=ts_model, predict_size=1)
    pi_up <- compute_pi_up(mu=prediction_result$mu, varcov=prediction_result$varcov, predict_size=1, prob_cutoff=prob_cut_off, granularity=granularity)
    
    ## Evalute schedulings based on prediction
    start_time <- current_end
    end_time <- current_end + seek_length - 1
    position_vec <- NULL
    if (mode == "max") {
      position_vec <- convert_frequency_dataset(test_dataset_max[start_time:end_time, ts_num], window_size, mode)
    } else {
      position_vec <- convert_frequency_dataset(test_dataset_avg[start_time:end_time, ts_num], window_size, mode)
    }
    evalulation <- find_evaluation(pi_up=pi_up, actual_obs=position_vec, granularity=granularity)
    utilization <- c(utilization, evalulation$usage)
    survival <- c(survival, evalulation$survival)
    
    if (schedule_policy == "dynamic") {
      if (!is.na(evalulation$survival) & evalulation$survival == 1) {
        update_policy <- window_size
        if (run_switch) {
          idx <- ifelse(run_counter > 5, 5, run_counter)
          runs[idx] <- runs[idx] + 1
          run_counter <- 0
          run_switch <- FALSE
        }
      } else if (!is.na(evalulation$survival) & evalulation$survival == 0) {
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


mvt_stationary_model <- function(dataset_avg, dataset_max, initial_train_size, p, q, job_length, window_size, cpu_required, prob_cut_off, update_freq, mode, granularity, schedule_policy) {
  #### input dataset_avg: N by M matrix, N being number of observations, M being number of time series
  #### input dataset_max: N by M matrix, N being number of observations, M being number of time series
  #### input initial_train_size: The number of first observations used to train the model'
  #### input p: max order of p
  #### input q: max order of q
  #### input job_length: The time that the foreground job will be runing
  #### input cpu_required: A vector, the cpu that the foreground job requires in percentage
  #### input prob_cut_off: If the probability of background job exceeding 100-cpu_required is smaller than prob_cut_off, then schedule it. Otherwise, don't.
  #### input update_freq: The number of observations for each update of the model, and do the prediction
  
  if (granularity > 0) {
    cpu_required <- sapply(cpu_required, round_to_nearest, granularity, FALSE)
  }
  
  scheduled_num <- data.frame(matrix(nrow=ncol(dataset_max), ncol=0))
  unscheduled_num <- data.frame(matrix(nrow=ncol(dataset_max), ncol=0))
  correct_scheduled_num <- data.frame(matrix(nrow=ncol(dataset_max), ncol=0))
  correct_unscheduled_num <- data.frame(matrix(nrow=ncol(dataset_max), ncol=0))
  
  avg_usage <- data.frame(matrix(nrow=ncol(dataset_max), ncol=0))
  job_survival <- data.frame(matrix(nrow=ncol(dataset_max), ncol=0))
  overall_runs <- data.frame(matrix(nrow=ncol(dataset_max), ncol = 0))
  
  ## Dictionaries
  ts_models <- dict()

  ## Split in Training and Testing Set
  train_dataset_max <- dataset_max[1:initial_train_size,]
  test_dataset_max <- dataset_max[(initial_train_size+1):nrow(dataset_max),]
  train_dataset_avg <- dataset_avg[1:initial_train_size,]
  test_dataset_avg <- dataset_avg[(initial_train_size+1):nrow(dataset_avg),]
  
  ## Convert Frequency for training set
  new_trainset_max <- apply(train_dataset_max, 2, convert_frequency_dataset, new_freq=window_size, mode=mode)
  rownames(new_trainset_max) <- seq(1, 1 + window_size * (nrow(new_trainset_max) - 1), window_size)
  colnames(new_trainset_max) <- colnames(train_dataset_max)
  
  new_trainset_avg <- apply(train_dataset_avg, 2, convert_frequency_dataset, new_freq=window_size, mode=mode)
  rownames(new_trainset_avg) <- seq(1, 1 + window_size * (nrow(new_trainset_avg) - 1), window_size)
  colnames(new_trainset_avg) <- colnames(train_dataset_avg)
  
  ## Train Model
  for (ts_num in 1:ncol(new_trainset_max)) {
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
    ts_models[[ts_num]] <- VARMACpp(uni_data_matrix, p=p, q=0,include.mean = TRUE)
  }
  
  ## Test Model
  print("Testing on Foreground job:")
  result_foreground <- sapply(1:ncol(dataset_max), scheduling_foreground, test_dataset_max=test_dataset_max, test_dataset_avg=test_dataset_avg, ts_models=ts_models, window_size=window_size, job_length=job_length, prob_cut_off=prob_cut_off, cpu_required=cpu_required, granularity=granularity, mode=mode, schedule_policy=schedule_policy)
  print("Testing on Model:")
  result_model <- sapply(1:ncol(dataset_max), scheduling_model, test_dataset_max=test_dataset_max, test_dataset_avg=test_dataset_avg, ts_models=ts_models, window_size=window_size, prob_cut_off=prob_cut_off, granularity=granularity, mode=mode, schedule_policy=schedule_policy)
  
  if (schedule_policy == "dynamic") {
    for (i in 1:5) {
      overall_runs <- cbind(overall_runs, unlist(result_model[2+i,]))
    }
  } 
  scheduled_num <- cbind(scheduled_num, unlist(result_foreground[1,]))
  unscheduled_num <- cbind(unscheduled_num, unlist(result_foreground[2,]))
  correct_scheduled_num <- cbind(correct_scheduled_num, unlist(result_foreground[3,]))
  correct_unscheduled_num <- cbind(correct_unscheduled_num, unlist(result_foreground[4,]))
  
  avg_usage <- cbind(avg_usage, unlist(result_model[1,]))
  job_survival <- cbind(job_survival, unlist(result_model[2,]))
  
  ## Change column and row names, N by M
  rownames(scheduled_num) <- colnames(dataset_max)
  colnames(scheduled_num) <- "scheduled_num"
  rownames(unscheduled_num) <- colnames(dataset_max)
  colnames(unscheduled_num) <- "unscheduled_num"
  rownames(correct_scheduled_num) <- colnames(dataset_max)
  colnames(correct_scheduled_num) <- "correct_scheduled_num"
  rownames(correct_unscheduled_num) <- colnames(dataset_max)
  colnames(correct_unscheduled_num) <- "correct_unscheduled_num"
  rownames(avg_usage) <- colnames(dataset_max)
  colnames(avg_usage) <- "avg_usage"
  rownames(job_survival) <- colnames(dataset_max)
  colnames(job_survival) <- "survival"
  if (schedule_policy == "dynamic") {
    rownames(overall_runs) <- colnames(dataset_max)
    colnames(overall_runs) <- sapply(1:5, function(i) as.character(i))
    result <- list('avg_usage'=avg_usage, 'job_survival'=job_survival, 'scheduled_num'=scheduled_num, "unscheduled_num"=unscheduled_num, "correct_scheduled_num"=correct_scheduled_num, "correct_unscheduled_num"=correct_unscheduled_num, "overall_runs"=overall_runs)
    return(result)  
  } else {
    result <- list('avg_usage'=avg_usage, 'job_survival'=job_survival, 'scheduled_num'=scheduled_num, "unscheduled_num"=unscheduled_num, "correct_scheduled_num"=correct_scheduled_num, "correct_unscheduled_num"=correct_unscheduled_num)
    return(result)
  }
  return(result)
}


wrapper.epoche <- function(parameter, dataset_avg, dataset_max, cpu_required, initial_train_size, update_freq, output_dp, schedule_policy) {
  
  window_size <- as.numeric(parameter[1])
  prob_cut_off <- as.numeric(parameter[2])
  granularity <- as.numeric(parameter[3])
  
  output <- mvt_stationary_model(dataset_avg=dataset_avg, dataset_max=dataset_max, p=1, q=0, job_length=1, window_size=window_size, cpu_required=cpu_required, prob_cut_off=prob_cut_off, initial_train_size=initial_train_size, update_freq=1, mode="max", granularity=granularity, schedule_policy=schedule_policy)
  overall_evaluation <- find_overall_evaluation(output$avg_usage, output$job_survival)
  
  utilization_rate <- overall_evaluation$utilization_rate
  survival_rate <- overall_evaluation$survival_rate
  
  scheduled_num <- sum(output$scheduled_num[,1])
  unscheduled_num <- sum(output$unscheduled_num[,1])
  correct_scheduled_num <- sum(output$correct_scheduled_num[,1])
  correct_unscheduled_num <- sum(output$correct_unscheduled_num[,1])
  
  correct_scheduled_rate <- correct_scheduled_num / scheduled_num
  correct_unscheduled_rate <- correct_unscheduled_num / unscheduled_num
  
  print(paste("Avg cycle used:", "job length", window_size, utilization_rate))
  print(paste("Job survival rate:", "job length", window_size, survival_rate))
  print(paste("Scheduling summary:", "Correct scheduled rate:", correct_scheduled_rate, "Correct unscheduled rate:", correct_unscheduled_rate))
  
  result_path.xlsx <- read.xlsx(output_dp, sheetIndex = 1)
  result_path.xlsx <- update.xlsx.df(result_path.xlsx, "VAR1", prob_cut_off, NA, sample_size, window_size, granularity, avg_utilization, survival, correct_scheduled_rate, correct_unscheduled_rate)
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

schedule_policy <- "dynamic"

bg_jobs_path = "C://Users//carlo//Documents//sample background jobs//"
bg_job_pool <- NULL
if (sample_size == 100 ) {
  bg_job_pool <- read.csv("C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//pythonscripts//list of sampled 100 background jobs.csv")[,1]
  bg_job_pool <- sub(".pd", "", bg_job_pool)
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
slt <- apply(parameter.df, 1, wrapper.epoche, dataset_avg=data_matrix_avg, dataset_max=data_matrix_max, cpu_required=(100-cpu_required), initial_train_size=initial_train_size, update_freq=1, output_dp=output_dp, schedule_policy=schedule_policy)
