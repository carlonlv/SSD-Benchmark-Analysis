
if (Sys.info()["sysname"] == "Windows") {
  source("C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//rscript//helper_functions.R")
} else if (Sys.info()["sysname"] == "Darwin") {
  source("/Users/carlonlv/Documents/Github/Research-Projects/ForegroundJobScheduler/rscript/helper_functions.R")
} else {
  source("/home/jialun/Research-Projects/ForegroundJobScheduler/rscript/helper_functions.R")
}

cores <- ifelse(Sys.info()["sysname"] == "Windows", 1, detectCores(all.tests = FALSE, logical = TRUE))


train_mvt_model <- function(train_dataset_max, train_dataset_avg, p, q) {
  
  uni_data_matrix <- matrix(nrow = length(train_dataset_max), ncol = 2)
  uni_data_matrix[,1] <- train_dataset_max
  uni_data_matrix[,2] <- train_dataset_avg
  return(suppressWarnings(suppressMessages(VARMACpp(uni_data_matrix, p=p, q=q, include.mean = TRUE))))
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
  return(list('prob' = as.numeric(prob), 'mu' = mu, 'varcov'=varcov))
}


scheduling_foreground <- function(test_dataset_max, test_dataset_avg, ts_model, window_size, prob_cut_off, cpu_required, granularity, schedule_policy) {
  
  cpu_required <- ifelse(granularity>0, round_to_nearest(cpu_required, granularity, FALSE), cpu_required)
  
  scheduled_num <- 0
  unscheduled_num <- 0
  correct_scheduled_num <- 0
  correct_unscheduled_num <- 0
  
  last_time_schedule <- length(test_dataset_max) - window_size + 1

  p <- ts_model$ARorder
  
  update_policy = ifelse(schedule_policy == "disjoint", window_size, 1)
  current_end <- window_size + 1
  while (current_end <= last_time_schedule) {
    ## Schedule based on model predictions
    last_obs_max <- convert_frequency_dataset(test_dataset_max[(current_end-p*window_size):(current_end-1)], window_size, mode="max")
    last_obs_avg <- convert_frequency_dataset(test_dataset_avg[(current_end-p*window_size):(current_end-1)], window_size, mode="avg")
    last_obs <- matrix(nrow = 2, ncol = length(last_obs_max))
    last_obs[1,] <- last_obs_max
    last_obs[2,] <- last_obs_avg
    
    prediction_result <- do_prediction(last_obs=last_obs, ts_model=ts_model, predict_size=1, level=(100-cpu_required))
    prediction <- ifelse(prediction_result$prob <= prob_cut_off, 1, 0)
    scheduled_num <- ifelse(prediction == 1, scheduled_num + 1, scheduled_num)
    unscheduled_num <- ifelse(prediction == 1, unscheduled_num, unscheduled_num + 1)
    
    ## Evalute schedulings based on prediction
    start_time <- current_end
    end_time <- current_end + window_size - 1
    position_vec <- convert_frequency_dataset(test_dataset_max[start_time:end_time], window_size, mode = "max")
    actual <- ifelse(all(position_vec <= (100-cpu_required)), 1, 0)
    correct_scheduled_num <- ifelse(prediction == 1 & actual == 1, correct_scheduled_num + 1, correct_scheduled_num)
    correct_unscheduled_num <- ifelse(prediction == 0 & actual == 0, correct_unscheduled_num + 1, correct_unscheduled_num)
    
    if (schedule_policy == "dynamic") {
      if (prediction == 1) {
        update_policy = ifelse(actual == 1, window_size, 1)
      } else {
        update_policy = 1
      }
    }
    current_end <- current_end + update_policy
  }
  
  return(list("scheduled_num"=scheduled_num, "unscheduled_num"=unscheduled_num, "correct_scheduled_num"=correct_scheduled_num, "correct_unscheduled_num"=correct_unscheduled_num))
}


scheduling_model <- function(test_dataset_max, test_dataset_avg, ts_model, window_size, prob_cut_off, granularity, schedule_policy, adjustment) {
  
  run_switch <- FALSE
  
  last_time_schedule <- length(test_dataset_max) - window_size + 1
  p <- ts_model$ARorder
  
  current_end <- window_size + 1
  update_policy <- ifelse(schedule_policy == "disjoint", window_size, 1)
  
  pi_ups <- c()
  
  utilization <- c()
  survival <- c()
  while (current_end <= last_time_schedule) {
    ## Schedule based on model predictions
    last_obs_max <- convert_frequency_dataset(test_dataset_max[(current_end-p*window_size):(current_end-1)], window_size, mode="max")
    last_obs_avg <- convert_frequency_dataset(test_dataset_avg[(current_end-p*window_size):(current_end-1)], window_size, mode="avg")
    last_obs <- matrix(nrow = 2, ncol = length(last_obs_max))
    last_obs[1,] <- last_obs_max
    last_obs[2,] <- last_obs_avg
    
    prediction_result <- do_prediction(last_obs=last_obs, ts_model=ts_model, predict_size=1)
    pi_up <- compute_pi_up(prediction_result$mu, prediction_result$varcov, 1, prob_cut_off, granularity)
    pi_ups <- c(pi_ups, pi_up)
    
    ## Evaluate schedulings based on prediction
    start_time <- current_end
    end_time <- current_end + window_size - 1
    
    utilization <- c(utilization, check_utilization(pi_up, granularity))
    survival <- c(survival, check_survival(pi_up, test_dataset_max[start_time:end_time], granularity))
    
    if (schedule_policy == "dynamic") {
      if (!is.na(survival[length(survival)]) & survival[length(survival)] == 0) {
        update_policy <- window_size
        if (run_switch) {
          run_switch <- FALSE
        } 
      } else if (is.na(survival[length(survival)])){
        update_policy <- 1
        if (!run_switch) {
          run_switch <- TRUE
        }
      } else {
        update_policy <- survival[length(survival)]
        if (!run_switch) {
          run_switch <- TRUE
        } else {
          survival[length(survival)] <- ifelse(adjustment, NA, survival[length(survival)])
        }
      }
    }
    current_end <- current_end + update_policy
  }
  
  overall_survival <- compute_survival(ifelse(is.na(survival), NA, ifelse(survival == 0, 1, 0)))
  overall_utilization <- compute_utilization(pi_ups, survival, test_dataset_max[(window_size+1):(current_end-update_policy+window_size-1)], window_size, granularity, schedule_policy)
  return(list("util_numerator"=overall_utilization$numerator, "util_denominator"=overall_utilization$denominator, "sur_numerator"=overall_survival$numerator, "sur_denominator"=overall_survival$denominator))
}


svt_model <- function(ts_num, dataset_max, dataset_avg, train_size, window_size, update_freq, prob_cut_off, cpu_required, granularity, schedule_policy="disjoint", adjustment) {
  
  dataset_max <- dataset_max[,ts_num]
  dataset_avg <- dataset_avg[,ts_num]
  cpu_required <- cpu_required[ts_num]
  
  scheduled_num <- 0
  unscheduled_num <- 0
  correct_scheduled_num <- 0
  correct_unscheduled_num <- 0
  
  util_numerator <- 0
  util_denominator<- 0
  sur_numerator <- 0
  sur_denominator <- 0
  
  current <- 1
  last_time_update <- length(dataset_max) - update_freq - train_size + 1
  while (current <= last_time_update) {
    ## Split into train set and test set
    train_set_max <- dataset_max[current:(current+train_size-1)]
    train_set_avg <- dataset_avg[current:(current+train_size-1)]
    test_set_max <- dataset_max[(current+train_size+1):(current+train_size+update_freq)]
    test_set_avg <- dataset_avg[(current+train_size+1):(current+train_size+update_freq)]
    
    ## Convert Frequency for training set
    new_trainset_max <- convert_frequency_dataset(train_set_max, window_size, "max")
    new_trainset_avg <- convert_frequency_dataset(train_set_avg, window_size, "avg")
    
    ## Train Model
    trained_result <- train_mvt_model(new_trainset_max, new_trainset_avg, p=1, q=0)
    
    ## Test Model
    result_foreground <- scheduling_foreground(test_set_max, test_set_avg, trained_result, window_size, prob_cut_off, cpu_required, granularity, schedule_policy)
    result_model <- scheduling_model(test_set_max, test_set_avg, trained_result, window_size, prob_cut_off, granularity, schedule_policy, adjustment)
    
    ## Write Result
    scheduled_num <- scheduled_num + result_foreground$scheduled_num
    unscheduled_num <- unscheduled_num + result_foreground$unscheduled_num
    correct_scheduled_num <- correct_scheduled_num + result_foreground$correct_scheduled_num
    correct_unscheduled_num <- correct_unscheduled_num + result_foreground$correct_unscheduled_num
    
    util_numerator <- util_numerator + ifelse(is.na(result_model$util_numerator), 0, result_model$util_numerator)
    util_denominator <- util_denominator + ifelse(is.na(result_model$util_denominator), 0, result_model$util_denominator)
    sur_numerator <- sur_numerator + ifelse(is.na(result_model$sur_numerator), 0, result_model$sur_numerator)
    sur_denominator <- sur_denominator + ifelse(is.na(result_model$sur_denominator), 0, result_model$sur_denominator)
    
    ## Update current
    current <- current + update_freq
  }
  
  return(list("scheduled_num"=scheduled_num, "unscheduled_num"=unscheduled_num, "correct_scheduled_num"=correct_scheduled_num, "correct_unscheduled_num"=correct_unscheduled_num, "util_numerator"=util_numerator, "util_denominator"=util_denominator, "sur_numerator"=sur_numerator, "sur_denominator"=sur_denominator))
}


svt_stationary_model <- function(dataset_max, dataset_avg, train_size, window_size, update_freq, prob_cut_off, cpu_required, granularity, schedule_policy="disjoint", adjustment) {
  
  scheduled_num <- c()
  unscheduled_num <- c()
  correct_scheduled_num <- c()
  correct_unscheduled_num <- c()
  
  util_numerator <- c()
  util_denominator <- c()
  sur_numerator <- c()
  sur_denominator <- c()
  
  ts_names <- colnames(dataset_max)
  
  result <- mclapply(1:length(ts_names), svt_model, dataset_max, dataset_avg, train_size, window_size, update_freq, prob_cut_off, cpu_required, granularity, schedule_policy, adjustment, mc.cores=cores)
  
  for (ts_num in 1:length(ts_names)) {
    scheduled_num <- c(scheduled_num, result[[ts_num]]$scheduled_num)
    unscheduled_num <- c(unscheduled_num, result[[ts_num]]$unscheduled_num)
    correct_scheduled_num <- c(correct_scheduled_num, result[[ts_num]]$correct_scheduled_num)
    correct_unscheduled_num <- c(correct_unscheduled_num, result[[ts_num]]$correct_unscheduled_num)
    
    util_numerator <- c(util_numerator, result[[ts_num]]$util_numerator)
    util_denominator <- c(util_denominator, result[[ts_num]]$util_denominator)
    sur_numerator <- c(sur_numerator, result[[ts_num]]$sur_numerator)
    sur_denominator <- c(sur_denominator, result[[ts_num]]$sur_denominator)
  }
  
  schedule_decision <- data.frame("scheduled_num"=scheduled_num, "unscheduled_num"=unscheduled_num, "correct_scheduled_num"=correct_scheduled_num, "correct_unscheduled_num"=correct_unscheduled_num)
  rownames(schedule_decision) <- ts_names
  
  avg_usage <- data.frame("numerator"=util_numerator, "denominator"=util_denominator)
  rownames(avg_usage) <- ts_names
  
  job_survival <- data.frame("numerator"=sur_numerator, "denominator"=sur_denominator)
  rownames(job_survival) <- ts_names
  
  return(list('usage'=avg_usage, 'survival'=job_survival, 'schedule'=schedule_decision))  
}


wrapper.epoche <- function(parameter, dataset_max, dataset_avg, cpu_required, output_dp, schedule_policy, sample_size, write_result, write_result_path, adjustment) {
  
  window_size <- as.numeric(parameter["window_size"])
  prob_cut_off <- as.numeric(parameter["prob_cut_off"])
  granularity <- as.numeric(parameter["granularity"])
  train_size <- as.numeric(parameter["train_size"])
  update_freq <- as.numeric(parameter["update_freq"])

  print(paste("Job len:", window_size))
  print(paste("Cut off prob:", prob_cut_off))
  print(paste("Granularity:", granularity))
  print(paste("Train Size:", train_size))
  print(paste("Update Freq:", update_freq))
  
  print(system.time(output <- svt_stationary_model(dataset_max, dataset_avg, train_size, window_size, update_freq, prob_cut_off, cpu_required, granularity, schedule_policy, adjustment)))
  
  overall_evaluation <- find_overall_evaluation(output$usage$numerator, output$usage$denominator, output$survival$numerator, output$survival$denominator)

  avg_utilization <- overall_evaluation$avg_utilization
  avg_survival <- overall_evaluation$avg_survival
  agg_utilization <- overall_evaluation$agg_utilization
  agg_survival <- overall_evaluation$agg_survival
  
  scheduled_num <- sum(output$schedule$scheduled_num)
  unscheduled_num <- sum(output$schedule$unscheduled_num)
  correct_scheduled_num <- sum(output$schedule$correct_scheduled_num)
  correct_unscheduled_num <- sum(output$schedule$correct_unscheduled_num)
  
  correct_scheduled_rate <- correct_scheduled_num / scheduled_num
  correct_unscheduled_rate <- correct_unscheduled_num / unscheduled_num
  
  print(paste("Avg cycle used mode:", "job length", window_size, avg_utilization))
  print(paste("Agg cycle used mode:", "job length", window_size, agg_utilization))
  print(paste("Avg job survival rate:", "job length", window_size, avg_survival))
  print(paste("Agg job survival rate:", "job length", window_size, agg_survival))
  
  if (write_result == TRUE) {
    ts_results <- data.frame("utilization"=(output$usage$numerator/output$usage$denominator),
                             "survival"=(output$survival$numerator/output$survival$denominator),
                             "correct_scheduled_rate"=(output$schedule$correct_scheduled_num / (output$schedule$scheduled_num)),
                             "correct_unscheduled_rate"=(output$schedule$correct_unscheduled_num / (output$schedule$unscheduled_num)))
    rownames(ts_results) <- colnames(dataset_max)
    result_file_name <- paste("VAR1", schedule_policy, adjustment, 0, prob_cut_off, granularity, window_size, 0, train_size, update_freq)
    write.csv(ts_results, file = paste0(write_result_path, result_file_name, ".csv"), row.names = TRUE)
  }
  
  result_path.csv <- read.csv(output_dp)
  result_path.csv <- update.df.online(result_path.csv, 
                                      "VAR1", 
                                      prob_cut_off, 
                                      0, 
                                      sample_size, 
                                      window_size, 
                                      granularity, 
                                      0, 
                                      train_size, 
                                      update_freq, 
                                      avg_utilization, 
                                      agg_utilization, 
                                      avg_survival,
                                      agg_survival,
                                      correct_scheduled_rate, 
                                      correct_unscheduled_rate)
  write.csv(result_path.csv, file = output_dp, row.names = FALSE)
}
