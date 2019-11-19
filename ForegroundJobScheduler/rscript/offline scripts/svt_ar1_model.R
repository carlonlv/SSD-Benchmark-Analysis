
if (Sys.info()["sysname"] == "Windows") {
  source("C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//rscript//helper_functions.R")
} else if (Sys.info()["sysname"] == "Darwin") {
  source("/Users/carlonlv/Documents/Github/Research-Projects/ForegroundJobScheduler/rscript/helper_functions.R")
} else {
  source("/home/jialun/Research-Projects/ForegroundJobScheduler/rscript/helper_functions.R")
}

cores <- ifelse(Sys.info()["sysname"] == "Windows", 1, detectCores(all.tests = FALSE, logical = TRUE))


calculate_var_cov_matrix_ar1 <-function(var, l, phi) {
  #### input var: A vector from var(an+l) to var(an+1) of length l
  #### input l: number of prediction
  #### input phi: coeff of AR1 model
  
  dm=abs(outer(1:l,1:l,"-"))
  var_cov <- matrix(var[outer(1:l,1:l,"pmin")],l,l)*phi^dm
  return(var_cov)
}


do_prediction <- function(last_obs, phi, mean, variance, predict_size, level=NULL) {
  
  # Construct mean
  mu <- rep(last_obs, predict_size)
  mu <- mu * phi^(1:predict_size) + (1 - phi^(1:predict_size)) * mean
  # Construct Var-cov matrix
  var <- cumsum((phi^2)^(0:(predict_size-1)))*variance
  varcov <- calculate_var_cov_matrix_ar1(var, predict_size, phi)
  # caclulate probability
  prob <- NULL
  if (!is.null(level)) {
    prob <- 1 - pmvnorm(lower = rep(0, predict_size), upper = rep(level, predict_size), mean = mu, sigma = varcov)
  }
  return(list("prob"=as.numeric(prob), "mu"=mu, "varcov"=varcov))
}


train_ar1_model <- function(ts_num, train_dataset) {
  
  ts_model <- tryCatch({
    arima(x=train_dataset[, ts_num], order = c(1,0,0), include.mean = TRUE, method = "CSS-ML", optim.control = list(maxit=2000))
  }, error = function(cond) {
    return(arima(x=train_dataset[, ts_num], order = c(1,0,0), include.mean = TRUE, method = "ML", optim.control = list(maxit=2000)))
  })
  return(list("coeffs"=as.numeric(ts_model$coef[1]), "means"= as.numeric(ts_model$coef[2]), "vars"=ts_model$sigma2))
}


scheduling_foreground <- function(ts_num, test_dataset, coeffs, means, vars, window_size, prob_cut_off, cpu_required, granularity, schedule_policy, mode) {
  
  if (granularity > 0) {
    cpu_required <- round_to_nearest(cpu_required[ts_num], granularity, FALSE)
  } else {
    cpu_required <- cpu_required[ts_num]
  }
  
  scheduled_num <- 0
  unscheduled_num <- 0
  correct_scheduled_num <- 0
  correct_unscheduled_num <- 0
  
  last_time_schedule <- nrow(test_dataset) - window_size + 1

  update_policy = ifelse(schedule_policy == "disjoint", window_size, 1)
  current_end <- window_size + 1
  while (current_end <= last_time_schedule) {
    ## Schedule based on model predictions
    last_obs <- convert_frequency_dataset(test_dataset[(current_end-window_size):(current_end-1), ts_num], window_size, mode = mode)
    prediction_result <- do_prediction(last_obs, coeffs[ts_num], means[ts_num], vars[ts_num], 1, level=(100-cpu_required))
    prediction <- ifelse(prediction_result$prob <= prob_cut_off, 1, 0)
    scheduled_num <- ifelse(prediction == 1, scheduled_num + 1, scheduled_num)
    unscheduled_num <- ifelse(prediction == 1, unscheduled_num, unscheduled_num + 1)
    
    ## Evalute schedulings based on prediction
    start_time <- current_end
    end_time <- current_end + window_size - 1
    position_vec <- convert_frequency_dataset(test_dataset[start_time:end_time, ts_num], window_size, mode = mode)
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


scheduling_model <- function(ts_num, test_dataset, coeffs, means, vars, window_size, prob_cut_off, granularity, max_run_length=25, schedule_policy, mode, adjustment) {
  
  runs <- rep(0, max_run_length)
  run_counter <- 0
  run_switch <- FALSE
  
  last_time_schedule <- nrow(test_dataset) - window_size + 1
  
  current_end <- window_size + 1
  update_policy <- ifelse(schedule_policy == "disjoint", window_size, 1)
  
  pi_ups <- c()
  
  utilization <- c()
  survival <- c()
  while (current_end <= last_time_schedule) {
    ## Schedule based on model predictions
    last_obs <- convert_frequency_dataset(test_dataset[(current_end-window_size):(current_end-1), ts_num], window_size, mode = mode)
    prediction_result <- do_prediction(last_obs=last_obs, phi=coeffs[ts_num], mean=means[ts_num], variance=vars[ts_num], predict_size=1)
    pi_up <- compute_pi_up(mu=prediction_result$mu, varcov=prediction_result$varcov, predict_size=1, prob_cutoff=prob_cut_off, granularity=granularity)
    pi_ups <- c(pi_ups, pi_up)
    ## Evalute schedulings based on prediction
    start_time <- current_end
    end_time <- current_end + window_size - 1
    
    utilization <- c(utilization, check_utilization(pi_up, granularity))
    survival <- c(survival, check_survival(pi_up, test_dataset[start_time:end_time, ts_num], granularity))
    
    if (schedule_policy == "dynamic") {
      if (!is.na(survival[length(survival)]) & survival[length(survival)] == 0) {
        update_policy <- window_size
        if (run_switch) {
          idx <- ifelse(run_counter > max_run_length, max_run_length, run_counter)
          runs[idx] <- runs[idx] + 1
          run_counter <- 0
          run_switch <- FALSE
        } 
      } else if (is.na(survival[length(survival)])){
        update_policy <- 1
        if (!run_switch) {
          run_switch <- TRUE
        }
        run_counter <- run_counter + 1
      } else {
        update_policy <- survival[length(survival)]
        if (!run_switch) {
          run_switch <- TRUE
        } else {
          survival[length(survival)] <- ifelse(adjustment, NA, survival[length(survival)])
        }
        run_counter <- run_counter + 1
      }
    }
    current_end <- current_end + update_policy
  }
  
  overall_survival <- compute_survival(ifelse(is.na(survival), NA, ifelse(survival == 0, 1, 0)))
  overall_utilization <- compute_utilization(pi_ups, survival, test_dataset[(window_size+1):(current_end-update_policy+window_size-1), ts_num], window_size, granularity, schedule_policy)
  return(list("utilization1"=overall_utilization$utilization1, "utilization2"=overall_utilization$utilization2, "survival"=overall_survival$survival, "run"=runs))
}


svt_stationary_model <- function(dataset, initial_train_size, window_size, prob_cut_off, max_run_length, cpu_required, granularity, mode, schedule_policy="disjoint", adjustment) {
  #### input dataset: N by M matrix, N being number of observations, M being number of time series
  #### input initial_train_size: The number of first observations used to train the model
  #### input window_size: The number of observations used to train and predict
  #### input cpu_required: A vector, the cpu that the foreground job requires in percentage
  #### input prob_cut_off: If the probability of background job exceeding 100-cpu_required is smaller than prob_cut_off, then schedule it. Otherwise, don't.
  #### input mode: max or avg
  #### input granularity:
  #### input schedule_policy: dynamic, disjoint, overlap
  
  ts_names <- colnames(dataset)
  
  scheduled_num <- data.frame(matrix(nrow=length(ts_names), ncol=0))
  unscheduled_num <- data.frame(matrix(nrow=length(ts_names), ncol=0))
  correct_scheduled_num <- data.frame(matrix(nrow=length(ts_names), ncol=0))
  correct_unscheduled_num <- data.frame(matrix(nrow=length(ts_names), ncol=0))
  
  avg_usage <- data.frame()
  job_survival <- data.frame()
  overall_runs <- data.frame()
  
  ## Split in Training and Testing Set
  train_dataset <- dataset[1:initial_train_size, 1:length(ts_names)]
  test_dataset <- dataset[(initial_train_size+1):nrow(dataset), 1:length(ts_names)]
  
  ## Convert Frequency for training set
  new_trainset <- apply(train_dataset, 2, convert_frequency_dataset, new_freq=window_size, mode=mode)
  rownames(new_trainset) <- seq(1, 1 + window_size * (nrow(new_trainset) - 1), window_size)
  colnames(new_trainset) <- ts_names
  
  ## Train Model
  print("Training:")
  train_result <- mclapply(1:length(ts_names), train_ar1_model, new_trainset, mc.cores=cores)
  coeffs <- c()
  means <- c()
  vars <- c()
  for (ts_num in 1:length(ts_names)) {
    coeffs <- c(coeffs, train_result[[ts_num]]$coeffs)
    means <- c(means, train_result[[ts_num]]$means)
    vars <- c(vars, train_result[[ts_num]]$vars)
  }
  
  ## Test Model
  print("Testing on Foreground job:")
  result_foreground <- mclapply(1:length(ts_names), scheduling_foreground, test_dataset, coeffs, means, vars, window_size, prob_cut_off, cpu_required, granularity, schedule_policy, mode, mc.cores=cores)
  print("Testing on Model:")
  result_model <- mclapply(1:length(ts_names), scheduling_model, test_dataset, coeffs, means, vars, window_size, prob_cut_off, granularity, max_run_length, schedule_policy, mode, adjustment, mc.cores=cores)
  
  for (ts_num in 1:length(ts_names)) {
    scheduled_num <- rbind(scheduled_num, result_foreground[[ts_num]]$scheduled_num)
    unscheduled_num <- rbind(unscheduled_num, result_foreground[[ts_num]]$unscheduled_num)
    correct_scheduled_num <- rbind(correct_scheduled_num, result_foreground[[ts_num]]$correct_scheduled_num)
    correct_unscheduled_num <- rbind(correct_unscheduled_num, result_foreground[[ts_num]]$correct_unscheduled_num)
    
    avg_usage <- rbind(avg_usage, c(result_model[[ts_num]]$utilization1, result_model[[ts_num]]$utilization2))
    job_survival <- rbind(job_survival, result_model[[ts_num]]$survival)
    if (schedule_policy == "dynamic") {
      overall_runs <- rbind(overall_runs, result_model[[ts_num]]$run)
    }
  }
  
  rownames(scheduled_num) <- ts_names
  colnames(scheduled_num) <- "scheduled_num"
  rownames(unscheduled_num) <- ts_names
  colnames(unscheduled_num) <- "unscheduled_num"
  rownames(correct_scheduled_num) <- ts_names
  colnames(correct_scheduled_num) <- "correct_scheduled_num"
  rownames(correct_unscheduled_num) <- ts_names
  colnames(correct_unscheduled_num) <- "correct_unscheduled_num"
  rownames(avg_usage) <- ts_names
  colnames(avg_usage) <- c("avg_usage1", "avg_usage2")
  rownames(job_survival) <- ts_names
  colnames(job_survival) <- "survival"
  if (schedule_policy == "dynamic") {
    rownames(overall_runs) <- ts_names
    colnames(overall_runs) <- sapply(1:max_run_length, function(i) as.character(i))
    result <- list('avg_usage'=avg_usage, 'job_survival'=job_survival, 'scheduled_num'=scheduled_num, "unscheduled_num"=unscheduled_num, "correct_scheduled_num"=correct_scheduled_num, "correct_unscheduled_num"=correct_unscheduled_num, "overall_runs"=overall_runs)
    return(result)  
  } else {
    result <- list('avg_usage'=avg_usage, 'job_survival'=job_survival, 'scheduled_num'=scheduled_num, "unscheduled_num"=unscheduled_num, "correct_scheduled_num"=correct_scheduled_num, "correct_unscheduled_num"=correct_unscheduled_num)
    return(result)
  }
}


wrapper.epoche <- function(parameter, dataset, cpu_required, initial_train_size, max_run_length, output_dp, schedule_policy, adjustment, write_result, write_result_path=NULL) {
  
  window_size <- as.numeric(parameter["window_size"])
  prob_cut_off <- as.numeric(parameter["prob_cut_off"])
  granularity <- as.numeric(parameter["granularity"])

  print(paste("Job len:", window_size))
  print(paste("Cut off prob:", prob_cut_off))
  print(paste("Granularity:", granularity))
  
  output <- svt_stationary_model(dataset, initial_train_size, window_size, prob_cut_off, max_run_length, cpu_required, granularity, "max", schedule_policy, adjustment)
  overall_evaluation <- find_overall_evaluation(output$avg_usage[,1], output$avg_usage[,2], output$job_survival[,1])
  
  utilization_rate1 <- overall_evaluation$utilization_rate1
  utilization_rate2 <- overall_evaluation$utilization_rate2
  survival_rate <- overall_evaluation$survival_rate
  
  scheduled_num <- sum(output$scheduled_num[,1])
  unscheduled_num <- sum(output$unscheduled_num[,1])
  correct_scheduled_num <- sum(output$correct_scheduled_num[,1])
  correct_unscheduled_num <- sum(output$correct_unscheduled_num[,1])
  
  correct_scheduled_rate <- correct_scheduled_num / scheduled_num
  correct_unscheduled_rate <- correct_unscheduled_num / unscheduled_num
  
  print(paste("Avg cycle used mode 1:", "job length", window_size, utilization_rate1))
  print(paste("Avg cycle used mode 2:", "job length", window_size, utilization_rate2))
  print(paste("Job survival rate:", "job length", window_size, survival_rate))
  print(paste("Scheduling summary:", "Correct scheduled rate:", correct_scheduled_rate, "Correct unscheduled rate:", correct_unscheduled_rate))
  
  if (write_result == TRUE) {
    ts_results <- data.frame("utilization_rate1"=output$avg_usage[,1],
                             "utilization_rate2"=output$avg_usage[,2],
                             "survival_rate"=output$job_survival[,1],
                             "correct_scheduled_rate"=(output$correct_scheduled_num[,1] / output$scheduled_num[,1]),
                             "correct_unscheduled_rate"=(output$correct_unscheduled_num[,1] / output$unscheduled_num[,1]))
    rownames(ts_results) <- colnames(dataset)
    result_file_name <- paste("AR1", schedule_policy, 0, prob_cut_off, granularity, window_size, nrow(dataset), 0, train_size, update_freq)
    write.csv(ts_results, file = paste0(write_result_path, result_file_name), row.names = TRUE)
  }
  
  result_path.csv <- read.csv(output_dp)
  result_path.csv <- update.df(result_path.csv, "AR1", prob_cut_off, 0, sample_size, window_size, granularity, 0, utilization_rate1, utilization_rate2, survival_rate, correct_scheduled_rate, correct_unscheduled_rate)
  write.csv(result_path.csv, file = output_dp, row.names = FALSE)
}
