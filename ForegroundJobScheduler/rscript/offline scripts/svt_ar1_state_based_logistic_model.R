
if (Sys.info()["sysname"] == "Windows") {
  source("C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//rscript//helper_functions.R")
} else if (Sys.info()["sysname"] == "Darwin") {
  source("/Users/carlonlv/Documents/Github/Research-Projects/ForegroundJobScheduler/rscript/helper_functions.R")
} else {
  source("/home/jialun/Research-Projects/ForegroundJobScheduler/rscript/helper_functions.R")
}

cores <- ifelse(Sys.info()["sysname"] == "Windows", 1, detectCores(all.tests = FALSE, logical = TRUE))


do_prediction_ar1 <- function(last_obs, phi, mean, variance) {
  
  # Construct mean
  mu <- last_obs * phi + (1 - phi) * mean
  # Construct Var-cov matrix
  var <- variance
  result <- list('mu' = mu, 'var'=var)
  return(result)
}


train_ar1_model <- function(ts_num, train_dataset) {
  
  suppressWarnings(ts_model <- tryCatch({
    arima(x=train_dataset[, ts_num], order = c(1,0,0), include.mean = TRUE, method = "CSS-ML", optim.control = list(maxit=2000), optim.method="Nelder-Mead")
  }, warning = function(w) {
    arima(x=train_dataset[, ts_num], order = c(1,0,0), include.mean = TRUE, method = "CSS-ML", optim.control = list(maxit=2000), optim.method="BFGS")
  }, error = function(cond) {
    ts_model_relax <- tryCatch({
      arima(x=train_dataset[, ts_num], order = c(1,0,0), include.mean = TRUE, method = "ML", optim.control = list(maxit=2000), transform.pars = FALSE, optim.method="BFGS")
    }, error = function(cond) {
      arima(x=train_dataset[, ts_num], order = c(1,0,0), include.mean = TRUE, method = "CSS", optim.control = list(maxit=2000), transform.pars = TRUE, optim.method="CG")
    })
  }))
  return(list("coeffs"=as.numeric(ts_model$coef[1]), "means"= as.numeric(ts_model$coef[2]), "vars"=ts_model$sigma2))
}


parser_logistic_model_state <- function(state_num, train_set_avg, train_set_max, breaks) {
  
  df <- data.frame("avg"=train_set_avg, "max"=train_set_max)
  df$survived <- factor(ifelse(train_set_max < breaks[state_num+1], 1, 0), levels=c(0, 1))
  return(df)
}


parser_logistic_model_states <- function(ts_num, train_set_avg, train_set_max, breaks) {
  
  ts_logistic_input <- sapply(1:(length(breaks) - 1), parser_logistic_model_state, train_set_avg[,ts_num], train_set_max[,ts_num], breaks, simplify=FALSE)
  return(ts_logistic_input)
}


train_multi_state_logistic_model <- function(state_num, parsed_states_input) {
  
  df <- parsed_states_input[[state_num]]
  suppressWarnings(log.lm <- glm(survived~avg, data=df, family="binomial", control=glm.control(maxit=2000)))
  return(log.lm)
}


train_multi_state_logistic_models <- function(ts_num, parsed_states_input, num_of_states) {
  
  multi_state_logistic <- sapply(1:num_of_states, train_multi_state_logistic_model, parsed_states_input[[ts_num]], simplify=FALSE)
  return(multi_state_logistic)
}


calculate_probability_table <- function(state_num, expected_avgs, trained_logistic_models) {
  
  prob <- predict(trained_logistic_models[[state_num]], newdata = data.frame("avg"=expected_avgs), type = "response")
  return(prob)
}


adjust_probability <- function(prob) {
  
  for (state_num in 2:length(prob)) {
    prob[state_num] <- ifelse(prob[state_num] < prob[state_num-1], prob[state_num-1], prob[state_num])
  }
  prob[length(prob)] <- 1
  return(prob)
}


calculate_probability_foreground <- function(probability, cpu_required, num_of_states) {
  
  state <- find_state_num(100-cpu_required, num_of_states)
  return(1-probability[state])
}


scheduling_foreground <- function(ts_num, test_dataset_max, test_dataset_avg, coeffs, means, vars, logistic_models, window_size, prob_cut_off, cpu_required, granularity, num_of_states, schedule_policy) {
  
  if (granularity > 0) {
    cpu_required <- round_to_nearest(cpu_required[ts_num], granularity, FALSE)
  } else {
    cpu_required <- cpu_required[ts_num]
  }
  
  scheduled_num <- 0
  unscheduled_num <- 0
  correct_scheduled_num <- 0
  correct_unscheduled_num <- 0
  
  last_time_schedule <- nrow(test_dataset_avg) - window_size + 1

  update_policy = ifelse(schedule_policy == "disjoint", window_size, 1)
  current_end <- window_size + 1
  while (current_end <= last_time_schedule) {
    ## Predict current avgs using AR1
    last_obs <- convert_frequency_dataset(test_dataset_avg[(current_end-window_size):(current_end-1), ts_num], window_size, mode = 'avg')
    expected_avgs <- do_prediction_ar1(last_obs, coeffs[ts_num], means[ts_num], vars[ts_num])$mu
    
    probability <- sapply(1:num_of_states, calculate_probability_table, expected_avgs, logistic_models[[ts_num]], simplify=FALSE)
    probability <- adjust_probability(unlist(probability))
    prediction_prob <- calculate_probability_foreground(probability, cpu_required, num_of_states)
    
    prediction <- ifelse(prediction_prob <= prob_cut_off, 1, 0)
    scheduled_num <- ifelse(prediction == 1, scheduled_num + 1, scheduled_num)
    unscheduled_num <- ifelse(prediction == 1, unscheduled_num, unscheduled_num + 1)

    ## Evalute schedulings based on prediction
    start_time <- current_end
    end_time <- current_end + window_size - 1
    position_vec <- convert_frequency_dataset(test_dataset_max[start_time:end_time, ts_num], window_size, mode = 'max')
    actual <- ifelse(all(position_vec <= (100 - cpu_required)), 1, 0)
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


compute_pi_up_states <- function(expected_avgs, probability, granularity, prob_cutoff) {
  
  current_state <- 1
  current_prob <- 0
  while (current_state <= length(probability)) {
    current_prob <- probability[current_state]
    if (current_prob >= 1 - prob_cutoff) {
      break
    }
    current_state <- current_state + 1
  }
  upper_bounds <- current_state * (100 / length(probability))
  if (granularity > 0) {
    scheduled_size <- round_to_nearest(100 - upper_bounds, granularity, TRUE)
    upper_bounds <- 100 - scheduled_size
  }
  return(upper_bounds)
}


scheduling_model <- function(ts_num, test_dataset_max, test_dataset_avg, coeffs, means, vars, logistic_models, window_size, prob_cut_off, granularity, max_run_length=25, num_of_states, schedule_policy, adjustment) {
  
  runs <- rep(0, max_run_length)
  run_counter <- 0
  run_switch <- FALSE
  
  last_time_schedule <- nrow(test_dataset_avg) - window_size + 1
  
  logistic_model <- logistic_models[[ts_num]]
  current_end <- window_size + 1
  update_policy <- ifelse(schedule_policy == "disjoint", window_size, 1)
  
  pi_ups <- c()
  
  utilization <- c()
  survival <- c()
  while (current_end <= last_time_schedule) {
    ## Schedule based on model predictions
    last_obs <- convert_frequency_dataset(test_dataset_avg[(current_end-window_size):(current_end-1), ts_num], window_size, mode = 'avg')
    expected_avgs <- do_prediction_ar1(last_obs, coeffs[ts_num], means[ts_num], vars[ts_num])$mu
    
    probability <- sapply(1:num_of_states, calculate_probability_table, expected_avgs, logistic_models[[ts_num]], simplify=FALSE)
    probability <- adjust_probability(unlist(probability))
    
    pi_up <- compute_pi_up_states(expected_avgs, probability, granularity, prob_cut_off)
    pi_ups <- c(pi_ups, pi_up)
    
    ## Evalute schedulings based on prediction
    start_time <- current_end
    end_time <- current_end + window_size - 1

    utilization <- c(utilization, check_utilization(pi_up, granularity))
    survival <- c(survival, check_survival(pi_up, test_dataset_max[start_time:end_time, ts_num], granularity))
    
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
  overall_utilization <- compute_utilization(pi_ups, survival, test_dataset_max[(window_size+1):(current_end-update_policy+window_size-1), ts_num], window_size, granularity, schedule_policy)
  return(list("util_numerator"=overall_utilization$numerator, "util_denominator"=overall_utilization$denominator, "sur_numerator"=overall_survival$numerator, "sur_denominator"=overall_survival$denominator, "run"=runs))
}


ar_logistic_model <- function(dataset_avg, dataset_max, initial_train_size, prob_cut_off, max_run_length, window_size, cpu_required, num_of_states, granularity, schedule_policy, adjustment) {
  #### input dataset_avg, dataset_max: N by M matrix, N being number of observations, M being number of time series
  #### input initial_train_size: The number of first observations used to train the model
  #### input window_size: The number of observations used to train and predict as one sample
  #### input prob_cut_off: If the probability of background job exceeding 100-cpu_required is smaller than prob_cut_off, then schedule it. Otherwise, don't.
  #### input job_length: The length of job to be scheduled
  
  ts_names <- colnames(dataset_avg)
  
  scheduled_num <- c()
  unscheduled_num <- c()
  correct_scheduled_num <- c()
  correct_unscheduled_num <- c()
  
  util_numerator <- c()
  util_denominator <- c()
  sur_numerator <- c()
  sur_denominator <- c()
  
  overall_runs <- data.frame()
  
  ## Two Level Lists
  logistic_inputs <- NULL
  logistic_models <- NULL
  
  ## Split the dataset into training and testing sets
  train_dataset_max <- dataset_max[1:initial_train_size,]
  test_dataset_max <- dataset_max[(initial_train_size+1):nrow(dataset_max),]
  train_dataset_avg <- dataset_avg[1:initial_train_size,]
  test_dataset_avg <- dataset_avg[(initial_train_size+1):nrow(dataset_avg),]
  
  ## Convert Frequency for trainning set
  new_trainset_max <- apply(train_dataset_max, 2, convert_frequency_dataset, new_freq=window_size, mode="max")
  rownames(new_trainset_max) <- seq(1, 1 + window_size * (nrow(new_trainset_max) - 1), window_size)
  colnames(new_trainset_max) <- ts_names
  
  new_trainset_avg <- apply(train_dataset_avg, 2, convert_frequency_dataset, new_freq=window_size, mode="avg")
  rownames(new_trainset_avg) <- seq(1, 1 + window_size * (nrow(new_trainset_avg) - 1), window_size)
  colnames(new_trainset_avg) <- ts_names
  
  ## Training AR1 Model
  print("Training: AR1.")
  train_result <- mclapply(1:length(ts_names), train_ar1_model, new_trainset_avg)
  coeffs <- c()
  means <- c()
  vars <- c()
  for (ts_num in 1:length(ts_names)) {
    coeffs <- c(coeffs, train_result[[ts_num]]$coeffs)
    means <- c(means, train_result[[ts_num]]$means)
    vars <- c(vars, train_result[[ts_num]]$vars)
  }
  
  ## Training State Based Logistic Model
  print("Training: Logistic.")
  ### Generate logistc inputs
  logistic_inputs <- mclapply(1:length(ts_names), parser_logistic_model_states, new_trainset_avg, new_trainset_max, discretize(0:100, method = "interval", breaks = num_of_states, onlycuts = TRUE), mc.cores=cores)
  ### Train logistic models
  logistic_models <- mclapply(1:length(ts_names), train_multi_state_logistic_models, logistic_inputs, num_of_states, mc.cores=cores)
  
  ## Testing
  print("Testing on Foreground job:")
  result_foreground <- mclapply(1:length(ts_names), scheduling_foreground, test_dataset_max, test_dataset_avg, coeffs, means, vars, logistic_models, window_size, prob_cut_off, cpu_required, granularity, num_of_states, schedule_policy, mc.cores=cores)
  
  print("Testing on Model:")
  result_model <- mclapply(1:length(ts_names), scheduling_model, test_dataset_max, test_dataset_avg, coeffs, means, vars, logistic_models, window_size, prob_cut_off, granularity, max_run_length, num_of_states, schedule_policy, adjustment, mc.cores=cores)
  
  for (ts_num in 1:length(ts_names)) {
    scheduled_num <- c(scheduled_num, result_foreground[[ts_num]]$scheduled_num)
    unscheduled_num <- c(unscheduled_num, result_foreground[[ts_num]]$unscheduled_num)
    correct_scheduled_num <- c(correct_scheduled_num, result_foreground[[ts_num]]$correct_scheduled_num)
    correct_unscheduled_num <- c(correct_unscheduled_num, result_foreground[[ts_num]]$correct_unscheduled_num)
    
    util_numerator <- c(util_numerator, result_model[[ts_num]]$util_numerator)
    util_denominator <- c(util_denominator, result_model[[ts_num]]$util_denominator)
    sur_numerator <- c(sur_numerator, result_model[[ts_num]]$sur_numerator)
    sur_denominator <- c(sur_denominator, result_model[[ts_num]]$sur_denominator)
    
    if (schedule_policy == "dynamic") {
      overall_runs <- rbind(overall_runs, result_model[[ts_num]]$run)
    }
  }
  
  schedule_decision <- data.frame("scheduled_num"=scheduled_num, "unscheduled_num"=unscheduled_num, "correct_scheduled_num"=correct_scheduled_num, "correct_unscheduled_num"=correct_unscheduled_num)
  rownames(schedule_decision) <- ts_names
  
  avg_usage <- data.frame("numerator"=util_numerator, "denominator"=util_denominator)  
  rownames(avg_usage) <- ts_names
  
  job_survival <- data.frame("numerator"=sur_numerator, "denominator"=sur_denominator)
  rownames(job_survival) <- ts_names
  
  if (schedule_policy == "dynamic") {
    rownames(overall_runs) <- ts_names
    colnames(overall_runs) <- sapply(1:max_run_length, function(i) as.character(i))
  } 
  
  return(list('usage'=avg_usage, 'survival'=job_survival, 'schedule'=schedule_decision, "overall_runs"=overall_runs))
}


wrapper.epoche <- function(parameter, dataset_avg, dataset_max, cpu_required, initial_train_size, max_run_length, output_dp, schedule_policy, adjustment, sample_size, write_result, write_result_path) {
  
  window_size <- as.numeric(parameter["window_size"])
  prob_cut_off <- as.numeric(parameter["prob_cut_off"])
  granularity <- as.numeric(parameter["granularity"])
  num_of_states <- as.numeric(parameter["num_of_states"])
  
  print(paste("Job len:", window_size))
  print(paste("Cut off prob:", prob_cut_off))
  print(paste("Granularity:", granularity))
  print(paste("State Num:", num_of_states))
  
  print(system.time(output <- ar_logistic_model(dataset_avg, dataset_max, initial_train_size, prob_cut_off, max_run_length, window_size, cpu_required, num_of_states, granularity, schedule_policy, adjustment)))
  
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
    if (schedule_policy == "dynamic") {
      ts_results <- cbind(ts_results, output$overall_runs)
    }
    result_file_name <- paste("AR1_state_based_logistic", schedule_policy, adjustment, num_of_states, prob_cut_off, granularity, window_size, 0)
    write.csv(ts_results, file = paste0(write_result_path, result_file_name, ".csv"), row.names = TRUE)
  }
  
  result_path.csv <- read.csv(output_dp)
  result_path.csv <- update.df.offline(result_path.csv, 
                                      "AR1_state_based_logistic", 
                                      prob_cut_off, 
                                      num_of_states, 
                                      sample_size, 
                                      window_size, 
                                      granularity, 
                                      0, 
                                      avg_utilization, 
                                      agg_utilization, 
                                      avg_survival,
                                      agg_survival,
                                      correct_scheduled_rate, 
                                      correct_unscheduled_rate)
  write.csv(result_path.csv, file = output_dp, row.names = FALSE)
}
