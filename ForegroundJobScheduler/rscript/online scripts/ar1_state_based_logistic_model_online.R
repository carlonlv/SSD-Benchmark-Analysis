
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


train_ar1_model <- function(train_dataset) {
  
  ts_model <- tryCatch({
    arima(x=train_dataset, order = c(1,0,0), include.mean = TRUE, method = "CSS-ML", optim.control = list(maxit=2000), optim.method="Nelder-Mead")
  }, warning = function(w) {
    arima(x=train_dataset, order = c(1,0,0), include.mean = TRUE, method = "CSS-ML", optim.control = list(maxit=2000), optim.method="BFGS")
  }, error = function(cond) {
    ts_model_relax <- tryCatch({
      arima(x=train_dataset, order = c(1,0,0), include.mean = TRUE, method = "ML", optim.control = list(maxit=2000), transform.pars = FALSE, optim.method="BFGS")
    }, error = function(cond) {
      arima(x=train_dataset, order = c(1,0,0), include.mean = TRUE, method = "CSS", optim.control = list(maxit=2000), transform.pars = TRUE, optim.method="CG")
    })
  })
  return(list("coeffs"=as.numeric(ts_model$coef[1]), "means"= as.numeric(ts_model$coef[2]), "vars"=ts_model$sigma2))
}


parser_logistic_model_state <- function(state_num, train_set_avg, train_set_max, breaks) {
  
  df <- data.frame("avg"=train_set_avg, "max"=train_set_max)
  df$survived <- factor(ifelse(train_set_max < breaks[state_num+1], 1, 0), levels=c(0, 1))
  return(df)
}


parser_logistic_model_states <- function(train_set_avg, train_set_max, breaks) {
  
  ts_logistic_input <- lapply(1:(length(breaks) - 1), parser_logistic_model_state, train_set_avg, train_set_max, breaks)
  return(ts_logistic_input)
}


train_multi_state_logistic_model <- function(state_num, parsed_states_input) {
  
  df <- parsed_states_input[[state_num]]
  suppressWarnings(log.lm <- glm(survived~avg, data=df, family="binomial", control=glm.control(maxit=200)))
  return(log.lm)
}


train_multi_state_logistic_models <- function(parsed_states_input, num_of_states) {
  
  multi_state_logistic <- lapply(1:num_of_states, train_multi_state_logistic_model, parsed_states_input)
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


scheduling_foreground <- function(test_dataset_max, test_dataset_avg, coeffs, means, vars, logistic_models, window_size, prob_cut_off, cpu_required, granularity, schedule_policy, num_of_states) {
  
  cpu_required <- ifelse(granularity>0, round_to_nearest(cpu_required, granularity, FALSE), cpu_required)
  
  scheduled_num <- 0
  unscheduled_num <- 0
  correct_scheduled_num <- 0
  correct_unscheduled_num <- 0
  
  last_time_schedule <- length(test_dataset_max) - window_size + 1
  
  update_policy = ifelse(schedule_policy == "disjoint", window_size, 1)
  current_end <- window_size + 1
  while (current_end <= last_time_schedule) {
    ## Predict current avgs using AR1
    last_obs_avg <- convert_frequency_dataset(test_dataset_avg[(current_end-window_size):(current_end-1)], window_size, mode="avg")
    expected_avgs <- max(do_prediction_ar1(last_obs_avg, coeffs, means, vars)$mu, 0)
    
    probability <- lapply(1:num_of_states, calculate_probability_table, expected_avgs, logistic_models)
    probability <- adjust_probability(unlist(probability))
    prediction_prob <- calculate_probability_foreground(probability, cpu_required, num_of_states)
    
    ## Evalute schedulings based on prediction
    prediction <- ifelse(prediction_prob <= prob_cut_off, 1, 0)
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


scheduling_model <- function(test_dataset_max, test_dataset_avg, coeffs, means, vars, logistic_models, window_size, prob_cut_off, granularity, schedule_policy, num_of_states, adjustment) {
  
  run_switch <- FALSE
  
  last_time_schedule <- length(test_dataset_max) - window_size + 1
  
  current_end <- window_size + 1
  update_policy <- ifelse(schedule_policy == "disjoint", window_size, 1)
  
  pi_ups <- c()
  
  utilization <- c()
  survival <- c()
  while (current_end <= last_time_schedule) {
    ## Schedule based on model predictions
    last_obs_avg <- convert_frequency_dataset(test_dataset_avg[(current_end-window_size):(current_end-1)], window_size, mode = 'avg')
    expected_avgs <- max(do_prediction_ar1(last_obs_avg, coeffs, means, vars)$mu, 0)

    probability <- lapply(1:num_of_states, calculate_probability_table, expected_avgs, logistic_models)
    probability <- adjust_probability(unlist(probability))
    
    pi_up <- compute_pi_up_states(expected_avgs, probability, granularity, prob_cut_off)
    pi_ups <- c(pi_ups, pi_up)
    
    ## Evalute schedulings based on prediction
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
  return(list("util_numerator"=overall_utilization$numerator, "util_denominator1"=overall_utilization$denominator1, "util_denominator2"=overall_utilization$denominator2, "sur_numerator"=overall_survival$numerator, "sur_denominator"=overall_survival$denominator))
}


svt_model <- function(ts_num, dataset_max, dataset_avg, train_size, window_size, update_freq, prob_cut_off, cpu_required, granularity, schedule_policy="disjoint", num_of_states, adjustment) {
  
  dataset_max <- dataset_max[,ts_num]
  dataset_avg <- dataset_avg[,ts_num]
  cpu_required <- cpu_required[ts_num]
  
  scheduled_num <- 0
  unscheduled_num <- 0
  correct_scheduled_num <- 0
  correct_unscheduled_num <- 0
  
  util_numerator <- 0
  util_denominator1 <- 0
  util_denominator2 <- 0
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
    new_trainset_overlap_max <- convert_frequency_dataset_overlapping(train_set_max, window_size, "max")
    new_trainset_overlap_avg <- convert_frequency_dataset_overlapping(train_set_avg, window_size, "avg")
    
    ## Train Model
    trained_ar1 <- train_ar1_model(new_trainset_avg)
    ### Generate logistc inputs
    logistic_inputs <- parser_logistic_model_states(new_trainset_avg, new_trainset_max, discretize(0:100, method = "interval", breaks = num_of_states, onlycuts = TRUE))
    ### Train logistic models
    logistic_models <- train_multi_state_logistic_models(logistic_inputs, num_of_states)

    ## Test Model
    result_foreground <- scheduling_foreground(test_set_max, test_set_avg, trained_ar1$coeffs, trained_ar1$means, trained_ar1$vars, logistic_models, window_size, prob_cut_off, cpu_required, granularity, schedule_policy, num_of_states)
    result_model <- scheduling_model(test_set_max, test_set_avg, trained_ar1$coeffs, trained_ar1$means, trained_ar1$vars, logistic_models, window_size, prob_cut_off, granularity, schedule_policy, num_of_states, adjustment)
    
    ## Write Result
    scheduled_num <- scheduled_num + result_foreground$scheduled_num
    unscheduled_num <- unscheduled_num + result_foreground$unscheduled_num
    correct_scheduled_num <- correct_scheduled_num + result_foreground$correct_scheduled_num
    correct_unscheduled_num <- correct_unscheduled_num + result_foreground$correct_unscheduled_num
    
    util_numerator <- util_numerator + ifelse(is.na(result_model$util_numerator), 0, result_model$util_numerator)
    util_denominator1 <- util_denominator1 + ifelse(is.na(result_model$util_denominator1), 0, result_model$util_denominator1)
    util_denominator2 <- util_denominator2 + ifelse(is.na(result_model$util_denominator2), 0, result_model$util_denominator2)
    sur_numerator <- sur_numerator + ifelse(is.na(result_model$sur_numerator), 0, result_model$sur_numerator)
    sur_denominator <- sur_denominator + ifelse(is.na(result_model$sur_denominator), 0, result_model$sur_denominator)
    
    ## Update current
    current <- current + update_freq
  }
  
  return(list("scheduled_num"=scheduled_num, "unscheduled_num"=unscheduled_num, "correct_scheduled_num"=correct_scheduled_num, "correct_unscheduled_num"=correct_unscheduled_num, "util_numerator"=util_numerator, "util_denominator1"=util_denominator1, "util_denominator2"=util_denominator2, "sur_numerator"=sur_numerator, "sur_denominator"=sur_denominator))
}


svt_stationary_model <- function(dataset_max, dataset_avg, train_size, window_size, update_freq, prob_cut_off, cpu_required, granularity, schedule_policy="disjoint", num_of_states, adjustment) {
  
  scheduled_num <- c()
  unscheduled_num <- c()
  correct_scheduled_num <- c()
  correct_unscheduled_num <- c()
  
  util_numerator <- c()
  util_denominator1 <- c()
  util_denominator2 <- c()
  sur_numerator <- c()
  sur_denominator <- c()
  
  ts_names <- colnames(dataset_max)
  
  result <- mclapply(1:length(ts_names), svt_model, dataset_max, dataset_avg, train_size, window_size, update_freq, prob_cut_off, cpu_required, granularity, schedule_policy, num_of_states, adjustment, mc.cores=cores)
  
  for (ts_num in 1:length(ts_names)) {
    scheduled_num <- c(scheduled_num, result[[ts_num]]$scheduled_num)
    unscheduled_num <- c(unscheduled_num, result[[ts_num]]$unscheduled_num)
    correct_scheduled_num <- c(correct_scheduled_num, result[[ts_num]]$correct_scheduled_num)
    correct_unscheduled_num <- c(correct_unscheduled_num, result[[ts_num]]$correct_unscheduled_num)
    
    util_numerator <- c(util_numerator, result[[ts_num]]$util_numerator)
    util_denominator1 <- c(util_denominator1, result[[ts_num]]$util_denominator1)
    util_denominator2 <- c(util_denominator2, result[[ts_num]]$util_denominator2)
    sur_numerator <- c(sur_numerator, result[[ts_num]]$sur_numerator)
    sur_denominator <- c(sur_denominator, result[[ts_num]]$sur_denominator)
  }
  
  scheduled_num <- data.frame("scheduled_num"=scheduled_num)
  rownames(scheduled_num) <- ts_names
  unscheduled_num <- data.frame("unscheduled_num"=unscheduled_num)
  rownames(unscheduled_num) <- ts_names
  correct_scheduled_num <- data.frame("correct_scheduled_num"=correct_scheduled_num)
  rownames(correct_scheduled_num) <- ts_names
  correct_unscheduled_num <- data.frame("correct_unscheduled_num"=correct_unscheduled_num)
  rownames(correct_unscheduled_num) <- ts_names
  
  avg_usage <- data.frame("avg_usage1"=util_numerator/util_denominator1, "avg_usage2"=util_numerator/util_denominator2)
  rownames(avg_usage) <- ts_names
  job_survival <- data.frame("survival"=sur_numerator/sur_denominator)
  rownames(job_survival) <- ts_names
  result <- list('avg_usage'=avg_usage, 'job_survival'=job_survival, 'scheduled_num'=scheduled_num, "unscheduled_num"=unscheduled_num, "correct_scheduled_num"=correct_scheduled_num, "correct_unscheduled_num"=correct_unscheduled_num)
  return(result)  
}


wrapper.epoche <- function(parameter, dataset_max, dataset_avg, cpu_required, output_dp, schedule_policy, write_result, write_result_path, adjustment) {
  
  window_size <- as.numeric(parameter["window_size"])
  prob_cut_off <- as.numeric(parameter["prob_cut_off"])
  granularity <- as.numeric(parameter["granularity"])
  train_size <- as.numeric(parameter["train_size"])
  update_freq <- as.numeric(parameter["update_freq"])
  num_of_states <- as.numeric(parameter["num_of_states"])
  
  print(paste("Job len:", window_size))
  print(paste("Cut off prob:", prob_cut_off))
  print(paste("Granularity:", granularity))
  print(paste("Train Size:", train_size))
  print(paste("Update Freq:", update_freq))
  
  print(system.time(output <- svt_stationary_model(dataset_max, dataset_avg, train_size, window_size, update_freq, prob_cut_off, cpu_required, granularity, schedule_policy, num_of_states, adjustment)))
  
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
    rownames(ts_results) <- colnames(dataset_max)
    result_file_name <- paste("AR1_state_based_logistic", schedule_policy, num_of_states, prob_cut_off, granularity, window_size, 0, train_size, update_freq)
    write.csv(ts_results, file = paste0(write_result_path, result_file_name, ".csv"), row.names = TRUE)
  }
  
  result_path.csv <- read.csv(output_dp)
  result_path.csv <- update.df.online(result_path.csv, "AR1_state_based_logistic", prob_cut_off, num_of_states, sample_size, window_size, granularity, 0, train_size, update_freq, utilization_rate1, utilization_rate2, survival_rate, correct_scheduled_rate, correct_unscheduled_rate)
  write.csv(result_path.csv, file = output_dp, row.names = FALSE)
}
