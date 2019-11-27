
if (Sys.info()["sysname"] == "Windows") {
  source("C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//rscript//helper_functions.R")
} else if (Sys.info()["sysname"] == "Darwin") {
  source("/Users/carlonlv/Documents/Github/Research-Projects/ForegroundJobScheduler/rscript/helper_functions.R")
} else {
  source("/home/jialun/Research-Projects/ForegroundJobScheduler/rscript/helper_functions.R")
}

cores <- ifelse(Sys.info()["sysname"] == "Windows", 1, detectCores(all.tests = FALSE, logical = TRUE))


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


train_markov_model <- function(train_dataset_avg, train_dataset_max, num_of_states) {
  
  from_states <- sapply(train_dataset_avg, find_state_num, num_of_states)
  to_states <- sapply(train_dataset_max, find_state_num, num_of_states)
  uncond_dist <- rep(0, num_of_states)
  transition <- matrix(0, nrow=num_of_states, ncol=num_of_states)
  for (i in 1:length(from_states)) {
    from <- from_states[i]
    to <- to_states[i]
    transition[from, to] <- transition[from, to] + 1
    uncond_dist[to] <- uncond_dist[to] + 1
  }
  for (r in 1:ncol(transition)) {
    if (sum(transition[r,]) == 0) {
      transition[r,] <- uncond_dist
    } else {
      transition[r,] <- transition[r,] / sum(transition[r,])
    }
  }
  return(transition)
}


do_prediction_ar1 <- function(last_obs, phi, mean, variance) {
  
  # Construct mean
  mu <- last_obs * phi + (1 - phi) * mean
  # Construct Var-cov matrix
  var <- variance
  return(list('mu' = mu, 'var'=var))
}


do_prediction_markov <- function(predictor, transition, predict_size, level=NULL) {
  
  final_transition <- transition
  parsed_transition <- transition
  if (!is.null(level)) {
    level_state <- find_state_num(level, nrow(transition))
    for (i in level_state:nrow(transition)) {
      parsed_transition[i,] <- rep(0, nrow(transition))
      parsed_transition[i, i] <- 1
    }
  }
  from <- find_state_num(predictor, nrow(transition))
  to_states <- data.frame()
  if (predict_size > 1) {
    for (i in 1:(predict_size-1)) {
      final_transition <- final_transition %*% parsed_transition
      to_states <- rbind(to_states, final_transition[from,])
    }
  } else {
    to_states <- rbind(to_states, final_transition[from,])
  }
  
  # calculate probability
  prob <- NULL
  if (!is.null(level)) {
    to <- find_state_num(level, nrow(transition))
    prob <- sum(final_transition[from, to:(nrow(transition))])
  }
  return(list("prob"=prob, "to_states"=to_states))
}


scheduling_foreground <- function(test_dataset_max, test_dataset_avg, coeffs, means, vars, transition, window_size, prob_cut_off, cpu_required, granularity, schedule_policy) {
  
  cpu_required <- ifelse(granularity>0, round_to_nearest(cpu_required, granularity, FALSE), cpu_required)
  
  scheduled_num <- 0
  unscheduled_num <- 0
  correct_scheduled_num <- 0
  correct_unscheduled_num <- 0
  
  last_time_schedule <- length(test_dataset_max) - window_size + 1
  
  update_policy = ifelse(schedule_policy == "disjoint", window_size, 1)
  current_end <- window_size + 1
  while (current_end <= last_time_schedule) {
    ## Schedule based on model predictions
    last_obs_avg <- convert_frequency_dataset(test_dataset_avg[(current_end-window_size):(current_end-1)], window_size, mode="avg")
    expected_avgs <- max(do_prediction_ar1(last_obs_avg, coeffs, means, vars)$mu, 0)
    prediction_result <- do_prediction_markov(expected_avgs, transition, 1, 100-cpu_required)
    
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


compute_pi_up_markov_single <- function(to_states, prob_cut_off, granularity) {
  
  current_state <- 1
  current_prob <- 0
  while (current_state <= length(to_states)) {
    current_prob <- current_prob + to_states[current_state]
    if (current_prob < 1-prob_cut_off) {
      current_state <- current_state + 1
    } else {
      break
    }
  }
  
  pi_up <- current_state * (100 / length(to_states))
  if (granularity > 0) {
    scheduled_size <- round_to_nearest(100 - pi_up, granularity, TRUE)
    pi_up <- 100 - scheduled_size
  }
  return(pi_up)
}


compute_pi_up_markov <- function(to_states, prob_cut_off, granularity) {
  
  pi_ups <- apply(to_states, 1, compute_pi_up_markov_single, prob_cut_off, granularity)
  return(max(pi_ups))
}


scheduling_model <- function(test_dataset_max, test_dataset_avg, coeffs, means, vars, transition, window_size, prob_cut_off, granularity, schedule_policy, adjustment) {
  
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
    expected_avgs <- max(0, do_prediction_ar1(last_obs_avg, coeffs, means, vars)$mu)
    prediction_result <- do_prediction_markov(expected_avgs, transition, 1, NULL)
    
    pi_up <- compute_pi_up_markov(prediction_result$to_states, prob_cut_off, granularity)
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


svt_model <- function(ts_num, dataset_max, dataset_avg, train_size, window_size, update_freq, prob_cut_off, cpu_required, granularity, schedule_policy="disjoint", num_of_states, adjustment) {
  
  dataset_max <- dataset_max[,ts_num]
  dataset_avg <- dataset_avg[,ts_num]
  cpu_required <- cpu_required[ts_num]
  
  scheduled_num <- 0
  unscheduled_num <- 0
  correct_scheduled_num <- 0
  correct_unscheduled_num <- 0
  
  util_numerator <- 0
  util_denominator <- 0
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
    new_trainset_avg <- convert_frequency_dataset(train_set_avg, window_size, "avg")
    new_trainset_overlap_max <- convert_frequency_dataset_overlapping(train_set_max, window_size, "max")
    new_trainset_overlap_avg <- convert_frequency_dataset_overlapping(train_set_avg, window_size, "avg")
    
    ## Train Model
    trained_ar1 <- train_ar1_model(new_trainset_avg)
    transition <- train_markov_model(new_trainset_overlap_avg, new_trainset_overlap_max, num_of_states)

    ## Test Model
    result_foreground <- scheduling_foreground(test_set_max, test_set_avg, trained_ar1$coeffs, trained_ar1$means, trained_ar1$vars, transition, window_size, prob_cut_off, cpu_required, granularity, schedule_policy)
    result_model <- scheduling_model(test_set_max, test_set_avg, trained_ar1$coeffs, trained_ar1$means, trained_ar1$vars, transition, window_size, prob_cut_off, granularity, schedule_policy, adjustment)
    
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


svt_stationary_model <- function(dataset_max, dataset_avg, train_size, window_size, update_freq, prob_cut_off, cpu_required, granularity, schedule_policy="disjoint", num_of_states, adjustment) {
  
  scheduled_num <- c()
  unscheduled_num <- c()
  correct_scheduled_num <- c()
  correct_unscheduled_num <- c()
  
  util_numerator <- c()
  util_denominator <- c()
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
  num_of_states <- as.numeric(parameter["num_of_states"])
  
  print(paste("Job len:", window_size))
  print(paste("Cut off prob:", prob_cut_off))
  print(paste("Granularity:", granularity))
  print(paste("Train Size:", train_size))
  print(paste("Update Freq:", update_freq))
  print(paste("State Num:", num_of_states))
  
  print(system.time(output <- svt_stationary_model(dataset_max, dataset_avg, train_size, window_size, update_freq, prob_cut_off, cpu_required, granularity, schedule_policy, num_of_states, adjustment)))
    
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
    result_file_name <- paste("AR1_Markov", schedule_policy, adjustment, num_of_states, prob_cut_off, granularity, window_size, 0, train_size, update_freq)
    write.csv(ts_results, file = paste0(write_result_path, result_file_name, ".csv"), row.names = TRUE)
  }
  
  result_path.csv <- read.csv(output_dp)
  result_path.csv <- update.df.online(result_path.csv, 
                                      "AR1_Markov", 
                                      prob_cut_off, 
                                      num_of_states, 
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
