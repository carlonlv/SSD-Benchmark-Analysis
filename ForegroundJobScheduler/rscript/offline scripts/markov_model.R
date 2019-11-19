
if (Sys.info()["sysname"] == "Windows") {
  source("C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//rscript//helper_functions.R")
} else if (Sys.info()["sysname"] == "Darwin") {
  source("/Users/carlonlv/Documents/Github/Research-Projects/ForegroundJobScheduler/rscript/helper_functions.R")
} else {
  source("/home/jialun/Research-Projects/ForegroundJobScheduler/rscript/helper_functions.R")
}

cores <- ifelse(Sys.info()["sysname"] == "Windows", 1, detectCores(all.tests = FALSE, logical = TRUE))


train_markov_model <- function(ts_num, dataset, num_of_states) {
  
  dataset <- dataset[, ts_num]
  from_states <- sapply(dataset[-length(dataset)], find_state_num, num_of_states)
  to_states <- sapply(dataset[-1], find_state_num, num_of_states)
  transition <- matrix(0, nrow=num_of_states, ncol=num_of_states)
  for (i in 1:length(from_states)) {
    from <- from_states[i]
    to <- to_states[i]
    transition[from, to] <- transition[from, to] + 1
  }
  for (r in 1:ncol(transition)) {
    if (sum(transition[r,]) == 0) {
      transition[r,] <- rep(100 / num_of_states, num_of_states)
    } else {
      transition[r,] <- transition[r,] / sum(transition[r,])
    }
  }
  return(transition)
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
      to_states <- rbind(to_states, final_transition[from, ])
    }
  } else {
    to_states <- rbind(to_states, final_transition[from, ])
  }
  
  # calculate probability
  prob <- NULL
  if (!is.null(level)) {
    to <- find_state_num(level, nrow(transition))
    prob <- sum(final_transition[from, to:(nrow(transition))])
  }
  return(list("prob"=prob, "to_states"=to_states))
}


scheduling_foreground <- function(ts_num, test_set, transition, window_size, prob_cut_off, cpu_required, granularity, schedule_policy) {
	
  if (granularity > 0) {
		cpu_required <- round_to_nearest(cpu_required[ts_num], granularity, FALSE)
	} else {
		cpu_required <- cpu_required[ts_num]
	}
	
	scheduled_num <- 0
	unscheduled_num <- 0
	correct_scheduled_num <- 0
	correct_unscheduled_num <- 0
	
	last_time_schedule <- nrow(test_set) - window_size + 1
	
	update_policy <- ifelse(schedule_policy == "disjoint", window_size, 1)
	current_end <- window_size + 1
	while (current_end <= last_time_schedule) {
		## Schedule based on model predictions
		last_obs <- convert_frequency_dataset(test_set[(current_end-window_size):(current_end-1), ts_num], window_size, mode="max")
		prediction_result <- do_prediction_markov(last_obs, transition[[ts_num]], 1, 100-cpu_required)
		prediction <- ifelse(prediction_result$prob <= prob_cut_off, 1, 0)
		scheduled_num <- ifelse(prediction == 1, scheduled_num + 1, scheduled_num)
		unscheduled_num <- ifelse(prediction == 1, unscheduled_num, unscheduled_num + 1)
		
		## Evaluate Schedulings based on prediction
		start_time <- current_end
		end_time <- current_end + window_size - 1
		position_vec <- convert_frequency_dataset(test_set[start_time:end_time, ts_num], window_size, "max")
		actual <- ifelse(all(position_vec <= (100 - cpu_required)), 1, 0)
		correct_scheduled_num <- ifelse(prediction == 1 & actual == 1, correct_scheduled_num + 1, correct_scheduled_num)
		correct_unscheduled_num <- ifelse(prediction == 0 & actual == 0, correct_unscheduled_num + 1, correct_unscheduled_num)
		
		if (schedule_policy == "dynamic") {
		  if (prediction == 1) {
		    update_policy <- ifelse(actual == 1, window_size, 1)
		  } else {
		    update_policy <- 1
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


scheduling_model <- function(ts_num, test_set, transition, window_size, prob_cut_off, granularity, max_run_length=25, schedule_policy, adjustment) {
	
  runs <- rep(0, max_run_length)
	run_counter <- 0
	run_switch <- FALSE
	
	last_time_schedule <- nrow(test_set) - window_size + 1
	
	current_end <- window_size + 1
	update_policy <- ifelse(schedule_policy == "disjoint", window_size, 1)
	
	pi_ups <- c()
	
	utilization <- c()
	survival <- c()
	while (current_end <= last_time_schedule) {
		## Schedule based on model predictions
		last_obs <- convert_frequency_dataset(test_set[(current_end-window_size):(current_end-1), ts_num], window_size, mode="max")
		prediction_result <- do_prediction_markov(last_obs, transition[[ts_num]], 1, NULL)
		pi_up <- compute_pi_up_markov(prediction_result$to_states, prob_cut_off, granularity)
		pi_ups <- c(pi_ups, pi_up)
		## Evaluate schedulings based on prediction
		start_time <- current_end
		end_time <- current_end + window_size - 1
		
		utilization <- c(utilization, check_utilization(pi_up, granularity))
		survival <- c(survival, check_survival(pi_up, test_set[start_time:end_time, ts_num], granularity))
		
		if (schedule_policy == "dynamic") {
			if (!is.na(survival[length(survival)]) & survival[length(survival)] == 0) {
				update_policy <- window_size
				if (run_switch) {
					idx <- ifelse(run_counter > max_run_length, max_run_length, run_counter)
					runs[idx] <- runs[idx] + 1
					run_counter <- 0
					run_switch <- FALSE
				}
			} else if (is.na(survival[length(survival)])) {
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
	overall_utilization <- compute_utilization(pi_ups, survival, test_set[(window_size+1):(current_end-update_policy+window_size-1), ts_num], window_size, granularity, schedule_policy)
	return(list("utilization1"=overall_utilization$utilization1, "utilization2"=overall_utilization$utilization2, "survival"=overall_survival$survival, "run"=runs))
}


markov_model <- function(dataset, initial_train_size, window_size, prob_cut_off, max_run_length, cpu_required, granularity, num_of_states, schedule_policy="disjoint", adjustment) {
	
	ts_names <- colnames(dataset)
	
	scheduled_num <- data.frame()
	unscheduled_num <- data.frame()
	correct_scheduled_num <- data.frame()
	correct_unscheduled_num <- data.frame()
	
	avg_usage <- data.frame()
	job_survival <- data.frame()
	overall_runs <- data.frame()
	
	## Split the dataset into training and testing sets
	train_set <- dataset[1:initial_train_size,]
	test_set <- dataset[(initial_train_size+1):nrow(dataset),]
	
	## Convert Frequency for training set
	new_trainset <- apply(train_set, 2, convert_frequency_dataset, new_freq=window_size, mode="max")
	rownames(new_trainset) <- seq(1, 1 + window_size * (nrow(new_trainset) - 1), window_size)
	colnames(new_trainset) <- ts_names
	
	new_trainset_overlap <- apply(train_set, 2, convert_frequency_dataset_overlapping, new_freq=window_size, mode="max")
	rownames(new_trainset_overlap) <- 1:nrow(new_trainset_overlap)
	colnames(new_trainset_overlap) <- ts_names
	
	## Train Model
	print("Training")
	train_result <- mclapply(1:length(ts_names), train_markov_model, new_trainset_overlap, num_of_states, mc.cores=cores)
	
	## Test Model
	print("Testing on Foreground job:")
	result_foreground <- mclapply(1:length(ts_names), scheduling_foreground, test_set, train_result, window_size, prob_cut_off, cpu_required, granularity, schedule_policy, mc.cores=cores)
	
	print("Testing on Model:")
	result_model <- mclapply(1:length(ts_names), scheduling_model, test_set, train_result, window_size, prob_cut_off, granularity, max_run_length, schedule_policy, adjustment, mc.cores=cores)
	
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

wrapper.epoche <- function(parameter, dataset, cpu_required, initial_train_size, max_run_length, output_dp, schedule_policy, adjustment, write_result, write_result_path) {
	
	window_size <- as.numeric(parameter["window_size"])
	prob_cut_off <- as.numeric(parameter["prob_cut_off"])
	granularity <- as.numeric(parameter["granularity"])
	num_of_states <- as.numeric(parameter["num_of_states"])
	
	print(paste("Job len:", window_size))
	print(paste("Cut off prob:", prob_cut_off))
	print(paste("Granularity:", granularity))
	print(paste("Num of States:", num_of_states))
	
	output <- markov_model(dataset, initial_train_size, window_size, prob_cut_off, max_run_length, cpu_required, granularity, num_of_states, schedule_policy, adjustment)
	
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
	  ts_results <- data.frame()
	  ts_results$utilization_rate1 <- output$avg_usage[,1]
	  ts_results$utilization_rate2 <- output$avg_usage[,2]
	  ts_results$survival_rate <- output$job_survival[,1]
	  ts_results$correct_scheduled_rate <- output$correct_scheduled_num[,1] / output$scheduled_num[,1]
	  ts_results$correct_unscheduled_rate <- output$correct_unscheduled_num[,1] / output$unscheduled_num[,1]
	  rownames(ts_results) <- rownames(dataset)
	  result_file_name <- paste("Markov", schedule_policy, num_of_states, prob_cut_off, granularity, window_size, nrow(dataset), 0, train_size, update_freq)
	  write.csv(ts_results, file = paste0(write_result_path, result_file_name), row.names = TRUE)
	}
	
	result_path.csv <- read.csv(output_dp)
	result_path.csv <- update.df(result_path.csv, "Markov", prob_cut_off, num_of_states, sample_size, window_size, granularity, 0, utilization_rate1, utilization_rate2, survival_rate, correct_scheduled_rate, correct_unscheduled_rate)
	write.csv(result_path.csv, file=output_dp, row.names=FALSE)
}
