library("mvtnorm")
library("dict")
library("dplyr")
library("xlsx")

source("C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//rscript//helper_functions.R")


find_state_num <- function(obs, num_of_states) {
	binsize <- 100 / num_of_states
	state <- NULL
	if (obs == 0) {
		state <- 1
	} else {
		state <- ifelse(obs %% binsize == 0, obs %/% binsize - 1, ceiling(obs / binsize))
	}
	return(state)
}


train_markov_model <- function(dataset, num_of_states) {
	from_states <- sapply(dataset[-length(dataset)], find_state_num, num_of_states)
	to_states <- sapply(dataset[-1], find_state_num, num_of_states)
	transition <- matrix(0, nrow=num_of_states, ncol=num_of_states)
	for (i in 1:length(from_states)) {
		from <- from_states[i]
		to <- to_states[i]
		transition[from, to] <- transition[from, to] + 1
	}
	for (col in 1:ncol(transition)) {
		trasition[,col] <- transition[,col] / sum(transition[,col])
	}
	return(transition)
}


do_prediction <- function(last_obs, transition, predict_size, level=NULL) {
	final_transition <- diag(x=1, nrow=nrow(transition), ncol=ncol(transition))
	for (i in 1:predict_size) {
		final_transition <- final_transition %*% transition
	}
	from <- find_state_num(last_obs, nrow(transition))
	to_states <- final_transition[from,]
	# calculate probability
	to <- find_state_num(level, nrow(transition))
	prob <- ifelse(is.null(level), NULL, final_transition[from, to])
	return(list("prob"=prob, "to_states"=to_states))
}


scheduling_foreground <- function(ts_num, test_set, trainsition, window_size, prob_cut_off, cpu_required, granularity, schedule_policy, mode) {
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
	current <- window_size + 1
	while (current_end <= last_time_schedule) {
		## Schedule based on model predictions
		last_obs <- convert_frequency_dataset(test_set[(current_end-window_size):(current_end-1), ts_num], window_size, mode="max")
		prediction_result <- do_prediction(last_obs, transition[[ts_num]], 1, 100-cpu_required)
		prediction <- ifelse(prediction_result$prob <= prob_cut_off, 1, 0)
		scheduled_num <- ifelse(prediction == 1, scheduled_num + 1, scheduled_num)
		unscheduled_num <- ifelse(prediction == 1, unscheduled_num, unscheduled_num + 1)
		
		## Evaluate Schedulings based on prediction
		start_time <- current_end
		end_time <- current_end + window_size - 1
		postion_vec <- convert_frequency_dataset(test_set[start_time:end_time, ts_num], window_size, "max")
		actual <- ifelse(all(position_vec <= (100 - cpu_required), 1, 0))
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
	
	return(list("scheduled_num"=scheduled_num, "unscheduled_num"=unscheduled_num, "corret_scheduled_num"=correct_scheduled_num, "corret_unscheduled_num"=correct_unscheduled_num))
}


compute_pi_up <- function(to_states, prob_cut_off, granularity) {
	current_state <- 1
	current_prob <- 0
	while (current_state <= length(to_states)) {
		if (current_prob < prob_cut_off) {
			current_prob <- current_prob + to_states[current_state]
			current_state <- current_state + 1
		} else {
			break
		}
	}
	
	pi_up <- current_state * (100 / length(to_states))
	if (granularity > 0) {
		scheduled_size <- round_to_nearest(100 - upper_bounds, granularity, TRUE)
		pi_up <- 100 - scheduled_size
	}
	return(pi_up)
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
		prediction_result <- do_prediction(last_obs, transition[[ts_num]], 1, NULL)
		pi_up <- compute_pi_up(prediction_result$to_states, 1, prob_cut_off, granularity)
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
	return(list("utilization1"=overall_utilization$utilization1, "utilization2"=overall_utilization$utilization2, "survival"=overall_survival, "run"=runs))
}


markov_model <- function(dataset, initial_train_size, window_size, prob_cut_off, max_run_length, cpu_required, granularity, num_of_states, schedule_policy="disjoint", adjustment) {
	
	ts_name <- colnames(dataset)
	
	scheduled_num <- data.frame(matrix(nrow=length(ts_names), ncol=0))
	unscheduled_num <- data.frame(matrix(nrow=length(ts_names), ncol=0))
	corrected_scheduled_num <- data.frame(matrix(nrow=length(ts_names), ncol=0))
	corrected_unscheduled_num <- data.frame(matrix(nrow=length(ts_names), ncol=0))
	
	avg_usage <- data.frame(matrix(nrow=0, ncol=1))
	job_survival <- data.frame(matrix(nrow=0, ncol=1))
	overall_runs <- data.frame(matrix(nrow=0, ncol=max_run_length)))
	
	## Split the dataset into training and testing sets
	train_set <- dataset[1:initial_train_size,]
	test_set <- dataset[(initial_train_size+1):nrow(dataset),]
	
	## Convert Frequency for training set
	new_trainset <- apply(train_set, 2, convert_frequency_dataset, new_freq=window_size, mode="max")
	rownames(new_trainset) <- seq(1, 1 + window * (nrow(new_trainset) - 1), window_size)
	colnames(new_trainset) <- ts_names
	
	## Train Model
	print("Training")
	train_result <- sapply(1:length(ts_names), train_markov_model, num_of_states, simplify=FALSE)
	
	## Test Model
	print("Testing on Foreground job:")
	result_foreground <- sapply(1:length(ts_names), scheduling_foreground, test_set, train_result, window_size, prob_cut_off, cpu_required, granularity, schedule_policy)
	print("Testing on Model:")
	result_model <- sapply(1:length(ts_names), scheduling_model, test_set, train_result, window_size, prob_cut_off, cpu_required, granularity, schedule_policy, adjustment, simplify=FALSE)
	
	scheduled_num <- cbind(scheduled_num, unlist(result_foreground[1,]))
	unscheduled_num <- cbind(unscheduled_num, unlist(result_foreground[2,]))
	correct_scheduled_num <- cbind(correct_scheduled_num, unlist(result_foreground[3,]))
	correct_unscheduled_num <- cbind(correct_unscheduled, unlist(result_foreground[4,]))
	
	for (ts_num in 1:length(ts_names)) {
		avg_usage <- rbind(avg_usage, c(result_model[ts_num]$utilization1, result_model[ts_num]$utilization2))
		job_survival <- rbind(job_survival, result_model[ts_num]$survival)
		if (schedule_policy == "dynamic") {
			overall_runs <- rbind(overall_runs, result_model[ts_num]$run)
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
	colnames(avg_usage) <- "survival"
	if (scheduled_policy == "dynamic") {
		rownames(overall_runs) <- ts_names
		colnames(overall_runs) <- sapply(1:max_run_length, function(i) as.character(i))
		result <- list("avg_usage"=avg_usage, "job_survival"=job_survival, "scheduled_num"=scheduled_num, "unscheduled_num"=unscheduled_num, "correct_scheduled_num"=correct_scheduled_num, "correct_unscheduled_num"=correct_unscheduled_num, "overall_runs"=overall_runs)
		return(result)
	} else {
		result <- list("avg_usage"=avg_usage, "job_survival"=job_survival, "scheduled_num"=scheduled_num, "unscheduled_num"=unscheduled_num, "correct_scheduled_num"=correct_scheduled_num, "correct_unscheduled_num"=correct_unscheduled_num)
		return(result)
	}
}


wrapper.epoche <- function(parameter, dataset, cpu_required, initial_train_size, max_run_length, output_dp, schedule_policy, adjustment) {
	
	window_size <- as.numeric(parameter[1])
	prob_cut_off <- as.numeric(parameter[2])
	granularity <- as.numeric(parameter[3])
	num_of_states <- as.numeric(parameter[4])
	
	print(paste("Job len:", window_size))
	print(paste("Cut off prob:", prob_cut_off))
	print(paste("Granularity:", granularity))
	
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
	correct_unscheduled_rate <- correct_unscheduled_num / scheduled_num
	
	print(paste("Avg cycle used mode 1:", "job length", window_size, utilization_rate1))
	print(paste("Avg cycle used mode 2:", "job length", window_size, utilization_rate2))
	print(paste("Job survival rate:", job_length, window_size, survival_rate))
	print(paste("Scheduling summary:", "Correct scheduled rate": correct_scheduled_rate, "Correct unscheduled rate:", correct_unscheduled_rate))
	
	result_path.xlsx <- read.xlsx(output_dp, sheetIndex = 1)
	if (schedule_policy == "dynamic") {
		write.csv(output$overall_runs, paste("Overall Runs", "Markov", sample_size, window_size, prob_cut_off, granularity, num_of_states, ".csv"))
	}
	result_path.xlsx <- update.xlsx.df(result_path.xlsx, "Markov", prob_cut_off, num_of_states, sample_size, window_size, granularity, 0, utilization_rate1, utilization_rate2, survival_rate, correct_scheduled_rate, correct_unscheduled_rate)
	write.xlsx(result_path.xlsx, showNA=FALSE, file=output_dp, row.names=FALSE)
}

## Read Background job pool