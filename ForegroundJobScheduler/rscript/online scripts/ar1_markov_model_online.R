library("forecast")
library("mvtnorm")
library("dplyr")
library("parallel")

if (Sys.info()["sysname"] == "Windows") {
  source("C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//rscript//helper_functions.R")
} else {
  source("/Users/carlonlv/Documents/Github/Research-Projects/ForegroundJobScheduler/rscript/helper_functions.R")
}

cores <- ifelse(Sys.info()["sysname"] == "Windows", 1, detectCores(all.tests = FALSE, logical = TRUE))


train_ar1_model <- function(train_dataset) {
  
  ts_model <- tryCatch({
    arima(x=train_dataset, order = c(1,0,0), include.mean = TRUE, method = "CSS-ML", optim.control = list(maxit=2000), optim.method="BFGS")
  }, error = function(cond) {
    return(arima(x=train_dataset, order = c(1,0,0), include.mean = TRUE, method = "ML", optim.control = list(maxit=2000), optim.method="BFGS"))
  })
  return(list("coeffs"=as.numeric(ts_model$coef[1]), "means"= as.numeric(ts_model$coef[2]), "vars"=ts_model$sigma2))
}


train_markov_model <- function(train_dataset_avg, train_dataset_max, num_of_states) {
  
  from_states <- sapply(train_dataset_avg, find_state_num, num_of_states)
  to_states <- sapply(train_dataset_max, find_state_num, num_of_states)
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


do_prediction_ar1 <- function(last_obs, phi, mean, variance) {
  
  # Construct mean
  mu <- last_obs * phi + (1 - phi) * mean
  # Construct Var-cov matrix
  var <- variance
  result <- list('mu' = mu, 'var'=var)
  return(result)
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


scheduling_model <- function(test_dataset_max, test_dataset_avg, coeffs, means, vars, transition, window_size, prob_cut_off, granularity, schedule_policy) {
  
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
          survival[length(survival)] <- survival[length(survival)]
        }
      }
    }
    current_end <- current_end + update_policy
  }
  
  overall_survival <- compute_survival(ifelse(is.na(survival), NA, ifelse(survival == 0, 1, 0)))
  overall_utilization <- compute_utilization(pi_ups, survival, test_dataset_max[(window_size+1):(current_end-update_policy+window_size-1)], window_size, granularity, schedule_policy)
  return(list("util_numerator"=overall_utilization$numerator, "util_denominator1"=overall_utilization$denominator1, "util_denominator2"=overall_utilization$denominator2, "sur_numerator"=overall_survival$numerator, "sur_denominator"=overall_survival$denominator))
}


svt_model <- function(ts_num, dataset_max, dataset_avg, train_size, window_size, update_freq, prob_cut_off, cpu_required, granularity, schedule_policy="disjoint", num_of_states) {
  
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
    new_trainset_avg <- convert_frequency_dataset(train_set_avg, window_size, "avg")
    new_trainset_overlap_max <- convert_frequency_dataset_overlapping(train_set_max, window_size, "max")
    new_trainset_overlap_avg <- convert_frequency_dataset_overlapping(train_set_avg, window_size, "avg")
    
    ## Train Model
    trained_ar1 <- train_ar1_model(new_trainset_avg)
    transition <- train_markov_model(new_trainset_overlap_avg, new_trainset_overlap_max, num_of_states)

    ## Test Model
    result_foreground <- scheduling_foreground(test_set_max, test_set_avg, trained_ar1$coeffs, trained_ar1$means, trained_ar1$vars, transition, window_size, prob_cut_off, cpu_required, granularity, schedule_policy)
    result_model <- scheduling_model(test_set_max, test_set_avg, trained_ar1$coeffs, trained_ar1$means, trained_ar1$vars, transition, window_size, prob_cut_off, granularity, schedule_policy)
    
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


svt_stationary_model <- function(dataset_max, dataset_avg, train_size, window_size, update_freq, prob_cut_off, cpu_required, granularity, schedule_policy="disjoint", num_of_states) {
  
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
  
  result <- lapply(1:length(ts_names), svt_model, dataset_max, dataset_avg, train_size, window_size, update_freq, prob_cut_off, cpu_required, granularity, schedule_policy, num_of_states)
  
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


wrapper.epoche <- function(parameter, dataset_max, dataset_avg, cpu_required, output_dp, schedule_policy) {
  
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
  
  output <- svt_stationary_model(dataset_max, dataset_avg, train_size, window_size, update_freq, prob_cut_off, cpu_required, granularity, schedule_policy, num_of_states)
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
  
  result_path.csv <- read.csv(output_dp)
  result_path.csv <- update.df.online(result_path.csv, "AR1_Markov", prob_cut_off, num_of_states, sample_size, window_size, granularity, 0, train_size, update_freq, utilization_rate1, utilization_rate2, survival_rate, correct_scheduled_rate, correct_unscheduled_rate)
  write.csv(result_path.csv, file = output_dp, row.names = FALSE)
}

## Read background job pool

sample_size <- 100
cpu_usage <- 3
total_trace_length <- 8000

window_sizes <- c(12, 36)
prob_cut_offs <- c(0.01, 0.1)
granularity <- c(100/32, 0)

train_size <- c(2000, 4000)

num_of_states <- c(8, 16, 32, 64)

schedule_policy <- "dynamic"

bg_jobs_path <- NULL
if (Sys.info()["sysname"] == "Windows") {
  bg_jobs_path <- "C://Users//carlo//Documents//sample background jobs//"
} else {
  bg_jobs_path <- "/Users/carlonlv/Documents/microsoft traces/"
}

bg_job_pool <- NULL
if (sample_size == 100 ) {
  if (Sys.info()["sysname"] == "Windows") {
    bg_job_pool <- read.csv("C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//pythonscripts//list of sampled 100 background jobs.csv")[,1]
  } else {
    bg_job_pool <- read.csv("/Users/carlonlv/Documents/GitHub/Research-Projects/ForegroundJobScheduler/pythonscripts/list of sampled 100 background jobs.csv")[,1]
  }
  bg_job_pool <- sub(".pd", "", bg_job_pool)
} else {
  if (Sys.info()["sysname"] == "Windows") {
    bg_job_pool <- read.csv("C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//pythonscripts//list of sampled background jobs.csv")[,1]
  } else {
    bg_job_pool <- read.csv("/Users/carlonlv/Documents/GitHub/Research-Projects/ForegroundJobScheduler/pythonscripts/list of sampled background jobs.csv")[,1]
  }
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
if (schedule_policy == "dynamic") {
  if (Sys.info()["sysname"] == "Windows") {
    output_dp <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//online results//summary dynamic (windows,granularity).csv"
  } else {
    output_dp <- "/Users/carlonlv/Documents/Github/Research-Projects/ForegroundJobScheduler/results/online results/summary dynamic (windows,granularity).csv"
  }
} else {
  if (Sys.info()["sysname"] == "Windows") {
    output_dp <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//online results//summary disjoint (windows,granularity).csv"
  } else {
    output_dp <- "/Users/carlonlv/Documents/Github/Research-Projects/ForegroundJobScheduler/results/online results/summary disjoint (windows,granularity).csv"
  }
}

parameter.df <- expand.grid(window_sizes, prob_cut_offs, granularity, train_size, num_of_states)
colnames(parameter.df) <- c("window_size", "prob_cut_off", "granularity", "train_size", "num_of_states")
parameter.df$update_freq <- 3 * parameter.df$window_size
parameter.df <- parameter.df %>%
  arrange()
parameter.df <- parameter.df[2,]
slt <- apply(parameter.df, 1, wrapper.epoche, data_matrix_avg, data_matrix_max, (100-cpu_required), output_dp, schedule_policy)
