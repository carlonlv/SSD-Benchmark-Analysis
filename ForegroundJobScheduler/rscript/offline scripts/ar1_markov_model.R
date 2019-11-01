library("forecast")
library("mvtnorm")
library("dplyr")
library("arules")
library("dict")
library("cluster")
library("xlsx")
library("parallel")

if (Sys.info()["sysname"] == "Windows") {
  source("C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//rscript//helper_functions.R")
} else {
  source("/Users/carlonlv/Documents/Github/Research-Projects/ForegroundJobScheduler/rscript/helper_functions.R")
}


train_ar1_model <- function(ts_num, train_dataset) {
  
  ts_model <- tryCatch({
    arima(x=train_dataset[, ts_num], order = c(1,0,0), include.mean = TRUE, method = "CSS-ML", optim.control = list(maxit=2000))
  }, error = function(cond) {
    return(arima(x=train_dataset[, ts_num], order = c(1,0,0), include.mean = TRUE, method = "ML", optim.control = list(maxit=2000)))
  })
  return(list("coeffs"=as.numeric(ts_model$coef[1]), "means"= as.numeric(ts_model$coef[2]), "vars"=ts_model$sigma2))
}


train_markov_model <- function(ts_num, train_dataset_avg, train_dataset_max, num_of_states) {
  
  dataset_avg <- train_dataset_avg[, ts_num]
  dataset_max <- train_dataset_max[, ts_num]
  from_states <- sapply(dataset_avg, find_state_num, num_of_states)
  to_states <- sapply(dataset_max, find_state_num, num_of_states)
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
  
  final_transition <- diag(x=1, nrow=nrow(transition), ncol=ncol(transition))
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
  for (i in 1:predict_size) {
    final_transition <- final_transition %*% parsed_transition
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


scheduling_foreground <- function(ts_num, test_dataset_max, test_dataset_avg, coeffs, means, vars, transition_matrix, window_size, prob_cut_off, cpu_required, granularity, schedule_policy) {
  
  if (granularity > 0) {
    cpu_required <- round_to_nearest(cpu_required[ts_num], granularity, FALSE)
  } else {
    cpu_required <- cpu_required[ts_num]
  }
  
  scheduled_num <- 0
  unscheduled_num <- 0
  correct_scheduled_num <- 0
  correct_unscheduled_num <- 0
  
  seek_length <- window_size
  last_time_schedule <- nrow(test_dataset_avg) - window_size + 1
  
  current_end <- window_size + 1
  update_policy <- ifelse(schedule_policy == "disjoint", window_size, 1)
  while (current_end <= last_time_schedule) {
    ## Predict current avgs using AR1
    last_obs <- convert_frequency_dataset(test_dataset_avg[(current_end-window_size):(current_end-1), ts_num], window_size, mode = "avg")
    expected_avgs <- do_prediction_ar1(last_obs, coeffs[ts_num], means[ts_num], vars[ts_num])$mu
    prediction_prob <- do_prediction_markov(expected_avgs, transition_matrix[[ts_num]], 1, 100-cpu_required)
    
    prediction <- ifelse(prediction_prob$prob <= prob_cut_off, 1, 0)
    scheduled_num <- ifelse(prediction == 1, scheduled_num + 1, scheduled_num)
    unscheduled_num <- ifelse(prediction == 1, unscheduled_num, unscheduled_num + 1)
    
    ## Evalute schedulings based on prediction
    start_time <- current_end
    end_time <- current_end + seek_length - 1
    position_vec <- convert_frequency_dataset(test_dataset_max[start_time:end_time, ts_num], window_size, mode = "max")
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


scheduling_model <- function(ts_num, test_dataset_max, test_dataset_avg, coeffs, means, vars, transition, window_size, prob_cut_off, cpu_required, granularity, max_run_length=25, schedule_policy, adjustment) {
  
  runs <- rep(0, max_run_length)
  run_counter <- 0
  run_switch <- FALSE
  
  last_time_schedule <- nrow(test_dataset_max) - window_size + 1
  
  current_end <- window_size + 1
  update_policy <- ifelse(schedule_policy == "disjoint", window_size, 1)
  
  pi_ups <- c()
  
  utilization <- c()
  survival <- c()
  while (current_end <= last_time_schedule) {
    ## Schedule based on model predictions
    last_obs <- convert_frequency_dataset(test_dataset_avg[(current_end-window_size):(current_end-1), ts_num], window_size, mode = 'avg')
    expected_avgs <- do_prediction_ar1(last_obs, coeffs[ts_num], means[ts_num], vars[ts_num])$mu
    prediction_result <- do_prediction_markov(expected_avgs, transition[[ts_num]], 1, NULL)
    
    pi_up <- compute_pi_up_markov(prediction_result$to_states, prob_cut_off, granularity)
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
  return(list("utilization1"=overall_utilization$utilization1, "utilization2"=overall_utilization$utilization2, "survival"=overall_survival$survival, "run"=runs))
}


ar1_markov_model <- function(dataset_avg, dataset_max, initial_train_size, prob_cut_off, max_run_length, window_size, cpu_required, num_of_states, granularity, adjustment) {
  #### input dataset_avg, dataset_max: N by M matrix, N being number of observations, M being number of time series
  #### input initial_train_size: The number of first observations used to train the model
  #### input window_size: The number of observations used to train and predict as one sample
  #### input prob_cut_off: If the probability of background job exceeding 100-cpu_required is smaller than prob_cut_off, then schedule it. Otherwise, don't.
  
  ts_names <- colnames(dataset_avg)
  
  scheduled_num <- data.frame(matrix(nrow=length(ts_names), ncol=0))
  unscheduled_num <- data.frame(matrix(nrow=length(ts_names), ncol=0))
  correct_scheduled_num <- data.frame(matrix(nrow=length(ts_names), ncol=0))
  correct_unscheduled_num <- data.frame(matrix(nrow=length(ts_names), ncol=0))
  
  avg_usage <- data.frame()
  job_survival <- data.frame()
  overall_runs <- data.frame()
  
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
  
  new_trainset_max_overlap <- apply(train_dataset_max, 2, convert_frequency_dataset_overlapping, new_freq=window_size, mode="max")
  rownames(new_trainset_max_overlap) <- 1:nrow(new_trainset_max_overlap)
  colnames(new_trainset_max_overlap) <- ts_names
  
  new_trainset_avg_overlap <- apply(train_dataset_avg, 2, convert_frequency_dataset_overlapping, new_freq=window_size, mode="avg")
  rownames(new_trainset_avg_overlap) <- 1:nrow(new_trainset_avg_overlap)
  colnames(new_trainset_avg_overlap) <- ts_names
  
  ## Training AR1 Model
  print("Training: AR1.")
  train_result_ar1 <- mclapply(1:length(ts_names), train_ar1_model, new_trainset_avg)
  coeffs <- c()
  means <- c()
  vars <- c()
  for (ts_num in 1:length(ts_names)) {
    coeffs <- c(coeffs, train_result_ar1[[ts_num]]$coeffs)
    means <- c(means, train_result_ar1[[ts_num]]$means)
    vars <- c(vars, train_result_ar1[[ts_num]]$vars)
  }
  
  ## Training Markov Model
  print("Training: Markov.")
  train_result_markov <- mclapply(1:length(ts_names), train_markov_model, new_trainset_avg_overlap, new_trainset_max_overlap, num_of_states)
  
  ## Test Model
  print("Testing on Foreground job:")
  result_foreground <- mclapply(1:length(ts_names), scheduling_foreground, test_dataset_max, test_dataset_avg, coeffs, means, vars, train_result_markov, window_size, prob_cut_off, cpu_required, granularity, schedule_policy)
  
  print("Testing on Model:")
  result_model <- mclapply(1:length(ts_names), scheduling_model, test_dataset_max, test_dataset_avg, coeffs, means, vars, train_result_markov, window_size, prob_cut_off, cpu_required, granularity, max_run_length, schedule_policy, adjustment)
  
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


wrapper.epoche <- function(parameter, dataset_avg, dataset_max, cpu_required, initial_train_size, max_run_length, output_dp, schedule_policy, adjustment) {
  
  window_size <- as.numeric(parameter[1])
  prob_cut_off <- as.numeric(parameter[2])
  granularity <- as.numeric(parameter[3])
  num_of_states <- as.numeric(parameter[4])
  
  print(paste("Job len:", window_size))
  print(paste("Cut off prob:", prob_cut_off))
  print(paste("Granularity:", granularity))
  print(paste("Num of States:", num_of_states))
  
  output <- ar1_markov_model(dataset_avg, dataset_max, initial_train_size, prob_cut_off, max_run_length, window_size, cpu_required, num_of_states, granularity, adjustment)
  
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
  
  if (schedule_policy == "dynamic") {
    write.csv(output$overall_runs, paste("Overall Runs", "AR1_Markov", sample_size, window_size, prob_cut_off, granularity, num_of_states, ".csv"))
  }
  result_path.xlsx <- read.xlsx(output_dp, sheetIndex = 1)
  result_path.xlsx <- update.xlsx.df(result_path.xlsx, "AR1_Markov", prob_cut_off, num_of_states, sample_size, window_size, granularity, 0, utilization_rate1, utilization_rate2, survival_rate, correct_scheduled_rate, correct_unscheduled_rate)
  write.xlsx(result_path.xlsx, showNA = FALSE, file = output_dp, row.names = FALSE)
}


## Read background job pool
sample_size <- 100
cpu_usage <- 3
max_run_length <- 37
total_trace_length <- 8000
initial_train_size <- 6000
adjustment <- FALSE

window_sizes <- c(12, 36)
prob_cut_offs <- c(0.005, 0.01, 0.1, 0.75)
granularity <- c(100/32, 100/64, 100/128, 0)
num_of_states <- c(32, 64)

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
if (adjustment) {
  if (schedule_policy == "dynamic") {
    if (Sys.info()["sysname"] == "Windows") {
      output_dp <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//offline results//summary dynamic (windows,granularity) post adj.xlsx"
    } else {
      output_dp <- "/Users/carlonlv/Documents/Github/Research-Projects/ForegroundJobScheduler/results/offline results/summary dynamic (windows,granularity) post adj.xlsx"
    }
  } else {
    if (Sys.info()["sysname"] == "Windows") {
      output_dp <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//offline results//summary disjoint (windows,granularity) post adj.xlsx"
    } else {
      output_dp <- "/Users/carlonlv/Documents/Github/Research-Projects/ForegroundJobScheduler/results/offline results/summary disjoint (windows,granularity) post adj.xlsx"
    }
  }
} else {
  if (schedule_policy == "dynamic") {
    if (Sys.info()["sysname"] == "Windows") {
      output_dp <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//offline results//summary dynamic (windows,granularity).xlsx"
    } else {
      output_dp <- "/Users/carlonlv/Documents/Github/Research-Projects/ForegroundJobScheduler/results/offline results/summary dynamic (windows,granularity).xlsx"
    }
  } else {
    if (Sys.info()["sysname"] == "Windows") {
      output_dp <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//offline results//summary disjoint (windows,granularity).xlsx"
    } else {
      output_dp <- "/Users/carlonlv/Documents/Github/Research-Projects/ForegroundJobScheduler/results/offline results/summary disjoint (windows,granularity).xlsx"
    }
  }
}

parameter.df <- expand.grid(window_sizes, prob_cut_offs, granularity, num_of_states)
colnames(parameter.df) <- c("window_size", "prob_cut_off", "granularity", "num_of_states")
parameter.df <- parameter.df %>% 
  arrange(window_size)

slt <- apply(parameter.df, 1, wrapper.epoche, data_matrix_avg, data_matrix_max, (100-cpu_required), initial_train_size, max_run_length, output_dp, schedule_policy, adjustment)
