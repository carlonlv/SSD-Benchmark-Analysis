library("forecast")
library("mvtnorm")
library("dict")
library("dplyr")
library("xlsx")

source("C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//rscript//helper_functions.R")


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


scheduling_foreground <- function(ts_num, test_dataset, coeffs, means, vars, window_size, job_length, prob_cut_off, cpu_required, granularity, schedule_policy, mode) {
  scheduled_num <- 0
  unscheduled_num <- 0
  correct_scheduled_num <- 0
  correct_unscheduled_num <- 0
  
  seek_length <- window_size * job_length
  last_time_schedule <- nrow(test_dataset) - window_size * job_length + 1

  update_policy = ifelse(schedule_policy == "disjoint", window_size, 1)
  current_end <- window_size + 1
  while (current_end <= last_time_schedule) {
    ## Schedule based on model predictions
    last_obs <- convert_frequency_dataset(test_dataset[(current_end-window_size):(current_end-1), ts_num], window_size, mode = mode)
    prediction_result <- do_prediction(last_obs=last_obs, phi=coeffs[ts_num], mean=means[ts_num], variance=vars[ts_num], predict_size=job_length, level=(100-cpu_required[ts_num]))
    prediction <- ifelse(prediction_result$prob <= prob_cut_off, 1, 0)
    scheduled_num <- ifelse(prediction == 1, scheduled_num + 1, scheduled_num)
    unscheduled_num <- ifelse(prediction == 1, unscheduled_num, unscheduled_num + 1)
    
    ## Evalute schedulings based on prediction
    start_time <- current_end
    end_time <- current_end + seek_length - 1
    position_vec <- convert_frequency_dataset(test_dataset[start_time:end_time, ts_num], window_size, mode = mode)
    actual <- ifelse(all(position_vec <= (100 - cpu_required[ts_num])), 1, 0)
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


scheduling_model <- function(ts_num, test_dataset, coeffs, means, vars, window_size, prob_cut_off, granularity, schedule_policy, mode) {
  utilization <- c()
  survival <- c()
  runs <- rep(0, 20)
  run_counter <- 0
  run_switch <- FALSE
  
  seek_length <- window_size
  last_time_schedule <- nrow(test_dataset) - window_size + 1
  
  current_end <- window_size + 1
  update_policy <- ifelse(schedule_policy == "disjoint", window_size, 1)
  while (current_end <= last_time_schedule) {
    ## Schedule based on model predictions
    last_obs <- convert_frequency_dataset(test_dataset[(current_end-window_size):(current_end-1), ts_num], window_size, mode = mode)
    prediction_result <- do_prediction(last_obs=last_obs, phi=coeffs[ts_num], mean=means[ts_num], variance=vars[ts_num], predict_size=1)
    pi_up <- compute_pi_up(mu=prediction_result$mu, varcov=prediction_result$varcov, predict_size=1, prob_cutoff=prob_cut_off, granularity=granularity)
    
    ## Evalute schedulings based on prediction
    start_time <- current_end
    end_time <- current_end + seek_length - 1
    position_vec <- convert_frequency_dataset(test_dataset[start_time:end_time, ts_num], window_size, mode = mode)
    evalulation <- find_evaluation(pi_up=pi_up, actual_obs=position_vec, granularity=granularity)
    utilization <- c(utilization, evalulation$usage)
    survival <- c(survival, evalulation$survival)
    
    if (schedule_policy == "dynamic") {
      if (!is.na(evalulation$survival) & evalulation$survival == 1) {
        update_policy <- window_size
        if (run_switch) {
          idx <- ifelse(run_counter > 20, 20, run_counter)
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
  return(list("utilization"=overall_rate$utilization_rate, "survival"=overall_rate$survival_rate, "runs"=runs))
}


svt_stationary_model <- function(dataset, initial_train_size, window_size, job_length, cpu_required, prob_cut_off, mode, granularity, schedule_policy="disjoint") {
  #### input dataset: N by M matrix, N being number of observations, M being number of time series
  #### input initial_train_size: The number of first observations used to train the model
  #### input window_size: The number of observations used to train and predict
  #### input job_length: The number of windows that the foreground job will be runing
  #### input cpu_required: A vector, the cpu that the foreground job requires in percentage
  #### input prob_cut_off: If the probability of background job exceeding 100-cpu_required is smaller than prob_cut_off, then schedule it. Otherwise, don't.
  #### input mode: max or avg
  #### input granularity:
  #### input schedule_policy: dynamic, disjoint, overlap
  
  if (granularity > 0) {
    cpu_required <- sapply(cpu_required, round_to_nearest, granularity, FALSE)
  }
  
  ts_names <- colnames(dataset)
  
  scheduled_num <- data.frame(matrix(nrow=length(ts_names), ncol=0))
  unscheduled_num <- data.frame(matrix(nrow=length(ts_names), ncol=0))
  correct_scheduled_num <- data.frame(matrix(nrow=length(ts_names), ncol=0))
  correct_unscheduled_num <- data.frame(matrix(nrow=length(ts_names), ncol=0))
  
  avg_usage <- data.frame()
  job_survival <- data.frame()
  overall_runs <- data.frame(matrix(nrow=0, ncol=20))
  
  ## Split in Training and Testing Set
  train_dataset <- dataset[1:initial_train_size, 1:length(ts_names)]
  test_dataset <- dataset[(initial_train_size+1):nrow(dataset), 1:length(ts_names)]
  
  ## Convert Frequency for trainning set
  new_trainset <- apply(train_dataset, 2, convert_frequency_dataset, new_freq=window_size, mode=mode)
  rownames(new_trainset) <- seq(1, 1 + window_size * (nrow(new_trainset) - 1), window_size)
  colnames(new_trainset) <- ts_names
  
  ## Train Model
  print("Training:")
  train_result <- sapply(1:length(ts_names), train_ar1_model, new_trainset)
  coeffs <- unlist(train_result[1,])
  means <- unlist(train_result[2,])
  vars <- unlist(train_result[3,])
  
  ## Test Model
  print("Testing on Foreground job:")
  result_foreground <- sapply(1:length(ts_names), scheduling_foreground, test_dataset, coeffs, means, vars, window_size, job_length, prob_cut_off, cpu_required, granularity, schedule_policy, mode)
  print("Testing on Model:")
  result_model <- sapply(1:length(ts_names), scheduling_model, test_dataset, coeffs, means, vars, window_size, prob_cut_off, granularity, schedule_policy, mode, simplify = FALSE)
  
  scheduled_num <- cbind(scheduled_num, unlist(result_foreground[1,]))
  unscheduled_num <- cbind(unscheduled_num, unlist(result_foreground[2,]))
  correct_scheduled_num <- cbind(correct_scheduled_num, unlist(result_foreground[3,]))
  correct_unscheduled_num <- cbind(correct_unscheduled_num, unlist(result_foreground[4,]))
  
  
  for (ts_num in 1:length(ts_names)) {
    avg_usage <- rbind(avg_usage, result_model[[ts_num]]$utilization)
    job_survival <- rbind(job_survival, result_model[[ts_num]]$survival)
    if (schedule_policy == "dynamic") {
      overall_runs <- rbind(overall_runs, result_model[[ts_num]]$runs)
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
  colnames(avg_usage) <- "avg_usage"
  rownames(job_survival) <- ts_names
  colnames(job_survival) <- "survival"
  if (schedule_policy == "dynamic") {
    rownames(overall_runs) <- ts_names
    colnames(overall_runs) <- sapply(1:20, function(i) as.character(i))
    result <- list('avg_usage'=avg_usage, 'job_survival'=job_survival, 'scheduled_num'=scheduled_num, "unscheduled_num"=unscheduled_num, "correct_scheduled_num"=correct_scheduled_num, "correct_unscheduled_num"=correct_unscheduled_num, "overall_runs"=overall_runs)
    return(result)  
  } else {
    result <- list('avg_usage'=avg_usage, 'job_survival'=job_survival, 'scheduled_num'=scheduled_num, "unscheduled_num"=unscheduled_num, "correct_scheduled_num"=correct_scheduled_num, "correct_unscheduled_num"=correct_unscheduled_num)
    return(result)
  }
}


wrapper.epoche <- function(parameter, dataset, cpu_required, initial_train_size, output_dp, schedule_policy) {
  
  window_size <- as.numeric(parameter[1])
  prob_cut_off <- as.numeric(parameter[2])
  granularity <- as.numeric(parameter[3])
  
  output <- svt_stationary_model(datase =dataset, window_size=window_size, job_length=1, cpu_required=cpu_required, prob_cut_off=prob_cut_off, initial_train_size=initial_train_size, mode="max", granularity=granularity, schedule_policy=schedule_policy)
  overall_evaluation <- find_overall_evaluation(output$avg_usage[,1], output$job_survival[,1])
  
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
  if (schedule_policy == "dynamic") {
    write.csv(output$overall_runs, paste("Overall Runs", "AR1", sample_size, window_size, prob_cut_off, granularity, ".csv"))
  }
  result_path.xlsx <- update.xlsx.df(result_path.xlsx, "AR1", prob_cut_off, 0, sample_size, window_size, granularity, 0, utilization_rate, survival_rate, correct_scheduled_rate, correct_unscheduled_rate)
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

data_matrix <- matrix(nrow = total_trace_length, ncol = 0)
for (job_num in bg_job_pool) {
  bg_job <- read.csv(paste(bg_jobs_path, job_num, ".csv", sep = ""))
  data_matrix <- cbind(data_matrix, bg_job$max_cpu[1:total_trace_length])
}
rownames(data_matrix) <- seq(1, nrow(data_matrix) ,1)
colnames(data_matrix) <- bg_job_pool

cpu_required <- rep(0, ncol(data_matrix))
for (j in 1:ncol(data_matrix)) {
  cpu_required[j] <- as.numeric(quantile(data_matrix[,j], c(0.15, 0.5, 0.85), type = 4)[cpu_usage])
}

output_dp <- NULL
if (bad.seq.adj) {
  #output_dp <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//Nonoverlapping windows//summary (windows) max post adj.xlsx"
  
  if (schedule_policy == "dynamic") {
    output_dp <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//Nonoverlapping windows//summary dynamic (windows,granularity) post adj.xlsx"
  } else {
    output_dp <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//Nonoverlapping windows//summary (windows,granularity) post adj.xlsx"
  }
} else {
  
  #output_dp <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//Nonoverlapping windows//summary (windows) max.xlsx"
  if (schedule_policy == "dynamic") {
    output_dp <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//Nonoverlapping windows//summary dynamic (windows,granularity).xlsx"
  } else {
    output_dp <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//Nonoverlapping windows//summary (windows,granularity).xlsx"
  }
}

parameter.df <- expand.grid(window_sizes, prob_cut_offs, granularity)
colnames(parameter.df) <- c("window_size", "prob_cut_off", "granularity")
parameter.df <- parameter.df %>%
  arrange(window_size)
slt <- apply(parameter.df, 1, wrapper.epoche, data_matrix, (100-cpu_required), initial_train_size, output_dp, schedule_policy)
