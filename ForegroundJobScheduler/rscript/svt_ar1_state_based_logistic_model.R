library("forecast")
library("mvtnorm")
library("dplyr")
library("dict")
library("arules")
library("cluster")
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


do_prediction <- function(last_obs, phi, mean, variance) {
  # Construct mean
  mu <- last_obs * phi + (1 - phi) * mean
  # Construct Var-cov matrix
  var <- variance
  result <- list('mu' = mu, 'var'=var)
  return(result)
}


train_ar1_model <- function(ts_num, train_dataset) {
  ts_model <- tryCatch({
    arima(x=train_dataset[, ts_num], order = c(1,0,0), include.mean = TRUE, method = "CSS-ML", optim.control = list(maxit=2000))
  }, error = function(cond) {
    return(arima(x=train_dataset[, ts_num], order = c(1,0,0), include.mean = TRUE, method = "ML", optim.control = list(maxit=2000)))
  })
  return(list("coeffs"=as.numeric(ts_model$coef[1]), "means"= as.numeric(ts_model$coef[2]), "vars"=ts_model$sigma2))
}


upper_state_num <- function(cpu_required ,num_of_states) {
  return(floor((100 - cpu_required) / (100 / num_of_states)))
}


multi_state_logistic_model <- function(train_set_avg, train_set_max, predicted_avgs, num_of_states) {
  
  parsed_states_input <- parser_for_logistic_model_states(train_set_avg, train_set_max, discretize(0:100, method = "interval", breaks = num_of_states, onlycuts = TRUE))
  
  ## Training Step
  state_based_logistic <- list()
  
  train_percent <- 0.00
  for (ts_num in 1:ncol(train_set_avg)) {
    
    if (round(ts_num / ncol(train_set_avg), 2) != train_percent) {
      print(paste("Training", train_percent))
      train_percent <- round(ts_num / ncol(train_set_avg), 2)
    }
    
    for (state in 1:num_of_states) {
      df <- parsed_states_input[[paste(ts_num, ",", state, sep = "")]]
      log.lm <- glm(survived~avg, data = df, family = "binomial", control=glm.control(maxit=50))
      state_based_logistic[[paste(ts_num, ",", state, sep = "")]] <- log.lm
    }
  }
  
  ## Testing Step
  job_based_probability <- list()
  
  test_percent <- 0.00
  for (ts_num in 1:ncol(train_set_avg)) {
    
    if (round(ts_num / ncol(train_set_avg), 2) != test_percent) {
      print(paste("Testing", test_percent))
      test_percent <- round(ts_num / ncol(train_set_avg), 2)
    }
    
    probability <- data.frame(matrix(nrow = nrow(predicted_avgs), ncol = num_of_states))
    for (state in 1:num_of_states) {
      expected_avgs <- predicted_avgs[, ts_num]
      prob <- predict(state_based_logistic[[paste(ts_num, ",", state, sep = "")]], newdata = data.frame("avg"=expected_avgs), type = "response")
      if (state != 1) {
        prob[which(prob < probability[,(state - 1)])] <- 1
      } 
      probability[,state] <- prob
    }
    job_based_probability[[ts_num]] <- probability
  }
  
  return(job_based_probability)
}


parser_for_logistic_model_states <- function(ts_num, train_set_avg, train_set_max, breaks) {
  ts_logistic_input <- list()
  for (state in 1:(length(breaks) - 1)) {
    train_avg <- train_set_avg[,ts_num]
    train_max <- train_set_max[,ts_num]
    df <- data.frame("avg"=train_avg, "max"=train_max)
    df$survived <- factor(ifelse(train_max <= breaks[state+1], 1, 0), levels = c(0, 1))
    ts_logistic_input[[state]] <- df
  }
  return(ts_logistic_input)
}


train_multi_state_logistic_model <- function(ts_num, train_set_avg, train_set_max, parsed_states_input, num_of_states) {
  
  multi_state_logistic <- list()
  for (state in 1:num_of_states) {
    df <- parsed_states_input[[ts_num]][[state]]
    log.lm <- glm(survived~avg, data = df, family = "binomial", control=glm.control(maxit=50))
    multi_state_logistic[[state]] <- log.lm
  }
  
  return(multi_state_logistic)
}


ar_logistic_model <- function(dataset_avg, dataset_max, initial_train_size, prob_cut_off, update_freq, window_size, cpu_required, num_of_states, granularity) {
  #### input dataset_avg, dataset_max: N by M matrix, N being number of observations, M being number of time series
  #### input initial_train_size: The number of first observations used to train the model
  #### input window_size: The number of observations used to train and predict as one sample
  #### input update_freq: The number of observations for each update of the model, and do the prediction
  #### input prob_cut_off: If the probability of background job exceeding 100-cpu_required is smaller than prob_cut_off, then schedule it. Otherwise, don't.
  #### input job_length: The length of job to be scheduled
  #### input update_freq: The number of windows to do the prediction
  
  if (granularity > 0) {
    cpu_required <- sapply(cpu_required, round_to_nearest, granularity, FALSE)
  }
  
  ## Convert Frequency
  ts_names <- colnames(dataset_avg)
  
  scheduled_num <- data.frame(matrix(nrow=length(ts_names), ncol=0))
  unscheduled_num <- data.frame(matrix(nrow=length(ts_names), ncol=0))
  correct_scheduled_num <- data.frame(matrix(nrow=length(ts_names), ncol=0))
  correct_unscheduled_num <- data.frame(matrix(nrow=length(ts_names), ncol=0))
  
  avg_usage <- data.frame(matrix(nrow=length(ts_names), ncol=0))
  job_survival <- data.frame(matrix(nrow=length(ts_names), ncol=0))
  overall_runs <- data.frame(matrix(nrow=length(ts_names), ncol = 0))
  
  ## Lists
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
  colnames(new_trainset_avg) <- ts_name
  
  ## Training AR1 Model
  print("Training: AR1.")
  train_result <- sapply(1:length(ts_names), train_ar1_model, new_trainset_avg)
  coeffs <- unlist(train_result[1,])
  means <- unlist(train_result[2,])
  vars <- unlist(train_result[3,])
  
  ## Training State Based Logistic Model
  print("Training: Logistic.")
  logistic_inputs <- sapply(1:length(ts_names), parser_for_logistic_model_states, train_set_avg, train_set_max, discretize(0:100, method = "interval", breaks = num_of_states, onlycuts = TRUE))
  
  sapply(1:length(ts_names))
  
  
  for(ts_num in 1:ncol(test_set_max)) {
    
    state_num <- upper_state_num(cpu_required[ts_num], num_of_states)
    
    ## Store Probability
    if (state_num == 0) {
      probability[,ts_num] <- rep(1, nrow(probability))
      warning("State number might not be large enough to calclulate probability for some traces.")
    } else {
      probability[,ts_num] <- (1 - job_based_prob[[ts_num]][,state_num])
    }
    
    ## Scoring
    pi_up <- compute_pi_up(job_based_prob[[ts_num]], prob_cut_off, granularity)
    pi_upper_bounds[,ts_num] <- pi_up
    
    ##Evaluation
    evaluation <- find_evaluation(pi_up, test_set_max[, ts_num][-1], granularity)
    avg_usage[,ts_num] <- evaluation$usage
    job_survival[,ts_num] <- evaluation$survival
    
    ## Scheduling
    predicted <- ifelse(probability[,ts_num] <= prob_cut_off, 1, 0)
    predict_result[,ts_num] <- predicted
    
    ## Actual
    actual <- ifelse(test_set_max[,ts_num][-1] <= (100 - cpu_required[ts_num]), 1, 0)
    actual_result[,ts_num] <- actual
    
    ## Summary
    scheduled_num[ts_num] <- sum(predicted)
    unscheduled_num[ts_num] <- length(predicted) - sum(predicted)
    falsely_scheduled_num[ts_num] <- sum(ifelse(predicted != actual & predicted == 1, 1, 0))
    falsely_unscheduled_num[ts_num] <- sum(ifelse(predicted != actual & predicted == 0, 1, 0))
    
    if (evaluation_percent != round(ts_num / ncol(dataset_max), digits = 2)) {
      print(paste("Evaluating", evaluation_percent))
      evaluation_percent <- round(ts_num / ncol(dataset_max), digits = 2)
    }
  }
  
  colnames(probability) <- colnames(dataset_max)
  rownames(probability) <- seq(initial_train_size + window_size + 1, initial_train_size + window_size + 1 + update_freq * (nrow(probability) - 1), update_freq)
  
  colnames(pi_upper_bounds) <- colnames(dataset_max)
  rownames(pi_upper_bounds) <- seq(initial_train_size + window_size + 1, initial_train_size + window_size + 1 + update_freq * (nrow(pi_upper_bounds) - 1), update_freq)
  
  colnames(avg_usage) <- colnames(dataset_max)
  rownames(avg_usage) <- seq(initial_train_size + window_size + 1, initial_train_size + window_size + 1 + update_freq * (nrow(avg_usage) - 1), update_freq)
  
  colnames(job_survival) <- colnames(dataset_max)
  rownames(job_survival) <- seq(initial_train_size + window_size + 1, initial_train_size + window_size + 1 + update_freq * (nrow(job_survival) - 1), update_freq)
  
  colnames(predict_result) <- colnames(dataset_max)
  rownames(predict_result) <- seq(initial_train_size + window_size + 1, initial_train_size + window_size + 1 + update_freq * (nrow(predict_result) - 1), update_freq)
  
  colnames(actual_result) <- colnames(dataset_max)
  rownames(actual_result) <- seq(initial_train_size + window_size + 1, initial_train_size + window_size + 1 + update_freq * (nrow(actual_result) - 1), update_freq)
  
  colnames(scheduling_summary) <- colnames(dataset_max)
  scheduling_summary[1,] <- scheduled_num
  scheduling_summary[2,] <- unscheduled_num
  scheduling_summary[3,] <- falsely_scheduled_num
  scheduling_summary[4,] <- falsely_unscheduled_num
  rownames(scheduling_summary) <- c('Scheduled_Num', 'Unscheduled_Num', 'Falsely_scheduled_Num', 'Falsely_unscheduled_Num')
  
  result <- list('prob' = probability, 'pi_up' = pi_upper_bounds, 'avg_usage'=avg_usage, 'job_survival'=job_survival, 'predict' = predict_result, 'actual' = actual_result, 'scheduling_summary' = scheduling_summary)
  return(result)
}


wrapper.epoche <- function(parameter, dataset_avg, dataset_max, cpu_required, initial_train_size, update_freq, bad.seq.adj, output_dp) {
  
  job_length <- as.numeric(parameter[1])
  prob_cut_off <- as.numeric(parameter[2])
  num_of_states <- as.numeric(parameter[3])
  granularity <- as.numeric(parameter[4])
  
  output <- ar_logistic_model(dataset_avg=dataset_avg, dataset_max=dataset_max, job_length=job_length, cpu_required=cpu_required, prob_cut_off=prob_cut_off, initial_train_size=initial_train_size, update_freq=1, num_of_states=num_of_states, granularity=granularity)
  
  overall_evaluation <- find_overall_evaluation(output$avg_usage, output$job_survival, bad.seq.adj)
  avg_utilization <- overall_evaluation$avg_utilization
  survival <- overall_evaluation$survival
  
  scheduled_num <- sum(output$scheduling_summary[1,])
  unscheduled_num <- sum(output$scheduling_summary[2,])
  correct_scheduled_num <- scheduled_num - sum(output$scheduling_summary[3,])
  correct_unscheduled_num <- unscheduled_num - sum(output$scheduling_summary[4,])
  correct_scheduled_rate <- correct_scheduled_num / scheduled_num
  correct_unscheduled_rate <- correct_unscheduled_num / unscheduled_num
  
  print(paste("Avg cycle used:", "job length", job_length, avg_utilization))
  print(paste("Job survival rate:", "job length", job_length, survival))
  print(paste("Scheduling summary:", "Correct scheduled rate:", correct_scheduled_rate, "Correct unscheduled rate:", correct_unscheduled_rate))
  
  write.csv(output$pi_up, file = paste("AR1_state_based_logistic",job_length, num_of_states, sample_size, prob_cut_off, granularity, "pi_upper.csv"))
  write.csv(output$scheduling_summary, file = paste("AR1_state_based_logistic", job_length, num_of_states, sample_size, prob_cut_off, granularity, "scheduling_sum.csv"))
  write.csv(output$avg_usage, file = paste("AR1_state_based_logistic",job_length, num_of_states, sample_size, prob_cut_off, granularity, "avg_usage.csv"))
  write.csv(output$job_survival, file = paste("AR1_state_based_logistic", job_length, num_of_states, sample_size, prob_cut_off, granularity, "job_survival.csv"))
  
  result_path.xlsx <- read.xlsx(output_dp, sheetIndex = 1)
  result_path.xlsx <- update.xlsx.df(result_path.xlsx, "AR1_state_based_logistic", prob_cut_off, num_of_states, sample_size, job_length, granularity, avg_utilization, survival, correct_scheduled_rate, correct_unscheduled_rate)
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
num_of_states_pool <- c(5, 8, 10, 16, 20, 30, 50)
granularity <- c(10, 100/32, 100/64, 100/128, 0)

bg_jobs_path = "C://Users//carlo//Documents//sample background jobs//"
bg_job_pool <- NULL
if (sample_size == 100 ) {
  bg_job_pool <- read.csv("C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//pythonscripts//list of sampled 100 background jobs.csv")[,1]
  bg_job_pool <- sub(".pd", "", bg_job_pool)
} else {
  bg_job_pool <- read.csv("C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//pythonscripts//list of sampled background jobs.csv")[,1]
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

parameter.df <- expand.grid(window_sizes, prob_cut_offs, num_of_states_pool, granularity)
colnames(parameter.df) <- c("job_length", "prob_cut_off", "num_of_states", "granularity")
parameter.df <- parameter.df %>%
  arrange(num_of_states)
slt <- apply(parameter.df, 1, wrapper.epoche, dataset_avg=data_matrix_avg, dataset_max=data_matrix_max, cpu_required=(100-cpu_required), initial_train_size=initial_train_size, update_freq=1, bad.seq.adj=bad.seq.adj, output_dp=output_dp)
