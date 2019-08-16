library("forecast")
library("mvtnorm")
library("dplyr")
library("dict")
library("arules")
library("cluster")

convert_frequency_dataset <- function(dataset, new_freq, mode) {
  new_avg_cpu <- c()
  window_num <- NULL
  window_num <- floor(length(dataset) / new_freq)
  for (i in 1:window_num) {
    from <- (i - 1) * new_freq + 1
    to <- i * new_freq
    new_val <- NULL
    if (mode == 'max') {
      new_val <- max(dataset[from:to], na.rm = TRUE)
    } else {
      new_val <- mean(dataset[from:to], na.rm = TRUE)
    }
    new_avg_cpu <- c(new_avg_cpu, new_val)
  }
  return(new_avg_cpu)
}

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

find_evaluation <- function(pi_up, actual_obs) {
  usage <- (100 - pi_up) / (100 - actual_obs)
  survival <- ifelse(actual_obs <= pi_up, 1, 0)
  result <- list('usage' = ifelse(usage <= 1, usage, NA), 'survival'= survival)
  return(result)
}

ar1_model <- function(train_set, test_set, update_freq=1) {
  
  ## N by M dataframe
  predicted_result <- data.frame(row.names = 1)
  
  ## Train Model
  coeffs <- rep(NA, ncol(train_set))
  means <- rep(NA, ncol(train_set))
  vars <- rep(NA, ncol(train_set))
  
  train_percent <- 0.00
  for (ts_num in 1:ncol(train_set)) {
    if (round(ts_num / ncol(train_set), 2) != train_percent) {
      print(paste("Training", train_percent))
      train_percent <- round(ts_num / ncol(train_set), 2)
    }
    ts_model <- arima(train_set[, ts_num], order = c(1,0,0), include.mean = TRUE, method = "ML", optim.control = list(maxit=2000))
    coeffs[ts_num] <- as.numeric(ts_model$coef[1])
    means[ts_num] <- as.numeric(ts_model$coef[2])
    vars[ts_num] <- ts_model$sigma2
  }
  
  ## Test Model
  current_end <- 2
  test_percent <- 0.00
  while (current_end <= nrow(test_set)) {
    
    ## Initialize Model 
    predicted_mean <- c()
    
    for (ts_num in 1:ncol(test_set)) {
      
      ## Schedule the job
      last_obs <- test_set[(current_end - 1), ts_num]
      predicted_mean[ts_num] <- do_prediction(last_obs, coeffs[ts_num], means[ts_num], vars[ts_num])$mu
    }
    predicted_result <- rbind(predicted_result, predicted_mean)
    
    ## Update current_end
    current_end <- current_end + update_freq
    if (test_percent != round((current_end - 1) / nrow(test_set), digits = 2)) {
      print(paste("Testing", test_percent))
      test_percent <- round((current_end - 1) / nrow(test_set), digits = 2)
    }
  }
  
  ## Change column and row names, N by M
  colnames(predicted_result) <- colnames(test_set)
  rownames(predicted_result) <- rownames(test_set)[-1]
  
  return(predicted_result)
}

parser_for_logistic_model_states <- function(train_set_avg, train_set_max, breaks) {
  ts_logistic_input <- list()
  for (ts_num in 1:ncol(train_set_avg)) {
    
    for (state in 1:(length(breaks) - 1)) {
      train_avg <- train_set_avg[,ts_num]
      train_max <- train_set_max[,ts_num]
      df <- data.frame("avg"=train_avg, "max"=train_max)
      df$survived <- factor(ifelse(train_max <= breaks[state+1], 1, 0), levels = c(0, 1))
      ts_logistic_input[[paste(ts_num, ",", state, sep = "")]] <- df
    }
  }
  return(ts_logistic_input)
}

compute_pi_up <- function(job_based_prob, prob_cut_off) {
  
  num_of_states <- ncol(job_based_prob)
  num_of_obs <- nrow(job_based_prob)
  
  pi_up <- rep(NA, num_of_obs)
  for (obs in 1:num_of_obs) {
    current_state <- 1
    current_prob <- job_based_prob[,current_state]
    while (current_prob < (1 - prob_cut_off) & current_state < num_of_states) {
      current_state <- current_state + 1
      current_prob <- job_based_prob[,current_state]
    }
    pi_up[obs] <- current_state * (100 / num_of_states)
  }
  return(pi_up)
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

ar_logistic_model <- function(dataset_avg, dataset_max, initial_train_size, prob_cut_off, update_freq, job_length, cpu_required, num_of_states) {
  #### input dataset_avg, dataset_max: N by M matrix, N being number of observations, M being number of time series
  #### input initial_train_size: The number of first observations used to train the model
  #### input window_size: The number of observations used to train and predict as one sample
  #### input update_freq: The number of observations for each update of the model, and do the prediction
  #### input prob_cut_off: If the probability of background job exceeding 100-cpu_required is smaller than prob_cut_off, then schedule it. Otherwise, don't.
  #### input job_length: The length of job to be scheduled
  #### input update_freq: The number of windows to do the prediction
  
  window_size <- job_length
  ## Convert Frequency
  train_set_avg <- matrix(nrow = floor(initial_train_size / window_size), ncol = 0)
  train_set_max <- matrix(nrow = floor(initial_train_size / window_size), ncol = 0)
  for (ts_num in 1:ncol(dataset_avg)) {
    converted_data <- convert_frequency_dataset(dataset_avg[1:initial_train_size, ts_num], window_size, 'avg')
    train_set_avg <- cbind(train_set_avg, converted_data)
    converted_data <- convert_frequency_dataset(dataset_max[1:initial_train_size, ts_num], window_size, 'max')
    train_set_max <- cbind(train_set_max, converted_data)
  }
  rownames(train_set_avg) <- seq(1, 1 + window_size * (nrow(train_set_avg) - 1), window_size)
  colnames(train_set_avg) <- colnames(dataset_avg)
  rownames(train_set_max) <- seq(1, 1 + window_size * (nrow(train_set_max) - 1), window_size)
  colnames(train_set_max) <- colnames(dataset_max)
  
  test_set_avg <- matrix(nrow = floor((nrow(dataset_avg) - initial_train_size) / window_size), ncol = 0)
  test_set_max <- matrix(nrow = floor((nrow(dataset_max) - initial_train_size) / window_size), ncol = 0)
  for (ts_num in 1:ncol(dataset_avg)) {
    converted_data <- convert_frequency_dataset(dataset_avg[(initial_train_size+1):nrow(dataset_avg), ts_num], window_size, 'max')
    test_set_avg <- cbind(test_set_avg, converted_data)
    converted_data <- convert_frequency_dataset(dataset_max[(initial_train_size+1):nrow(dataset_max), ts_num], window_size, 'avg')
    test_set_max <- cbind(test_set_max, converted_data)
  }
  rownames(test_set_avg) <- seq(initial_train_size + 1, initial_train_size + 1 + window_size * (nrow(test_set_avg) - 1), window_size)
  colnames(test_set_avg) <- colnames(dataset_avg)
  rownames(test_set_max) <- seq(initial_train_size + 1, initial_train_size + 1 + window_size * (nrow(test_set_max) - 1), window_size)
  colnames(test_set_max) <- colnames(dataset_max)
  
  print("First layer: AR1.")
  test_avgs <- ar1_model(train_set_avg, test_set_avg, update_freq=1)
  
  print("Second layer: Logistic Regression.")
  job_based_prob <- multi_state_logistic_model(train_set_avg, train_set_max, test_avgs, num_of_states)
  
  ## N by M dataframe
  probability <- data.frame(matrix(nrow = (nrow(test_set_max) - 1), ncol = ncol(test_set_max)))
  ## N by M dataframe
  avg_usage <- data.frame(matrix(nrow = (nrow(test_set_max) - 1), ncol = ncol(test_set_max)))
  job_survival <- data.frame(matrix(nrow = (nrow(test_set_max) - 1), ncol = ncol(test_set_max)))
  ## N by M dataframe
  predict_result <- data.frame(matrix(nrow = (nrow(test_set_max) - 1), ncol = ncol(test_set_max)))
  ## N by M dataframe
  actual_result <- data.frame(matrix(nrow = (nrow(test_set_max) - 1), ncol = ncol(test_set_max)))
  ## 4 by M dataframe
  scheduling_summary <- data.frame(matrix(nrow = 4, ncol = ncol(test_set_max)))
  
  scheduled_num <- rep(0, ncol(test_set_max))
  unscheduled_num <- rep(0, ncol(test_set_max))
  falsely_scheduled_num <- rep(0, ncol(test_set_max))
  falsely_unscheduled_num <- rep(0, ncol(test_set_max))
  
  evaluation_percent <- 0.00
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
    pi_up <- NULL
    if (state_num == 0) {
      pi_up <- rep(0, nrow(probability))
    } else {
      pi_up <- compute_pi_up(job_based_prob[[ts_num]], prob_cut_off)
    }
    
    ##Evaluation
    evaluation <- find_evaluation(pi_up, test_set_max[, ts_num][-1])
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
  
  result <- list('prob' = probability, 'avg_usage'=avg_usage, 'job_survival'=job_survival, 'predict' = predict_result, 'actual' = actual_result, 'scheduling_summary' = scheduling_summary)
  return(result)
}

## Read back ground job pool

arg <- commandArgs(trailingOnly = TRUE)
sample_size <- 100
window_size <- 36
job_length <- window_size
cpu_usage <- 3
prob_cut_off <- 0.1
num_of_states <- 100

cat(arg, sep = "\n")

bg_jobs_path = "C://Users//carlo//Documents//sample background jobs//"
bg_job_pool <- NULL
if (sample_size == 100 ) {
  bg_job_pool <- read.csv("C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//pythonscripts//list of sampled 100 bg jobs.csv")[,2]
} else {
  bg_job_pool <- read.csv("C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//pythonscripts//list of sampled background jobs.csv")[,1]
  bg_job_pool <- sub(".pd", "", bg_job_pool)
}

data_matrix_avg <- matrix(nrow = 4000, ncol = 0)
data_matrix_max <- matrix(nrow = 4000, ncol = 0)
for (job_num in bg_job_pool) {
  bg_job <- read.csv(paste(bg_jobs_path, job_num, ".csv", sep = ""))
  data_matrix_avg <- cbind(data_matrix_avg, bg_job$avg_cpu[4033:8032])
  data_matrix_max <- cbind(data_matrix_max, bg_job$max_cpu[4033:8032])
}
rownames(data_matrix_avg) <- seq(1, 1 + 5 * (nrow(data_matrix_avg) - 1),5)
rownames(data_matrix_max) <- seq(1, 1 + 5 * (nrow(data_matrix_max) - 1),5)
colnames(data_matrix_avg) <- bg_job_pool
colnames(data_matrix_max) <- bg_job_pool

cpu_required <- rep(0, ncol(data_matrix_max))
for (j in 1:ncol(data_matrix_max)) {
  cpu_required[j] <- as.numeric(quantile(data_matrix_max[,j], c(0.15, 0.5, 0.85), type = 4)[cpu_usage])
}

output <- ar_logistic_model(dataset_avg=data_matrix_avg, dataset_max = data_matrix_max, job_length=job_length, cpu_required=(100-cpu_required), prob_cut_off=prob_cut_off, initial_train_size = 2000, update_freq=1, num_of_states = num_of_states)
write.csv(output$avg_usage, file = paste("AR1_logistic_state",job_length, num_of_states, sample_size, prob_cut_off, "avg_usage.csv"))
print(paste("Avg cycle used:", "job length", job_length, num_of_states, mean(as.matrix(output$avg_usage), na.rm = TRUE)))
write.csv(output$job_survival, file = paste("AR1_logistic_state",job_length, num_of_states, sample_size, prob_cut_off,"job_survival.csv"))
print(paste("Job survival rate:", "job length", job_length, num_of_states, sum(as.matrix(output$job_survival)) / (length(as.matrix(output$job_survival)))))
write.csv(output$scheduling_summary, file = paste("AR1_logistic_state", job_length, num_of_states, sample_size, prob_cut_off, "scheduling_sum.csv"))
scheduled_num <- sum(output$scheduling_summary[1,])
unscheduled_num <- sum(output$scheduling_summary[2,])
correct_scheduled_num <- scheduled_num - sum(output$scheduling_summary[3,])
correct_unscheduled_num <- unscheduled_num - sum(output$scheduling_summary[4,])
print(paste("Scheduling summary:", "Correct scheduled rate:", correct_scheduled_num / scheduled_num, "Correct unscheduled rate:", correct_unscheduled_num / unscheduled_num))