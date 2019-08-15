library("forecast")
library("dplyr")
library("dict")

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

do_prediction <- function(last_obs, phi, mean, variance) {
  # Construct mean
  mu <- last_obs * phi + (1 - phi) * mean
  # Construct Var-cov matrix
  var <- variance
  result <- list('mu' = mu, 'var'=var)
  return(result)
}

find_evaluation <- function(pi_up, actual_obs, predict_size) {
  usage <- (100 - pi_up) / (100 - actual_obs)
  if (all(actual_obs <= pi_up)) {
    survival = 1
  } else {
    survival = 0
  }
  avg_usage <- mean(usage[!is.infinite(usage) & !is.na(usage) & usage <= 1])
  result <- list('avg_usage' = avg_usage, 'survival'= survival)
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
    ts_model <- arima(train_set[1:nrow(train_set), ts_num], order = c(1,0,0), include.mean = TRUE, method = "ML", optim.control = list(maxit=2000))
    coeffs[ts_num] <- as.numeric(ts_model$coef[1])
    means[ts_num] <- as.numeric(ts_model$coef[2])
    vars[ts_num] <- ts_model$sigma2
  }
  
  ## Test Model
  current_end <- 2
  current_percent <- 0.00
  while (current_end <= nrow(test_set)) {
    
    ## Initialize Model 
    predicted_mean <- c()
    
    for (ts_num in 1:ncol(test_set)) {
      
      ## Schedule the job
      if (current_end <= nrow(test_set)) {
        last_obs <- test_set[(current_end-1), ts_num]
        predicted_mean[ts_num] <- do_prediction(last_obs, coeffs[ts_num], means[ts_num], vars[ts_num])$mu
      }
    }
    
    predicted_result <- rbind(predicted_result, predicted_mean)
    
    ## Update current_end
    current_end <- current_end + update_freq
    if (current_percent != round(current_end / nrow(test_set), digits = 2)) {
      print(paste("Testing", current_percent))
      current_percent <- round(current_end / nrow(test_set), digits = 2)
    }
  }
  
  ## Change column and row names, N by M
  colnames(predicted_result) <- colnames(test_set)
  rownames(predicted_result) <- rownames(test_set)[-1]
  
  return(predicted_result)
}

ar1_hidden_layer_wrapper <- function(dataset_max, dataset_avg, initial_train_size, window_size, update_freq) {
  
  ## Split Training Set and Test Set
  train_dataset_max <- dataset_max[1:initial_train_size,1:ncol(dataset_max)]
  test_dataset_max <- dataset_max[(initial_train_size+1):nrow(dataset_max),1:ncol(dataset_max)]
  train_dataset_avg <- dataset_avg[1:initial_train_size,1:ncol(dataset_avg)]
  test_dataset_avg <- dataset_avg[(initial_train_size+1):nrow(dataset_avg),1:ncol(dataset_avg)]
  
  ## Convert frequency
  new_trainset_max <- matrix(nrow = floor(nrow(train_dataset_max) / window_size), ncol = 0)
  new_testset_max <- matrix(nrow = floor(nrow(test_dataset_max) / window_size), ncol = 0)
  new_trainset_avg <- matrix(nrow = floor(nrow(train_dataset_avg) / window_size), ncol = 0)
  new_testset_avg <- matrix(nrow = floor(nrow(test_dataset_avg) / window_size), ncol = 0)
  
  for (ts_num in 1:ncol(dataset_max)) {
    converted_dat <- convert_frequency_dataset(train_dataset_max[1:nrow(train_dataset_max), ts_num], window_size, "max")
    new_trainset_max <- cbind(new_trainset_max, converted_dat)
    converted_dat <- convert_frequency_dataset(test_dataset_max[1:nrow(test_dataset_max), ts_num], window_size, "max")
    new_testset_max <- cbind(new_testset_max, converted_dat)
    converted_dat <- convert_frequency_dataset(train_dataset_avg[1:nrow(train_dataset_avg), ts_num], window_size, "avg")
    new_trainset_avg <- cbind(new_trainset_avg, converted_dat)
    converted_dat <- convert_frequency_dataset(test_dataset_avg[1:nrow(test_dataset_avg), ts_num], window_size, "avg")
    new_testset_avg <- cbind(new_testset_avg, converted_dat)
  }
  
  rownames(new_trainset_max) <- seq(4033, 4033 + window_size * (nrow(new_trainset_max) - 1), window_size)
  colnames(new_trainset_max) <- colnames(train_dataset_max)
  rownames(new_testset_max) <- seq(initial_train_size + 4033, initial_train_size + 4033 + window_size * (nrow(new_testset_max) - 1), window_size)
  colnames(new_testset_max) <- colnames(test_dataset_max)
  rownames(new_trainset_avg) <- seq(4033, 4033 + window_size * (nrow(new_trainset_avg) - 1), window_size)
  colnames(new_trainset_avg) <- colnames(train_dataset_avg)
  rownames(new_testset_avg) <- seq(initial_train_size + 4033, initial_train_size + 4033 + window_size * (nrow(new_testset_avg) - 1), window_size)
  colnames(new_testset_avg) <- colnames(test_dataset_avg)
  
  ar_predicted_result <- ar1_model(train_set = new_trainset_avg, test_set = new_testset_avg, update_freq = 1)
  
  write.csv(new_trainset_max, file = paste(window_size, "trainset_max.csv", sep = ""))
  write.csv(new_testset_max, file = paste(window_size, "testset_max.csv", sep = ""))
  write.csv(new_trainset_avg, file = paste(window_size, "trainset_avg.csv", sep = ""))
  write.csv(new_testset_avg, file = paste(window_size, "testset_avg.csv", sep = ""))
  write.csv(ar_predicted_result, file = paste(window_size, "testset_predicted_avg.csv", sep = ""))
}


bg_job_pool <- read.csv("C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//pythonscripts//list of sampled 100 bg jobs.csv")[,2]
bg_jobs_path = "C://Users//carlo//Documents//sample background jobs//"

data_matrix_max <- matrix(nrow = 4000, ncol = 0)
data_matrix_avg <- matrix(nrow = 4000, ncol = 0)
for (job_num in bg_job_pool) {
  bg_job <- read.csv(paste(bg_jobs_path, job_num, ".csv", sep = ""))
  data_matrix_max <- cbind(data_matrix_max, bg_job$max_cpu[4033:8032])
  data_matrix_avg <- cbind(data_matrix_avg, bg_job$avg_cpu[4033:8032])
}
rownames(data_matrix_max) <- seq(1, 1 + (nrow(data_matrix_max) - 1),1)
colnames(data_matrix_max) <- bg_job_pool
rownames(data_matrix_avg) <- seq(1, 1 + (nrow(data_matrix_avg) - 1),1)
colnames(data_matrix_avg) <- bg_job_pool

for (window_size in c(12, 36)) {
  ar1_hidden_layer_wrapper(dataset_max = data_matrix_max, dataset_avg = data_matrix_avg, initial_train_size = 2000, window_size = window_size, update_freq = 1)
}
write.csv(bg_job_pool, "filenames.csv")