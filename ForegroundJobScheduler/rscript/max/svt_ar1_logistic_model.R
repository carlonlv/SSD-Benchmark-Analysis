library("forecast")
library("mvtnorm")
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

ar1_model <- function(dataset, initial_train_size, window_size, update_freq=1) {
  #### input dataset: N by M matrix, N being number of observations, M being number of time series
  #### input initial_train_size: The number of first observations used to train the model
  #### input window_size: The number of observations used to train and predict
  #### input update_freq: The number of observations for each update of the model, and do the prediction

  ## N by M dataframe
  predicted_result <- data.frame(row.names = 1)

  ## Summary Counts
  scheduled_num <- rep(0, ncol(dataset))
  unscheduled_num <- rep(0, ncol(dataset))
  falsely_scheduled_num <- rep(0, ncol(dataset))
  falsely_unscheduled_num <- rep(0, ncol(dataset))
  
  ## Convert Frequency
  new_dat <- matrix(nrow = floor(initial_train_size / window_size), ncol = 0)
  for (ts_num in 1:ncol(dataset)) {
    converted_data <- convert_frequency_dataset(dataset[1:initial_train_size, ts_num], window_size, 'max')
    new_dat <- cbind(new_dat, converted_data)
  }
  rownames(new_dat) <- seq(1, 1 + (window_size * 5) * (nrow(new_dat) - 1), (window_size * 5))
  colnames(new_dat) <- colnames(dataset)
  
  ## Train Model
  coeffs <- rep(NA, ncol(new_dat))
  means <- rep(NA, ncol(new_dat))
  vars <- rep(NA, ncol(new_dat))
  train_percent <- 0.00
  for (ts_num in 1:ncol(new_dat)) {
    if (round(ts_num / ncol(new_dat), 2) != train_percent) {
      print(paste("Training", train_percent))
      train_percent <- round(ts_num / ncol(new_dat), 2)
    }
    ts_model <- arima(new_dat[1:floor(initial_train_size / window_size), ts_num], order = c(1,0,0), include.mean = TRUE)
    coeffs[ts_num] <- as.numeric(ts_model$coef[1])
    means[ts_num] <- as.numeric(ts_model$coef[2])
    vars[ts_num] <- ts_model$sigma2
  }
  
  ## Test Model
  current_end <- initial_train_size
  current_percent <- 0.00
  while (current_end <= nrow(dataset)) {
    
    ## Initialize Model 
    predicted_mean <- c()
    avg_cycle_used <- c()
    survival <- c()

    for (ts_num in 1:ncol(dataset)) {
      
      ## Schedule the job
      if (current_end <= nrow(dataset) - window_size) {
        last_obs <- dataset[current_end, ts_num]
        predicted_mean[ts_num] <- prediction_result$mu
      }
    }
    
    ## Update current_end
    current_end <- current_end + update_freq
    if (current_percent != round((current_end - initial_train_size) / (nrow(dataset) - initial_train_size), digits = 2)) {
      print(paste("Testing", current_percent))
      current_percent <- round((current_end - initial_train_size) / (nrow(dataset) - initial_train_size), digits = 2)
    }
  }
  
  ## Change column and row names, N by M
  colnames(predicted_result) <- colnames(dataset)
  rownames(predicted_result) <- seq(initial_train_size + 1, initial_train_size + 1 + update_freq * (nrow(predicted_result) - 1), update_freq)
  
  result <- list("predicted_mean" = predicted_result, "coeffs"=coeffs, "means"=means, "vars"=vars)
  
  return(result)
}

logistic_model <- function(predicted_avgs, initial_train_size, window_size, job_length=5, cpu_required, prob_cut_off=0.01, update_freq=1) {
  
}


multilayer_model <- function() {
  
}