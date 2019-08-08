library("forecast")
library("mvtnorm")
library("dplyr")
library("dict")
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
    ts_model <- arima(train_set[1:nrow(train_set), ts_num], order = c(1,0,0), include.mean = TRUE)
    coeffs[ts_num] <- as.numeric(ts_model$coef[1])
    means[ts_num] <- as.numeric(ts_model$coef[2])
    vars[ts_num] <- ts_model$sigma2
  }
  
  ## Test Model
  current_end <- initial_train_size
  current_percent <- 0.00
  while (current_end <= nrow(test_set)) {
    
    ## Initialize Model 
    predicted_mean <- c()
    
    for (ts_num in 1:ncol(test_set)) {
      
      ## Schedule the job
      if (current_end <= nrow(test_set) - 1) {
        last_obs <- test_set[current_end, ts_num]
        predicted_mean[ts_num] <- prediction_result$mu
      }
    }
    
    ## Update current_end
    current_end <- current_end + update_freq
    if (current_percent != round((current_end - initial_train_size) / (nrow(test_set) - initial_train_size), digits = 2)) {
      print(paste("Testing", current_percent))
      current_percent <- round((current_end - initial_train_size) / (nrow(test_set) - initial_train_size), digits = 2)
    }
  }
  
  ## Change column and row names, N by M
  colnames(predicted_result) <- colnames(test_set)
  rownames(predicted_result) <- rownames(test_set)
  
  result <- list("predicted_mean"=predicted_result, "coeffs"=coeffs, "means"=means, "vars"=vars)
  
  return(result)
}

parser_for_logistic_model <- function(train_set_avg, train_set_max, cpu_required) {
  ts_logistic_input <- list()
  for (ts_num in 1:ncol(train_set_avg)) {
    train_avg <- train_set_avg[1:nrow(train_set_avg),ts_num]
    train_max <- train_set_max[1:nrow(train_set_max),ts_num]
    df <- data.frame("avg"=train_avg, "max"=train_avg)
    df$survived <- factor(ifelse((100 - df$max) >= cpu_required, TRUE, FALSE))
    ts_logistic_input[[ts_num]] <- df
  }
  return(ts_logistic_input)
}

build_conditional_variation <- function(train_set_avg, train_set_max, precision=1, method) {
  if (method == "lm") {
    new_parsed_dat <- data.frame(row.names = 1)
    for (obs in 1:nrow(train_set_avg)) {
      new_parsed_dat <- cbind(new_parsed_dat, c(train_set_max[obs], round(train_set_avg[obs], precision)))
    }
    colnames(new_parsed_dat) <- c('max', 'avg')
    new_parsed_dat <- new_parsed_dat %>% 
      group_by(avg) %>% 
      summarise(vars=var(max)) %>%
      filter(!is.na(vars))
    var.lm <- lm(vars~avg, data = new_parsed_dat)
    return(var.lm)
  } else {
    clustering_result <- list()
    avg_silhouette <- c()
    avg_silhouette[1] <- -Inf
    for (cluster_num in 2:10) {
      clustering_result[[cluster_num]] <- kmeans(train_set_avg, centers = cluster_num, iter.max = 20, nstart = 25)
      avg_silhouette[clustering_num] <- mean(silhouette(clustering_result$cluster, dist = dist(train_set_avg))[,3])
    }
    best_cluster_num <- which(avg_silhouette == max(avg_silhouette))
    best_cluster_result <- clustering_result[[best_cluster_num]]
    new_parsed_dat <- data.frame("cluster"=best_cluster_result$cluster, "max"=train_set_max)
    new_parsed_dat <- new_parsed_dat %>% 
      group_by(cluster) %>% 
      summarise(vars=var(max)) %>%
      filter(!is.na(vars))
    new_parsed_dat$cluster_mean <- best_cluster_result$centers[new_parsed_dat$cluster]
    return(new_parsed_dat)
  }
}

kmeans_find_var <- function(expected_avg, kmeans_model_df) {
  kmeans_model_df <- kmeans_model_df %>%
    arrange(cluster)
  closest_cluster <- kmeans_model_df$cluster[which(abs(expected_avg - kmeans_model_df$cluster_mean) == min(expected_avg - kmeans_model_df$cluster_mean))]
  cluster_var <- kmeans_model_df$vars[kmeans_model_df$cluster == closest_cluster]
  return(cluster_var)
}

generate_expected_conditional_var <- function(expected_avgs, mode, variance_model) {
  expected_var <- c()
  if (mode == "lm") {
    expected_var <- predict(variance_model, newdata=data.frame("avg"=expected_avgs), type = "response")
  } else {
    expected_var <- sapply(expected_avgs, kmeans_find_var, kmeans_model_df = variance_model)
  }
  return(expected_var)
}


compute_pi_up <- function(prob_cross_threshold, expected_var) {
  expected_max
}

logistic_model <- function(train_set_avg, train_set_max, predicted_avgs, update_freq=1) {
  
  fitted_logistic_models <- list()
  fitted_conditional_var_models <- list()
  conditional_var_model_choice <- c()
  parsed_inputs <- parser_for_logistic_model(train_set_avg, train_set_max)
  
  ## Training Step
  for (ts_num in 1:ncol(train_set_avg)) {
    df <- parsed_inputs[[ts_num]]
    log.lm <- glm(survived~avg, data = df, family = "binomial")
    fitted_logistic_models[[ts_num]] <- log.lm
    conditional_var_model <- build_conditional_variation(train_set_avg[1:nrow(train_set_avg), ts_num], train_set_max[1:nrow(train_set_max), ts_num], precision = 1, method = "lm")
    ss <- summary(conditional_var_model)
    lm_good_enough <- pf(ss$fstatistic[1], ss$fstatistic[2], ss$fstatistic[3], lower.tail=FALSE) <= 0.05
    if (!lm_good_enough) {
      conditional_var_model_choice[ts_num] <- "kmeans"
      conditional_var_model <- build_conditional_variation(train_set_avg[1:nrow(train_set_avg), ts_num], train_set_max[1:nrow(train_set_max), ts_num], precision = 1, method = "kmeans")
    } else {
      conditional_var_model_choice[ts_num] <- "lm"
    }
    fitted_conditional_var_models[[ts_num]] <- conditional_var_model
  }
  
  predicted_results <- list()
  expected_ts_variance <- list()
  prob_ts_cross_threshold <- list()
  
  ## Testing Step
  for (ts_num in 1:ncol(train_set_avg)) {
    expected_avgs <- predicted_avgs[seq(1, nrow(predicted_avgs), update_freq), ts_num]
    conditional_var_model <- fitted_conditional_var_models[[ts_num]]
    expected_vars <- generate_expected_conditional_var(expected_avgs, mode = conditional_var_model_choice[ts_num], variance_model = conditional_var_model)
    expected_ts_variance[[ts_num]] <- expected_vars
    prob <- predict(fitted_logistic_models[[ts_num]], newdata = expected_avgs, type = "response")
    prob_ts_cross_threshold[[ts_num]] <- prob
  }
  result <- list("prob"=prob_ts_cross_threshold, "expected_var"=expected_ts_variance)
  return(expected_ts_variance)
}

multi_state_logistic_model <- function(predicted_avgs, initial_train_size, window_size, job_length=5, prob_cut_off=0.01, update_freq=1) {
  
  
  return(result)
}

ar_logistic_model <- function(dataset_avg, dataset_max, initial_train_size, window_size, prob_cut_off, update_freq, job_length, cpu_required) {
  #### input dataset_avg, dataset_max: N by M matrix, N being number of observations, M being number of time series
  #### input initial_train_size: The number of first observations used to train the model
  #### input window_size: The number of observations used to train and predict as one sample
  #### input update_freq: The number of observations for each update of the model, and do the prediction
  #### input prob_cut_off: If the probability of background job exceeding 100-cpu_required is smaller than prob_cut_off, then schedule it. Otherwise, don't.
  #### input job_length: The length of job to be scheduled
  #### input update_freq: The number of windows to do the prediction

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
  rownames(train_set_max) <- seq(1, 1 + window_size * (nrow(train_set_avg) - 1), window_size)
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
  logistic_output <- logistic_model(train_set_avg, train_set_max, predicted_avgs, update_freq=1, job_length, cpu_required, prob_cut_off)
  
  ## N by M dataframe
  probability <- data.frame(row.names = 1)
  ## N by M dataframe
  avg_usage <- data.frame(row.names = 1)
  job_survival <- data.frame(row.names = 1)
  ## N by M dataframe
  predict_result <- data.frame(row.names = 1)
  ## N by M dataframe
  actual_result <- data.frame(row.names = 1)
  ## 4 by M dataframe
  scheduling_summary <- data.frame(matrix(nrow = 4, ncol = ncol(dataset)))
  
  prob_info <- logistic_output$prob
  var_info <- logistic_output$expected_var
  for(ts_num in 1:ncol(dataset_max)) {
    ## Scheduling
    
    
    ## Scoring
    pi_up <- compute_pi_up(prob_info[[ts_num]], var_info[[ts_num]])
  }
}