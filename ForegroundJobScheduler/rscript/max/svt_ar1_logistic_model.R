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

find_evaluation <- function(pi_up, actual_obs) {
  usage <- (100 - pi_up) / (100 - actual_obs)
  survival <- ifelse(actual_obs <= pi_up, 1, 0)
  result <- list('usage' = usage, 'survival'= survival)
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
    ts_model <- arima(train_set[, ts_num], order = c(1,0,0), include.mean = TRUE, method = "ML")
    coeffs[ts_num] <- as.numeric(ts_model$coef[1])
    means[ts_num] <- as.numeric(ts_model$coef[2])
    vars[ts_num] <- ts_model$sigma2
  }
  
  ## Test Model
  current_end <- 1
  current_percent <- 0.00
  while (current_end <= nrow(test_set)) {
    
    ## Initialize Model 
    predicted_mean <- c()
    
    for (ts_num in 1:ncol(test_set)) {
      
      ## Schedule the job
      if (current_end <= nrow(test_set)) {
        last_obs <- test_set[current_end, ts_num]
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
  rownames(predicted_result) <- rownames(test_set)
  
  return(predicted_result)
}

parser_for_logistic_model <- function(train_set_avg, train_set_max, cpu_required) {
  ts_logistic_input <- list()
  for (ts_num in 1:ncol(train_set_avg)) {
    train_avg <- train_set_avg[,ts_num]
    train_max <- train_set_max[,ts_num]
    df <- data.frame("avg"=train_avg, "max"=train_max)
    df$survived <- as.factor(ifelse((100 - df$max) >= cpu_required[ts_num], 1, 0))
    ts_logistic_input[[ts_num]] <- df
  }
  return(ts_logistic_input)
}

build_conditional_variation <- function(train_set_avg, train_set_max, precision=1, method) {
  if (method == "lm") {
    new_parsed_dat <- data.frame(row.names = 1)
    for (obs in 1:length(train_set_avg)) {
      new_parsed_dat <- rbind(new_parsed_dat, c(train_set_max[obs], round(train_set_avg[obs], precision)))
    }
    colnames(new_parsed_dat) <- c('max', 'avg')
    new_parsed_dat <- new_parsed_dat %>% 
      group_by(avg) %>% 
      summarise(sd=sqrt(var(max))) %>%
      filter(!is.na(sd))
    sd.lm <- lm(sd~avg+I(avg^2), data = new_parsed_dat)
    return(sd.lm)
  } else {
    clustering_result <- list()
    avg_silhouette <- c()
    avg_silhouette[1] <- -Inf
    for (cluster_num in 2:10) {
      clustering_result[[cluster_num]] <- kmeans(train_set_avg, centers = cluster_num, iter.max = 20, nstart = 25)
      avg_silhouette[cluster_num] <- mean(silhouette(clustering_result[[cluster_num]]$cluster, dist = dist(train_set_avg))[,3])
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
  closest_cluster <- kmeans_model_df$cluster[which(abs(expected_avg - kmeans_model_df$cluster_mean) == min(abs(expected_avg - kmeans_model_df$cluster_mean)))]
  cluster_var <- kmeans_model_df$vars[kmeans_model_df$cluster == closest_cluster]
  return(cluster_var)
}

generate_expected_conditional_var <- function(expected_avgs, mode, variance_model) {
  expected_var <- c()
  if (mode == "lm") {
    expected_var <- as.numeric(predict(variance_model, newdata=data.frame("avg"=expected_avgs), type = "response")^2)
  } else {
    expected_var <- sapply(expected_avgs, kmeans_find_var, kmeans_model_df = variance_model)
  }
  return(expected_var)
}


find_expected_max <- function(probability, variance, cpu_required) {
  return(max((100 - cpu_required) - qnorm(p=probability) * sqrt(variance), 0))
}


compute_pi_up <- function(expected_max, expected_var, prob_cut_off) {
  return(min(expected_max + qnorm(p=(1-prob_cut_off)) * sqrt(expected_var), 100))
}

logistic_model <- function(train_set_avg, train_set_max, predicted_avgs, update_freq=1, cpu_required) {
  
  fitted_logistic_models <- list()
  fitted_conditional_var_models <- list()
  conditional_var_model_choice <- c()
  parsed_inputs <- parser_for_logistic_model(train_set_avg, train_set_max, cpu_required)
  
  ## Training Step
  for (ts_num in 1:ncol(train_set_avg)) {
    df <- parsed_inputs[[ts_num]]
    log.lm <- glm(survived~avg, data = df, family = "binomial", control=glm.control(maxit=50))
    fitted_logistic_models[[ts_num]] <- log.lm
    conditional_var_model <- build_conditional_variation(train_set_avg[, ts_num], train_set_max[, ts_num], precision = 1, method = "lm")
    ss <- summary(conditional_var_model)
    lm_good_enough <- pf(ss$fstatistic[1], ss$fstatistic[2], ss$fstatistic[3], lower.tail=FALSE) <= 0.05
    if (!lm_good_enough | is.na(lm_good_enough)) {
      conditional_var_model_choice[ts_num] <- "kmeans"
      conditional_var_model <- build_conditional_variation(train_set_avg[, ts_num], train_set_max[, ts_num], precision = 1, method = "kmeans")
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
    prob <- predict(fitted_logistic_models[[ts_num]], newdata = data.frame("avg"=expected_avgs), type = "response")
    prob_ts_cross_threshold[[ts_num]] <- prob
  }
  result <- list("prob"=prob_ts_cross_threshold, "expected_var"=expected_ts_variance)
  return(result)
}

multi_state_logistic_model <- function(train_set_avg, train_set_max, predicted_avgs, update_freq=1, cpu_required) {
  
  
  return(result)
}

ar_logistic_model <- function(dataset_avg, dataset_max, initial_train_size, prob_cut_off, update_freq, job_length, cpu_required) {
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
  logistic_output <- logistic_model(train_set_avg, train_set_max, test_avgs, update_freq=1, cpu_required)
  
  ## N by M dataframe
  probability <- data.frame(matrix(nrow = nrow(test_set_max), ncol = ncol(test_set_max)))
  ## N by M dataframe
  avg_usage <- data.frame(matrix(nrow = nrow(test_set_max), ncol = ncol(test_set_max)))
  job_survival <- data.frame(matrix(nrow = nrow(test_set_max), ncol = ncol(test_set_max)))
  ## N by M dataframe
  predict_result <- data.frame(matrix(nrow = nrow(test_set_max), ncol = ncol(test_set_max)))
  ## N by M dataframe
  actual_result <- data.frame(matrix(nrow = nrow(test_set_max), ncol = ncol(test_set_max)))
  ## 4 by M dataframe
  scheduling_summary <- data.frame(matrix(nrow = 4, ncol = ncol(dataset_max)))
  
  scheduled_num <- rep(0, ncol(dataset_max))
  unscheduled_num <- rep(0, ncol(dataset_max))
  falsely_scheduled_num <- rep(0, ncol(dataset_max))
  falsely_unscheduled_num <- rep(0, ncol(dataset_max))
  
  prob_info <- logistic_output$prob
  var_info <- logistic_output$expected_var
  for(ts_num in 1:ncol(dataset_max)) {
    
    ## Estimate expected of max
    expected_max <- mapply(find_expected_max, prob_info[[ts_num]], var_info[[ts_num]], MoreArgs = list(cpu_required = cpu_required[ts_num]))

    ## Store Probability
    probability[,ts_num] <- prob_info[[ts_num]]
    
    ## Scoring
    pi_up <- mapply(compute_pi_up, expected_max, var_info[[ts_num]], MoreArgs = list(prob_cut_off=prob_cut_off))
    
    ##Evaluation
    evaluation <- find_evaluation(pi_up, test_set_max[, ts_num])
    avg_usage[,ts_num] <- evaluation$usage
    job_survival[,ts_num] <- evaluation$survival
    
    ## Scheduling
    predicted <- ifelse(prob_info[[ts_num]] <= prob_cut_off, 1, 0)
    predict_result[,ts_num] <- predicted
    
    ## Actual
    actual <- ifelse(test_set_max[, ts_num] <= (100 - cpu_required[ts_num]), 1, 0)
    actual_result[,ts_num] <- actual
    
    ## Summary
    scheduled_num[ts_num] <- sum(predicted)
    unscheduled_num[ts_num] <- length(predicted) - sum(predicted)
    falsely_scheduled_num[ts_num] <- sum(ifelse(predicted != actual & predicted == 1, 1, 0))
    falsely_unscheduled_num[ts_num] <- sum(ifelse(predicted != actual & predicted == 0, 1, 0))
  }
  
  colnames(probability) <- colnames(dataset_max)
  rownames(probability) <- seq(initial_train_size + 1, initial_train_size + 1 + update_freq * (nrow(probability) - 1), update_freq)
  
  colnames(avg_usage) <- colnames(dataset_max)
  rownames(avg_usage) <- seq(initial_train_size + 1, initial_train_size + 1 + update_freq * (nrow(avg_usage) - 1), update_freq)
  
  colnames(job_survival) <- colnames(dataset_max)
  rownames(job_survival) <- seq(initial_train_size + 1, initial_train_size + 1 + update_freq * (nrow(job_survival) - 1), update_freq)
  
  colnames(predict_result) <- colnames(dataset_max)
  rownames(predict_result) <- seq(initial_train_size + 1, initial_train_size + 1 + update_freq * (nrow(predict_result) - 1), update_freq)
  
  colnames(actual_result) <- colnames(dataset_max)
  rownames(actual_result) <- seq(initial_train_size + 1, initial_train_size + 1 + update_freq * (nrow(actual_result) - 1), update_freq)
  
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

cpu_required <- rep(0, ncol(data_matrix_max))
for (j in 1:ncol((data_matrix_max))) {
  cpu_required[j] <- as.numeric(quantile((data_matrix_max)[,j], c(0.15, 0.5, 0.85), type = 4)[3])
}

for (job_length in c(12, 36)) {
  output <- ar_logistic_model(dataset_max = data_matrix_max, dataset_avg = data_matrix_avg, initial_train_size = 2000, update_freq = 1, prob_cut_off = 0.01, job_length = job_length, cpu_required = (100 - cpu_required))
  write.csv(output$avg_usage, file = paste("AR_logistic", job_length, "100", 0.1, "avg_usage.csv"))
  print(paste("Avg cycle used:", "job length", job_length, mean(as.matrix(output$avg_usage), na.rm = TRUE)))
  write.csv(output$job_survival, file = paste("AR_logistic", job_length, "100", 0.1, "job_survival.csv"))
  print(paste("Job survival rate:", "job length", job_length, sum(as.matrix(output$job_survival)) / (length(as.matrix(output$job_survival)))))
  write.csv(output$scheduling_summary, file = paste("AR_logistic",job_length, "100", 0.1,"scheduling_sum.csv"))
  scheduled_num <- sum(output$scheduling_summary[1,])
  unscheduled_num <- sum(output$scheduling_summary[2,])
  correct_scheduled_num <- scheduled_num - sum(output$scheduling_summary[3,])
  correct_unscheduled_num <- unscheduled_num - sum(output$scheduling_summary[4,])
  print(paste("Scheduling summary:", "Correct scheduled rate:", correct_scheduled_num / scheduled_num, "Correct unscheduled rate:", correct_unscheduled_num / unscheduled_num))
  
}
write.csv(bg_job_pool, "filenames.csv")