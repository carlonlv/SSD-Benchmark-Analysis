library("forecast")
library("mvtnorm")
library("dplyr")
library("arules")
library("dict")
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
    if (is.numeric(variance_model)) {
      expected_var <- rep(variance_model, length(expected_avgs))
    } else {
      expected_var <- predict(variance_model, newdata=data.frame("avg"=expected_avgs), type = "response")^2
    }
  } else {
    expected_var <- sapply(expected_avgs, kmeans_find_var, kmeans_model_df = variance_model)
  }
  return(expected_var)
}


find_expected_max <- function(probability, variance, cpu_required) {
  return(max((100 - cpu_required) - qnorm(p=(1-probability)) * sqrt(variance), 0))
}


compute_pi_up <- function(expected_max, expected_var, prob_cut_off, granularity) {
  pi_up <- min(expected_max + qnorm(p=(1-prob_cut_off)) * sqrt(expected_var), 100)
  if (granularity > 0) {
    scheduled_size <- sapply(100 - pi_up, round_to_nearest, granularity, TRUE)
    pi_up <- 100 - scheduled_size
  }
  return(pi_up)
}


logistic_model <- function(train_set_avg, train_set_max, predicted_avgs, update_freq=1, cpu_required, cond.var) {
  
  ## Training Step
  fitted_logistic_models <- list()
  fitted_conditional_var_models <- list()
  parsed_inputs <- parser_for_logistic_model(train_set_avg, train_set_max, cpu_required)
  
  for (ts_num in 1:ncol(train_set_avg)) {
    df <- parsed_inputs[[ts_num]]
    log.lm <- glm(survived~avg, data = df, family = "binomial", control=glm.control(maxit=50))
    fitted_logistic_models[[ts_num]] <- log.lm
    conditional_var_model <- NULL
    if (cond.var == "lm") {
      conditional_var_model <- build_conditional_variation(train_set_avg[, ts_num], train_set_max[, ts_num], precision = 1, method = "lm")
    } else {
      conditional_var_model <- build_conditional_variation(train_set_avg[, ts_num], train_set_max[, ts_num], precision = 1, method = "kmeans")
    }
    fitted_conditional_var_models[[ts_num]] <- conditional_var_model
  }
  
  ## Testing Step
  expected_ts_variance <- list()
  prob_ts_cross_threshold <- list()
  
  for (ts_num in 1:ncol(train_set_avg)) {
    expected_avgs <- predicted_avgs[seq(1, nrow(predicted_avgs), update_freq), ts_num]
    conditional_var_model <- fitted_conditional_var_models[[ts_num]]
    expected_vars <- generate_expected_conditional_var(expected_avgs, mode = cond.var, variance_model = conditional_var_model)
    expected_ts_variance[[ts_num]] <- expected_vars
    prob <- predict(fitted_logistic_models[[ts_num]], newdata = data.frame("avg"=expected_avgs), type = "response")
    prob_ts_cross_threshold[[ts_num]] <- 1 - prob
  }
  result <- list("prob"=prob_ts_cross_threshold, "expected_var"=expected_ts_variance)
  return(result)
}


scheduling_foreground <- function(ts_num, test_dataset_max, test_dataset_avg, coeffs, means, vars, logistic_models, window_size, job_length, prob_cut_off, cpu_required, granularity, schedule_policy) {
  scheduled_num <- 0
  unscheduled_num <- 0
  correct_scheduled_num <- 0
  correct_unscheduled_num <- 0
  
  seek_length <- window_size * job_length
  last_time_schedule <- nrow(test_dataset) - window_size * job_length + 1

  update_policy = ifelse(schedule_policy == "disjoint", window_size, 1)
  current_end <- window_size + 1
  while (current_end <= last_time_schedule) {
  
    ## Predict current avgs using AR1
    last_obs <- convert_frequency_dataset(test_dataset_avg[(current_end-window_size):(current_end-1), ts_num], window_size, mode = 'avg')
    
    expected_avgs <- do_prediction(last_obs, phi, mean, variance)$mu
    
    logistic_model <- logistic_models[[ts_num]]
    
    prediction_prob <- 1 - predict(logistic_model, newdata = data.frame("avg"=expected_avgs), type = "response")
    
    prediction <- ifelse(prediction_prob <= prob_cut_off, 1, 0)
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


scheduling_model <- function(ts_num, test_dataset_max, test_dataset_avg, coeffs, means, vars, logistic_models, cond_var_models, cond.var, window_size, prob_cut_off, granularity, schedule_policy) {
  utilization <- c()
  survival <- c()
  runs <- rep(0, 5)
  run_counter <- 0
  run_switch <- FALSE
  
  seek_length <- window_size
  last_time_schedule <- nrow(test_dataset_max) - window_size + 1
  
  logistic_model <- logistic_models[[ts_num]]
  cond_var_model <- cond_var_models[[ts_num]]
  
  current_end <- window_size + 1
  update_policy <- ifelse(schedule_policy == "disjoint", window_size, 1)
  while (current_end <= last_time_schedule) {
    ## Schedule based on model predictions
    last_obs <- convert_frequency_dataset(test_dataset_avg[(current_end-window_size):(current_end-1), ts_num], window_size, mode = 'avg')
    
    expected_avgs <- do_prediction(last_obs, phi, mean, variance)$mu
    
    expected_vars <- generate_expected_conditional_var(expected_avgs, mode = cond.var, variance_model = cond_var_model)
    
    prob <- 1 - predict(logistic_model, newdata = data.frame("avg"=expected_avgs), type = "response")
    
    expected_max <- find_expected_max(prob, expected_vars, cpu_required[ts_num])
    
    pi_up <- compute_pi_up(mu=expected_max, varcov=expeted_vars, predict_size=1, prob_cutoff=prob_cut_off, granularity=granularity)
    
    ## Evalute schedulings based on prediction
    start_time <- current_end
    end_time <- current_end + seek_length - 1
    position_vec <- convert_frequency_dataset(test_dataset_max[start_time:end_time, ts_num], window_size, "max")
    evalulation <- find_evaluation(pi_up=pi_up, actual_obs=position_vec, granularity=granularity)
    utilization <- c(utilization, evalulation$usage)
    survival <- c(survival, evalulation$survival)
    
    if (schedule_policy == "dynamic") {
      if (!is.na(evalulation$survival) & evalulation$survival == 1) {
        update_policy <- window_size
        if (run_switch) {
          idx <- ifelse(run_counter > 5, 5, run_counter)
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
  return(list("utilization"=overall_rate$utilization_rate, "survival"=overall_rate$survival_rate, "one"=runs[1], "two"=runs[2], "thr"=runs[3], "fou"=runs[4], "fiv"=runs[5]))
}


train_ar1_model <- function(ts_num, train_dataset) {
  ts_model <- tryCatch({
    arima(x=train_dataset[, ts_num], order = c(1,0,0), include.mean = TRUE, method = "CSS-ML", optim.control = list(maxit=2000))
  }, error = function(cond) {
    return(arima(x=train_dataset[, ts_num], order = c(1,0,0), include.mean = TRUE, method = "ML", optim.control = list(maxit=2000)))
  })
  return(list("coeffs"=as.numeric(ts_model$coef[1]), "means"= as.numeric(ts_model$coef[2]), "vars"=ts_model$sigma2))
}


parser_for_logistic_model <- function(train_set_max, train_set_avg, cpu_required) {
  df <- data.frame("avg"=train_set_avg, "max"=train_set_max)
  df$survived <- ifelse(df$max <= (100 - cpu_required), 1, 0)
  return(df)
}


train_logistic_model <- function(ts_num, train_dataset_max, train_dataset_avg, cpu_required) {
  logistic_input <- parser_for_logistic_model(train_dataset_max[,ts_num], train_dataset_avg[,ts_num], cpu_required[ts_num])
  log.lm <- glm(survived~avg, data = logistic_input, family = "binomial", control=glm.control(maxit=50))
  return(log.lm) 
}


find_bin_obs <- function(avg, bin_num) {
  bin_size = 100 / bin_num
  return(ifelse(avg % bin_size == 0, (avg / bin_size)-1, floor(avg / bin_size)))
}


train_cond_var_model <- function(train_set_avg, train_set_max, bin_num=100, method) {
  if (method == "lm") {
    new_parsed_dat <- data.frame(matrix(nrow=length(train_set_avg), ncol=3))
    bin <- sapply(train_set_avg, find_bin_obs, bin_num)
    for (i in 1:nrow(train_set_avg)) {
      new_parsed_dat[i,] = c(train_set_avg[i,], train_set_max[i,], bin[i])
    }
    colnames(new_parsed_dat) <- c('max', 'avg', 'bin')
    new_parsed_dat <- new_parsed_dat %>% 
      group_by(bin) %>% 
      summarise(sd=sqrt(var(max))) %>%
      filter(!is.na(sd))
     
    sd.lm <- NULL
    if (nrow(new_parsed_dat) >= 3) {
      sd.lm <- lm(sd~avg+I(avg^2), data = new_parsed_dat)
    } else if (nrow(new_parsed_dat) == 2) {
      sd.lm <- lm(sd~avg, data = new_parsed_dat)
    } else {
      sd.lm <- new_parsed_dat$sd^2
    }
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


ar_logistic_model <- function(dataset_avg, dataset_max, initial_train_size, prob_cut_off, update_freq, job_length, cpu_required, cond.var, granularity) {
  #### input dataset_avg, dataset_max: N by M matrix, N being number of observations, M being number of time series
  #### input initial_train_size: The number of first observations used to train the model
  #### input window_size: The number of observations used to train and predict as one sample
  #### input update_freq: The number of observations for each update of the model, and do the prediction
  #### input prob_cut_off: If the probability of background job exceeding 100-cpu_required is smaller than prob_cut_off, then schedule it. Otherwise, don't.
  #### input job_length: The length of job to be scheduled
  #### input update_freq: The number of windows to do the prediction
  
  window_size <- job_length
  if (granularity > 0) {
    cpu_required <- sapply(cpu_required, round_to_nearest, granularity, FALSE)
  }
  
  scheduled_num <- data.frame(matrix(nrow=ncol(dataset_max), ncol=0))
  unscheduled_num <- data.frame(matrix(nrow=ncol(dataset_max), ncol=0))
  correct_scheduled_num <- data.frame(matrix(nrow=ncol(dataset_max), ncol=0))
  correct_unscheduled_num <- data.frame(matrix(nrow=ncol(dataset_max), ncol=0))
  
  avg_usage <- data.frame(matrix(nrow=ncol(dataset_max), ncol=0))
  job_survival <- data.frame(matrix(nrow=ncol(dataset_max), ncol=0))
  overall_runs <- data.frame(matrix(nrow=ncol(dataset_max), ncol = 0))
  
  ## Dictionaries
  logistic_models <- dict()
  var_lm_models <- dict()
  
  ## Split the dataset into training and testing sets
  train_dataset <- dataset[1:initial_train_size, 1:ncol(dataset)]
  test_dataset <- dataset[(initial_train_size+1):nrow(dataset), 1:ncol(dataset)]
  
  ## Convert Frequency for trainning set
  new_trainset_max <- apply(train_dataset_max, 2, convert_frequency_dataset, new_freq=window_size, mode="max")
  rownames(new_trainset_max) <- seq(1, 1 + window_size * (nrow(new_trainset_max) - 1), window_size)
  colnames(new_trainset_max) <- colnames(train_dataset_max)
  
  new_trainset_avg <- apply(train_dataset_avg, 2, convert_frequency_dataset, new_freq=window_size, mode="avg")
  rownames(new_trainset_avg) <- seq(1, 1 + window_size * (nrow(new_trainset_avg) - 1), window_size)
  colnames(new_trainset_avg) <- colnames(train_dataset_avg)
  
  ## Training AR1 Model
  print("Training: AR1.")
  train_result <- sapply(1:ncol(new_trainset_avg), train_ar1_model, new_trainset_avg)
  coeffs <- unlist(train_result[1,])
  means <- unlist(train_result[2,])
  vars <- unlist(train_result[3,])
  
  ## Training Logistic Model
  print("Training: Logistic.")
  logistic_result <- sapply(1:ncol(new_trainset_avg), train_logistic_model, new_trainset_max, new_trainset_avg, cpu_required, simplify=FALSE)
  for (ts_num in 1:ncol(new_trainset_avg)) {
    logistic_models[[ts_num]] <- logistic_result[,ts_num]
  }
  
  ## Training Polynomial Regression Model
  print("Training: Polynomial Regression.")
  var_lm_result <- sapply(1:ncol(new_trainset_avg), train_cond_var_model, bin_num, cond.var, simplify=FALSE)
  for (ts_num in 1:ncol(new_trainset_avg)) {
    var_lm_models[[ts_num]] <- var_lm_result[,ts_num]
  }
  
  ## Test Model
  
  
  ## N by M dataframe
  probability <- data.frame(matrix(nrow = (nrow(test_set_max) - 1), ncol = ncol(test_set_max)))
  pi_upper_bounds <- data.frame(matrix(nrow = (nrow(test_set_max) - 1), ncol = ncol(test_set_max)))
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
  
  prob_info <- logistic_output$prob
  var_info <- logistic_output$expected_var
  for(ts_num in 1:ncol(test_set_max)) {
    
    ## Estimate expected of max
    expected_max <- mapply(find_expected_max, prob_info[[ts_num]], var_info[[ts_num]], MoreArgs = list(cpu_required = cpu_required[ts_num]))

    ## Store Probability
    probability[,ts_num] <- prob_info[[ts_num]]
    
    ## Scoring
    pi_up <- mapply(compute_pi_up, expected_max, var_info[[ts_num]], MoreArgs = list(prob_cut_off=prob_cut_off, granularity=granularity))
    pi_upper_bounds[,ts_num] <- pi_up
    
    ##Evaluation
    evaluation <- find_evaluation(pi_up, test_set_max[, ts_num][-1], granularity=granularity)
    avg_usage[,ts_num] <- evaluation$usage
    job_survival[,ts_num] <- evaluation$survival
    
    ## Scheduling
    predicted <- ifelse(prob_info[[ts_num]] <= prob_cut_off, 1, 0)
    predict_result[,ts_num] <- predicted
    
    ## Actual
    actual <- ifelse(test_set_max[, ts_num][-1] <= (100 - cpu_required[ts_num]), 1, 0)
    actual_result[,ts_num] <- actual
    
    ## Summary
    scheduled_num[ts_num] <- sum(predicted)
    unscheduled_num[ts_num] <- length(predicted) - sum(predicted)
    falsely_scheduled_num[ts_num] <- sum(ifelse(predicted != actual & predicted == 1, 1, 0))
    falsely_unscheduled_num[ts_num] <- sum(ifelse(predicted != actual & predicted == 0, 1, 0))
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


wrapper.epoche <- function(parameter, dataset_avg, dataset_max, cpu_required, initial_train_size, update_freq, cond.var, bad.seq.adj, output_dp) {
  
  job_length <- as.numeric(parameter[1])
  prob_cut_off <- as.numeric(parameter[2])
  granularity <- as.numeric(parameter[3])
  
  output <- ar_logistic_model(dataset_avg=dataset_avg, dataset_max=dataset_max, job_length=job_length, cpu_required=cpu_required, prob_cut_off=prob_cut_off, initial_train_size=initial_train_size, update_freq=1, cond.var=cond.var, granularity=granularity)
  
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
  
  if (cond.var == "lm") {
    
    write.csv(output$pi_up, file = paste("AR1_logistic_lm",job_length, sample_size, prob_cut_off, granularity, "pi_upper.csv"))
    write.csv(output$scheduling_summary, file = paste("AR1_logistic_lm", job_length, sample_size, prob_cut_off, granularity, "scheduling_sum.csv"))
    write.csv(output$avg_usage, file = paste("AR1_logistic_lm",job_length, sample_size, prob_cut_off, granularity, "avg_usage.csv"))
    write.csv(output$job_survival, file = paste("AR1_logistic_lm", job_length, sample_size, prob_cut_off, granularity, "job_survival.csv"))
    
    result_path.xlsx <- read.xlsx(output_dp, sheetIndex = 1)
    result_path.xlsx <- update.xlsx.df(result_path.xlsx, "AR1_logistic_lm", prob_cut_off, NA, sample_size, job_length, granularity, avg_utilization, survival, correct_scheduled_rate, correct_unscheduled_rate)
    write.xlsx(result_path.xlsx, showNA = FALSE, file = output_dp, row.names = FALSE)
  } else {
    
    write.csv(output$pi_up, file = paste("AR1_logistic_kmeans",job_length, sample_size, prob_cut_off, granularity, "pi_upper.csv"))
    write.csv(output$scheduling_summary, file = paste("AR1_logistic_kmeans", job_length, sample_size, prob_cut_off, granularity, "scheduling_sum.csv"))
    write.csv(output$avg_usage, file = paste("AR1_logistic_kmeans",job_length, sample_size, prob_cut_off, granularity, "avg_usage.csv"))
    write.csv(output$job_survival, file = paste("AR1_logistic_kmeans", job_length, sample_size, prob_cut_off, granularity, "job_survival.csv"))
    
    result_path.xlsx <- read.xlsx(output_dp, sheetIndex = 1)
    result_path.xlsx <- update.xlsx.df(result_path.xlsx, "AR1_logistic_kmeans", prob_cut_off, NA, sample_size, job_length, granularity, avg_utilization, survival, correct_scheduled_rate, correct_unscheduled_rate)
    write.xlsx(result_path.xlsx, showNA = FALSE, file = output_dp, row.names = FALSE)
  }
}

## Read back ground job pool

sample_size <- 100
cpu_usage <- 3
total_trace_length <- 8000
initial_train_size <- 6000
bad.seq.adj <- FALSE
cond.var <- "lm"

window_sizes <- c(12, 36)
prob_cut_offs <- c(0.005, 0.01, 0.02, 0.1, 0.125, 0.15, 0.175, 0.2, 0.25)
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
  output_dp <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//Nonoverlapping windows//summary (windows,granularity) post adj.xlsx"
} else {
  #output_dp <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//Nonoverlapping windows//summary (windows) max.xlsx"
  output_dp <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//Nonoverlapping windows//summary (windows,granularity).xlsx"
}

parameter.df <- expand.grid(window_sizes, prob_cut_offs, granularity)
colnames(parameter.df) <- c("job_length", "prob_cut_off", "granularity")
parameter.df <- parameter.df %>% 
  arrange(job_length)
slt <- apply(parameter.df, 1, wrapper.epoche, dataset_avg=data_matrix_avg, dataset_max=data_matrix_max, cpu_required=(100-cpu_required), initial_train_size=initial_train_size, update_freq=1, cond.var=cond.var, bad.seq.adj=bad.seq.adj, output_dp=output_dp)
