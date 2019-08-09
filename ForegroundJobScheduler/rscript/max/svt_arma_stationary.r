library("ggplot2")
library("ggfortify")
library("dplyr")
library("forecast")
library("mvtnorm")
library("dict")


calculate_var_cov_matrix <-function(var, predict_size, ar_coef, ma_coef) {
  forecast_var <- dict()
  for (i in 1:predict_size) {
    initial <- c(rep(0, predict_size-i), c(1, ma_coef), rep(0, i-1))
    if (length(ar_coef) != 0) {
      for (k in 1:length(ar_coef)) {
        if (i > k) {
          initial <- initial + ar_coef[k] * forecast_var[[i-k]]  
        }
      }
    }
    forecast_var[[i]] <- initial
  }
  var_cov <- matrix(nrow = predict_size, ncol = predict_size)
  for (row in 1:predict_size) {
    for (col in 1:predict_size) {
      if (row > col){
        var_cov[row, col] <- var_cov[col, row]
      } else {
        vec1 <- forecast_var[[row]]
        vec2 <- forecast_var[[col]]
        var_cov[row, col] <- sum(vec1 * vec2) * var
      }
    }
  }
  return(var_cov)
}

calculate_estimates <- function(ar_coef, last_obs, predict_size, intercept) {
  p <- length(ar_coef)
  if (p == 0) {
    return(rep(intercept, predict_size))
  } else {
    last_obs <- as.numeric(last_obs)
    last_obs <- last_obs - rep(intercept, p)
    for (i in 1:predict_size) {
      last_ob <- sum(last_obs[1:p] * ar_coef) 
      last_obs <- c(last_ob, last_obs)
    }
    last_obs <- last_obs + rep(intercept, length(last_obs))
    return(last_obs[1:predict_size])
  }
}

do_prediction <- function(last_obs, ts_model, predict_size=1, level) {
  p <- as.numeric(arimaorder(ts_model)[1])
  q <- as.numeric(arimaorder(ts_model)[3])
  ar_coef <- NULL
  ma_coef <- NULL
  if (p != 0) {
    ar_coef <- as.numeric(ts_model$coef[1:p])
  }
  if (q != 0 & p != 0) {
    ma_coef <- as.numeric(ts_model$coef[(p+1):(p+q)])
  } else if (q != 0 & p == 0) {
    ma_coef <- as.numeric(ts_model$coef[1:q])
  }
  intercept <- as.numeric(ts_model$coef['intercept'])
  sample_var <- as.numeric(ts_model$sigma2)
  mu <- calculate_estimates(ar_coef, last_obs, predict_size, intercept)
  varcov <- calculate_var_cov_matrix(sample_var, predict_size, ar_coef, ma_coef)
  prob <- 1 - pmvnorm(lower = rep(0, predict_size), upper = rep(level, predict_size), mean = mu, sigma = varcov)
  result <- list('prob' = as.numeric(prob), 'mu' = mu, 'varcov'=varcov)
  return(result)
}

compute_pi_up <- function(mu, varcov, predict_size, prob_cutoff) {
  upper_bounds <- rep(NA, predict_size)
  for (i in 1:predict_size) {
    upper_bounds[i] <- min(mu[i] + qnorm((1-prob_cutoff), 0, 1) * sqrt(varcov[i,i]), 100)
  }
  return(upper_bounds)
}

find_evaluation <- function(pi_up, actual_obs) {
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

svt_stationary_model <- function(dataset, initial_train_size, job_length=5, cpu_required, prob_cut_off=0.01, update_freq=1, ts_models_import = NULL) {
  #### input dataset: N by M matrix, N being number of observations, M being number of time series
  #### input initial_train_size: The number of first observations used to train the model
  #### input job_length: The time that the foreground job will be runing
  #### input cpu_required: A vector, the cpu that the foreground job requires in percentage
  #### input prob_cut_off: If the probability of background job exceeding 100-cpu_required is smaller than prob_cut_off, then schedule it. Otherwise, don't.
  #### input update_freq: The number of observations for each update of the model, and do the prediction
  
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
  
  ## Dictionaries
  end_time_testing_queue <- numvecdict()
  ts_models <- dict()
  pi_up_bounds <- dict()
  
  ## Summary Counts
  scheduled_num <- rep(0, ncol(dataset))
  unscheduled_num <- rep(0, ncol(dataset))
  falsely_scheduled_num <- rep(0, ncol(dataset))
  falsely_unscheduled_num <- rep(0, ncol(dataset))
  
  ## Train Model
  max_p <- 0
  max_q <- 0
  train_percent <- 0.00
  for (ts_num in 1:ncol(dataset)) {
    if (is.null(ts_models_import)) {
      if (round(ts_num / ncol(dataset), 2) != train_percent) {
        print(paste("Training", train_percent))
        train_percent <- round(ts_num / ncol(dataset), 2)
      }
      ts_model <- auto.arima(dataset[1:initial_train_size, ts_num], stationary = TRUE, seasonal = FALSE, allowdrift = FALSE, allowmean = TRUE)
      if (as.numeric(arimaorder(ts_model)[1]) > max_p) {
        max_p <- as.numeric(arimaorder(ts_model)[1])
      }
      if (as.numeric(arimaorder(ts_model)[3]) > max_q) {
        max_q <- as.numeric(arimaorder(ts_model)[3])
      }
      ts_models[[ts_num]] <- ts_model
    } else {
      ts_models[[ts_num]] <- ts_models_import[[ts_num]]
    }
  }
  if (is.null(ts_models_import)) {
    print(paste("Max order of p", max_p, "Max order of q", max_q))
  }
  
  current_end <- initial_train_size
  current_percent <- 0.00
  while (current_end <= nrow(dataset)) {
    
    ## Initialize Model 
    prob_vector <- c()
    prediction <- c()
    actual <- c()
    avg_cycle_used <- c()
    survival <- c()
    
    for (ts_num in 1:ncol(dataset)) {
      
      ## Schedule the job
      if (current_end <= nrow(dataset) - job_length) {
        ts_model <- ts_models[[ts_num]]
        p <- as.numeric(arimaorder(ts_model)[1])
        last_obs <- dataset[(current_end-p+1):current_end, ts_num]
        prediction_result <- do_prediction(last_obs = last_obs, ts_model = ts_model, predict_size = job_length, level = (100 - cpu_required[ts_num]))
        prob_vector[ts_num] <- prediction_result$prob
        if (prob_vector[ts_num] < prob_cut_off) {
          prediction[ts_num] <- 1
          scheduled_num[ts_num] <- scheduled_num[ts_num] + 1
        } else {
          prediction[ts_num] <- 0
          unscheduled_num[ts_num] <- unscheduled_num[ts_num] + 1
        }
        pi_up_bounds[[paste(ts_num, ",", (current_end + 1), sep = "")]] <- compute_pi_up(mu=prediction_result$mu, varcov=prediction_result$varcov, predict_size=job_length, prob_cutoff=prob_cut_off)
      }
      
      ## Check correctness of previous schedulings
      if (length(end_time_testing_queue[[current_end]]) != 0) {
        info_lst <- end_time_testing_queue[[current_end]]
        lst_len <- length(info_lst)
        for (i in seq(1, lst_len, 3)) {
          start_time <- info_lst[i]
          end_time <- info_lst[i+1]
          row_num <- info_lst[i+2]
          
          position_vec <- dataset[start_time:end_time, ts_num]
          if (all(position_vec < (100 - cpu_required[ts_num]))) {
            actual[ts_num] <- 1
            if (predict_result[row_num, ts_num] == 0) {
              falsely_unscheduled_num[ts_num] <- falsely_unscheduled_num[ts_num] + 1
            }
          } else {
            actual[ts_num] <- 0
            if (predict_result[row_num, ts_num] == 1) {
              falsely_scheduled_num[ts_num] <- falsely_scheduled_num[ts_num] + 1
            }
          }
          pi_up <- pi_up_bounds[[paste(ts_num, ",", start_time, sep = "")]]
          evalulation <- find_evaluation(pi_up=pi_up, actual_obs=position_vec)
          avg_cycle_used[ts_num] <- evalulation$avg_usage
          survival[ts_num] <- evalulation$survival
        }
      }
    }
    
    ## Store probability
    probability <- rbind(probability, prob_vector)
    ## Store Evaluations
    avg_usage <- rbind(avg_usage, avg_cycle_used)
    job_survival <- rbind(job_survival, survival)
    ## Store Prediction
    predict_result <- rbind(predict_result, prediction)
    ## Store Actual
    actual_result <- rbind(actual_result, actual)
    
    ## Queue the jobs to check their correctness later
    closest_update <- current_end + ceiling(job_length / update_freq) * update_freq
    end_time_testing_queue$append_number(closest_update, current_end + 1)
    end_time_testing_queue$append_number(closest_update, current_end + job_length)
    end_time_testing_queue$append_number(closest_update, nrow(predict_result))
    
    ## Update current_end
    current_end <- current_end + update_freq
    if (current_percent != round((current_end - initial_train_size) / (nrow(dataset) - initial_train_size), digits = 2)) {
      print(paste("Testing", current_percent))
      current_percent <- round((current_end - initial_train_size) / (nrow(dataset) - initial_train_size), digits = 2)
    }
  }
  
  ## Change column and row names, N by M
  colnames(probability) <- colnames(dataset)
  rownames(probability) <- seq(initial_train_size + 1, initial_train_size + 1 + update_freq * (nrow(probability) - 1), update_freq)
  
  colnames(avg_usage) <- colnames(dataset)
  rownames(avg_usage) <- seq(initial_train_size + 1, initial_train_size + 1 + update_freq * (nrow(avg_usage) - 1), update_freq)
  
  colnames(job_survival) <- colnames(dataset)
  rownames(job_survival) <- seq(initial_train_size + 1, initial_train_size + 1 + update_freq * (nrow(job_survival) - 1), update_freq)
  
  colnames(predict_result) <- colnames(dataset)
  rownames(predict_result) <- seq(initial_train_size + 1, initial_train_size + 1 + update_freq * (nrow(predict_result) - 1), update_freq)
  
  colnames(actual_result) <- colnames(dataset)
  rownames(actual_result) <- seq(initial_train_size + 1, initial_train_size + 1 + update_freq * (nrow(actual_result) - 1), update_freq)
  
  colnames(scheduling_summary) <- colnames(dataset)
  scheduling_summary[1,] <- scheduled_num
  scheduling_summary[2,] <- unscheduled_num
  scheduling_summary[3,] <- falsely_scheduled_num
  scheduling_summary[4,] <- falsely_unscheduled_num
  rownames(scheduling_summary) <- c('Scheduled_Num', 'Unscheduled_Num', 'Falsly_scheduled_Num', 'Falsely_unscheduled_Num')
  
  result <- list('prob' = probability, 'predict' = predict_result, 'avg_usage'=avg_usage, 'job_survival'=job_survival, 'actual' = actual_result, 'scheduling_summary' = scheduling_summary, 'ts_models' = ts_models)
  
  return(result)
}

## Read back ground job pool

bg_job_pool <- read.csv("C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//pythonscripts//list of sampled 100 bg jobs.csv")[,2]
bg_jobs_path = "C://Users//carlo//Documents//sample background jobs//"

data_matrix <- matrix(nrow = 4000, ncol = 0)
for (job_num in bg_job_pool) {
  bg_job <- read.csv(paste(bg_jobs_path, job_num, ".csv", sep = ""))
  data_matrix <- cbind(data_matrix, bg_job$max_cpu[4033:8032])
}
rownames(data_matrix) <- seq(0, 5 * (nrow(data_matrix) - 1),5)
colnames(data_matrix) <- bg_job_pool

cpu_required <- rep(0, ncol(data_matrix))
for (j in 1:ncol(data_matrix)) {
  cpu_required[j] <- as.numeric(quantile(data_matrix[,j], c(0.15, 0.5, 0.85), type = 4)[3])
}

for (job_length in c(1, 12)) {
  print(paste("Job_length", job_length))
  
  output <- svt_stationary_model(dataset=data_matrix, job_length=job_length, cpu_required=(100-cpu_required), prob_cut_off=0.1, initial_train_size = 2000, update_freq=1)
  write.csv(output$avg_usage, file = paste("ARMA", job_length, "100", 0.1, "avg_usage.csv"))
  print(paste("Avg cycle used:", "job length", job_length, mean(as.matrix(output$avg_usage), na.rm = TRUE)))
  write.csv(output$job_survival, file = paste("ARMA", job_length, "100", 0.1, "job_survival.csv"))
  print(paste("Job survival rate:", "job length", job_length, sum(as.matrix(output$job_survival)) / (length(as.matrix(output$job_survival)))))
  write.csv(output$scheduling_summary, file = paste("ARMA",job_length, "100", 0.1,"scheduling_sum.csv"))
  scheduled_num <- sum(output$scheduling_summary[1,])
  unscheduled_num <- sum(output$scheduling_summary[2,])
  correct_scheduled_num <- scheduled_num - sum(output$scheduling_summary[3,])
  correct_unscheduled_num <- unscheduled_num - sum(output$scheduling_summary[4,])
  print(paste("Scheduling summary:", "Correct scheduled rate:", correct_scheduled_num / scheduled_num, "Correct unscheduled rate:", correct_unscheduled_num / unscheduled_num))
}