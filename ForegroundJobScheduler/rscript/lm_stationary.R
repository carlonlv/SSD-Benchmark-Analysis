library("MASS")
library("EnvStats")
library("DescTools")
library("mvtnorm")
library("dict")

generate_previous_aom <- function(dataset, frequency) {
  previous_obs <- NULL
  for (i in (frequency+1):length(dataset)) {
    previous_obs <- c(previous_obs, mean(dataset[(i-frequency):(i-1)]))
  }
  return(previous_obs)
}

calculate_var_cov_matrix <-function(sample_var, predict_size, beta1, window_size) {
  forecast_var <- dict()
  for (i in 1:predict_size) {
    initial <- c(rep(0, predict_size-i), 1, rep(0, i-1))
    for (k in 1:window_size) {
      if (i > k) {
        initial <- initial + (beta1 / window_size) * forecast_var[[i-k]]  
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
        var_cov[row, col] <- sum(vec1 * vec2) * sample_var
      }
    }
  }
  return(var_cov)
}

calculate_estimates <- function(beta0, beta1, last_obs, lambda, predict_size, window_size) {
  last_obs <- as.numeric(last_obs)
  last_obs <- boxcoxTransform(last_obs, lambda, eps = 0.1)
  for (i in 1:predict_size) {
    last_ob <- mean(last_obs[1:window_size]) * beta1 + beta0
    last_obs <- c(last_ob, last_obs)
  }
  return(last_obs[1:predict_size])
}


do_prediction <- function(last_obs, beta0, beta1, var, lambda, window_size, predict_size, level) {
  mu <- calculate_estimates(beta1, beta0, last_obs, lambda, predict_size, window_size)
  varcov <- calculate_var_cov_matrix(var, predict_size, beta1, window_size)
  prob <- 1 - pmvnorm(lower = rep(boxcoxTransform(1, lambda, 0.1), predict_size), upper = rep(boxcoxTransform(level+1, lambda, 0.1)), mean = mu, sigma = varcov)
  result <- list('prob' = as.numeric(prob), 'mu' = mu, 'varcov'=varcov)
  return(result)
}

compute_pi_up <- function(mu, varcov, predict_size, prob_cutoff, lambda) {
  trans_upper_bounds <- rep(NA, predict_size)
  upper_bounds <- NULL
  for (i in 1:predict_size) {
    trans_upper_bounds[i] <- mu[i] + qnorm((1-prob_cutoff), 0, 1) * sqrt(varcov[i,i])
  }
  if ((-0.1 < lambda) & (lambda < 0.1)) {
    upper_bounds <- BoxCoxInv(trans_upper_bounds, 0)
  } else {
    upper_bounds <- BoxCoxInv(trans_upper_bounds, lambda) - 1
  }
  for (l in 1:predict_size) {
    if (is.na(upper_bounds[l])) {
      upper_bounds[l] <- 100
    } else {
      upper_bounds[l] <- min(upper_bounds[l], 100)
    }
  }
  return(upper_bounds)
}

find_evaluation <- function(pi_up, actual_obs, predict_size) {
  usage <- (100 - pi_up) / (100 - actual_obs + 1)
  if (all((actual_obs - 1) <= pi_up)) {
    survival = 1
  } else {
    survival = 0
  }
  avg_usage <- mean(usage[!is.infinite(usage) & !is.na(usage) & usage <= 1])
  result <- list('avg_usage' = avg_usage, 'survival'= survival)
  return(result)
}

lm_stationary_model <- function(dataset_max, window_size, job_length, cpu_required, prob_cut_off, initial_train_size = 2000, update_freq=1) {
  
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
  scheduling_summary <- data.frame(matrix(nrow = 4, ncol = ncol(dataset_max)))
  
  ## Dictionaries
  end_time_testing_queue <- numvecdict()
  pi_up_bounds <- dict()
  
  ## Summary Counts
  scheduled_num <- rep(0, ncol(dataset_max))
  unscheduled_num <- rep(0, ncol(dataset_max))
  falsely_scheduled_num <- rep(0, ncol(dataset_max))
  falsely_unscheduled_num <- rep(0, ncol(dataset_max))
  
  ## Training
  intercept <- c()
  slope <- c()
  sample_var <- c()
  lambda <- c()
  
  train_percent <- 0.00
  dataset_max <- dataset_max + 1
  for (ts_num in 1:ncol(dataset_max)) {
    if (round(ts_num / ncol(dataset_max), 2) != train_percent) {
      print(paste("Training", train_percent))
      train_percent <- round(ts_num / ncol(dataset_max), 2)
    }
    data <- dataset_max[1:initial_train_size, ts_num]
    bc <- EnvStats::boxcox(data[(window_size + 1):length(data)], c(-3, 3), eps = 0.1, optimize = TRUE)
    best.lambda <- bc$lambda
    response <- boxcoxTransform(data[(window_size + 1):length(data)], best.lambda, eps = 0.1)
    predictor <- generate_previous_aom(data, window_size)
    predictor <- boxcoxTransform(predictor, best.lambda, eps = 0.1)
    dataset <- data.frame(x = predictor, y = response)
    transformed.lm <- lm(y ~ x, dataset)
    intercept[ts_num] <- as.numeric(transformed.lm$coefficients[1])
    slope[ts_num] <- as.numeric(transformed.lm$coefficients[2])
    sample_var[ts_num] <- summary(transformed.lm)$sigma ^ 2
    lambda[ts_num] <- best.lambda
  }
  
  
  ## Testing
  current_end <- initial_train_size
  current_percent <- 0.00
  while (current_end <= nrow(dataset_max)) {
    
    ## Initialize Model 
    prob_vector <- c()
    prediction <- c()
    actual <- c()
    avg_cycle_used <- c()
    survival <- c()
    
    for (ts_num in 1:ncol(dataset_max)) {
      
      ## Schedule the job
      if (current_end <= nrow(dataset_max) - job_length) {
        beta0 <- intercept[ts_num]
        beta1 <- slope[ts_num]
        var <- sample_var[ts_num]
        l <- lambda[ts_num]
        last_obs <- dataset_max[(current_end-window_size+1):current_end, ts_num]
        prediction_result <- do_prediction(last_obs = last_obs, beta0 = beta0, beta1 = beta1, var = var, lambda = l, window_size = window_size, predict_size = job_length, level = (100 - cpu_required[ts_num]))
        prob_vector[ts_num] <- prediction_result$prob
        if (prob_vector[ts_num] < prob_cut_off) {
          prediction[ts_num] <- 1
          scheduled_num[ts_num] <- scheduled_num[ts_num] + 1
        } else {
          prediction[ts_num] <- 0
          unscheduled_num[ts_num] <- unscheduled_num[ts_num] + 1
        }
        pi_up_bounds[[paste(ts_num, ",", (current_end + 1), sep = "")]] <- compute_pi_up(mu=prediction_result$mu, varcov=prediction_result$varcov, predict_size=job_length, prob_cutoff=prob_cut_off, lambda = l)
      }
      
      ## Check correctness of previous schedulings
      if (length(end_time_testing_queue[[current_end]]) != 0) {
        info_lst <- end_time_testing_queue[[current_end]]
        lst_len <- length(info_lst)
        for (i in seq(1, lst_len, 3)) {
          start_time <- info_lst[i]
          end_time <- info_lst[i+1]
          row_num <- info_lst[i+2]
          
          position_vec <- dataset_max[start_time:end_time, ts_num]
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
          evalulation <- find_evaluation(pi_up=pi_up, actual_obs=position_vec, predict_size=job_length)
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
    if (current_percent != round((current_end - initial_train_size) / (nrow(dataset_max) - initial_train_size), digits = 2)) {
      print(paste("Testing", current_percent))
      current_percent <- round((current_end - initial_train_size) / (nrow(dataset_max) - initial_train_size), digits = 2)
    }
  }
  
  ## Change column and row names, N by M
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
  rownames(scheduling_summary) <- c('Scheduled_Num', 'Unscheduled_Num', 'Falsly_scheduled_Num', 'Falsely_unscheduled_Num')
  
  result <- list('prob' = probability, 'predict' = predict_result, 'avg_usage'=avg_usage, 'job_survival'=job_survival, 'actual' = actual_result, 'scheduling_summary' = scheduling_summary)
  
  return(result)
}


## Read back ground job pool

bg_job_pool <- read.csv("C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//pythonscripts//list of sampled background jobs.csv")[,1]
bg_job_pool <- sub(".pd", "", bg_job_pool)
bg_jobs_path = "C://Users//carlo//Documents//sample background jobs//"

data_matrix_avg <- matrix(nrow = 4000, ncol = 0)
data_matrix_max <- matrix(nrow = 4000, ncol = 0)
for (job_num in bg_job_pool) {
  bg_job <- read.csv(paste(bg_jobs_path, job_num, ".csv", sep = ""))
  data_matrix_avg <- cbind(data_matrix_avg, bg_job$avg_cpu[4033:8032])
  data_matrix_max <- cbind(data_matrix_max, bg_job$max_cpu[4033:8032])
}
rownames(data_matrix_avg) <- seq(0, 5 * (nrow(data_matrix_avg) - 1),5)
rownames(data_matrix_max) <- seq(0, 5 * (nrow(data_matrix_max) - 1),5)
colnames(data_matrix_avg) <- bg_job_pool
colnames(data_matrix_max) <- bg_job_pool

cpu_required <- rep(0, ncol(data_matrix_max))
for (j in 1:ncol(data_matrix_max)) {
  cpu_required[j] <- as.numeric(quantile(data_matrix_max[,j], c(0.15, 0.5, 0.85), type = 4)[3])
}

for (job_length in c(1,12)) {
  print(paste("Job_length", job_length))
  output <- lm_stationary_model(dataset_max = data_matrix_max, window_size = 2,job_length=job_length, cpu_required=(100-cpu_required), prob_cut_off=0.01, initial_train_size = 2000, update_freq=1)
  write.csv(output$avg_usage, file = paste("LMAOM", job_length, "1000", 0.01, "avg_usage.csv"))
  print(paste("Avg cycle used:", "job length", job_length, mean(as.matrix(output$avg_usage), na.rm = TRUE)))
  write.csv(output$job_survival, file = paste("LMAOM", job_length, "1000", 0.01, "job_survival.csv"))
  print(paste("Job survival rate:", "job length", job_length, sum(as.matrix(output$job_survival)) / (length(as.matrix(output$job_survival)))))
  write.csv(output$scheduling_summary, file = paste("LMAOM",job_length, "1000", 0.01,"scheduling_sum.csv"))
  scheduled_num <- sum(output$scheduling_summary[1,])
  unscheduled_num <- sum(output$scheduling_summary[2,])
  correct_scheduled_num <- scheduled_num - sum(output$scheduling_summary[3,])
  correct_unscheduled_num <- unscheduled_num - sum(output$scheduling_summary[4,])
  print(paste("Scheduling summary:", "Correct scheduled rate:", correct_scheduled_num / scheduled_num, "Correct unscheduled rate:", correct_unscheduled_num / unscheduled_num))
  
}