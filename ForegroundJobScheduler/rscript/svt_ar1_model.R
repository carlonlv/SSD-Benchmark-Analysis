library("forecast")
library("mvtnorm")
library("dict")
library("dplyr")
library("xlsx")

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

round_to_nearest <- function(data, divisor, lower) {
  if (lower) {
    return(floor(data / divisor) * divisor)
  } else {
    return(ceiling(data / divisor) * divisor)
  }
}

calculate_var_cov_matrix_ar1 <-function(var, l, phi) {
  #### input var: A vector from var(an+l) to var(an+1) of length l
  #### input l: number of prediction
  #### input phi: coeff of AR1 model
  dm=abs(outer(1:l,1:l,"-"))
  var_cov <- matrix(var[outer(1:l,1:l,"pmin")],l,l)*phi^dm
  return(var_cov)
}

do_prediction <- function(last_obs, phi, mean, variance, predict_size, level) {
  # Construct mean
  mu <- rep(last_obs, predict_size)
  mu <- mu * phi^(1:predict_size) + (1 - phi^(1:predict_size)) * mean
  # Construct Var-cov matrix
  var <- cumsum((phi^2)^(0:(predict_size-1)))*variance
  varcov <- calculate_var_cov_matrix_ar1(var, predict_size, phi)
  # caclulate probability
  up_bound <- rep(level, predict_size)
  lower_bound <- rep(0, predict_size)
  prob <- 1 - pmvnorm(upper = up_bound, lower = lower_bound, mean = mu, sigma = varcov)
  result <- list('prob' = as.numeric(prob), 'mu' = mu, 'varcov'=varcov)
  return(result)
}

compute_pi_up <- function(mu, varcov, predict_size, prob_cutoff, granularity) {
  upper_bounds <- rep(NA, predict_size)
  for (i in 1:predict_size) {
    upper_bounds[i] <- min(mu[i] + qnorm((1-prob_cutoff), 0, 1) * sqrt(varcov[i,i]), 100)
  }
  if (granularity > 0) {
    scheduled_size <- sapply(100 - upper_bounds, round_to_nearest, granularity, TRUE)
    upper_bounds <- 100 - scheduled_size
  }
  return(upper_bounds)
}

find_evaluation <- function(pi_up, actual_obs, granularity=0) {
  if (granularity != 0) {
    actual_available <- round_to_nearest(100 - actual_obs, granularity, TRUE)
    actual_obs <- 100 - actual_available
  }
  usage <- c()
  survival <- c()
  for (i in 1:length(pi_up)) {
    if (granularity == 0) {
      if ((100 - pi_up[i]) == 0 & (100 - actual_obs[i]) == 0) {
        survival[i] <- NA
        usage[i] <- NA
      } else {
        survival[i] <- ifelse(actual_obs[i] <= pi_up[i], 1, 0)
        usage[i] <- ifelse(survival[i] == 0, NA, ifelse(actual_obs[i] == pi_up[i], NA, (100 - pi_up[i]) / (100 - actual_obs[i])))
      }
    } else {
      if ((100 - pi_up[i]) < granularity) {
        if ((100 - actual_obs[i]) < granularity) {
          survival[i] <- NA
          usage[i] <- NA
        } else {
          survival[i] <- 1
          usage[i] <- 0
        }
      } else {
        if ((100 - actual_obs[i]) < granularity) {
          survival[i] <- 0
          usage[i] <- NA
        } else {
          survival[i] <- ifelse(actual_obs[i] <= pi_up[i], 1, 0)
          usage[i] <- ifelse(survival[i] == 0, NA, ifelse(actual_obs[i] == pi_up[i], NA, (100 - pi_up[i]) / (100 - actual_obs[i])))
        }
      }
    }
  }
  avg_usage <- mean(usage, na.rm=TRUE)
  overall_survival <- ifelse(any(is.na(survival)), NA, ifelse(any(survival == 0), 0, 1))
  result <- list('avg_usage' = avg_usage, 'survival'= overall_survival)
  return(result)
}

svt_stationary_model <- function(dataset, initial_train_size, window_size, job_length, cpu_required, prob_cut_off, update_freq, mode, granularity) {
  #### input dataset: N by M matrix, N being number of observations, M being number of time series
  #### input initial_train_size: The number of first observations used to train the model
  #### input window_size: The number of observations used to train and predict
  #### input job_length: The time that the foreground job will be runing
  #### input cpu_required: A vector, the cpu that the foreground job requires in percentage
  #### input prob_cut_off: If the probability of background job exceeding 100-cpu_required is smaller than prob_cut_off, then schedule it. Otherwise, don't.
  #### input update_freq: The number of observations for each update of the model, and do the prediction
  
  if (granularity > 0) {
    cpu_required <- sapply(cpu_required, round_to_nearest, granularity, FALSE)
  }
  
  ## N by M dataframe
  probability <- data.frame(row.names = 1)
  pi_upper_bounds <- data.frame(row.names = 1)
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
  pi_up_bounds <- dict()
  
  ## Summary Counts
  scheduled_num <- rep(0, ncol(dataset))
  unscheduled_num <- rep(0, ncol(dataset))
  falsely_scheduled_num <- rep(0, ncol(dataset))
  falsely_unscheduled_num <- rep(0, ncol(dataset))
  
  ## Split in Training and Testing Set
  train_dataset <- dataset[1:initial_train_size, 1:ncol(dataset)]
  test_dataset <- dataset[(initial_train_size+1):nrow(dataset), 1:ncol(dataset)]
  
  ## Convert Frequency
  new_trainset <- matrix(nrow = floor(nrow(train_dataset) / window_size), ncol = 0)
  new_testset <- matrix(nrow = floor(nrow(test_dataset) / window_size), ncol = 0)
  for (ts_num in 1:ncol(dataset)) {
    converted_data <- convert_frequency_dataset(train_dataset[1:nrow(train_dataset), ts_num], window_size, mode)
    new_trainset <- cbind(new_trainset, converted_data)
    converted_data <- convert_frequency_dataset(test_dataset[1:nrow(test_dataset), ts_num], window_size, mode)
    new_testset <- cbind(new_testset, converted_data)
  }
  rownames(new_trainset) <- seq(1, 1 + window_size * (nrow(new_trainset) - 1), window_size)
  colnames(new_trainset) <- colnames(train_dataset)
  rownames(new_testset) <- seq(1, 1+ window_size * (nrow(new_testset) - 1), window_size)
  colnames(new_testset) <- colnames(test_dataset)
  
  ## Train Model
  coeffs <- rep(NA, ncol(new_trainset))
  means <- rep(NA, ncol(new_trainset))
  vars <- rep(NA, ncol(new_trainset))
  train_percent <- 0.00
  for (ts_num in 1:ncol(new_trainset)) {
    if (round(ts_num / ncol(new_trainset), 2) != train_percent) {
      print(paste("Training", train_percent))
      train_percent <- round(ts_num / ncol(new_trainset), 2)
    }
    ts_model <- tryCatch({
      arima(x=new_trainset[, ts_num], order = c(1,0,0), include.mean = TRUE, method = "CSS-ML", optim.control = list(maxit=2000))
    }, error = function(cond) {
      return(arima(x=new_trainset[, ts_num], order = c(1,0,0), include.mean = TRUE, method = "ML", optim.control = list(maxit=2000)))
    })
    coeffs[ts_num] <- as.numeric(ts_model$coef[1])
    means[ts_num] <- as.numeric(ts_model$coef[2])
    vars[ts_num] <- ts_model$sigma2
  }
  
  ## Test Model
  current_end <- 2
  current_percent <- 0.00
  while (current_end <= nrow(new_testset)) {
    
    ## Initialize Model 
    prob_vector <- c()
    pi_up <- c()
    prediction <- c()
    
    for (ts_num in 1:ncol(new_testset)) {
      ## Schedule the job
      if (current_end <= (nrow(new_testset) - job_length + 1)) {
        last_obs <- new_testset[(current_end-1), ts_num]
        prediction_result <- do_prediction(last_obs = last_obs, phi = coeffs[ts_num], mean = means[ts_num], variance = vars[ts_num],predict_size=job_length, level = (100 - cpu_required[ts_num]))
        prob_vector[ts_num] <- prediction_result$prob
        if (prob_vector[ts_num] < prob_cut_off) {
          prediction[ts_num] <- 1
          scheduled_num[ts_num] <- scheduled_num[ts_num] + 1
        } else {
          prediction[ts_num] <- 0
          unscheduled_num[ts_num] <- unscheduled_num[ts_num] + 1
        }
        pi_up_bounds[[paste(ts_num, ",", current_end, sep = "")]] <- compute_pi_up(mu=prediction_result$mu, varcov=prediction_result$varcov, predict_size=job_length, prob_cutoff=prob_cut_off, granularity=granularity)
        if (job_length == 1) {
          pi_up[ts_num] <- pi_up_bounds[[paste(ts_num, ",", current_end, sep = "")]]
        }
      }
    }
    
    ## Store Probability
    probability <- rbind(probability, prob_vector)
    
    ## Store Prediction Upper Bounds
    pi_upper_bounds <- rbind(pi_upper_bounds, pi_up)
    
    ## Store Prediction
    predict_result <- rbind(predict_result, prediction)
    
    ## Queue the jobs to check their correctness later
    closest_update <- NA
    if (job_length == 1) {
      closest_update <- current_end
    } else {
      closest_update <- current_end + ceiling(job_length / update_freq) * update_freq
    }
    end_time_testing_queue$append_number(closest_update, current_end)
    end_time_testing_queue$append_number(closest_update, current_end + job_length - 1)
    end_time_testing_queue$append_number(closest_update, nrow(predict_result))
    
    actual <- c()
    avg_cycle_used <- c()
    survival <- c()
    
    for (ts_num in 1:ncol(new_testset)) {
      ## Check correctness of previous schedulings
      if (length(end_time_testing_queue[[current_end]]) != 0) {
        info_lst <- end_time_testing_queue[[current_end]]
        lst_len <- length(info_lst)
        for (i in seq(1, lst_len, 3)) {
          start_time <- info_lst[i]
          end_time <- info_lst[i+1]
          row_num <- info_lst[i+2]
          
          position_vec <- new_testset[start_time:end_time, ts_num]
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
        }
        pi_up <- pi_up_bounds[[paste(ts_num, ",", start_time, sep = "")]]
        evalulation <- find_evaluation(pi_up=pi_up, actual_obs=position_vec, granularity=granularity)
        avg_cycle_used[ts_num] <- evalulation$avg_usage
        survival[ts_num] <- evalulation$survival
      }
    }
    
    ## Store Actual
    actual_result <- rbind(actual_result, actual)
    
    ## Store Evaluations
    avg_usage <- rbind(avg_usage, avg_cycle_used)
    job_survival <- rbind(job_survival, survival)
    
    ## Update current_end
    current_end <- current_end + update_freq
    if (current_percent != round((current_end - 1) / (nrow(new_testset) - job_length), digits = 2)) {
      print(paste("Testing", current_percent))
      current_percent <- round((current_end - 1) / (nrow(new_testset) - job_length), digits = 2)
    }
  }
  
  ## Change column and row names, N by M
  colnames(probability) <- colnames(new_testset)
  rownames(probability) <- seq(initial_train_size + window_size + 1, initial_train_size + window_size + 1 + (update_freq * window_size) * (nrow(probability) - 1), update_freq * window_size)
  
  colnames(pi_upper_bounds) <- colnames(new_testset)
  rownames(pi_upper_bounds) <- seq(initial_train_size + window_size + 1, initial_train_size + window_size + 1 + update_freq * (nrow(pi_upper_bounds) - 1), update_freq)
  
  colnames(avg_usage) <- colnames(new_testset)
  rownames(avg_usage) <- seq(initial_train_size + window_size + 1, initial_train_size + window_size + 1 + (update_freq * window_size) * (nrow(avg_usage) - 1), update_freq * window_size)
  
  colnames(job_survival) <- colnames(new_testset)
  rownames(job_survival) <- seq(initial_train_size + window_size + 1, initial_train_size + window_size + 1 + (update_freq * window_size) * (nrow(job_survival) - 1), update_freq * window_size)
  
  colnames(predict_result) <- colnames(new_testset)
  rownames(predict_result) <- seq(initial_train_size + window_size + 1, initial_train_size + window_size + 1 + (update_freq * window_size) * (nrow(predict_result) - 1), update_freq * window_size)
  
  colnames(actual_result) <- colnames(new_testset)
  rownames(actual_result) <- seq(initial_train_size + window_size + 1, initial_train_size + window_size + 1 + (update_freq * window_size) * (nrow(actual_result) - 1), update_freq * window_size)
  
  colnames(scheduling_summary) <- colnames(new_testset)
  scheduling_summary[1,] <- scheduled_num
  scheduling_summary[2,] <- unscheduled_num
  scheduling_summary[3,] <- falsely_scheduled_num
  scheduling_summary[4,] <- falsely_unscheduled_num
  rownames(scheduling_summary) <- c('Scheduled_Num', 'Unscheduled_Num', 'Falsly_scheduled_Num', 'Falsely_unscheduled_Num')
  
  result <- list('prob' = probability, 'pi_up' = pi_upper_bounds, 'avg_usage'=avg_usage, 'job_survival'=job_survival, 'predict' = predict_result, 'actual' = actual_result, 'scheduling_summary' = scheduling_summary)
  return(result)
}

update.xlsx.df <- function(xlsx_file, model_name, prob_cut_off, state_num, sample_size, window_size, granularity,utilization, survival, correct_scheduled_rate, correct_unscheduled_rate) {
  if (is.na(state_num)) {
    xlsx_file <- xlsx_file %>%
      mutate(Avg.Cycle.Usage = ifelse(Model == model_name & 
                                        Probability.Cut.Off == prob_cut_off & 
                                        Sample.Size == sample_size &
                                        Window.Size == window_size &
                                        Granularity == granularity, 
                                      utilization, Avg.Cycle.Usage)) %>%
      mutate(Survival.Rate = ifelse(Model == model_name & 
                                      Probability.Cut.Off == prob_cut_off & 
                                      Sample.Size == sample_size &
                                      Window.Size == window_size &
                                      Granularity == granularity, 
                                    survival, Survival.Rate)) %>%
      mutate(Correctly.Scheduled = ifelse(Model == model_name & 
                                            Probability.Cut.Off == prob_cut_off & 
                                            Sample.Size == sample_size &
                                            Window.Size == window_size &
                                            Granularity == granularity, 
                                          correct_scheduled_rate, Correctly.Scheduled)) %>%
      mutate(Correctly.Unscheduled = ifelse(Model == model_name & 
                                              Probability.Cut.Off == prob_cut_off & 
                                              Sample.Size == sample_size &
                                              Window.Size == window_size &
                                              Granularity == granularity, 
                                            correct_unscheduled_rate, Correctly.Unscheduled))
  } else {
    xlsx_file <- xlsx_file %>%
      mutate(Avg.Cycle.Usage = ifelse(Model == model_name & 
                                        Probability.Cut.Off == prob_cut_off & 
                                        Sample.Size == sample_size &
                                        StateNum == state_num & 
                                        Window.Size == window_size &
                                        Granularity == granularity, 
                                      utilization, Avg.Cycle.Usage)) %>%
      mutate(Survival.Rate = ifelse(Model == model_name & 
                                      Probability.Cut.Off == prob_cut_off & 
                                      Sample.Size == sample_size &
                                      StateNum == state_num & 
                                      Window.Size == window_size &
                                      Granularity == granularity, 
                                    survival, Survival.Rate)) %>%
      mutate(Correctly.Scheduled = ifelse(Model == model_name & 
                                            Probability.Cut.Off == prob_cut_off & 
                                            Sample.Size == sample_size &
                                            StateNum == state_num & 
                                            Window.Size == window_size &
                                            Granularity == granularity, 
                                          correct_scheduled_rate, Correctly.Scheduled)) %>%
      mutate(Correctly.Unscheduled = ifelse(Model == model_name & 
                                              Probability.Cut.Off == prob_cut_off & 
                                              Sample.Size == sample_size &
                                              StateNum == state_num & 
                                              Window.Size == window_size &
                                              Granularity == granularity, 
                                            correct_unscheduled_rate, Correctly.Unscheduled))
  }
  return(xlsx_file)
}

bad_seq_adjustment <- function(survivals) {
  if (length(survivals) >= 2) {
    result <- survivals[1]
    schedule <- 0
    i <- 2
    while (i <= length(survivals)) {
      if (schedule < 2) {
        if (!is.na(survivals[i-1]) & survivals[i-1] == 0) {
          schedule <- schedule + 1
        } else if (!is.na(survivals[i-1]) & survivals[i-1] == 1) {
          schedule <- 0
        }
        result[i] <- survivals[i]
      } else {
        if (survivals[i-1] == 1) {
          schedule <- 0
        }
        result[i] <- NA
      }
      i <- i + 1
    }
    return(result)
  } else {
    return(survivals)
  }
}

find_overall_evaluation <- function(avg_usages, survivals, bad.seq.adj) {
  if (bad.seq.adj) {
    survivals <- apply(survivals, 2, bad_seq_adjustment)
  }
  avg_utilization <- mean(as.matrix(avg_usages), na.rm = TRUE)
  survival <- sum(as.matrix(survivals), na.rm = TRUE) / (length(as.matrix(survivals)[!is.na(as.matrix(survivals))]))
  return(list("avg_utilization"=avg_utilization, "survival"=survival))
}

wrapper.epoche <- function(parameter, dataset, cpu_required, initial_train_size, update_freq, bad.seq.adj, output_dp) {
  
  window_size <- as.numeric(parameter[1])
  prob_cut_off <- as.numeric(parameter[2])
  granularity <- as.numeric(parameter[3])
  
  output <- svt_stationary_model(datase =dataset, window_size=window_size, job_length=1, cpu_required=cpu_required, prob_cut_off=prob_cut_off, initial_train_size=initial_train_size, update_freq=1, mode="max", granularity=granularity)
  overall_evaluation <- find_overall_evaluation(output$avg_usage, output$job_survival, bad.seq.adj)
  avg_utilization <- overall_evaluation$avg_utilization
  survival <- overall_evaluation$survival
  
  scheduled_num <- sum(output$scheduling_summary[1,])
  unscheduled_num <- sum(output$scheduling_summary[2,])
  correct_scheduled_num <- scheduled_num - sum(output$scheduling_summary[3,])
  correct_unscheduled_num <- unscheduled_num - sum(output$scheduling_summary[4,])
  correct_scheduled_rate <- correct_scheduled_num / scheduled_num
  correct_unscheduled_rate <- correct_unscheduled_num / unscheduled_num
  
  print(paste("Avg cycle used:", "job length", window_size, avg_utilization))
  print(paste("Job survival rate:", "job length", window_size, survival))
  print(paste("Scheduling summary:", "Correct scheduled rate:", correct_scheduled_rate, "Correct unscheduled rate:", correct_unscheduled_rate))
  
  write.csv(output$pi_up, file = paste("AR1",window_size, sample_size, prob_cut_off, granularity, "pi_upper.csv"))
  write.csv(output$scheduling_summary, file = paste("AR1", window_size, sample_size, prob_cut_off, granularity, "scheduling_sum.csv"))
  write.csv(output$avg_usage, file = paste("AR1", window_size, sample_size, prob_cut_off, granularity, "avg_usage.csv"))
  write.csv(output$job_survival, file = paste("AR1", window_size, sample_size, prob_cut_off, granularity, "job_survival.csv"))
  
  result_path.xlsx <- read.xlsx(output_dp, sheetIndex = 1)
  result_path.xlsx <- update.xlsx.df(result_path.xlsx, "AR1", prob_cut_off, NA, sample_size, window_size, granularity, avg_utilization, survival, correct_scheduled_rate, correct_unscheduled_rate)
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
granularity <- c(10, 100/32, 100/64, 100/32, 0)

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
  output_dp <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//Nonoverlapping windows//summary (windows,granularity) post adj.xlsx"
} else {
  #output_dp <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//Nonoverlapping windows//summary (windows) max.xlsx"
  output_dp <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//Nonoverlapping windows//summary (windows,granularity).xlsx"
}

parameter.df <- expand.grid(window_sizes, prob_cut_offs, granularity)
colnames(parameter.df) <- c("window_size", "prob_cut_off", "granularity")
slt <- apply(parameter.df, 1, wrapper.epoche, dataset=data_matrix, cpu_required=(100-cpu_required), initial_train_size=initial_train_size, update_freq=1, bad.seq.adj=bad.seq.adj, output_dp=output_dp)
