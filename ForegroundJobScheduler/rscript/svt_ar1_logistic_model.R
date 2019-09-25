library("forecast")
library("mvtnorm")
library("dplyr")
library("arules")
library("dict")
library("cluster")
library("xlsx")

source("C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//rscript//helper_functions.R")


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
      expected_var <- predict(variance_model, newdata=data.frame("bin"=expected_avgs), type = "response")^2
    }
  } else {
    expected_var <- sapply(expected_avgs, kmeans_find_var, variance_model)
  }
  return(expected_var)
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


do_prediction <- function(last_obs, phi, mean, variance) {
  # Construct mean
  mu <- last_obs * phi + (1 - phi) * mean
  # Construct Var-cov matrix
  var <- variance
  result <- list('mu' = mu, 'var'=var)
  return(result)
}


find_bin_obs <- function(avg, binsize) {
  if (avg == 0) {
    return(0)
  }  else {
    return(ifelse(avg %% binsize == 0, (avg/binsize)-1, avg%/%binsize))
  }
}


train_cond_var_model <- function(ts_num, train_set_max, train_set_avg, bin_num, method) {
  if (method == "lm") {
    new_parsed_dat <- data.frame(matrix(nrow=nrow(train_set_avg), ncol=3))
    binsize <- 100 / bin_num
    bin <- as.numeric(sapply(train_set_avg[,ts_num], find_bin_obs, binsize))
    bin <- (bin + 1/2) * binsize
    for (i in 1:nrow(train_set_avg)) {
      new_parsed_dat[i,] = c(train_set_avg[i, ts_num], train_set_max[i, ts_num], bin[i])
    }
    for (i in 1:bin_num) {
      bin_mean <- (i - 1/2) * binsize
      new_parsed_dat <- rbind(new_parsed_dat, c(bin_mean, bin_mean, bin_mean))
      new_parsed_dat <- rbind(new_parsed_dat, c(bin_mean, 100, bin_mean))
    }
    colnames(new_parsed_dat) <- c('avg', 'max', 'bin')
    selected_bins <- new_parsed_dat %>%
      group_by(bin) %>%
      count()
    
    selected_bins <- subset(selected_bins, selected_bins$n >= 5)$bin
    new_parsed_dat <- new_parsed_dat %>%
      filter(bin %in% selected_bins) %>%
      group_by(bin) %>% 
      summarise(sd=sqrt(var(max))) %>%
      filter(!is.na(sd))
    sd.lm <- NULL
    if (nrow(new_parsed_dat) >= 3) {
      sd.lm <- lm(sd~bin+I(bin^2), data = new_parsed_dat)
    } else if (nrow(new_parsed_dat) == 2) {
      sd.lm <- lm(sd~bin, data = new_parsed_dat)
    } else {
      sd.lm <- new_parsed_dat$sd^2
    }
    return(sd.lm)
  } else {
    clustering_result <- list()
    avg_silhouette <- c()
    avg_silhouette[1] <- -Inf
    for (cluster_num in 2:10) {
      clustering_result[[cluster_num]] <- kmeans(train_set_avg[, ts_num], centers = cluster_num, iter.max = 20, nstart = 25)
      avg_silhouette[cluster_num] <- mean(silhouette(clustering_result[[cluster_num]]$cluster, dist = dist(train_set_avg[,ts_num]))[,3])
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


scheduling_foreground <- function(ts_num, test_dataset_max, test_dataset_avg, coeffs, means, vars, logistic_models, window_size, prob_cut_off, cpu_required, granularity, schedule_policy) {
  scheduled_num <- 0
  unscheduled_num <- 0
  correct_scheduled_num <- 0
  correct_unscheduled_num <- 0
  
  seek_length <- window_size
  last_time_schedule <- nrow(test_dataset_avg) - window_size + 1

  update_policy = ifelse(schedule_policy == "disjoint", window_size, 1)
  current_end <- window_size + 1
  while (current_end <= last_time_schedule) {
  
    ## Predict current avgs using AR1
    last_obs <- convert_frequency_dataset(test_dataset_avg[(current_end-window_size):(current_end-1), ts_num], window_size, mode = "avg")
    
    expected_avgs <- do_prediction(last_obs, coeffs[ts_num], means[ts_num], vars[ts_num])$mu
    
    logistic_model <- logistic_models[[ts_num]]
    
    prediction_prob <- 1 - predict(logistic_model, newdata = data.frame("avg"=expected_avgs), type = "response")
    
    prediction <- ifelse(prediction_prob <= prob_cut_off, 1, 0)
    scheduled_num <- ifelse(prediction == 1, scheduled_num + 1, scheduled_num)
    unscheduled_num <- ifelse(prediction == 1, unscheduled_num, unscheduled_num + 1)
    
    ## Evalute schedulings based on prediction
    start_time <- current_end
    end_time <- current_end + seek_length - 1
    position_vec <- convert_frequency_dataset(test_dataset_max[start_time:end_time, ts_num], window_size, mode = "max")
    actual <- ifelse(all(position_vec <= (100 - cpu_required[ts_num])), 1, 0)
    correct_scheduled_num <- ifelse(prediction == 1 & actual == 1, correct_scheduled_num + 1, correct_scheduled_num)
    correct_unscheduled_num <- ifelse(prediction == 0 & actual == 0, correct_unscheduled_num + 1, correct_unscheduled_num)
    
    if (schedule_policy == "dynamic") {
      if (prediction == 1) {
        update_policy = ifelse(actual == 1, window_size, 1)
      } else {
        update_policy = 1
      }
    }
    current_end <- current_end + update_policy
  }
  
  return(list("scheduled_num"=scheduled_num, "unscheduled_num"=unscheduled_num, "correct_scheduled_num"=correct_scheduled_num, "correct_unscheduled_num"=correct_unscheduled_num))
}


find_expected_max <- function(probability, variance, cpu_required) {
  return(max((100 - cpu_required) - qnorm(p=(1-probability)) * sqrt(variance), 0))
}


scheduling_model <- function(ts_num, test_dataset_max, test_dataset_avg, coeffs, means, vars, logistic_models, cond_var_models, cond.var, window_size, prob_cut_off, granularity, schedule_policy) {
  utilization <- c()
  survival <- c()
  runs <- rep(0, 20)
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
    
    expected_avgs <- do_prediction(last_obs, coeffs[ts_num], means[ts_num], vars[ts_num])$mu
    
    expected_vars <- generate_expected_conditional_var(expected_avgs, mode = cond.var, variance_model = cond_var_model)
    
    prob <- 1 - predict(logistic_model, newdata = data.frame("avg"=expected_avgs), type = "response")
    
    expected_max <- find_expected_max(prob, expected_vars, cpu_required[ts_num])
    
    pi_up <- compute_pi_up(mu=expected_max, varcov=as.matrix(expected_vars), predict_size=1, prob_cutoff=prob_cut_off, granularity=granularity)
    
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
          idx <- ifelse(run_counter > 20, 20, run_counter)
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
  return(list("utilization"=overall_rate$utilization_rate, "survival"=overall_rate$survival_rate, "run"=runs))
}


ar_logistic_model <- function(dataset_avg, dataset_max, initial_train_size, prob_cut_off, update_freq, window_size, cpu_required, cond.var, granularity, bin_num) {
  #### input dataset_avg, dataset_max: N by M matrix, N being number of observations, M being number of time series
  #### input initial_train_size: The number of first observations used to train the model
  #### input window_size: The number of observations used to train and predict as one sample
  #### input update_freq: The number of observations for each update of the model, and do the prediction
  #### input prob_cut_off: If the probability of background job exceeding 100-cpu_required is smaller than prob_cut_off, then schedule it. Otherwise, don't.
  #### input update_freq: The number of windows to do the prediction
  
  if (granularity > 0) {
    cpu_required <- sapply(cpu_required, round_to_nearest, granularity, FALSE)
  }
  
  ts_names <- colnames(dataset_avg)
  
  scheduled_num <- data.frame(matrix(nrow=length(ts_names), ncol=0))
  unscheduled_num <- data.frame(matrix(nrow=length(ts_names), ncol=0))
  correct_scheduled_num <- data.frame(matrix(nrow=length(ts_names), ncol=0))
  correct_unscheduled_num <- data.frame(matrix(nrow=length(ts_names), ncol=0))
  
  avg_usage <- data.frame(matrix(nrow=length(ts_names), ncol=0))
  job_survival <- data.frame(matrix(nrow=length(ts_names), ncol=0))
  overall_runs <- data.frame(matrix(nrow=length(ts_names), ncol = 0))
  
  ## Lists
  logistic_models <- NULL
  cond_var_models <- NULL
  
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
  colnames(new_trainset_avg) <- ts_names
  
  ## Training AR1 Model
  print("Training: AR1.")
  train_result <- sapply(1:length(ts_names), train_ar1_model, new_trainset_avg)
  coeffs <- unlist(train_result[1,])
  means <- unlist(train_result[2,])
  vars <- unlist(train_result[3,])
  
  ## Training Logistic Model
  print("Training: Logistic.")
  logistic_models <- sapply(1:length(ts_names), train_logistic_model, new_trainset_max, new_trainset_avg, cpu_required, simplify=FALSE)
  
  ## Training Polynomial Regression Model
  print("Training: Polynomial Regression.")
  cond_var_models <- sapply(1:length(ts_names), train_cond_var_model, new_trainset_max, new_trainset_avg, bin_num, cond.var, simplify=FALSE)
  
  ## Test Model
  print("Testing on Foreground job:")
  result_foreground <- sapply(1:length(ts_names), scheduling_foreground, test_dataset_max, test_dataset_avg, coeffs, means, vars, logistic_models, window_size, prob_cut_off, cpu_required, granularity, schedule_policy)
  
  print("Testing on Model:")
  result_model <- sapply(1:length(ts_names), scheduling_model, test_dataset_max, test_dataset_avg, coeffs, means, vars, logistic_models, cond_var_models, cond.var, window_size, prob_cut_off, granularity, schedule_policy, simplify=FALSE)
  
  scheduled_num <- cbind(scheduled_num, unlist(result_foreground[1,]))
  unscheduled_num <- cbind(unscheduled_num, unlist(result_foreground[2,]))
  correct_scheduled_num <- cbind(correct_scheduled_num, unlist(result_foreground[3,]))
  correct_unscheduled_num <- cbind(correct_unscheduled_num, unlist(result_foreground[4,]))
  
  for (ts_num in 1:length(ts_names)) {
    avg_usage <- rbind(avg_usage, result_model[[ts_num]]$utilization)
    job_survival <- rbind(job_survival, result_model[[ts_num]]$survival)
    if (schedule_policy == "dynamic") {
      overall_runs <- rbind(overall_runs, result_model[[ts_num]]$run)
    }
  }
  
  rownames(scheduled_num) <- ts_names
  colnames(scheduled_num) <- "scheduled_num"
  rownames(unscheduled_num) <- ts_names
  colnames(unscheduled_num) <- "unscheduled_num"
  rownames(correct_scheduled_num) <- ts_names
  colnames(correct_scheduled_num) <- "correct_scheduled_num"
  rownames(correct_unscheduled_num) <- ts_names
  colnames(correct_unscheduled_num) <- "correct_unscheduled_num"
  rownames(avg_usage) <- ts_names
  colnames(avg_usage) <- "avg_usage"
  rownames(job_survival) <- ts_names
  colnames(job_survival) <- "survival"
  if (schedule_policy == "dynamic") {
    rownames(overall_runs) <- ts_names
    colnames(overall_runs) <- sapply(1:20, function(i) as.character(i))
    result <- list('avg_usage'=avg_usage, 'job_survival'=job_survival, 'scheduled_num'=scheduled_num, "unscheduled_num"=unscheduled_num, "correct_scheduled_num"=correct_scheduled_num, "correct_unscheduled_num"=correct_unscheduled_num, "overall_runs"=overall_runs)
    return(result)  
  } else {
    result <- list('avg_usage'=avg_usage, 'job_survival'=job_survival, 'scheduled_num'=scheduled_num, "unscheduled_num"=unscheduled_num, "correct_scheduled_num"=correct_scheduled_num, "correct_unscheduled_num"=correct_unscheduled_num)
    return(result)
  }  
}


wrapper.epoche <- function(parameter, dataset_avg, dataset_max, cpu_required, initial_train_size, update_freq, cond.var, bin_num, bad.seq.adj, output_dp) {
  
  job_length <- as.numeric(parameter[1])
  prob_cut_off <- as.numeric(parameter[2])
  granularity <- as.numeric(parameter[3])
  
  output <- ar_logistic_model(dataset_avg, dataset_max, initial_train_size, prob_cut_off, 1, job_length, cpu_required, cond.var, granularity, bin_num)
  
  overall_evaluation <- find_overall_evaluation(output$avg_usage[,1], output$job_survival[,1])
  
  utilization_rate <- overall_evaluation$utilization_rate
  survival_rate <- overall_evaluation$survival_rate
  
  scheduled_num <- sum(output$scheduled_num[,1])
  unscheduled_num <- sum(output$unscheduled_num[,1])
  correct_scheduled_num <- sum(output$correct_scheduled_num[,1])
  correct_unscheduled_num <- sum(output$correct_unscheduled_num[,1])
  
  correct_scheduled_rate <- correct_scheduled_num / scheduled_num
  correct_unscheduled_rate <- correct_unscheduled_num / unscheduled_num
  
  print(paste("Avg cycle used:", "job length", job_length, utilization_rate))
  print(paste("Job survival rate:", "job length", job_length, survival_rate))
  print(paste("Scheduling summary:", "Correct scheduled rate:", correct_scheduled_rate, "Correct unscheduled rate:", correct_unscheduled_rate))
  
  if (cond.var == "lm") {
    
    if (schedule_policy == "dynamic") {
     write.csv(output$overall_runs, paste("Overall Runs", "AR1_logistic_lm", sample_size, job_length, prob_cut_off, granularity, ".csv"))
    }
    result_path.xlsx <- read.xlsx(output_dp, sheetIndex = 1)
    result_path.xlsx <- update.xlsx.df(result_path.xlsx, "AR1_logistic_lm", prob_cut_off, NA, sample_size, job_length, granularity, utilization_rate, survival_rate, correct_scheduled_rate, correct_unscheduled_rate)
    write.xlsx(result_path.xlsx, showNA = FALSE, file = output_dp, row.names = FALSE)
    
  } else {
    
    if (schedule_policy == "dynamic") {
     write.csv(output$overall_runs, paste("Overall Runs", "AR1_logistic_kmeans", sample_size, job_length, prob_cut_off, granularity, ".csv"))
    }
    result_path.xlsx <- read.xlsx(output_dp, sheetIndex = 1)
    result_path.xlsx <- update.xlsx.df(result_path.xlsx, "AR1_logistic_kmeans", prob_cut_off, NA, sample_size, job_length, granularity, utilization_rate, survival_rate, correct_scheduled_rate, correct_unscheduled_rate)
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

schedule_policy <- "dynamic"

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

parameter.df <- expand.grid(window_sizes, prob_cut_offs, granularity)
colnames(parameter.df) <- c("job_length", "prob_cut_off", "granularity")
parameter.df <- parameter.df %>% 
  arrange(job_length)

slt <- apply(parameter.df, 1, wrapper.epoche, data_matrix_avg, data_matrix_max, (100-cpu_required), initial_train_size, 1, cond.var, 100, bad.seq.adj, output_dp)
