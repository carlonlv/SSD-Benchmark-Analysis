
if (Sys.info()["sysname"] == "Windows") {
  source("C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//rscript//helper_functions.R")
} else {
  source("/Users/carlonlv/Documents/Github/Research-Projects/ForegroundJobScheduler/rscript/helper_functions.R")
}

cores <- ifelse(Sys.info()["sysname"] == "Windows", 1, detectCores(all.tests = FALSE, logical = TRUE))


train_ar1_model <- function(train_dataset) {
  
  ts_model <- tryCatch({
    arima(x=train_dataset, order = c(1,0,0), include.mean = TRUE, method = "CSS-ML", optim.control = list(maxit=2000))
  }, error = function(cond) {
    return(arima(x=train_dataset, order = c(1,0,0), include.mean = TRUE, method = "ML", optim.control = list(maxit=2000)))
  })
  return(list("coeffs"=as.numeric(ts_model$coef[1]), "means"= as.numeric(ts_model$coef[2]), "vars"=ts_model$sigma2))
}


calculate_var_cov_matrix_ar1 <-function(var, l, phi) {
  #### input var: A vector from var(an+l) to var(an+1) of length l
  #### input l: number of prediction
  #### input phi: coeff of AR1 model
  
  dm=abs(outer(1:l,1:l,"-"))
  var_cov <- matrix(var[outer(1:l,1:l,"pmin")],l,l)*phi^dm
  return(var_cov)
}


do_prediction <- function(last_obs, phi, mean, variance, predict_size, level=NULL) {
  
  # Construct mean
  mu <- rep(last_obs, predict_size)
  mu <- mu * phi^(1:predict_size) + (1 - phi^(1:predict_size)) * mean
  # Construct Var-cov matrix
  var <- cumsum((phi^2)^(0:(predict_size-1)))*variance
  varcov <- calculate_var_cov_matrix_ar1(var, predict_size, phi)
  # caclulate probability
  prob <- NULL
  if (!is.null(level)) {
    prob <- 1 - pmvnorm(lower = rep(0, predict_size), upper = rep(level, predict_size), mean = mu, sigma = varcov)
  }
  return(list("prob"=as.numeric(prob), "mu"=mu, "varcov"=varcov))
}


scheduling_foreground <- function(test_dataset, coeffs, means, vars, window_size, prob_cut_off, cpu_required, granularity, schedule_policy) {
  
  cpu_required <- ifelse(granularity>0, round_to_nearest(cpu_required, granularity, FALSE), cpu_required)
  
  scheduled_num <- 0
  unscheduled_num <- 0
  correct_scheduled_num <- 0
  correct_unscheduled_num <- 0
  
  last_time_schedule <- length(test_dataset) - window_size + 1
  
  update_policy = ifelse(schedule_policy == "disjoint", window_size, 1)
  current_end <- window_size + 1
  while (current_end <= last_time_schedule) {
    ## Schedule based on model predictions
    last_obs <- convert_frequency_dataset(test_dataset[(current_end-window_size):(current_end-1)], window_size, mode = "max")
    prediction_result <- do_prediction(last_obs, coeffs, means, vars, 1, level=(100-cpu_required))
    prediction <- ifelse(prediction_result$prob <= prob_cut_off, 1, 0)
    scheduled_num <- ifelse(prediction == 1, scheduled_num + 1, scheduled_num)
    unscheduled_num <- ifelse(prediction == 1, unscheduled_num, unscheduled_num + 1)
    
    ## Evalute schedulings based on prediction
    start_time <- current_end
    end_time <- current_end + window_size - 1
    position_vec <- convert_frequency_dataset(test_dataset[start_time:end_time], window_size, mode = "max")
    actual <- ifelse(all(position_vec <= (100-cpu_required)), 1, 0)
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


scheduling_model <- function(test_dataset, coeffs, means, vars, window_size, prob_cut_off, granularity, schedule_policy) {
  
  run_switch <- FALSE
  
  last_time_schedule <- length(test_dataset) - window_size + 1
  
  current_end <- window_size + 1
  update_policy <- ifelse(schedule_policy == "disjoint", window_size, 1)
  
  pi_ups <- c()
  
  utilization <- c()
  survival <- c()
  while (current_end <= last_time_schedule) {
    ## Schedule based on model predictions
    last_obs <- convert_frequency_dataset(test_dataset[(current_end-window_size):(current_end-1)], window_size, mode = "max")
    prediction_result <- do_prediction(last_obs=last_obs, phi=coeffs, mean=means, variance=vars, predict_size=1)
    pi_up <- compute_pi_up(mu=prediction_result$mu, varcov=prediction_result$varcov, predict_size=1, prob_cutoff=prob_cut_off, granularity=granularity)
    pi_ups <- c(pi_ups, pi_up)
    
    ## Evalute schedulings based on prediction
    start_time <- current_end
    end_time <- current_end + window_size - 1
    
    utilization <- c(utilization, check_utilization(pi_up, granularity))
    survival <- c(survival, check_survival(pi_up, test_dataset[start_time:end_time], granularity))
    
    if (schedule_policy == "dynamic") {
      if (!is.na(survival[length(survival)]) & survival[length(survival)] == 0) {
        update_policy <- window_size
        if (run_switch) {
          run_switch <- FALSE
        } 
      } else if (is.na(survival[length(survival)])){
        update_policy <- 1
        if (!run_switch) {
          run_switch <- TRUE
        }
      } else {
        update_policy <- survival[length(survival)]
        if (!run_switch) {
          run_switch <- TRUE
        } else {
          survival[length(survival)] <- survival[length(survival)]
        }
      }
    }
    current_end <- current_end + update_policy
  }
  
  overall_survival <- compute_survival(ifelse(is.na(survival), NA, ifelse(survival == 0, 1, 0)))
  overall_utilization <- compute_utilization(pi_ups, survival, test_dataset[(window_size+1):(current_end-update_policy+window_size-1)], window_size, granularity, schedule_policy)
  return(list("util_numerator"=overall_utilization$numerator, "util_denominator1"=overall_utilization$denominator1, "util_denominator2"=overall_utilization$denominator2, "sur_numerator"=overall_survival$numerator, "sur_denominator"=overall_survival$denominator))
}


svt_model <- function(ts_num, dataset, train_size, window_size, update_freq, prob_cut_off, cpu_required, granularity, schedule_policy="disjoint") {
  
  data_set <- dataset[,ts_num]
  cpu_required <- cpu_required[ts_num]
  
  scheduled_num <- 0
  unscheduled_num <- 0
  correct_scheduled_num <- 0
  correct_unscheduled_num <- 0
  
  util_numerator <- 0
  util_denominator1 <- 0
  util_denominator2 <- 0
  sur_numerator <- 0
  sur_denominator <- 0
  
  current <- 1
  last_time_update <- length(data_set) - update_freq - train_size + 1
  while (current <= last_time_update) {
    ## Split into train set and test set
    train_set <- data_set[current:(current+train_size-1)]
    test_set <- data_set[(current+train_size+1):(current+train_size+update_freq)]
    
    ## Convert Frequency for training set
    new_trainset <- convert_frequency_dataset(train_set, window_size, "max")
    
    ## Train Model
    trained_result <- train_ar1_model(new_trainset)
    coeffs <- trained_result$coeffs
    means <- trained_result$means
    vars <- trained_result$vars
    
    ## Test Model
    result_foreground <- scheduling_foreground(test_set, coeffs, means, vars, window_size, prob_cut_off, cpu_required, granularity, schedule_policy)
    result_model <- scheduling_model(test_set, coeffs, means, vars, window_size, prob_cut_off, granularity, schedule_policy)

    ## Write Result
    scheduled_num <- scheduled_num + result_foreground$scheduled_num
    unscheduled_num <- unscheduled_num + result_foreground$unscheduled_num
    correct_scheduled_num <- correct_scheduled_num + result_foreground$correct_scheduled_num
    correct_unscheduled_num <- correct_unscheduled_num + result_foreground$correct_unscheduled_num
    
    util_numerator <- util_numerator + ifelse(is.na(result_model$util_numerator), 0, result_model$util_numerator)
    util_denominator1 <- util_denominator1 + ifelse(is.na(result_model$util_denominator1), 0, result_model$util_denominator1)
    util_denominator2 <- util_denominator2 + ifelse(is.na(result_model$util_denominator2), 0, result_model$util_denominator2)
    sur_numerator <- sur_numerator + ifelse(is.na(result_model$sur_numerator), 0, result_model$sur_numerator)
    sur_denominator <- sur_denominator + ifelse(is.na(result_model$sur_denominator), 0, result_model$sur_denominator)
    
    ## Update current
    current <- current + update_freq
  }
  
  return(list("scheduled_num"=scheduled_num, "unscheduled_num"=unscheduled_num, "correct_scheduled_num"=correct_scheduled_num, "correct_unscheduled_num"=correct_unscheduled_num, "util_numerator"=util_numerator, "util_denominator1"=util_denominator1, "util_denominator2"=util_denominator2, "sur_numerator"=sur_numerator, "sur_denominator"=sur_denominator))
}


svt_stationary_model <- function(dataset, train_size, window_size, update_freq, prob_cut_off, cpu_required, granularity, schedule_policy="disjoint") {
  
  scheduled_num <- c()
  unscheduled_num <- c()
  correct_scheduled_num <- c()
  correct_unscheduled_num <- c()
  
  util_numerator <- c()
  util_denominator1 <- c()
  util_denominator2 <- c()
  sur_numerator <- c()
  sur_denominator <- c()
  
  ts_names <- colnames(dataset)
  
  result <- mclapply(1:length(ts_names), svt_model, dataset, train_size, window_size, update_freq, prob_cut_off, cpu_required, granularity, schedule_policy, mc.cores=cores)
  
  for (ts_num in 1:length(ts_names)) {
    scheduled_num <- c(scheduled_num, result[[ts_num]]$scheduled_num)
    unscheduled_num <- c(unscheduled_num, result[[ts_num]]$unscheduled_num)
    correct_scheduled_num <- c(correct_scheduled_num, result[[ts_num]]$correct_scheduled_num)
    correct_unscheduled_num <- c(correct_unscheduled_num, result[[ts_num]]$correct_unscheduled_num)
    
    util_numerator <- c(util_numerator, result[[ts_num]]$util_numerator)
    util_denominator1 <- c(util_denominator1, result[[ts_num]]$util_denominator1)
    util_denominator2 <- c(util_denominator2, result[[ts_num]]$util_denominator2)
    sur_numerator <- c(sur_numerator, result[[ts_num]]$sur_numerator)
    sur_denominator <- c(sur_denominator, result[[ts_num]]$sur_denominator)
  }
  
  scheduled_num <- data.frame("scheduled_num"=scheduled_num)
  rownames(scheduled_num) <- ts_names
  unscheduled_num <- data.frame("unscheduled_num"=unscheduled_num)
  rownames(unscheduled_num) <- ts_names
  correct_scheduled_num <- data.frame("correct_scheduled_num"=correct_scheduled_num)
  rownames(correct_scheduled_num) <- ts_names
  correct_unscheduled_num <- data.frame("correct_unscheduled_num"=correct_unscheduled_num)
  rownames(correct_unscheduled_num) <- ts_names
  
  avg_usage <- data.frame("avg_usage1"=util_numerator/util_denominator1, "avg_usage2"=util_numerator/util_denominator2)
  rownames(avg_usage) <- ts_names
  job_survival <- data.frame("survival"=sur_numerator/sur_denominator)
  rownames(job_survival) <- ts_names
  result <- list('avg_usage'=avg_usage, 'job_survival'=job_survival, 'scheduled_num'=scheduled_num, "unscheduled_num"=unscheduled_num, "correct_scheduled_num"=correct_scheduled_num, "correct_unscheduled_num"=correct_unscheduled_num)
  return(result)  
}


wrapper.epoche <- function(parameter, dataset, cpu_required, output_dp, schedule_policy, write_result, write_result_path=NULL) {
  
  window_size <- as.numeric(parameter["window_size"])
  prob_cut_off <- as.numeric(parameter["prob_cut_off"])
  granularity <- as.numeric(parameter["granularity"])
  train_size <- as.numeric(parameter["train_size"])
  update_freq <- as.numeric(parameter["update_freq"])

  print(paste("Job len:", window_size))
  print(paste("Cut off prob:", prob_cut_off))
  print(paste("Granularity:", granularity))
  print(paste("Train Size:", train_size))
  print(paste("Update Freq:", update_freq))
  
  output <- svt_stationary_model(dataset, train_size, window_size, update_freq, prob_cut_off, cpu_required, granularity, schedule_policy)
  overall_evaluation <- find_overall_evaluation(output$avg_usage[,1], output$avg_usage[,2], output$job_survival[,1])
  
  utilization_rate1 <- overall_evaluation$utilization_rate1
  utilization_rate2 <- overall_evaluation$utilization_rate2
  survival_rate <- overall_evaluation$survival_rate
  
  scheduled_num <- sum(output$scheduled_num[,1])
  unscheduled_num <- sum(output$unscheduled_num[,1])
  correct_scheduled_num <- sum(output$correct_scheduled_num[,1])
  correct_unscheduled_num <- sum(output$correct_unscheduled_num[,1])
  
  correct_scheduled_rate <- correct_scheduled_num / scheduled_num
  correct_unscheduled_rate <- correct_unscheduled_num / unscheduled_num
  
  print(paste("Avg cycle used mode 1:", "job length", window_size, utilization_rate1))
  print(paste("Avg cycle used mode 2:", "job length", window_size, utilization_rate2))
  print(paste("Job survival rate:", "job length", window_size, survival_rate))
  print(paste("Scheduling summary:", "Correct scheduled rate:", correct_scheduled_rate, "Correct unscheduled rate:", correct_unscheduled_rate))
  
  if (write_result == TRUE) {
    ts_results <- data.frame()
    ts_results$utilization_rate1 <- output$avg_usage[,1]
    ts_results$utilization_rate2 <- output$avg_usage[,2]
    ts_results$survival_rate <- output$job_survival[,1]
    ts_results$correct_scheduled_rate <- output$correct_scheduled_num[,1] / output$scheduled_num[,1]
    ts_results$correct_unscheduled_rate <- output$correct_unscheduled_num[,1] / output$unscheduled_num[,1]
    rownames(ts_results) <- rownames(dataset)
    result_file_name <- paste("AR1", schedule_policy, 0, prob_cut_off, granularity, window_size, nrow(dataset), 0, train_size, update_freq)
    write.csv(ts_results, file = paste0(write_result_path, result_file_name), row.names = TRUE)
  }
  
  result_path.csv <- read.csv(output_dp)
  result_path.csv <- update.df.online(result_path.csv, "AR1", prob_cut_off, 0, sample_size, window_size, granularity, 0, train_size, update_freq, utilization_rate1, utilization_rate2, survival_rate, correct_scheduled_rate, correct_unscheduled_rate)
  write.csv(result_path.csv, file = output_dp, row.names = FALSE)
}

## Read background job pool

sample_size <- 100
cpu_usage <- 3
total_trace_length <- 8000

window_sizes <- c(12, 36)
prob_cut_offs <- c(0.01, 0.1)
granularity <- c(100/32, 0)

train_size <- c(2000, 4000)

schedule_policy <- "dynamic"

write_result <- TRUE

write_result_path <- NULL
if (Sys.info()["sysname"] == "Windows") {
  write_result_path <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//online results//ts_results//"
} else {
  write_result_path <- "/Users/carlonlv/Documents/Github/Research-Projects/ForegroundJobScheduler/results/online results/ts_results/"
}

bg_jobs_path <- NULL
if (Sys.info()["sysname"] == "Windows") {
  bg_jobs_path <- "C://Users//carlo//Documents//sample background jobs//"
} else {
  bg_jobs_path <- "/Users/carlonlv/Documents/microsoft traces/"
}

bg_job_pool <- NULL
if (sample_size == 100 ) {
  if (Sys.info()["sysname"] == "Windows") {
    bg_job_pool <- read.csv("C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//pythonscripts//list of sampled 100 background jobs.csv")[,1]
  } else {
    bg_job_pool <- read.csv("/Users/carlonlv/Documents/GitHub/Research-Projects/ForegroundJobScheduler/pythonscripts/list of sampled 100 background jobs.csv")[,1]
  }
  bg_job_pool <- sub(".pd", "", bg_job_pool)
} else {
  if (Sys.info()["sysname"] == "Windows") {
    bg_job_pool <- read.csv("C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//pythonscripts//list of sampled background jobs.csv")[,1]
  } else {
    bg_job_pool <- read.csv("/Users/carlonlv/Documents/GitHub/Research-Projects/ForegroundJobScheduler/pythonscripts/list of sampled background jobs.csv")[,1]
  }
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
if (schedule_policy == "dynamic") {
  if (Sys.info()["sysname"] == "Windows") {
    output_dp <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//online results//summary dynamic (windows,granularity).csv"
  } else {
    output_dp <- "/Users/carlonlv/Documents/Github/Research-Projects/ForegroundJobScheduler/results/online results/summary dynamic (windows,granularity).csv"
  }
} else {
  if (Sys.info()["sysname"] == "Windows") {
    output_dp <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//online results//summary disjoint (windows,granularity).csv"
  } else {
    output_dp <- "/Users/carlonlv/Documents/Github/Research-Projects/ForegroundJobScheduler/results/online results/summary disjoint (windows,granularity).csv"
  }
}

parameter.df <- expand.grid(window_sizes, prob_cut_offs, granularity, train_size)
colnames(parameter.df) <- c("window_size", "prob_cut_off", "granularity", "train_size")
parameter.df$update_freq <- 3 * parameter.df$window_size
parameter.df <- parameter.df %>%
  arrange()
slt <- apply(parameter.df, 1, wrapper.epoche, data_matrix, (100-cpu_required), output_dp, schedule_policy, write_result, write_result_path)
