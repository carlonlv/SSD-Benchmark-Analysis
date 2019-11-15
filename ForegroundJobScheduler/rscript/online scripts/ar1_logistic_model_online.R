
if (Sys.info()["sysname"] == "Windows") {
  source("C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//rscript//helper_functions.R")
} else {
  source("/Users/carlonlv/Documents/Github/Research-Projects/ForegroundJobScheduler/rscript/helper_functions.R")
}

cores <- ifelse(Sys.info()["sysname"] == "Windows", 1, detectCores(all.tests = FALSE, logical = TRUE))


train_ar1_model <- function(train_dataset) {
  
  ts_model <- tryCatch({
    arima(x=train_dataset, order = c(1,0,0), include.mean = TRUE, method = "CSS-ML", optim.control = list(maxit=2000), optim.method="Nelder-Mead")
  }, warning = function(w) {
    arima(x=train_dataset, order = c(1,0,0), include.mean = TRUE, method = "CSS-ML", optim.control = list(maxit=2000), optim.method="BFGS")
  }, error = function(cond) {
    ts_model_relax <- tryCatch({
      arima(x=train_dataset, order = c(1,0,0), include.mean = TRUE, method = "ML", optim.control = list(maxit=2000), transform.pars = FALSE, optim.method="BFGS")
    }, error = function(cond) {
      arima(x=train_dataset, order = c(1,0,0), include.mean = TRUE, method = "CSS", optim.control = list(maxit=2000), transform.pars = TRUE, optim.method="CG")
    })
  })
  return(list("coeffs"=as.numeric(ts_model$coef[1]), "means"= as.numeric(ts_model$coef[2]), "vars"=ts_model$sigma2))
}


generate_expected_conditional_var <- function(expected_avgs, variance_model) {
  
  expected_var <- NULL
  if (is.numeric(variance_model)) {
    expected_var <- rep(variance_model^2, length(expected_avgs))
  } else {
    expected_var <- predict(variance_model, newdata=data.frame("bin"=expected_avgs), type = "response")^2
  }
  return(max(expected_var, 0))
}


parser_for_logistic_model <- function(train_set_max, train_set_avg, cpu_required) {
  
  df <- data.frame("avg"=train_set_avg, "max"=train_set_max)
  df$survived <- ifelse(df$max <= (100 - cpu_required), 1, 0)
  return(df)
}


train_logistic_model <- function(train_dataset_max, train_dataset_avg, cpu_required) {
  
  logistic_input <- parser_for_logistic_model(train_dataset_max, train_dataset_avg, cpu_required)
  suppressWarnings(log.lm <- glm(survived~avg, data = logistic_input, family = "binomial", control=glm.control(maxit=2000)))
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
  
  bin <- NULL
  if (avg == 0) {
    bin <- 1
  } else {
    bin <- ifelse(avg %% binsize == 0, avg %/% binsize - 1, ceiling(avg / binsize))
  }
  return(bin)
}


train_cond_var_model <- function(train_set_max, train_set_avg, bin_num, method) {
  
  new_parsed_dat <- data.frame(matrix(nrow=length(train_set_avg), ncol=3))
  binsize <- 100 / bin_num
  bin <- as.numeric(sapply(train_set_avg, find_bin_obs, binsize))
  bin <- bin * binsize
  for (i in 1:length(train_set_avg)) {
    new_parsed_dat[i,] = c(train_set_avg[i], train_set_max[i], bin[i])
  }
  
  colnames(new_parsed_dat) <- c('avg', 'max', 'bin')
  selected_bins <- new_parsed_dat %>%
    group_by(bin) %>%
    count()
  
  selected_bins <- subset(selected_bins, selected_bins$n >= 3)$bin
  selected_bins <- new_parsed_dat$bin
  new_parsed_dat <- new_parsed_dat %>%
    filter(bin %in% selected_bins) %>%
    group_by(bin) %>% 
    summarise(sd=sqrt(var(max))) %>%
    filter(!is.na(sd)) %>%
    filter(sd != 0)
  
  sd.lm <- NULL
  if (nrow(new_parsed_dat) >= 3) {
    if (method == "lm") {
      sd.lm <- lm(sd~bin+I(bin^2), data = new_parsed_dat)
    } else {
      sd.lm <- glm(sd~bin, data = new_parsed_dat, family = Gamma(link="log"))
    }
  } else if (nrow(new_parsed_dat) == 2) {
    if (method == "lm") {
      sd.lm <- lm(sd~bin, data = new_parsed_dat)
    } else {
      sd.lm <- glm(sd~bin, data = new_parsed_dat)
    }
  } else {
    sd.lm <- new_parsed_dat$sd
  }
  return(sd.lm)
}


scheduling_foreground <- function(test_dataset_max, test_dataset_avg, coeffs, means, vars, logistic_model, window_size, prob_cut_off, cpu_required, granularity, schedule_policy) {
  
  cpu_required <- ifelse(granularity>0, round_to_nearest(cpu_required, granularity, FALSE), cpu_required)
  
  scheduled_num <- 0
  unscheduled_num <- 0
  correct_scheduled_num <- 0
  correct_unscheduled_num <- 0
  
  last_time_schedule <- length(test_dataset_max) - window_size + 1
  
  update_policy = ifelse(schedule_policy == "disjoint", window_size, 1)
  current_end <- window_size + 1
  while (current_end <= last_time_schedule) {
    ## Schedule based on model predictions
    last_obs_avg <- convert_frequency_dataset(test_dataset_avg[(current_end-window_size):(current_end-1)], window_size, mode="avg")
    expected_avgs <- max(do_prediction(last_obs_avg, coeffs, means, vars)$mu, 0)
    prediction_prob <- 1 - predict(logistic_model, newdata = data.frame("avg"=expected_avgs), type = "response")
    
    prediction <- ifelse(prediction_prob <= prob_cut_off, 1, 0)
    scheduled_num <- ifelse(prediction == 1, scheduled_num + 1, scheduled_num)
    unscheduled_num <- ifelse(prediction == 1, unscheduled_num, unscheduled_num + 1)
    
    ## Evalute schedulings based on prediction
    start_time <- current_end
    end_time <- current_end + window_size - 1
    position_vec <- convert_frequency_dataset(test_dataset_max[start_time:end_time], window_size, mode = "max")
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


find_expected_max <- function(probability, variance, cpu_required, expected_avgs) {
  
  if (probability == 1) {
    return(100)
  } else if (probability == 0) {
    return(expected_avgs)
  } else {
    return(max((100 - cpu_required) - qnorm(p=(1-probability)) * sqrt(variance), expected_avgs))
  }
}


scheduling_model <- function(test_dataset_max, test_dataset_avg, coeffs, means, vars, logistic_model, cond_var_model, cond.var, window_size, prob_cut_off, cpu_required, granularity, schedule_policy) {
  
  run_switch <- FALSE
  
  last_time_schedule <- length(test_dataset_max) - window_size + 1
  
  current_end <- window_size + 1
  update_policy <- ifelse(schedule_policy == "disjoint", window_size, 1)
  
  pi_ups <- c()
  
  utilization <- c()
  survival <- c()
  while (current_end <= last_time_schedule) {
    ## Schedule based on model predictions
    last_obs_avg <- convert_frequency_dataset(test_dataset_avg[(current_end-window_size):(current_end-1)], window_size, mode = 'avg')
    expected_avgs <- max(do_prediction(last_obs_avg, coeffs, means, vars)$mu, 0)
    expected_vars <- generate_expected_conditional_var(expected_avgs, cond_var_model)
    prob <- 1 - predict(logistic_model, newdata = data.frame("avg"=expected_avgs), type = "response")
    expected_max <- find_expected_max(prob, expected_vars, cpu_required, expected_avgs)
    
    pi_up <- compute_pi_up(mu=expected_max, varcov=as.matrix(expected_vars), predict_size=1, prob_cutoff=prob_cut_off, granularity=granularity)
    pi_ups <- c(pi_ups, pi_up)
    
    ## Evalute schedulings based on prediction
    start_time <- current_end
    end_time <- current_end + window_size - 1
    
    utilization <- c(utilization, check_utilization(pi_up, granularity))
    survival <- c(survival, check_survival(pi_up, test_dataset_max[start_time:end_time], granularity))
    
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
  overall_utilization <- compute_utilization(pi_ups, survival, test_dataset_max[(window_size+1):(current_end-update_policy+window_size-1)], window_size, granularity, schedule_policy)
  return(list("util_numerator"=overall_utilization$numerator, "util_denominator1"=overall_utilization$denominator1, "util_denominator2"=overall_utilization$denominator2, "sur_numerator"=overall_survival$numerator, "sur_denominator"=overall_survival$denominator))
} 


svt_model <- function(ts_num, dataset_max, dataset_avg, train_size, window_size, update_freq, prob_cut_off, cpu_required, granularity, schedule_policy="disjoint", bin_num, cond.var) {
  
  dataset_max <- dataset_max[,ts_num]
  dataset_avg <- dataset_avg[,ts_num]
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
  last_time_update <- length(dataset_max) - update_freq - train_size + 1
  while (current <= last_time_update) {
    ## Split into train set and test set
    train_set_max <- dataset_max[current:(current+train_size-1)]
    train_set_avg <- dataset_avg[current:(current+train_size-1)]
    test_set_max <- dataset_max[(current+train_size+1):(current+train_size+update_freq)]
    test_set_avg <- dataset_avg[(current+train_size+1):(current+train_size+update_freq)]
    
    ## Convert Frequency for training set
    new_trainset_max <- convert_frequency_dataset(train_set_max, window_size, "max")
    new_trainset_avg <- convert_frequency_dataset(train_set_avg, window_size, "avg")
    
    ## Train Model
    trained_ar1 <- train_ar1_model(new_trainset_avg)
    logistic_model <- train_logistic_model(new_trainset_max, new_trainset_avg, cpu_required)
    cond_var_model <- train_cond_var_model(new_trainset_max, new_trainset_avg, bin_num, cond.var)

    ## Test Model
    result_foreground <- scheduling_foreground(test_set_max, test_set_avg, trained_ar1$coeffs, trained_ar1$means, trained_ar1$vars, logistic_model, window_size, prob_cut_off, cpu_required, granularity, schedule_policy)
    result_model <- scheduling_model(test_set_max, test_set_avg, trained_ar1$coeffs, trained_ar1$means, trained_ar1$vars, logistic_model, cond_var_model, cond.var, window_size, prob_cut_off, cpu_required, granularity, schedule_policy)
    
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


svt_stationary_model <- function(dataset_max, dataset_avg, train_size, window_size, update_freq, prob_cut_off, cpu_required, granularity, schedule_policy="disjoint", bin_num, cond.var) {
  
  scheduled_num <- c()
  unscheduled_num <- c()
  correct_scheduled_num <- c()
  correct_unscheduled_num <- c()
  
  util_numerator <- c()
  util_denominator1 <- c()
  util_denominator2 <- c()
  sur_numerator <- c()
  sur_denominator <- c()
  
  ts_names <- colnames(dataset_max)
  
  result <- mclapply(1:length(ts_names), svt_model, dataset_max, dataset_avg, train_size, window_size, update_freq, prob_cut_off, cpu_required, granularity, schedule_policy, bin_num, cond.var, mc.cores=cores)
  
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


wrapper.epoche <- function(parameter, dataset_max, dataset_avg, cpu_required, output_dp, schedule_policy, cond.var) {
  
  window_size <- as.numeric(parameter["window_size"])
  prob_cut_off <- as.numeric(parameter["prob_cut_off"])
  granularity <- as.numeric(parameter["granularity"])
  train_size <- as.numeric(parameter["train_size"])
  update_freq <- as.numeric(parameter["update_freq"])
  bin_num <- as.numeric(parameter["num_of_bins"])
  
  print(paste("Job len:", window_size))
  print(paste("Cut off prob:", prob_cut_off))
  print(paste("Granularity:", granularity))
  print(paste("Train Size:", train_size))
  print(paste("Update Freq:", update_freq))
  print(paste("Bin Num:", bin_num))
  
  output <- svt_stationary_model(dataset_max, dataset_avg, train_size, window_size, update_freq, prob_cut_off, cpu_required, granularity, schedule_policy, bin_num, cond.var)
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
  
  result_path.csv <- read.csv(output_dp)
  if (cond.var == "lm") {
    
    result_path.csv <- update.df.online(result_path.csv, "AR1_logistic_lm", prob_cut_off, 0, sample_size, window_size, granularity, bin_num, train_size, update_freq, utilization_rate1, utilization_rate2, survival_rate, correct_scheduled_rate, correct_unscheduled_rate)
    write.csv(result_path.csv, file = output_dp, row.names = FALSE)
  } else {
    
    result_path.csv <- update.df.online(result_path.csv, "AR1_logistic_glm", prob_cut_off, 0, sample_size, window_size, granularity, bin_num, train_size, update_freq, utilization_rate1, utilization_rate2, survival_rate, correct_scheduled_rate, correct_unscheduled_rate)
    write.csv(result_path.csv, file = output_dp, row.names = FALSE)
  }
}

## Read background job pool

sample_size <- 100
cpu_usage <- 3
total_trace_length <- 8000

window_sizes <- c(12, 36)
prob_cut_offs <- c(0.01, 0.1)
granularity <- c(100/32, 0)

train_size <- c(2000, 4000)

cond.var <- "lm"
num_of_bins <- c(1000, 500)

schedule_policy <- "dynamic"

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

parameter.df <- expand.grid(window_sizes, prob_cut_offs, granularity, train_size, num_of_bins)
colnames(parameter.df) <- c("window_size", "prob_cut_off", "granularity", "train_size", "num_of_bins")
parameter.df$update_freq <- 3 * parameter.df$window_size
parameter.df <- parameter.df %>%
  arrange()
slt <- apply(parameter.df, 1, wrapper.epoche, data_matrix_avg, data_matrix_max, (100-cpu_required), output_dp, schedule_policy, cond.var)
