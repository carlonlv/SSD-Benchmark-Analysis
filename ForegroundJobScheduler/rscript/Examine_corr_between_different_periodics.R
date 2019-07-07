library("ggplot2")
library("ggfortify")
library("dict")
library("forecast")

generate_dataframe_for_plotting <- function(vec_5m, dict_lst, mode, nobs=NA, lag=NA) {
  length_vec <- length(vec_5m)
  data_vec <- vec_5m
  cutoff_hi <- NULL
  cutoff_lo <- NULL
  if (mode == 'pacf') {
    cutoff_hi <- 1.96 / sqrt(nobs)
    cutoff_lo <- -1.96 / sqrt(nobs)
  } else if (mode == 'acf') {
    cutoff_hi <- 1.96 / sqrt(nobs - lag)
    cutoff_lo <- -1.96 / sqrt(nobs - lag)
  }
  for (freq in 1:24) {
    length_vec <- c(length_vec, length(dict_lst[[freq]]))
    if (mode == 'pacf') {
      cutoff_hi <- c(cutoff_hi, 1.96 / sqrt(floor(nobs / (12 * freq))))
      cutoff_lo <- c(cutoff_lo, -1.96 / sqrt(floor(nobs / (12 * freq))))
    } else if (mode == 'acf'){
      cutoff_hi <- c(cutoff_hi, 1.96 / sqrt(floor(nobs / (12 * freq)) - lag))
      cutoff_lo <- c(cutoff_lo, -1.96 / sqrt(floor(nobs / (12 * freq)) - lag))
    }
    data_vec <- c(data_vec, dict_lst[[freq]])
  }
  
  new_factor <- factor(rep(c('5m', 1:24), length_vec), levels = c('5m', 1:24))
  if (mode == 'pacf' | mode == 'acf') {
    cutoff_hi <- as.numeric(rep(cutoff_hi, length_vec))
    cutoff_lo <- as.numeric(rep(cutoff_lo, length_vec))
    new_dat <- data.frame(corr = data_vec, frequency = new_factor, cutoff_hi = cutoff_hi, cutoff_lo = cutoff_lo)
  } else {
    new_dat <- data.frame(corr = data_vec, frequency = new_factor)
  }
  
  return(new_dat)
}

construct_previous_obs_lst <- function(max_data_5m, avg_data_5m=NULL, frequency, mode) {
  previous_obs <- NULL
  for (i in (frequency+1):length(max_data_5m)) {
    if (mode == 'max') {
      previous_obs <- c(previous_obs, max(max_data_5m[(i-frequency):i-1]))
    } else {
      previous_obs <- c(previous_obs, mean(avg_data_5m[(i-frequency):i-1]))
    }
  }
  return(previous_obs)
}

data_path <- "C://Users//carlo//Documents//datasets//csvalldata//sample background jobs"
bg_job_file_lst <- list.files(path = data_path, full.names = FALSE, recursive = FALSE)
frequency_lst <- seq(1, 3, by=1) * 12

#########################################################Previous Maxes########################################################################################
current_percent <- -1
counter <- 0
for (file_name in bg_job_file_lst) {
  counter <- counter + 1
  if (round(counter / length(bg_job_file_lst), digits = 2) != current_percent) {
    current_percent <- round(counter / length(bg_job_file_lst), digits = 2)
    print(current_percent)
  }
  vmjob <- read.csv(paste(data_path, "//",file_name, sep = ""))[4033:8032,]
  # 5min. lag in lag_pool
  ts_5min <- ts(vmjob$max_cpu, start = 0, frequency = 1)
  for (l in lag_pool) {
    result <- find_sig_corrs(ts_5min, lag = l, mode = 'pacf')
    max_all_data_5m$append_number(l, result$val)
  }
  
  ## Count max lags
  max_signif_count_lst$append_number('5m', find_sig_corrs(ts_5min, 1, mode = 'pacf', recursive = TRUE))
  
  # 1:24h. lag in lag_pool for 1 and 2 h
  for (i in 1:length(frequency_lst)) {
    new_max_cpu <- convert_frequency_dataset(vmjob$max_cpu, frequency_lst[i], mode = 'max')
    new_ts <- ts(new_max_cpu, start = 0, frequency = frequency_lst[i])
    
    if (i == 1 | i == 2) {
      for (l in lag_pool) {
        result <- find_sig_corrs(new_ts, lag = l, mode = 'pacf')
        if (i == 1) {
          
          max_all_data_1h$append_number(l, result$val)
        } else {
          
          max_all_data_2h$append_number(l, result$val)
        } 
      }
    } 
    
    result <- find_sig_corrs(new_ts, lag = 1, mode = 'pacf')
    max_all_data_lst$append_number(i, result$val)
    
    ## Count max lags
    max_signif_count_lst$append_number(i, find_sig_corrs(new_ts, 1, length(new_max_cpu), mode = 'pacf', recursive = TRUE))
  }
}


#########################################################Previous Avgs#########################################################################################
