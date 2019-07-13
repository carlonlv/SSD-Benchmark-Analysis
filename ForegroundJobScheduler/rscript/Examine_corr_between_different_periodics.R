library("ggplot2")
library("ggfortify")
library("dict")
library("forecast")

generate_dataframe_for_plotting <- function(vec_5m, dict_lst, mode, nobs=NA, lag=NA, max_hour=3) {
  if (!is.null(vec_5m)) {
    length_vec <- length(vec_5m)
    data_vec <- vec_5m
  } else {
    length_vec <- NULL
    data_vec <- NULL
  }
  cutoff_hi <- NULL
  cutoff_lo <- NULL
  if (mode == 'pacf') {
    cutoff_hi <- 1.96 / sqrt(nobs)
    cutoff_lo <- -1.96 / sqrt(nobs)
  } else if (mode == 'acf') {
    cutoff_hi <- 1.96 / sqrt(nobs - lag)
    cutoff_lo <- -1.96 / sqrt(nobs - lag)
  }
  for (freq in 1:max_hour) {
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
  
  new_factor <- NULL
  if (!is.null(vec_5m)) {
    new_factor <- factor(rep(c('5m', 1:max_hour), length_vec), levels = c('5m', 1:max_hour))
  }  else {
    new_factor <- factor(rep(1:max_hour, length_vec), levels = 1:max_hour)
  }
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
    } else if (mode == 'avg') {
      previous_obs <- c(previous_obs, mean(avg_data_5m[(i-frequency):i-1]))
    } else {
      previous_obs <- c(previous_obs, mean(max_data_5m[(i-frequency):i-1]))
    }
  }
  return(previous_obs)
}

convert_frequency_dataset <- function(dataset, new_freq, mode) {
  new_max_cpu <- c()
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
    new_max_cpu <- c(new_max_cpu, new_val)
  }
  return(new_max_cpu)
}

data_path <- "C://Users//carlo//Documents//datasets//csvalldata//sample background jobs"
bg_job_file_lst <- list.files(path = data_path, full.names = FALSE, recursive = FALSE)
frequency_lst <- seq(1, 3, by=1) * 12

#########################################################Previous Maxes########################################################################################
correlation_max_max_lst <- numvecdict()

current_percent <- -1
counter <- 0
for (file_name in bg_job_file_lst) {
  counter <- counter + 1
  if (round(counter / length(bg_job_file_lst), digits = 2) != current_percent) {
    current_percent <- round(counter / length(bg_job_file_lst), digits = 2)
    print(current_percent)
  }
  vmjob <- read.csv(paste(data_path, "//",file_name, sep = ""))[4033:8032,]
  correlation_max_max_lst$append_number('5m', cor(vmjob$max_cpu[-1], vmjob$max_cpu[-length(vmjob$max_cpu)], method = "spearman"))
  for (i in 1:length(frequency_lst)) {
    previous_obs <- construct_previous_obs_lst(vmjob$max_cpu, frequency = frequency_lst[i], mode = 'max')
    correlation_max_max_lst$append_number(i, cor(vmjob$max_cpu[(frequency_lst[i]+1):length(vmjob$max_cpu)], previous_obs, method = "spearman"))
  }
}

new_dat <- generate_dataframe_for_plotting(correlation_max_max_lst[['5m']], correlation_max_max_lst, mode = 'NULL')
colnames(new_dat)[1] <- 'corr'
ggplot(subset(new_dat, !is.na(corr)), aes(corr, colour = frequency)) + 
  stat_ecdf() + 
  ylab("Fraction of Data") + 
  ggtitle("ECDF of Spearman correlation between Max at 5min and Max at previous window")
ggsave("ECDF of Spearman correlation between Max at 5min and Max at previous window.png")

#########################################################Previous Avgs#########################################################################################
correlation_max_avg_lst <- numvecdict()

current_percent <- -1
counter <- 0
for (file_name in bg_job_file_lst) {
  counter <- counter + 1
  if (round(counter / length(bg_job_file_lst), digits = 2) != current_percent) {
    current_percent <- round(counter / length(bg_job_file_lst), digits = 2)
    print(current_percent)
  }
  vmjob <- read.csv(paste(data_path, "//",file_name, sep = ""))[4033:8032,]
  correlation_max_avg_lst$append_number('5m', cor(vmjob$max_cpu[-1], vmjob$avg_cpu[-length(vmjob$avg_cpu)], method = "spearman"))
  for (i in 1:length(frequency_lst)) {
    previous_obs <- construct_previous_obs_lst(vmjob$max_cpu, vmjob$avg_cpu, frequency = frequency_lst[i], mode = 'avg')
    correlation_max_avg_lst$append_number(i, cor(vmjob$max_cpu[(frequency_lst[i]+1):length(vmjob$max_cpu)], previous_obs, method = "spearman"))
  }
}

new_dat <- generate_dataframe_for_plotting(correlation_max_avg_lst[['5m']], correlation_max_avg_lst, mode = 'NULL')
colnames(new_dat)[1] <- 'corr'
ggplot(subset(new_dat, !is.na(corr)), aes(corr, colour = frequency)) + 
  stat_ecdf() + 
  ylab("Fraction of Data") + 
  ggtitle("ECDF of Spearman correlation between Max at 5min and Avg at previous window")
ggsave("ECDF of Spearman correlation between Max at 5min and Avg at previous window.png")

#########################################################Previous Avgs of Maxes#################################################################################
correlation_max_aom_lst <- numvecdict()

current_percent <- -1
counter <- 0
for (file_name in bg_job_file_lst) {
  counter <- counter + 1
  if (round(counter / length(bg_job_file_lst), digits = 2) != current_percent) {
    current_percent <- round(counter / length(bg_job_file_lst), digits = 2)
    print(current_percent)
  }
  vmjob <- read.csv(paste(data_path, "//",file_name, sep = ""))[4033:8032,]
  correlation_max_aom_lst$append_number('5m', cor(vmjob$max_cpu[-1], vmjob$max_cpu[-length(vmjob$max_cpu)], method = "spearman"))
  for (i in 1:length(frequency_lst)) {
    previous_obs <- construct_previous_obs_lst(vmjob$max_cpu, NULL, frequency = frequency_lst[i], mode = 'aom')
    correlation_max_aom_lst$append_number(i, cor(vmjob$max_cpu[(frequency_lst[i]+1):length(vmjob$max_cpu)], previous_obs, method = "spearman"))
  }
  previous_obs <- construct_previous_obs_lst(vmjob$max_cpu, NULL, frequency = 3, mode = 'aom')
  correlation_max_aom_lst$append_number('15m', cor(vmjob$max_cpu[4:length(vmjob$max_cpu)], previous_obs, method = "spearman"))
  previous_obs <- construct_previous_obs_lst(vmjob$max_cpu, NULL, frequency = 6, mode = 'aom')
  correlation_max_aom_lst$append_number('30m', cor(vmjob$max_cpu[7:length(vmjob$max_cpu)], previous_obs, method = "spearman")) 
  previous_obs <- construct_previous_obs_lst(vmjob$max_cpu, NULL, frequency = 9, mode = 'aom')
  correlation_max_aom_lst$append_number('45m', cor(vmjob$max_cpu[10:length(vmjob$max_cpu)], previous_obs, method = "spearman"))
}

length_vec <- c(length(correlation_max_aom_lst[['5m']]), length(correlation_max_aom_lst[['15m']]), length(correlation_max_aom_lst[['30m']]), length(correlation_max_aom_lst[['45m']]))
data_vec <- c(correlation_max_aom_lst[['5m']], correlation_max_aom_lst[['15m']], correlation_max_aom_lst[['30m']], correlation_max_aom_lst[['45m']])
freq_factor <- factor(rep(c('5m', '15m', '30m', '45m'), length_vec), levels = c('5m', '15m', '30m', '45m'))
df_minutes <- data.frame(corr = data_vec, frequency = freq_factor)
new_dat <- generate_dataframe_for_plotting(NULL, correlation_max_aom_lst, mode = 'NULL')
colnames(new_dat)[1] <- 'corr'
new_dat <- rbind(df_minutes, new_dat)

ggplot(subset(new_dat, !is.na(corr)), aes(corr, colour = frequency)) + 
  stat_ecdf() + 
  ylab("Fraction of Data") + 
  ggtitle("ECDF of Spearman correlation between Max at 5min and Avg of Max at previous window")
ggsave("ECDF of Spearman correlation between Max at 5min and Avg of Max at previous window.png")


frequency_lst <- seq(1, 24, by=1) * 12
#########################################################Current Avgs###########################################################################################
correlation_max_current_avg_lst <- numvecdict()

current_percent <- -1
counter <- 0
for (file_name in bg_job_file_lst) {
  counter <- counter + 1
  if (round(counter / length(bg_job_file_lst), digits = 2) != current_percent) {
    current_percent <- round(counter / length(bg_job_file_lst), digits = 2)
    print(current_percent)
  }
  vmjob <- read.csv(paste(data_path, "//",file_name, sep = ""))[4033:8032,]
  correlation_max_current_avg_lst$append_number('5m', cor(vmjob$max_cpu, vmjob$avg_cpu, method = "spearman"))
  for (i in 1:length(frequency_lst)) {
    new_max <- convert_frequency_dataset(vmjob$max_cpu, frequency_lst[i], 'max')
    new_avg <- convert_frequency_dataset(vmjob$avg_cpu, frequency_lst[i], 'avg')
    correlation_max_current_avg_lst$append_number(i, cor(new_max, new_avg, method = "spearman"))
  }
}

new_dat <- generate_dataframe_for_plotting(correlation_max_current_avg_lst[['5m']], correlation_max_current_avg_lst, mode = 'NULL', max_hour = 24)
colnames(new_dat)[1] <- 'corr'
ggplot(subset(new_dat, !is.na(corr)), aes(corr, colour = frequency)) + 
  stat_ecdf() + 
  ylab("Fraction of Data") + 
  ggtitle("ECDF of Spearman correlation between Max and Avg at same window")
ggsave("ECDF of Spearman correlation between Max and Avg at same window.png")
