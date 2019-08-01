library("ggplot2")
library("dict")
library("forecast")

generate_dataframe_for_plotting <- function(lst_entries, dict_lst) {
  for (freq in lst_entries) {
    length_vec <- c(length_vec, length(dict_lst[[freq]]))
    data_vec <- c(data_vec, dict_lst[[freq]])
  }
  new_factor <- factor(rep(lst_entries, length_vec), levels = lst_entries)
  new_dat <- data.frame(corr = data_vec, frequency = new_factor)
  return(new_dat)
}

construct_previous_obs_lst <- function(dataset_5m, converted_frequency, frequency, mode, overlap){
  previous_obs <- c()
  if (overlap == TRUE) {
    for (i in frequency:(length(dataset_5m) - converted_frequency)) {
      if (mode == 'max') {
        previous_obs <- c(previous_obs, max(dataset_5m[(i-frequency+1):(i)]))
      } else {
        previous_obs <- c(previous_obs, mean(dataset_5m[(i-frequency+1):(i)]))
      }
    }
  } else {
    from <- ceiling(frequency / converted_frequency) * converted_frequency + 1
    for (i in seq(from, length(dataset_5m), converted_frequency)) {
      if (mode == 'max') {
        previous_obs <- c(previous_obs, max(dataset_5m[(i-frequency):(i-1)]))
      } else {
        previous_obs <- c(previous_obs, max(dataset_5m[(i-frequency):(i-1)]))
      }    }
  }
  return(previous_obs)
}

convert_frequency_dataset <- function(dataset, new_freq, mode, overlap) {
  new_dataset <- c()
  if (overlap == TRUE) {
    for (i in 1:(length(dataset) - new_freq + 1)) {
      from <- i
      to <- i + new_freq - 1
      new_val <- NULL
      if (mode == 'max') {
        new_val <- max(dataset[from:to], na.rm = TRUE)
      } else {
        new_val <- mean(dataset[from:to], na.rm = TRUE)
      }
      new_dataset <- c(new_dataset, new_val)
    }
  } else {
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
      new_dataset <- c(new_dataset, new_val)
    }
  }
  return(new_dataset)
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
  previous_obs <- construct_previous_obs_lst(vmjob$max_cpu, NULL, frequency = 2, mode = 'aom')
  correlation_max_aom_lst$append_number('10m', cor(vmjob$max_cpu[3:length(vmjob$max_cpu)], previous_obs, method = "spearman"))
  previous_obs <- construct_previous_obs_lst(vmjob$max_cpu, NULL, frequency = 3, mode = 'aom')
  correlation_max_aom_lst$append_number('15m', cor(vmjob$max_cpu[4:length(vmjob$max_cpu)], previous_obs, method = "spearman"))
  previous_obs <- construct_previous_obs_lst(vmjob$max_cpu, NULL, frequency = 6, mode = 'aom')
  correlation_max_aom_lst$append_number('30m', cor(vmjob$max_cpu[7:length(vmjob$max_cpu)], previous_obs, method = "spearman")) 
  previous_obs <- construct_previous_obs_lst(vmjob$max_cpu, NULL, frequency = 9, mode = 'aom')
  correlation_max_aom_lst$append_number('45m', cor(vmjob$max_cpu[10:length(vmjob$max_cpu)], previous_obs, method = "spearman"))
}

length_vec <- c(length(correlation_max_aom_lst[['5m']]), length(correlation_max_aom_lst[['10m']]),length(correlation_max_aom_lst[['15m']]), length(correlation_max_aom_lst[['30m']]), length(correlation_max_aom_lst[['45m']]))
data_vec <- c(correlation_max_aom_lst[['5m']], correlation_max_aom_lst[['10m']],correlation_max_aom_lst[['15m']], correlation_max_aom_lst[['30m']], correlation_max_aom_lst[['45m']])
freq_factor <- factor(rep(c('5m', '10m', '15m', '30m', '45m'), length_vec), levels = c('5m', '15m', '30m', '45m'))
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
