library("ggplot2")
library("dict")

generate_dataframe_for_plotting <- function(lst_entries, dict_lst) {
  length_vec <- NULL
  data_vec <- NULL
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
  from <- ceiling(frequency / converted_frequency) * converted_frequency + 1
  result <- NULL
  if (overlap == TRUE) {
    for (i in from:(length(dataset_5m) - converted_frequency + 1)) {
      if (mode == 'max') {
        previous_obs <- c(previous_obs, max(dataset_5m[(i-frequency):(i-1)]))
      } else {
        previous_obs <- c(previous_obs, mean(dataset_5m[(i-frequency):(i-1)]))
      }
    }
    result <- list('data' = previous_obs, 'from' = from)
  } else {
    for (i in seq(from, length(dataset_5m), converted_frequency)) {
      if (mode == 'max') {
        previous_obs <- c(previous_obs, max(dataset_5m[(i-frequency):(i-1)]))
      } else {
        previous_obs <- c(previous_obs, max(dataset_5m[(i-frequency):(i-1)]))
      }
    }
    result <- list('data' = previous_obs, 'from' = ceiling(frequency / converted_frequency) + 1)
  }
  return(result)
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
    for (i in seq(1, length(dataset), new_freq)[-length(seq(1, length(dataset), new_freq))]) {
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
  }
  return(new_dataset)
}

###############################################################################################################################################################

data_path <- "C://Users//carlo//Documents//datasets//csvalldata//sample background jobs"
bg_job_file_lst <- list.files(path = data_path, full.names = FALSE, recursive = FALSE)
frequency_lst <- c(1, 2, 3, 4, 6, 12)

#########################################################Previous Maxes########################################################################################
correlation_max_max_lst <- numvecdict()
correlation_avg_max_lst <- numvecdict()
name_lst <- c()

current_percent <- -1
counter <- 0
for (file_name in bg_job_file_lst) {
  counter <- counter + 1
  if (round(counter / length(bg_job_file_lst), digits = 2) != current_percent) {
    current_percent <- round(counter / length(bg_job_file_lst), digits = 2)
    print(current_percent)
  }
  vmjob <- read.csv(paste(data_path, "//",file_name, sep = ""))[4033:8032,]

  for (i in 1:length(frequency_lst)) {
    window_size <- frequency_lst[i]
    converted_max <- convert_frequency_dataset(dataset = vmjob$max_cpu, new_freq = window_size, mode = 'max', overlap = TRUE)
    converted_avg <- convert_frequency_dataset(dataset = vmjob$avg_cpu, new_freq = window_size, mode = 'avg', overlap = TRUE)
    
    ## Larger window previous obs
    for (j in 1:3) {
      result <- construct_previous_obs_lst(dataset_5m = vmjob$max_cpu, converted_frequency = window_size, frequency = window_size * j, mode = 'max', overlap = TRUE)
      hashed_name <- paste(window_size, ",", "*" , j, sep = "")
      name_lst <- c(name_lst, hashed_name)
      correlation_max_max_lst$append_number(hashed_name, cor(converted_max[result$from:length(converted_max)], result$data, method = "pearson"))
      correlation_avg_max_lst$append_number(hashed_name, cor(converted_avg[result$from:length(converted_avg)], result$data, method = "pearson"))
    }
    for (k in 2:4) {
      if (window_size %% k == 0) {
        result <- construct_previous_obs_lst(dataset_5m = vmjob$max_cpu, converted_frequency = window_size, frequency = window_size %/% k, mode = 'max', overlap = TRUE)
        hashed_name <- paste(window_size, ",", "/", j, sep = "")
        name_lst <- hashed_name
        correlation_max_max_lst$append_number(hashed_name, cor(converted_max[result$from:length(converted_max)], result$data, method = "pearson"))
        correlation_avg_max_lst$append_number(hashed_name, cor(converted_avg[result$from:length(converted_avg)], result$data, method = "pearson"))
      }
    }
  }
  max_max_cor <- generate_dataframe_for_plotting(name_lst, correlation_max_max_lst)
  colnames(max_max_cor)[1] <- 'corr'
  title <- paste("ECDF of Pearson correlation between Max of window size ", window_size * 5, "min and Max at previous window", sep = "")
  ggplot(subset(max_max_cor, !is.na(corr)), aes(corr, colour = frequency)) + 
    stat_ecdf() + 
    ylab("Fraction of Data") + 
    ggtitle(title)
  ggsave(title)
  
  avg_max_cor <- generate_dataframe_for_plotting(name_lst, correlation_avg_max_lst)
  colnames(avg_max_cor)[1] <- 'corr'
  title <- paste("ECDF of Pearson correlation between Avg of window size ", window_size * 5, "min and Max at previous window", sep = "")
  ggplot(subset(avg_max_cor, !is.na(corr)), aes(corr, colour = frequency)) + 
    stat_ecdf() + 
    ylab("Fraction of Data") + 
    ggtitle(title)
  ggsave(title)
}

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