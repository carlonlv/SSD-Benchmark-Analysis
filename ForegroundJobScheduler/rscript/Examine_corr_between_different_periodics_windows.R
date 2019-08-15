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

data_path <- "C://Users//carlo//Documents//sample background jobs"
bg_job_file_lst <- list.files(path = data_path, full.names = FALSE, recursive = FALSE)
frequency_lst <- c(1, 2, 3, 4, 6, 12, 72, 144, 288)

#########################################################Previous Maxes########################################################################################
correlation_max_max_lst <- numvecdict()
correlation_avg_max_lst <- numvecdict()

current_percent <- -1
counter <- 0
for (i in 1:length(frequency_lst)) {
  
  window_size <- frequency_lst[i]
  name_lst <- c()
  for (file_name in bg_job_file_lst) {
    counter <- counter + 1
    if (round(counter / (length(bg_job_file_lst) * length(frequency_lst)), digits = 2) != current_percent) {
      current_percent <- round(counter / (length(bg_job_file_lst) * length(frequency_lst)), digits = 2)
      print(current_percent)
    }
    vmjob <- read.csv(paste(data_path, "//",file_name, sep = ""))[4033:8032,]
    
    converted_max <- convert_frequency_dataset(dataset = vmjob$max_cpu, new_freq = window_size, mode = 'max', overlap = TRUE)
    converted_avg <- convert_frequency_dataset(dataset = vmjob$avg_cpu, new_freq = window_size, mode = 'avg', overlap = TRUE)
    
    ## Larger window previous obs
    for (j in 1:3) {
      result <- construct_previous_obs_lst(dataset_5m = vmjob$max_cpu, converted_frequency = window_size, frequency = window_size * j, mode = 'max', overlap = TRUE)
      hashed_name <- paste(window_size, " ", "*" , j, sep = "")
      name_lst <- c(name_lst, hashed_name)
      correlation_max_max_lst$append_number(hashed_name, cor(converted_max[result$from:length(converted_max)], result$data, method = "pearson"))
      correlation_avg_max_lst$append_number(hashed_name, cor(converted_avg[result$from:length(converted_avg)], result$data, method = "pearson"))
    }
    for (k in 2:288) {
      if (window_size %% k == 0) {
        result <- construct_previous_obs_lst(dataset_5m = vmjob$max_cpu, converted_frequency = window_size, frequency = window_size %/% k, mode = 'max', overlap = TRUE)
        hashed_name <- paste(window_size, " ", "/", k, sep = "")
        name_lst <- c(name_lst, hashed_name)
        correlation_max_max_lst$append_number(hashed_name, cor(converted_max[result$from:length(converted_max)], result$data, method = "pearson"))
        correlation_avg_max_lst$append_number(hashed_name, cor(converted_avg[result$from:length(converted_avg)], result$data, method = "pearson"))
      }
    }
  }
  max_max_cor <- generate_dataframe_for_plotting(unique(name_lst), correlation_max_max_lst)
  colnames(max_max_cor)[1] <- 'corr'
  title <- paste("ECDF of Pearson correlation between Max of window size ", window_size * 5, "min and Max of previous window", sep = "")
  max_max_plt <- ggplot(subset(max_max_cor, !is.na(corr)), aes(corr, colour = frequency)) + 
    stat_ecdf() + 
    ylab("Fraction of Data") + 
    ggtitle(title) + 
    theme(plot.title = element_text(size=10))
  ggsave(filename = paste(title, ".png", sep = ""), plot = max_max_plt)
  
  avg_max_cor <- generate_dataframe_for_plotting(unique(name_lst), correlation_avg_max_lst)
  colnames(avg_max_cor)[1] <- 'corr'
  title <- paste("ECDF of Pearson correlation between Avg of window size ", window_size * 5, "min and Max of previous window", sep = "")
  avg_max_plt <- ggplot(subset(avg_max_cor, !is.na(corr)), aes(corr, colour = frequency)) + 
    stat_ecdf() + 
    ylab("Fraction of Data") + 
    ggtitle(title) + 
    theme(plot.title = element_text(size=10))
  ggsave(filename = paste(title, ".png", sep = ""), plot = avg_max_plt)
}

#########################################################Previous Avgs#########################################################################################
correlation_max_avg_lst <- numvecdict()
correlation_avg_avg_lst <- numvecdict()

current_percent <- -1
counter <- 0
for (i in 1:length(frequency_lst)) {
  
  window_size <- frequency_lst[i]
  name_lst <- c()
  for (file_name in bg_job_file_lst) {
    counter <- counter + 1
    if (round(counter / (length(bg_job_file_lst) * length(frequency_lst)), digits = 2) != current_percent) {
      current_percent <- round(counter / (length(bg_job_file_lst) * length(frequency_lst)), digits = 2)
      print(current_percent)
    }
    vmjob <- read.csv(paste(data_path, "//",file_name, sep = ""))[4033:8032,]
    
    converted_max <- convert_frequency_dataset(dataset = vmjob$max_cpu, new_freq = window_size, mode = 'max', overlap = TRUE)
    converted_avg <- convert_frequency_dataset(dataset = vmjob$avg_cpu, new_freq = window_size, mode = 'avg', overlap = TRUE)
    
    ## Larger window previous obs
    for (j in 1:3) {
      result <- construct_previous_obs_lst(dataset_5m = vmjob$avg_cpu, converted_frequency = window_size, frequency = window_size * j, mode = 'avg', overlap = TRUE)
      hashed_name <- paste(window_size, " ", "*" , j, sep = "")
      name_lst <- c(name_lst, hashed_name)
      correlation_max_avg_lst$append_number(hashed_name, cor(converted_max[result$from:length(converted_max)], result$data, method = "pearson"))
      correlation_avg_avg_lst$append_number(hashed_name, cor(converted_avg[result$from:length(converted_avg)], result$data, method = "pearson"))
    }
    for (k in 2:288) {
      if (window_size %% k == 0) {
        result <- construct_previous_obs_lst(dataset_5m = vmjob$avg_cpu, converted_frequency = window_size, frequency = window_size %/% k, mode = 'avg', overlap = TRUE)
        hashed_name <- paste(window_size, " ", "/", k, sep = "")
        name_lst <- c(name_lst, hashed_name)
        correlation_max_avg_lst$append_number(hashed_name, cor(converted_max[result$from:length(converted_max)], result$data, method = "pearson"))
        correlation_avg_avg_lst$append_number(hashed_name, cor(converted_avg[result$from:length(converted_avg)], result$data, method = "pearson"))
      }
    }
  }
  max_avg_cor <- generate_dataframe_for_plotting(unique(name_lst), correlation_max_avg_lst)
  colnames(max_avg_cor)[1] <- 'corr'
  title <- paste("ECDF of Pearson correlation between Max of window size ", window_size * 5, "min and Avg of previous window", sep = "")
  max_max_plt <- ggplot(subset(max_avg_cor, !is.na(corr)), aes(corr, colour = frequency)) + 
    stat_ecdf() + 
    ylab("Fraction of Data") + 
    ggtitle(title) + 
    theme(plot.title = element_text(size=10))
  ggsave(filename = paste(title, ".png", sep = ""), plot = max_max_plt)
  
  avg_avg_cor <- generate_dataframe_for_plotting(unique(name_lst), correlation_avg_avg_lst)
  colnames(avg_avg_cor)[1] <- 'corr'
  title <- paste("ECDF of Pearson correlation between Avg of window size ", window_size * 5, "min and Avg of previous window", sep = "")
  avg_max_plt <- ggplot(subset(avg_avg_cor, !is.na(corr)), aes(corr, colour = frequency)) + 
    stat_ecdf() + 
    ylab("Fraction of Data") + 
    ggtitle(title) + 
    theme(plot.title = element_text(size=10))
  ggsave(filename = paste(title, ".png", sep = ""), plot = avg_max_plt)
}

#########################################################Previous Avgs of Maxes#################################################################################
correlation_max_aom_lst <- numvecdict()
correlation_avg_aom_lst <- numvecdict()

current_percent <- -1
counter <- 0
for (i in 1:length(frequency_lst)){
  
  window_size <- frequency_lst[i]
  name_lst <- c()
  for (file_name in bg_job_file_lst) {
    counter <- counter + 1
    if (round(counter / (length(bg_job_file_lst) * length(frequency_lst)), digits = 2) != current_percent) {
      current_percent <- round(counter / (length(bg_job_file_lst) * length(frequency_lst)), digits = 2)
      print(current_percent)
    }
    vmjob <- read.csv(paste(data_path, "//",file_name, sep = ""))[4033:8032,]
    
    converted_max <- convert_frequency_dataset(dataset = vmjob$max_cpu, new_freq = window_size, mode = 'max', overlap = TRUE)
    converted_avg <- convert_frequency_dataset(dataset = vmjob$avg_cpu, new_freq = window_size, mode = 'avg', overlap = TRUE)
    
    ## Larger window previous obs
    for (j in 1:3) {
      result <- construct_previous_obs_lst(dataset_5m = vmjob$max_cpu, converted_frequency = window_size, frequency = window_size * j, mode = 'avg', overlap = TRUE)
      hashed_name <- paste(window_size, " ", "*" , j, sep = "")
      name_lst <- c(name_lst, hashed_name)
      correlation_max_aom_lst$append_number(hashed_name, cor(converted_max[result$from:length(converted_max)], result$data, method = "pearson"))
      correlation_avg_aom_lst$append_number(hashed_name, cor(converted_avg[result$from:length(converted_avg)], result$data, method = "pearson"))
    }
    for (k in 2:288) {
      if (window_size %% k == 0) {
        result <- construct_previous_obs_lst(dataset_5m = vmjob$max_cpu, converted_frequency = window_size, frequency = window_size %/% k, mode = 'avg', overlap = TRUE)
        hashed_name <- paste(window_size, " ", "/", k, sep = "")
        name_lst <- c(name_lst, hashed_name)
        correlation_max_aom_lst$append_number(hashed_name, cor(converted_max[result$from:length(converted_max)], result$data, method = "pearson"))
        correlation_avg_aom_lst$append_number(hashed_name, cor(converted_avg[result$from:length(converted_avg)], result$data, method = "pearson"))
      }
    }
  }
  max_aom_cor <- generate_dataframe_for_plotting(unique(name_lst), correlation_max_aom_lst)
  colnames(max_aom_cor)[1] <- 'corr'
  title <- paste("ECDF of Pearson correlation between Max of window size ", window_size * 5, "min and Aom of previous window", sep = "")
  max_max_plt <- ggplot(subset(max_aom_cor, !is.na(corr)), aes(corr, colour = frequency)) + 
    stat_ecdf() + 
    ylab("Fraction of Data") + 
    ggtitle(title) + 
    theme(plot.title = element_text(size=10))
  ggsave(filename = paste(title, ".png", sep = ""), plot = max_max_plt)
  
  avg_aom_cor <- generate_dataframe_for_plotting(unique(name_lst), correlation_avg_aom_lst)
  colnames(avg_aom_cor)[1] <- 'corr'
  title <- paste("ECDF of Pearson correlation between Avg of window size ", window_size * 5, "min and Aom of previous window", sep = "")
  avg_max_plt <- ggplot(subset(avg_aom_cor, !is.na(corr)), aes(corr, colour = frequency)) + 
    stat_ecdf() + 
    ylab("Fraction of Data") + 
    ggtitle(title) + 
    theme(plot.title = element_text(size=10))
  ggsave(filename = paste(title, ".png", sep = ""), plot = avg_max_plt)
}