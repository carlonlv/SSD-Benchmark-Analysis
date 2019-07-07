library("ggplot2")
library("ggfortify")
library("dict")
library("forecast")
library("EnvStats")
library("entropy")
data_path <- "C://Users//carlo//Documents//datasets//csvalldata//sample background jobs"
bg_job_file_lst <- list.files(path = data_path, full.names = FALSE, recursive = FALSE)


max_entropy_data_5m <- NULL
max_cv_data_5m <- NULL
max_variance_data_5m <- NULL
avg_entropy_data_5m <- NULL
avg_cv_data_5m <- NULL
avg_variance_data_5m <- NULL

frequency_lst <- seq(1, 24, by=1) * 12

#key: frequency 1:24, value: list of observations
max_entropy_data_lst <- numvecdict()
max_cv_data_lst <- numvecdict()
max_variance_data_lst <- numvecdict()
avg_entropy_data_lst <- numvecdict()
avg_cv_data_lst <- numvecdict()
avg_variance_data_lst <- numvecdict()

convert_frequency_dataset <- function(dataset, new_freq, trim=TRUE, mode) {
  new_max_cpu <- c()
  window_num <- NULL
  if (trim == TRUE) {
    window_num <- floor(length(dataset) / new_freq)
  } else {
    window_num <- ceiling(length(dataset) / new_freq)
  }
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

generate_dataframe_for_plotting <- function(vec_5m, dict_lst) {
  length_vec <- length(vec_5m)
  data_vec <- vec_5m
  for (freq in 1:24) {
    length_vec <- c(length_vec, length(dict_lst[[freq]]))
    data_vec <- c(data_vec, dict_lst[[freq]])
  }
  new_factor <- factor(rep(c('5m', 1:24), length_vec), levels = rep(c('5m', 1:24)))
  new_dat <- data.frame(corr = data_vec, frequency = new_factor)
  return(new_dat)
}

###############################################################################################################################################################

counter <- 0
current_percent <- -1
for (file_name in bg_job_file_lst) {
  counter <- counter + 1
  if (round(counter / length(bg_job_file_lst), digits = 2) != current_percent) {
    current_percent <- round(counter / length(bg_job_file_lst), digits = 2)
    print(current_percent)
  }
  vmjob <- read.csv(paste(data_path, "//", file_name, sep = ""))[4033:8032,]
  # 5min. 
  ## Entropy
  max_entropy_data_5m <- c(max_entropy_data_5m, entropy(discretize(vmjob$max_cpu, numBins = 20, r=c(0,100))))
  ## Coefficient of Variation
  max_cv_data_5m <- c(max_cv_data_5m, cv(vmjob$max_cpu, na.rm = TRUE))
  ## Variance
  max_variance_data_5m <- c(max_variance_data_5m, var(vmjob$max_cpu, na.rm = TRUE))
  ## Hurst Parameter
  
  # 1-24h
  for (i in 1:length(frequency_lst)) {
    new_max_cpu <- convert_frequency_dataset(vmjob$max_cpu, frequency_lst[i], mode = 'max')
    ## Entropy
    max_entropy_data_lst$append_number(i, entropy(discretize(new_max_cpu, numBins = 20, r=c(0,100))))
    ## Coefficient of Variation
    max_cv_data_lst$append_number(i, cv(new_max_cpu, na.rm = TRUE))
    ## Variance
    max_variance_data_lst$append_number(i, var(new_max_cpu, na.rm = TRUE))
  }
}

# Plot ECDF
entropy_new_dat <- generate_dataframe_for_plotting(max_entropy_data_5m, max_entropy_data_lst)
colnames(entropy_new_dat)[1] <- "entropy"
ggplot(entropy_new_dat, aes(entropy, color = frequency)) + stat_ecdf() + ylab("Fraction of Data") + ggtitle("ECDF of Entropy Max Value")

cv_new_dat <- generate_dataframe_for_plotting(max_cv_data_5m, max_cv_data_lst)
colnames(cv_new_dat)[1] <- "cv"
ggplot(cv_new_dat, aes(cv, color = frequency)) + stat_ecdf() + ylab("Fraction of Data") + ggtitle("ECDF of CV Max Value")

variance_new_dat <- generate_dataframe_for_plotting(max_variance_data_5m, max_variance_data_lst)
colnames(variance_new_dat)[1] <- "variance"
ggplot(variance_new_dat, aes(variance, color = frequency)) + stat_ecdf() + ylab("Fraction of Data") + ggtitle("ECDF of Variance Max Value")

###############################################################################################################################################################

counter <- 0
current_percent <- -1
for (file_name in bg_job_file_lst) {
  counter <- counter + 1
  if (round(counter / length(bg_job_file_lst), digits = 2) != current_percent) {
    current_percent <- round(counter / length(bg_job_file_lst), digits = 2)
    print(current_percent)
  }
  vmjob <- read.csv(paste(data_path, "//", file_name, sep = ""))[4033:8032,]
  # 5min. 
  ## Entropy
  max_entropy_data_5m <- c(max_entropy_data_5m, entropy(discretize(vmjob$avg_cpu, numBins = 20, r=c(0,100))))
  ## Coefficient of Variation
  max_cv_data_5m <- c(max_cv_data_5m, cv(vmjob$avg_cpu, na.rm = TRUE))
  ## Variance
  max_variance_data_5m <- c(max_variance_data_5m, var(vmjob$avg_cpu, na.rm = TRUE))
  ## Hurst Parameter
  
  # 1-24h
  for (i in 1:length(frequency_lst)) {
    new_avg_cpu <- convert_frequency_dataset(vmjob$avg_cpu, frequency_lst[i], mode = 'avg')
    ## Entropy
    max_entropy_data_lst$append_number(i, entropy(discretize(new_avg_cpu, numBins = 20, r=c(0,100))))
    ## Coefficient of Variation
    max_cv_data_lst$append_number(i, cv(new_avg_cpu, na.rm = TRUE))
    ## Variance
    max_variance_data_lst$append_number(i, var(new_avg_cpu, na.rm = TRUE))
  }
}

# Plot ECDF
entropy_new_dat <- generate_dataframe_for_plotting(max_entropy_data_5m, max_entropy_data_lst)
colnames(entropy_new_dat)[1] <- "entropy"
ggplot(entropy_new_dat, aes(entropy, color = frequency)) + stat_ecdf() + ylab("Fraction of Data") + ggtitle("ECDF of Entropy Avg Value")

cv_new_dat <- generate_dataframe_for_plotting(max_cv_data_5m, max_cv_data_lst)
colnames(cv_new_dat)[1] <- "cv"
ggplot(cv_new_dat, aes(cv, color = frequency)) + stat_ecdf() + ylab("Fraction of Data") + ggtitle("ECDF of CV Avg Value")

variance_new_dat <- generate_dataframe_for_plotting(max_variance_data_5m, max_variance_data_lst)
colnames(variance_new_dat)[1] <- "variance"
ggplot(variance_new_dat, aes(variance, color = frequency)) + stat_ecdf() + ylab("Fraction of Data") + ggtitle("ECDF of Variance Avg Value")