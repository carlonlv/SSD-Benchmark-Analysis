library("ggplot2")
library("ggfortify")
library("dict")
library("forecast")
data_path <- "C://Users//carlo//Documents//datasets//csvalldata//sample background jobs"
bg_job_file_lst <- list.files(path = data_path, full.names = FALSE, recursive = FALSE)
frequency_lst <- seq(1, 24, by=1) * 12
lag_pool <- c(1, 2, 3)
#key: lag in lag_pool, value: list of observations
max_all_data_5m <- numvecdict()
max_all_data_1h <- numvecdict()
max_all_data_2h <- numvecdict()
avg_all_data_5m <- numvecdict()
avg_all_data_1h <- numvecdict()
avg_all_data_2h <- numvecdict()

#key: frequency 1:24, value: list of observations
max_all_data_lst <- numvecdict()
avg_all_data_lst <- numvecdict()
#max_signif_count_lst <- numvecdict()
#avg_signif_count_lst <- numvecdict()


##########################################################################################################################################################

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


find_sig_corrs <- function(ts, lag, nobs=length(ts), mode, recursive=FALSE) {
  if (recursive & nobs <= 2 * lag) {
    return(lag - 1)
  }
  
  if (mode == 'pacf') {
    plt <- ggPacf(ts, lag.max = lag)
    val <- plt$data[lag, 3]
    if (is.na(val)) {
      if (recursive) {
        return(NA)
      } else {
        result <- list('sig'=NA, 'val'=val)
        return(result)
      }
    } else if (val <= 1.96 / sqrt(nobs) & val >= -1.96 / sqrt(nobs)) {
      if (recursive) {
        return(lag - 1)
      } else {
        result <- list('sig'=FALSE, 'val'=val)
        return(result)
      }
    } else {
      if (recursive) {
        return(find_sig_corrs(ts, lag+1, nobs, mode, recursive=TRUE))
      } else {
        result <- list('sig'=TRUE, 'val'=val)
        return(result)
      }
    }
  } else {
    plt <- ggAcf(ts, lag.max = lag)
    val <- plt$data[lag, 3]
    if (is.na(val)) {
      if (recursive) {
        return(NA)
      } else {
        result <- list('sig'=NA, 'val'=val)
        return(result)
      }
    } else if (val <= 1.96 / sqrt(nobs- lag) & val >= -1.96 / sqrt(nobs - lag)) {
      if (recursive) {
        return(lag - 1)
      } else {
        result <- list('sig'=FALSE, 'val'=val)
        return(result)
      }
    } else {
      if (recursive) {
        return(find_sig_corrs(ts, lag+1, nobs, mode, recursive=TRUE))
      } else {
        result <- list('sig'=FALSE, 'val'=val)
        return(result)
      }
    }
  }
}

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

##############################################################maxes##############################################################################################

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
  #max_signif_count_lst$append_number('5m', find_sig_corrs(ts_5min, 1, mode = 'pacf', recursive = TRUE))
  
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
    #max_signif_count_lst$append_number(i, find_sig_corrs(new_ts, 1, length(new_max_cpu), mode = 'pacf', recursive = TRUE))
  }
}

## Plot 5m, 1h, 2h at different lags
for (l in lag_pool) {
  new_vector <- factor(rep(c("5min", "1h", "2h"), c(length(max_all_data_5m[[l]]), length(max_all_data_1h[[l]]), length(max_all_data_2h[[l]]))), levels = c("5min", "1h", "2h"))
  cutoff_hi <- c(1.96 / sqrt(4000), 1.96 / sqrt(floor(4000 / 12)), 1.96 / sqrt(floor(4000 / 24)))
  cutoff_lo <- c(-1.96 / sqrt(4000), -1.96 / sqrt(floor(4000 / 12)), -1.96 / sqrt(floor(4000 / 24)))
  cutoff_hi <- as.numeric(rep(cutoff_hi, c(length(max_all_data_5m[[l]]), length(max_all_data_1h[[l]]), length(max_all_data_2h[[l]]))))
  cutoff_lo <- as.numeric(rep(cutoff_lo, c(length(max_all_data_5m[[l]]), length(max_all_data_1h[[l]]), length(max_all_data_2h[[l]]))))
  new_dat <- data.frame(pacf = c(max_all_data_5m[[l]], max_all_data_1h[[l]], max_all_data_2h[[l]]), frequency = new_vector, cutoff_hi = cutoff_hi, cutoff_lo = cutoff_lo)
  print(ggplot(subset(new_dat, !is.na(pacf)), aes(pacf, colour = frequency)) + 
          stat_ecdf() + 
          geom_vline(aes(xintercept=cutoff_hi, colour=frequency), linetype='dashed') + 
          geom_vline(aes(xintercept=cutoff_lo, colour=frequency), linetype='dashed') + 
          ylab("Fraction of Data") + 
          ggtitle(paste("ECDF of Partial Autocorrelation of Maxes at lag", l)))
  ggsave(paste("ECDF of Paritial Autocorrelation of Maxes at lag", l, ".png"))
}

## Plot all freq at lag 1
new_dat <- generate_dataframe_for_plotting(max_all_data_5m[[1]], max_all_data_lst, mode = 'acf', nobs = 4000, lag = 1)
colnames(new_dat)[1] <- "pacf"
ggplot(subset(new_dat, !is.na(pacf)), aes(pacf, colour = frequency)) + 
  stat_ecdf() +
  geom_vline(aes(xintercept=cutoff_hi, colour=frequency), linetype='dashed') + 
  geom_vline(aes(xintercept=cutoff_lo, colour=frequency), linetype='dashed') + 
  ylab("Fraction of Data") + 
  ggtitle("ECDF of Paritial Autocorrelation of Maxes at lag 1")
ggsave(paste("ECDF of Partial Autocorrelation of Maxes at lag 1 (all).png"))

## Plot maximum number of significant pacfs
#new_dat <- generate_dataframe_for_plotting(max_signif_count_lst[['5m']], max_signif_count_lst, mode = 'NULL')
#colnames(new_dat)[1] <- 'count'
#ggplot(subset(new_dat, !is.na(count)), aes(count, colour = frequency)) + 
  #stat_ecdf() + 
  #ylab("Fraction of Data") + 
  #ggtitle("ECDF of maximum counts of significant PACF of Maxes")

######################################################avgs################################################################################################

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
  ts_5min <- ts(vmjob$avg_cpu, start = 0, frequency = 1)
  for (l in lag_pool) {
    result <- find_sig_corrs(ts_5min, lag = l, mode = 'pacf')
    avg_all_data_5m[[l]] <- c(avg_all_data_5m[[l]], result$val)
  }
  
  ## Count max lags
  #avg_signif_count_lst$append_number('5m', find_sig_corrs(ts_5min, 1, mode = 'pacf', recursive = TRUE))
  
  # 1:24h. lag in lag_pool for 1 and 2 h
  for (i in 1:length(frequency_lst)) {
    new_avg_cpu <- convert_frequency_dataset(vmjob$avg_cpu, frequency_lst[i], mode = 'avg')
    new_ts <- ts(new_avg_cpu, start = 0, frequency = frequency_lst[i])
    
    if (i == 1 | i == 2) {
      for (l in lag_pool) {
        result <- find_sig_corrs(new_ts, lag = l, mode = 'pacf')
        if (i == 1) {
          avg_all_data_1h$append_number(l, result$val)
        } else {
          avg_all_data_2h$append_number(l,result$val)
        } 
      }
    } 
    
    result <- find_sig_corrs(new_ts, lag = 1, mode = 'pacf')
    avg_all_data_lst$append_number(i, result$val)
    
    ## Count max lags
    #avg_signif_count_lst$append_number(i, find_sig_corrs(new_ts, 1, mode = 'pacf', recursive = TRUE))
  }
}

## Plot 5m, 1h, 2h at different lags
for (l in lag_pool) {
  new_vector <- factor(rep(c("5min", "1h", "2h"), c(length(avg_all_data_5m[[l]]), length(avg_all_data_1h[[l]]), length(avg_all_data_2h[[l]]))), levels = c("5min", "1h", "2h"))
  cutoff_hi <- c(1.96 / sqrt(4000), 1.96 / sqrt(floor(4000 / 12)), 1.96 / sqrt(floor(4000 / 24)))
  cutoff_lo <- c(-1.96 / sqrt(4000), -1.96 / sqrt(floor(4000 / 12)), -1.96 / sqrt(floor(4000 / 24)))
  cutoff_hi <- as.numeric(rep(cutoff_hi, c(length(max_all_data_5m[[l]]), length(max_all_data_1h[[l]]), length(max_all_data_2h[[l]]))))
  cutoff_lo <- as.numeric(rep(cutoff_lo, c(length(max_all_data_5m[[l]]), length(max_all_data_1h[[l]]), length(max_all_data_2h[[l]]))))
  new_dat <- data.frame(pacf = c(avg_all_data_5m[[l]], avg_all_data_1h[[l]], avg_all_data_2h[[l]]), frequency = new_vector, cutoff_hi = cutoff_hi, cutoff_lo = cutoff_lo)
  print(ggplot(subset(new_dat, !is.na(pacf)), aes(pacf, colour = frequency)) + 
          stat_ecdf() + 
          geom_vline(aes(xintercept=cutoff_hi, colour=frequency), linetype='dashed') + 
          geom_vline(aes(xintercept=cutoff_lo, colour=frequency), linetype='dashed') + 
          ylab("Fraction of Data") + 
          ggtitle(paste("ECDF of Partial Autocorrelation of Avgs at lag", l)))
  ggsave(paste("ECDF of Partial Autocorrelation of Avgs at lag", l, ".png"))
}


## Plot all freq at lag 1
new_dat <- generate_dataframe_for_plotting(avg_all_data_5m[[1]], avg_all_data_lst, mode = 'pacf', nobs = 4000, lag = 1)
colnames(new_dat)[1] <- "pacf"
ggplot(subset(new_dat, !is.na(pacf)), aes(pacf, colour = frequency)) + 
  stat_ecdf() + 
  geom_vline(aes(xintercept=cutoff_hi, colour=frequency), linetype='dashed') + 
  geom_vline(aes(xintercept=cutoff_lo, colour=frequency), linetype='dashed') + 
  ylab("Fraction of Data") + 
  ggtitle("ECDF of Partial Autocorrelation of Avgs at lag 1")
ggsave("ECDF of Partial Autocorrelation of Avgs at lag 1 (all).png")

## Plot maximum number of significant pacfs
#new_dat <- generate_dataframe_for_plotting(avg_signif_count_lst[['5m']], avg_signif_count_lst, mode = 'NULL')
#colnames(new_dat)[1] <- 'count'
#ggplot(subset(new_dat, !is.na(count)), aes(count, colour = frequency)) + 
  #stat_ecdf() + 
  #ylab("Fraction of Data") + 
  #ggtitle("ECDF of maximum counts of significant PACF of Avgs")