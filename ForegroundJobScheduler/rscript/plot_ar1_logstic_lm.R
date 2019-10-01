library("xlsx")
library("readxl")
library("ggplot2")
library("dplyr")
library("gridExtra")
library("pracma")

read_runs <- function(rownum, parameter.df, model_name, sample_size, result_folder_path) {
  param_list <- parameter.df[rownum,]
  window_size <- param_list[1]
  prob_cut_off <- param_list[2]
  granularity <- param_list[3]
  bin_num <- param_list[4]
  
  filename <- paste("Overall Runs", model_name, sample_size, window_size, prob_cut_off, granularity, bin_num, ".csv")
  
  runs <- read.csv(paste(result_folder_path, filename, sep=""))
  runs <- colSums(runs[, -1])
  
  return(runs)
}


convert_frequency_dataset <- function(dataset, new_freq, mode) {
  new_avg_cpu <- c()
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
    new_avg_cpu <- c(new_avg_cpu, new_val)
  }
  return(new_avg_cpu)
}


find_bin_obs <- function(avg, binsize) {
  return(floor(avg / binsize))
}


train_cond_var_model <- function(ts_num, train_set_max, train_set_avg, bin_num) {
  
  new_parsed_dat <- data.frame(matrix(nrow=nrow(train_set_avg), ncol=3))
  binsize <- 100 / bin_num
  bin <- as.numeric(sapply(train_set_avg[,ts_num], find_bin_obs, binsize))
  bin <- bin * binsize
  for (i in 1:nrow(train_set_avg)) {
    new_parsed_dat[i,] = c(train_set_avg[i, ts_num], train_set_max[i, ts_num], bin[i])
  }
  
  colnames(new_parsed_dat) <- c('avg', 'max', 'bin')
  selected_bins <- new_parsed_dat %>%
    group_by(bin) %>%
    count()
  
  selected_bins <- subset(selected_bins, selected_bins$n >= 3)$bin
  selected_bins <- new_parsed_dat$bin
  new_parsed_dat1 <- new_parsed_dat %>%
    filter(bin %in% selected_bins) %>%
    group_by(bin) %>% 
    summarise(sd=sqrt(var(max))) %>%
    filter(!is.na(sd))
  
  sd.lm <- NULL
  if (nrow(new_parsed_dat) >= 3) {
    sd.lm <- lm(sd~bin+I(bin^2), data = new_parsed_dat1)
  } else if (nrow(new_parsed_dat) == 2) {
    sd.lm <- lm(sd~bin, data = new_parsed_dat1)
  } else {
    sd.lm <- new_parsed_dat$sd
  }
  return(list("lm"=sd.lm, "data"=new_parsed_dat1, "data2"=new_parsed_dat))
}


make_fitted_model_plot <- function(dataset, lm) {
  predicted <- predict(lm, newdata=data.frame(bin=linspace(min(dataset$bin), max(dataset$bin), 100)))
  predicted_df <- data.frame(x=linspace(min(dataset$bin), max(dataset$bin), 100), y=predicted)
  ggplot() +
    geom_point(aes(x=bin, y=sd), dataset) +
    geom_line(aes(x=x, y=y), predicted_df) +
    ylab("Maxes") +
    xlab("Bin")
}


make_qqnorm_plot <- function(dataset, xlab, ylab, title) {
  df <- dataset %>%
    group_by(bin) %>%
    count()
  binnum <- df$bin[df$n == max(df$n)]
  
  df <- dataset %>%
    filter(bin == binnum)
  ggplot(df, aes(sample = max)) +
    stat_qq() +
    stat_qq_line()
}


make_hist <-  function(dataset) {
  breaks <- seq(0, 50, 10)
  dataset$hst <- NA
    
  for (i in 1:nrow(dataset)) {
    j = 1
    current_bin <- dataset[i, ]$bin
    while (j <= (length(breaks) - 1)) {
      if (current_bin <= breaks[j+1]) {
        break
      }
      j <- j + 1
    }
    dataset[i, ]$hst <- paste("From", breaks[j], "To", breaks[j+1])
  }
  
  
  ggplot(dataset, aes(x=max, color=hst, alpha=0.5)) + 
    geom_histogram(bins=20, fill="white", show.legend = FALSE) + 
    xlab("Maxes") +
    ylab("Frequency")
}


## Sample 9 jobs
bg_jobs_path = "C://Users//carlo//Documents//sample background jobs//"
bg_job_pool <- read.csv("C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//pythonscripts//list of sampled 100 background jobs.csv")[,1]
bg_job_pool <- sample(bg_job_pool, 9)
bg_job_pool <- sub(".pd", "", bg_job_pool)
train_set_avg <- matrix(nrow = 6000, ncol = 0)
train_set_max <- matrix(nrow = 6000, ncol = 0)
for (job_num in bg_job_pool) {
  bg_job <- read.csv(paste(bg_jobs_path, job_num, ".csv", sep = ""))
  train_set_avg <- cbind(train_set_avg, bg_job$avg_cpu[1:6000])
  train_set_max <- cbind(train_set_max, bg_job$max_cpu[1:6000])
}
rownames(train_set_avg) <- seq(1, nrow(train_set_avg) ,1)
rownames(train_set_max) <- seq(1, nrow(train_set_max) ,1)
colnames(train_set_avg) <- bg_job_pool
colnames(train_set_max) <- bg_job_pool

cpu_required <- rep(0, ncol(train_set_max))
for (j in 1:ncol(train_set_max)) {
  cpu_required[j] <- as.numeric(quantile(train_set_max[,j], c(0.15, 0.5, 0.85), type = 4)[3])
}

window_size <- c(12)
prob_cut_off <- c(0.1)
granularity <- c(100/128)
bin_num <- c(1000)

new_trainset_max <- apply(train_set_max, 2, convert_frequency_dataset, new_freq=window_size, mode="max")
rownames(new_trainset_max) <- seq(1, 1 + window_size * (nrow(new_trainset_max) - 1), window_size)
colnames(new_trainset_max) <- colnames(train_set_max)

new_trainset_avg <- apply(train_set_avg, 2, convert_frequency_dataset, new_freq=window_size, mode="avg")
rownames(new_trainset_avg) <- seq(1, 1 + window_size * (nrow(new_trainset_avg) - 1), window_size)
colnames(new_trainset_avg) <- colnames(train_set_max)

cond_var_models <- sapply(1:9, train_cond_var_model, new_trainset_max, new_trainset_avg, bin_num, simplify=FALSE)
p <- list()
for (i in 1:9) {
  p[[i]] <- make_fitted_model_plot(cond_var_models[[i]]$data, cond_var_models[[i]]$lm)
}
do.call(grid.arrange, p)

q <- list()
for (i in 1:9) {
  q[[i]] <- make_qqnorm_plot(cond_var_models[[i]]$data2)
}
do.call(grid.arrange, q)

r <- list()
for (i in 1:9) {
  r[[i]] <- make_hist(cond_var_models[[i]]$data2)
}
do.call(grid.arrange, r)