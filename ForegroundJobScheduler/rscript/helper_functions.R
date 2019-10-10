library("forecast")
library("mvtnorm")
library("dict")
library("dplyr")
library("xlsx")


convert_frequency_dataset <- function(dataset, new_freq, mode) {
  new_avg_cpu <- c()
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


round_to_nearest <- function(data, divisor, lower) {
  if (lower) {
    return(floor(data / divisor) * divisor)
  } else {
    return(ceiling(data / divisor) * divisor)
  }
}


compute_pi_up <- function(mu, varcov, predict_size, prob_cutoff, granularity) {
  upper_bounds <- rep(NA, predict_size)
  for (i in 1:predict_size) {
    upper_bounds[i] <- min(mu[i] + qnorm((1-prob_cutoff)) * sqrt(varcov[i,i]), 100)
  }
  if (granularity > 0) {
    scheduled_size <- sapply(100 - upper_bounds, round_to_nearest, granularity, TRUE)
    upper_bounds <- 100 - scheduled_size
  }
  return(upper_bounds)
}


check_utilization <- function(pi_up, granularity=0) {
  utilization <- NULL
  if (granularity > 0) {
    utilization <- round_to_nearest(100-pi_up, granularity, TRUE)
  } else {
    utilization <- 100 - pi_up
  }
  return(utilization)
}


check_survival <- function(pi_up, actual_obs, granularity=0) {
  position_vec <- convert_frequency_dataset(actual_obs, length(actual_obs), mode = "max")
  actual_available <- position_vec
  if (granularity > 0) {
    actual_available <- 100 - round_to_nearest(100-position_vec, granularity, TRUE)
    actual_obs <-  100 - round_to_nearest(100-actual_obs, granularity, TRUE)
  }
  
  survival <- NULL
  if (granularity == 0) {
    if ((100 - pi_up) == 0) {
      survival <- NA
    } else {
      survival <- ifelse(actual_available <= pi_up, 0, which(actual_obs > pi_up)[1])
    }
  } else {
    if ((100 - pi_up) < granularity) {
      survival <- NA
    } else {
      if ((100 - actual_available) < granularity) {
        survival <- which(actual_obs > pi_up)[1]
      } else {
        survival <- ifelse(actual_available <= pi_up, 0, which(actual_obs > pi_up)[1])
      }
    }
  }
  return(survival)
}


overlapping_total_utilization <- function(idx, actual_obs, window_size, mode) {
  position_vec <- actual_obs[idx:(idx+window_size-1)]
  total_available <- NULL
  if (mode == 1) {
    newmax <- convert_frequency_dataset(position_vec, window_size, 'max')
    total_available <- 100 - newmax
  } else {
    total_available <- 100 - position_vec
  }
  return(total_available)
}


dynamic_total_utilization <- function(actual_obs, survivals, window_size) {
  current <- 1
  idx <- 1
  total_utilization <- 0
  while(current <= (length(actual_obs) - window_size + 1)) {
    new_max <- convert_frequency_dataset(actual_obs[current:(current+window_size-1)], window_size, "max")
    actual_utilization <- ifelse(is.na(survivals[current]), 100-actual_obs[current], ifelse(survivals[current]!=0, sum(100-actual_obs[current:(current+survivals[current]-1)]), (100-new_max)*window_size))
    total_utilization <- total_utilization + actual_utilization
    current <- ifelse(is.na(survivals[current]), current+1, ifelse(survivals[current]==0, current+window_size, current+survivals[current]))
    idx <- idx + 1
  }
  return(total_utilization)
}


compute_survival <- function(survival) {
  return(sum(survival, na.rm = TRUE) / length(survival[!is.na(survival)]))
}


compute_utilization <- function(pi_ups, survivals, actual_obs, window_size, granularity, schedule_policy) {
  if (granularity > 0) {
    actual_available <- round_to_nearest(100 - actual_obs, granularity, TRUE)
    actual_obs <- 100 - actual_available
  }
  total_available1 <- NULL
  total_available2 <- NULL
  
  if (schedule_policy == "overlap"){
    total_available1 <- sapply(1:(length(actual_obs)-window_size), overlapping_total_utilization, actual_obs, window_size, 1)
    total_available1 <- sum(total_available1) * window_size
    total_available2 <- sapply(1:(length(actual_obs)-window_size), overlapping_total_utilization, actual_obs, window_size, 2)
    total_available1 <- sum(total_available2)
  } else if (schedule_policy == "dynamic") {
    total_available1 <- dynamic_total_utilization(actual_obs, survivals, window_size)
    total_available2 <- sum(100 - actual_obs)
  } else {
    new_max <- convert_frequency_dataset(actual_obs, window_size, 'max')
    total_available1 <- sum(100 - new_max) * window_size
    total_available2 <- sum(100 - actual_obs)
  }
  actual_used <- ifelse(is.na(survivals) | survivals!=0, 0, 100-pi_ups) * window_size
  return(list("utilization1"=(sum(actual_used) / total_available1), "utilization2"=(sum(actual_used) / total_available2)))
}


update.xlsx.df <- function(xlsx_file, model_name, prob_cut_off, state_num=0, sample_size, window_size, granularity, bin_num=0, utilization1, utilization2, survival, correct_scheduled_rate, correct_unscheduled_rate) {
  xlsx_file <- xlsx_file %>%
    mutate(Avg.Cycle.Usage1 = ifelse(Model == model_name & 
                                      Probability.Cut.Off == prob_cut_off & 
                                      Sample.Size == sample_size &
                                      Window.Size == window_size &
                                      Granularity == granularity &
                                      StateNum == state_num &
                                      BinNum == bin_num,
                                    utilization1, Avg.Cycle.Usage1)) %>%
    mutate(Avg.Cycle.Usage2 = ifelse(Model == model_name & 
                                       Probability.Cut.Off == prob_cut_off & 
                                       Sample.Size == sample_size &
                                       Window.Size == window_size &
                                       Granularity == granularity &
                                       StateNum == state_num &
                                       BinNum == bin_num,
                                     utilization2, Avg.Cycle.Usage2)) %>%
    mutate(Survival.Rate = ifelse(Model == model_name & 
                                    Probability.Cut.Off == prob_cut_off & 
                                    Sample.Size == sample_size &
                                    Window.Size == window_size &
                                    Granularity == granularity &
                                    StateNum == state_num &
                                    BinNum == bin_num,
                                  survival, Survival.Rate)) %>%
    mutate(Correctly.Scheduled = ifelse(Model == model_name & 
                                          Probability.Cut.Off == prob_cut_off & 
                                          Sample.Size == sample_size &
                                          Window.Size == window_size &
                                          Granularity == granularity &
                                          StateNum == state_num &
                                          BinNum == bin_num,
                                        correct_scheduled_rate, Correctly.Scheduled)) %>%
    mutate(Correctly.Unscheduled = ifelse(Model == model_name & 
                                            Probability.Cut.Off == prob_cut_off & 
                                            Sample.Size == sample_size &
                                            Window.Size == window_size &
                                            Granularity == granularity &
                                            StateNum == state_num &
                                            BinNum == bin_num,
                                          correct_unscheduled_rate, Correctly.Unscheduled))
  xlsx_file <- xlsx_file %>%
    arrange(Model, Sample.Size, Window.Size, Granularity, Probability.Cut.Off, StateNum, BinNum)
  return(xlsx_file)
}


find_overall_evaluation <- function(avg_usages1, avg_usages2, survivals) {
  avg_utilization1 <- mean(as.matrix(avg_usages1), na.rm = TRUE)
  avg_utilization2 <- mean(as.matrix(avg_usages2), na.rm = TRUE)
  survival <- sum(as.matrix(survivals), na.rm = TRUE) / (length(as.matrix(survivals)[!is.na(as.matrix(survivals))]))
  return(list("utilization_rate1"=avg_utilization1, "utilization_rate2"=avg_utilization2, "survival_rate"=survival))
}
