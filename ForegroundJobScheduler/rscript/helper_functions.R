library("forecast")
library("mvtnorm")
library("dict")
library("dplyr")
library("arules")
library("parallel")
library("MTS")
library("dict")


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


convert_frequency_dataset_overlapping <- function(dataset, new_freq, mode) {
  
  new_avg_cpu <- c()
  last_window <- length(dataset) - new_freq + 1
  for (i in 1:last_window) {
    from <- i
    to <- i + new_freq - 1
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
  
  return(100-pi_up)
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


compute_survival <- function(survival) {
  
  numerator <- sum(survival, na.rm = TRUE)
  denominator <- length(survival[!is.na(survival)])
  return(list("survival"=numerator/denominator, "numerator"=numerator, "denominator"=denominator))
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
    total_available2 <- sum(total_available2)
  } else {
    new_max <- convert_frequency_dataset(actual_obs, window_size, 'max')
    total_available1 <- sum(100 - new_max) * window_size
    total_available2 <- sum(100 - actual_obs)
  }
  actual_used <- ifelse(is.na(survivals) | survivals!=0, 0, 100-pi_ups) * window_size
  return(list("utilization1"=(sum(actual_used) / total_available1), "utilization2"=(sum(actual_used) / total_available2), "numerator"=sum(actual_used), "denominator1"=total_available1, "denominator2"=total_available2))
}


update.df <- function(file, model_name, prob_cut_off, state_num=0, sample_size, window_size, granularity, bin_num=0, utilization1, utilization2, survival, correct_scheduled_rate, correct_unscheduled_rate) {
  
  file <- file %>%
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
  file <- file %>%
    arrange(Model, Sample.Size, Window.Size, Granularity, Probability.Cut.Off, StateNum, BinNum)
  return(file)
}


update.df.online <- function(file, model_name, prob_cut_off, state_num=0, sample_size, window_size, granularity, bin_num=0, train_size, update_freq, utilization1, utilization2, survival, correct_scheduled_rate, correct_unscheduled_rate) {
  
  file <- file %>%
    mutate(Avg.Cycle.Usage1 = ifelse(Model == model_name & 
                                       Probability.Cut.Off == prob_cut_off & 
                                       Sample.Size == sample_size &
                                       Window.Size == window_size &
                                       Granularity == granularity &
                                       Training.Size == train_size &
                                       Update.Freq == update_freq &
                                       StateNum == state_num &
                                       BinNum == bin_num,
                                     utilization1, Avg.Cycle.Usage1)) %>%
    mutate(Avg.Cycle.Usage2 = ifelse(Model == model_name & 
                                       Probability.Cut.Off == prob_cut_off & 
                                       Sample.Size == sample_size &
                                       Window.Size == window_size &
                                       Granularity == granularity &
                                       Training.Size == train_size &
                                       Update.Freq == update_freq &
                                       StateNum == state_num &
                                       BinNum == bin_num,
                                     utilization2, Avg.Cycle.Usage2)) %>%
    mutate(Survival.Rate = ifelse(Model == model_name & 
                                    Probability.Cut.Off == prob_cut_off & 
                                    Sample.Size == sample_size &
                                    Window.Size == window_size &
                                    Granularity == granularity &
                                    Training.Size == train_size &
                                    Update.Freq == update_freq &
                                    StateNum == state_num &
                                    BinNum == bin_num,
                                  survival, Survival.Rate)) %>%
    mutate(Correctly.Scheduled = ifelse(Model == model_name & 
                                          Probability.Cut.Off == prob_cut_off & 
                                          Sample.Size == sample_size &
                                          Window.Size == window_size &
                                          Granularity == granularity &
                                          Training.Size == train_size &
                                          Update.Freq == update_freq &
                                          StateNum == state_num &
                                          BinNum == bin_num,
                                        correct_scheduled_rate, Correctly.Scheduled)) %>%
    mutate(Correctly.Unscheduled = ifelse(Model == model_name & 
                                            Probability.Cut.Off == prob_cut_off & 
                                            Sample.Size == sample_size &
                                            Window.Size == window_size &
                                            Granularity == granularity &
                                            Training.Size == train_size &
                                            Update.Freq == update_freq &
                                            StateNum == state_num &
                                            BinNum == bin_num,
                                          correct_unscheduled_rate, Correctly.Unscheduled))
  file <- file %>%
    arrange(Model, Sample.Size, Window.Size, Granularity, Probability.Cut.Off, StateNum, BinNum)
  return(file)
}


find_overall_evaluation <- function(avg_usages1, avg_usages2, survivals) {
  
  avg_utilization1 <- mean(as.matrix(avg_usages1), na.rm = TRUE)
  avg_utilization2 <- mean(as.matrix(avg_usages2), na.rm = TRUE)
  survival <- sum(as.matrix(survivals), na.rm = TRUE) / (length(as.matrix(survivals)[!is.na(as.matrix(survivals))]))
  return(list("utilization_rate1"=avg_utilization1, "utilization_rate2"=avg_utilization2, "survival_rate"=survival))
}


find_state_num <- function(obs, num_of_states) {
  
  binsize <- 100 / num_of_states
  state <- NULL
  if (obs == 0) {
    state <- 1
  } else {
    state <- ceiling(obs / binsize)
  }
  return(state)
}
