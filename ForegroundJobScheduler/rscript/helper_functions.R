suppressMessages(library("forecast"))
suppressMessages(library("mvtnorm"))
suppressMessages(library("dict"))
suppressMessages(library("dplyr"))
suppressMessages(library("arules"))
suppressMessages(library("parallel"))
suppressMessages(library("MTS"))


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
  
  total_available <- NULL
  if (schedule_policy == "overlap"){
    total_available <- sapply(1:(length(actual_obs)-window_size), overlapping_total_utilization, actual_obs, window_size, 2)
    total_available <- sum(total_available)
  } else {
    new_max <- convert_frequency_dataset(actual_obs, window_size, 'max')
    total_available <- sum(100 - actual_obs)
  }
  actual_used <- ifelse(is.na(survivals) | survivals!=0, 0, 100-pi_ups) * window_size
  return(list("utilization"=(sum(actual_used) / total_available), "numerator"=sum(actual_used), "denominator"=total_available))
}


update.df.offline <- function(file, model_name, prob_cut_off, state_num, sample_size, window_size, granularity, bin_num, avg_utilization, agg_utilization, avg_survival, agg_survival, correct_scheduled_rate, correct_unscheduled_rate) {
  
  ## See if this update is addition or update
  checker <- file %>%
    filter(Model == model_name & 
             Probability.Cut.Off == prob_cut_off & 
             Sample.Size == sample_size &
             Window.Size == window_size &
             Granularity == granularity &
             StateNum == state_num &
             BinNum == bin_num) %>%
    tally()
  if (checker$n == 0) {
    file <- file %>% 
      add_row(Model = model_name, 
              Probability.Cut.Off = prob_cut_off,
              Sample.Size = sample_size,
              Window.Size = window_size,
              Granularity = granularity,
              StateNum = state_num,
              BinNum = bin_num,
              Avg.Cycle.Usage = avg_utilization,
              Agg.Cycle.Usage = agg_utilization,
              Avg.Survival.Rate = avg_survival,
              Agg.Survival.Rate = agg_survival,
              Correctly.Scheduled = correct_scheduled_rate,
              Correctly.Unscheduled = correct_unscheduled_rate)
  } else {
    file <- file %>%
      mutate(Avg.Cycle.Usage = ifelse(Model == model_name & 
                                         Probability.Cut.Off == prob_cut_off & 
                                         Sample.Size == sample_size &
                                         Window.Size == window_size &
                                         Granularity == granularity &
                                         StateNum == state_num &
                                         BinNum == bin_num,
                                       avg_utilization, Avg.Cycle.Usage)) %>%
      mutate(Agg.Cycle.Usage = ifelse(Model == model_name & 
                                         Probability.Cut.Off == prob_cut_off & 
                                         Sample.Size == sample_size &
                                         Window.Size == window_size &
                                         Granularity == granularity &
                                         StateNum == state_num &
                                         BinNum == bin_num,
                                       agg_utilization, Agg.Cycle.Usage)) %>%
      mutate(Avg.Survival.Rate = ifelse(Model == model_name & 
                                      Probability.Cut.Off == prob_cut_off & 
                                      Sample.Size == sample_size &
                                      Window.Size == window_size &
                                      Granularity == granularity &
                                      StateNum == state_num &
                                      BinNum == bin_num,
                                    avg_survival, Avg.Survival.Rate)) %>%
      mutate(Agg.Survival.Rate = ifelse(Model == model_name & 
                                          Probability.Cut.Off == prob_cut_off & 
                                          Sample.Size == sample_size &
                                          Window.Size == window_size &
                                          Granularity == granularity &
                                          StateNum == state_num &
                                          BinNum == bin_num,
                                        agg_survival, Agg.Survival.Rate)) %>%
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
  }
  file <- file %>%
    arrange()
  return(file)
}


update.df.online <- function(file, model_name, prob_cut_off, state_num, sample_size, window_size, granularity, bin_num, train_size, update_freq, avg_utilization, agg_utilization, avg_survival, agg_survival, correct_scheduled_rate, correct_unscheduled_rate) {
  
  checker <- file %>%
    filter(Model == model_name & 
             Probability.Cut.Off == prob_cut_off & 
             Sample.Size == sample_size &
             Window.Size == window_size &
             Granularity == granularity &
             Training.Size == train_size &
             Update.Freq == update_freq &
             StateNum == state_num &
             BinNum == bin_num) %>%
    tally()
  if (checker$n == 0) {
    file <- file %>%
      add_row(Model = model_name, 
              Probability.Cut.Off = prob_cut_off,
              Sample.Size = sample_size,
              Window.Size = window_size,
              Granularity = granularity,
              Training.Size = train_size,
              Update.Freq = update_freq,
              StateNum = state_num,
              BinNum = bin_num,
              Avg.Cycle.Usage = avg_utilization,
              Agg.Cycle.Usage = agg_utilization,
              Avg.Survival.Rate = avg_survival,
              Agg.Survival.Rate = agg_survival,
              Correctly.Scheduled = correct_scheduled_rate,
              Correctly.Unscheduled = correct_unscheduled_rate)
  } else {
    file <- file %>%
      mutate(Avg.Cycle.Usage = ifelse(Model == model_name & 
                                         Probability.Cut.Off == prob_cut_off & 
                                         Sample.Size == sample_size &
                                         Window.Size == window_size &
                                         Granularity == granularity &
                                         Training.Size == train_size &
                                         Update.Freq == update_freq &
                                         StateNum == state_num &
                                         BinNum == bin_num,
                                       avg_utilization, Avg.Cycle.Usage)) %>%
      mutate(Agg.Cycle.Usage = ifelse(Model == model_name & 
                                         Probability.Cut.Off == prob_cut_off & 
                                         Sample.Size == sample_size &
                                         Window.Size == window_size &
                                         Granularity == granularity &
                                         Training.Size == train_size &
                                         Update.Freq == update_freq &
                                         StateNum == state_num &
                                         BinNum == bin_num,
                                       agg_utilization, Agg.Cycle.Usage)) %>%
      mutate(Avg.Survival.Rate = ifelse(Model == model_name & 
                                      Probability.Cut.Off == prob_cut_off & 
                                      Sample.Size == sample_size &
                                      Window.Size == window_size &
                                      Granularity == granularity &
                                      Training.Size == train_size &
                                      Update.Freq == update_freq &
                                      StateNum == state_num &
                                      BinNum == bin_num,
                                    avg_survival, Avg.Survival.Rate)) %>%
      mutate(Agg.Survival.Rate = ifelse(Model == model_name & 
                                      Probability.Cut.Off == prob_cut_off & 
                                      Sample.Size == sample_size &
                                      Window.Size == window_size &
                                      Granularity == granularity &
                                      Training.Size == train_size &
                                      Update.Freq == update_freq &
                                      StateNum == state_num &
                                      BinNum == bin_num,
                                    agg_survival, Agg.Survival.Rate)) %>%
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
  }
  file <- file %>%
    arrange()
  return(file)
}


find_overall_evaluation <- function(util_numerator, util_denominator, sur_numerator, sur_denominator) {
  
  avg_usages <- util_numerator / util_denominator
  avg_utilization <- mean(avg_usages, na.rm = TRUE)
  agg_utilization <- sum(util_numerator, na.rm = TRUE) / sum(util_denominator, na.rm = TRUE)
  survivals <- sur_numerator / sur_denominator
  avg_survival <- mean(survivals, na.rm = TRUE)
  agg_survival <- sum(sur_numerator, na.rm = TRUE) / sum(sur_denominator, na.rm = TRUE)
  return(list("avg_utilization"=avg_utilization, "avg_survival"=avg_survival, "agg_utilization"=agg_utilization, "agg_survival"=agg_survival))
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
