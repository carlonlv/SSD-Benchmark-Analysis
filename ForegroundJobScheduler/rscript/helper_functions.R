library("forecast")
library("mvtnorm")
library("dict")
library("dplyr")
library("xlsx")


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
    upper_bounds[i] <- min(mu[i] + qnorm((1-prob_cutoff), 0, 1) * sqrt(varcov[i,i]), 100)
  }
  if (granularity > 0) {
    scheduled_size <- sapply(100 - upper_bounds, round_to_nearest, granularity, TRUE)
    upper_bounds <- 100 - scheduled_size
  }
  return(upper_bounds)
}


find_evaluation <- function(pi_up, actual_obs, granularity=0) {
  if (granularity != 0) {
    actual_available <- round_to_nearest(100 - actual_obs, granularity, TRUE)
    actual_obs <- 100 - actual_available
  }
  usage <- c()
  survival <- c()
  for (i in 1:length(pi_up)) {
    if (granularity == 0) {
      if ((100 - pi_up[i]) == 0 & (100 - actual_obs[i]) == 0) {
        survival[i] <- NA
        usage[i] <- NA
      } else {
        survival[i] <- ifelse(actual_obs[i] <= pi_up[i], 1, 0)
        usage[i] <- ifelse(survival[i] == 0, NA, ifelse(actual_obs[i] == pi_up[i], NA, (100 - pi_up[i]) / (100 - actual_obs[i])))
      }
    } else {
      if ((100 - pi_up[i]) < granularity) {
        if ((100 - actual_obs[i]) < granularity) {
          survival[i] <- NA
          usage[i] <- NA
        } else {
          survival[i] <- 1
          usage[i] <- 0
        }
      } else {
        if ((100 - actual_obs[i]) < granularity) {
          survival[i] <- 0
          usage[i] <- NA
        } else {
          survival[i] <- ifelse(actual_obs[i] <= pi_up[i], 1, 0)
          usage[i] <- ifelse(survival[i] == 0, NA, ifelse(actual_obs[i] == pi_up[i], NA, (100 - pi_up[i]) / (100 - actual_obs[i])))
        }
      }
    }
  }
  avg_usage <- mean(usage, na.rm=TRUE)
  overall_survival <- ifelse(any(is.na(survival)), NA, ifelse(any(survival == 0), 0, 1))
  result <- list('usage' = avg_usage, 'survival'= overall_survival)
  return(result)
}


update.xlsx.df <- function(xlsx_file, model_name, prob_cut_off, state_num, sample_size, window_size, granularity, bin_num, utilization, survival, correct_scheduled_rate, correct_unscheduled_rate) {
  #if (is.na(state_num)) {
    #xlsx_file <- xlsx_file %>%
      #mutate(Avg.Cycle.Usage = ifelse(Model == model_name & 
                                        #Probability.Cut.Off == prob_cut_off & 
                                        #Sample.Size == sample_size &
                                        #Window.Size == window_size &
                                        #Granularity == granularity, 
                                      #utilization, Avg.Cycle.Usage)) %>%
      #mutate(Survival.Rate = ifelse(Model == model_name & 
                                      #Probability.Cut.Off == prob_cut_off & 
                                      #Sample.Size == sample_size &
                                      #Window.Size == window_size &
                                      #Granularity == granularity, 
                                    #survival, Survival.Rate)) %>%
      #mutate(Correctly.Scheduled = ifelse(Model == model_name & 
                                            #Probability.Cut.Off == prob_cut_off & 
                                            #Sample.Size == sample_size &
                                            #Window.Size == window_size &
                                            #Granularity == granularity, 
                                          #correct_scheduled_rate, Correctly.Scheduled)) %>%
      #mutate(Correctly.Unscheduled = ifelse(Model == model_name & 
                                              #Probability.Cut.Off == prob_cut_off & 
                                              #Sample.Size == sample_size &
                                              #Window.Size == window_size &
                                              #Granularity == granularity, 
                                            #correct_unscheduled_rate, Correctly.Unscheduled))
  #} else {
    #xlsx_file <- xlsx_file %>%
      #mutate(Avg.Cycle.Usage = ifelse(Model == model_name & 
                                        #Probability.Cut.Off == prob_cut_off & 
                                        #Sample.Size == sample_size &
                                        #StateNum == state_num & 
                                        #Window.Size == window_size &
                                        #Granularity == granularity, 
                                      #utilization, Avg.Cycle.Usage)) %>%
      #mutate(Survival.Rate = ifelse(Model == model_name & 
                                      #Probability.Cut.Off == prob_cut_off & 
                                      #Sample.Size == sample_size &
                                      #StateNum == state_num & 
                                      #Window.Size == window_size &
                                      #Granularity == granularity, 
                                    #survival, Survival.Rate)) %>%
      #mutate(Correctly.Scheduled = ifelse(Model == model_name & 
                                            #Probability.Cut.Off == prob_cut_off & 
                                            #Sample.Size == sample_size &
                                            #StateNum == state_num & 
                                            #Window.Size == window_size &
                                            #Granularity == granularity, 
                                          #correct_scheduled_rate, Correctly.Scheduled)) %>%
      #mutate(Correctly.Unscheduled = ifelse(Model == model_name & 
                                              #Probability.Cut.Off == prob_cut_off & 
                                              #Sample.Size == sample_size &
                                              #StateNum == state_num & 
                                              #Window.Size == window_size &
                                              #Granularity == granularity, 
                                            #correct_unscheduled_rate, Correctly.Unscheduled))
  #}
  xlsx_file <- xlsx_file %>%
    mutate(Avg.Cycle.Usage = ifelse(Model == model_name & 
                                      Probability.Cut.Off == prob_cut_off & 
                                      Sample.Size == sample_size &
                                      Window.Size == window_size &
                                      Granularity == granularity &
                                      ifelse(is.na(state_num), TRUE, StateNum == state_num), 
                                    utilization, Avg.Cycle.Usage)) %>%
    mutate(Survival.Rate = ifelse(Model == model_name & 
                                    Probability.Cut.Off == prob_cut_off & 
                                    Sample.Size == sample_size &
                                    Window.Size == window_size &
                                    Granularity == granularity &
                                    ifelse(is.na(state_num), TRUE, StateNum == state_num) &
                                    ifelse(is.na(bin_num), TRUE, BinNum == bin_num),
                                  survival, Survival.Rate)) %>%
    mutate(Correctly.Scheduled = ifelse(Model == model_name & 
                                          Probability.Cut.Off == prob_cut_off & 
                                          Sample.Size == sample_size &
                                          Window.Size == window_size &
                                          Granularity == granularity &
                                          ifelse(is.na(state_num), TRUE, StateNum == state_num) &
                                          ifelse(is.na(bin_num), TRUE, BinNum == bin_num),
                                        correct_scheduled_rate, Correctly.Scheduled)) %>%
    mutate(Correctly.Unscheduled = ifelse(Model == model_name & 
                                            Probability.Cut.Off == prob_cut_off & 
                                            Sample.Size == sample_size &
                                            Window.Size == window_size &
                                            Granularity == granularity &
                                            ifelse(is.na(state_num), TRUE, StateNum == state_num) &
                                            ifelse(is.na(bin_num), TRUE, BinNum == bin_num),
                                          correct_unscheduled_rate, Correctly.Unscheduled))
  return(xlsx_file)
}


find_overall_evaluation <- function(avg_usages, survivals) {
  avg_utilization <- mean(as.matrix(avg_usages), na.rm = TRUE)
  survival <- sum(as.matrix(survivals), na.rm = TRUE) / (length(as.matrix(survivals)[!is.na(as.matrix(survivals))]))
  return(list("utilization_rate"=avg_utilization, "survival_rate"=survival))
}
