library("xlsx")
library("readxl")
library("ggplot2")
library("dplyr")

calculate_utilization <- function(utilization_info) {
  return(mean(utilization_info, na.rm = TRUE))
}

calculate_survival <- function(survival_info) {
  return(sum(survival_info, na.rm = TRUE) / length(survival_info[!is.na(survival_info)]))
}

find_runs <- function(survival_for_trace) {
  count_of_runs <- rep(0, length(survival_for_trace))
  current_pos <- 1
  counter <- 0
  while (current_pos <= length(survival_for_trace)) {
    if (survival_for_trace[current_pos] == 0) {
      counter <- counter + 1
    } else if (counter > 0) {
      count_of_runs[counter] <- count_of_runs[counter] + 1
      counter <- 0
    }
    current_pos <- current_pos + 1
  }
  return(count_of_runs)
}

compute_distribution_of_runs <- function(survival_info) {
  total_count_of_runs <- rep(0, nrow(survival_info))
  result.lst <- apply(survival_info, 2, find_runs)
  sapply(list, sum)
}

read_from_ar_result <- function(model, window_size, sample_size, prob_cut_off, state_num, granularity, datapath) {
  utilization_filename <- NULL
  survival_filename <- NULL
  if (is.na(state_num)) {
    utilization_filename <- paste(model, window_size, sample_size, prob_cut_off, granularity, "avg_usage.csv")
    survival_filename <- paste(model, window_size, sample_size, prob_cut_off, granularity, "job_survival.csv")
  } else {
    utilization_filename <- paste(model, window_size, state_num, sample_size, prob_cut_off, granularity, "avg_usage.csv")
    survival_filename <- paste(model, window_size, state_num, sample_size, prob_cut_off, granularity, "job_survival.csv")
  }
  survival_dp <- paste(datapath, survival_filename, sep = "")
  utilization_info <- read.csv(utilization_dp)
  survival_info <- read.csv(survival_dp)
  
  
  utilization_info <- utilization_info %>%
    summarise_all(funs(calculate_utilization))
  utilization_info <- as.numeric(utilization_info[1,])
  survival_info <- survival_info %>%
    summarise_all(funs(calculate_survival))
  survival_info <- as.numeric(survival_info[1,])
  return(list("utilization_info"=utilization_info, "survival_info"=survival_info))
}

make_histogram <- function(info_df) {
  ggplot(info_df, aes(x=Survival, y=Utilization, group=factor(Model), colour=factor(Model))) +
    geom_point(na.rm=TRUE, aes(shape=as.factor(StateNum))) + 
    geom_vline(xintercept=0.99, linetype="dashed", color="red") + 
    ylab("Utilization") +
    xlab("Survival Rate") + 
    ggtitle(paste("Model Performance With Sample Size", sample_size, "and Window Size", window_size))
  ggsave(paste("Model Performance With Sample Size", sample_size, "and Window Size", window_size, ".png"))
}

result_folder_path <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//Nonoverlapping windows//maxes"
model_name <- "AR1"
window_size <- 12
sample_size <- 100
prob_cut_off <- 0.01
granularity <- 10
state_num <- NA

model_performance <- read_from_ar_result(model_name, window_size, sample_size, prob_cut_off, state_num, granularity, result_folder_path)