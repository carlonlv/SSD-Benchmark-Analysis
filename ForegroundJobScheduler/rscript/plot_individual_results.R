library("xlsx")
library("readxl")
library("ggplot2")
library("dplyr")

calculate_utilization <- function(utilization_info) {
  return(mean(utilization_info, na.rm = TRUE))
}

calculate_survival <- function(survival_info) {
  return(mean(survival_info, na.rm = TRUE))
}

find_runs <- function(survival_for_trace) {
  count_of_runs <- rep(0, length(survival_for_trace))
  current_pos <- 1
  counter <- 0
  while (current_pos <= length(survival_for_trace)) {
    if (!is.na(survival_for_trace[current_pos]) & survival_for_trace[current_pos] == 0) {
      counter <- counter + 1
    } else if (!is.na(survival_for_trace[current_pos]) & counter > 0) {
      count_of_runs[counter] <- count_of_runs[counter] + 1
      counter <- 0
    }
    current_pos <- current_pos + 1
  }
  return(count_of_runs)
}

compute_distribution_of_runs <- function(survival_info) {
  result.lst <- apply(survival_info, 2, find_runs)
  ## Here add to total_count_of_runs
  return(rowSums(result.lst))
}

read_from_ar_result <- function(model, window_size, sample_size, prob_cut_off, state_num, granularity, cpu_usage, datapath, mode="raw") {
  utilization_filename <- NULL
  survival_filename <- NULL
  if (!is.na(state_num)) {
    utilization_filename <- paste(model, window_size, state_num, sample_size, prob_cut_off, granularity, "avg_usage.csv")
    survival_filename <- paste(model, window_size, state_num, sample_size, prob_cut_off, granularity, "job_survival.csv")
  } else if (!is.na(cpu_usage)){
    utilization_filename <- paste(model, window_size, sample_size, prob_cut_off, granularity, cpu_usage,"avg_usage.csv")
    survival_filename <- paste(model, window_size, sample_size, prob_cut_off, granularity, cpu_usage,"job_survival.csv")
  } else {
    utilization_filename <- paste(model, window_size, sample_size, prob_cut_off, granularity,"avg_usage.csv")
    survival_filename <- paste(model, window_size, sample_size, prob_cut_off, granularity,"job_survival.csv")
  }
  utilization_dp <- paste(datapath, utilization_filename, sep = "")
  survival_dp <- paste(datapath, survival_filename, sep = "")
  utilization_info <- read.csv(utilization_dp)[-1]
  survival_info <- read.csv(survival_dp)[-1]
  
  if (mode == "raw") {
    return(list("utilization_info"=utilization_info, "survival_info"=survival_info))
  } else if (mode == "both"){
    utilization_summary <- utilization_info %>%
      summarise_all(funs(calculate_utilization))
    utilization_summary <- as.numeric(utilization_summary[1,])
    survival_summary<- survival_info %>%
      summarise_all(funs(calculate_survival))
    survival_summary <- as.numeric(survival_summary[1,])
    return(list("utilization_info"=utilization_info, "survival_info"=survival_info, "utilization_summ"=utilization_summary, "survivial_summ"=survival_summary))
  } else {
    utilization_summary <- utilization_info %>%
      summarise_all(funs(calculate_utilization))
    utilization_summary <- as.numeric(utilization_summary[1,])
    survivial_summ <- survival_info %>%
      summarise_all(funs(calculate_survival))
    survivial_summ <- as.numeric(survivial_summ[1,])
    return(list("utilization_summ"=utilization_info, "survivial_summ"=survival_info))
  }
}

make_plot <- function(info_df, sample_size, window_size) {
  ## Here plot a histogram
  ggplot(info_df, aes(x=x, y=y)) +
    geom_point() + 
    ylab("Count of Runs") +
    xlab("Runs") + 
    ggtitle(paste("Runs for trace", sample_size, "and Window Size", window_size))
  ggsave(paste("Runs for trace", sample_size, "and Window Size", window_size, ".png"))
}

result_folder_path <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//Nonoverlapping windows//maxes//"
model_name <- "AR1_logistic_lm"
window_size <- 12
sample_size <- 3000
prob_cut_off <- 0.01
granularity <- 0.78125
state_num <- NA
cpu_usage <- 1

model_performance <- read_from_ar_result(model_name, window_size, sample_size, prob_cut_off, state_num, granularity, cpu_usage, result_folder_path, mode="raw")

## Check traces performance

## Check histogram of runs
distribution_of_runs <- compute_distribution_of_runs(model_performance$survival_info)[1:50]
distribution_of_runs.df <- data.frame(x=1:length(distribution_of_runs), y=distribution_of_runs)
make_plot(distribution_of_runs.df, sample_size, window_size)
