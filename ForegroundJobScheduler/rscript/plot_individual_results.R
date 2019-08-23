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

read_from_ar_result <- function(model, window_size, sample_size, prob_cut_off, state_num, datapath) {
  utilization_filename <- NULL
  survival_filename <- NULL
  if (is.na(state_num)) {
    utilization_filename <- paste(model, window_size, sample_size, prob_cut_off,"avg_usage.csv")
    survival_filename <- paste(model, window_size, sample_size, prob_cut_off, "job_survival.csv")
  } else {
    utilization_filename <- paste(model, window_size, state_num, sample_size, prob_cut_off, "avg_usage.csv")
    survival_filename <- paste(model, window_size, state_num, sample_size, prob_cut_off, "job_survival.csv")
  }
  utilization_dp <- paste(datapath, utilization_filename, sep = "")
  survival_dp <- paste(datapath, survival_filename, sep = "")
  utilization_info <- read.csv(utilization_dp)
  utilization_info <- utilization_info %>%
    summarise_all(funs(calculate_utilization))
  utilization_info <- as.numeric(utilization_info[1,])
  survival_info <- read.csv(survival_dp)
  survival_info <- survival_info %>%
    summarise_all(funs(calculate_survival))
  survival_info <- as.numeric(survival_info[1,])
  return(data.frame("utilization_info"=utilization_info, "survival_info"=survival_info))
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