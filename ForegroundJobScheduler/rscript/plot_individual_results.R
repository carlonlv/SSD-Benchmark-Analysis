library("xlsx")
library("readxl")
library("ggplot2")
library("dplyr")


make_plot <- function(info_df, sample_size, window_size) {
  ## Here plot a histogram
  ggplot(info_df, aes(x=x, y=y)) +
    geom_point() + 
    ylab("Count of Runs") +
    xlab("Runs") + 
    ggtitle(paste("Runs for trace", sample_size, "and Window Size", window_size))
  ggsave(paste("Runs for trace", sample_size, "and Window Size", window_size, ".png"))
}


read_runs <- function(result_folder_path, variable_name, param_list) {
  
  sample_size <- param_list[["Sample.Size"]]
  window_size <- param_list[["Window.Size"]]
  model_name <- param_list[["Model"]]
  
  response <- param_list[[variable_name]]
  if (variable_name == "prob_cut_off") {
    
    granularity <- param_list[["Granularity"]]
    state_num <- param_list[["StateNum"]]
    bin_num <- param_list[["BinNum"]]
    
  } else if (variable_name == "granularity") {
    
    state_num <- param_list[["StateNum"]]
    bin_num <- param_list[["BinNum"]]
    prob_cutoff <- param_list[["Probability.Cut.Off"]]
  } else if (variable_name == "state_num") {
    
    granularity <- param_list[["Granularity"]]
    bin_num <- param_list[["BinNum"]]
    prob_cutoff <- param_list[["Probability.Cut.Off"]]
  } else if (variable_name == "bin_num") {
    
    granularity <- param_list[["Granularity"]]
    state_num <- param_list[["StateNum"]]
    prob_cutoff <- param_list[["Probability.Cut.Off"]]
  }
  
  if (model_name != "AR1_state_based_logistic") {
    state_num <- NA
  }
  
  if (model_name != "AR1_logistic_lm") {
    bin_num <- NA
  }
  
  
  total_runs <- data.frame()
  for (i in response) {
    filename <- NULL
    if (variable_name == "prob_cut_off") {
      filename <- paste("Overall Runs", "AR1_logistic_lm", sample_size, window_size, i, granularity, state_num, bin_num, ".csv")
    } else if (variable_name == "granularity") {
      filename <- paste("Overall Runs", "AR1_logistic_lm", sample_size, window_size, prob_cutoff, i, state_num, bin_num, ".csv")
    } else if (variable_name == "state_num") {
      filename <- paste("Overall Runs", "AR1_logistic_lm", sample_size, window_size, prob_cutoff, granularity, i, bin_num, ".csv")
    } else if (variable_name == "bin_num") {
      filename <- paste("Overall Runs", "AR1_logistic_lm", sample_size, window_size, prob_cutoff, granularity, state_num, i, ".csv")
    }
    runs <- read.csv(paste(result_folder_path, filename, sep=""))
    runs <- colSums(runs)
    
    total_runs <- rbind(runs)
  }
  rownames(total_runs) <- response
  colnames(total_runs) <- 1:20
  
  return(total_runs)
}


result_folder_path <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//Nonoverlapping windows//maxes//"
model_name <- "AR1_logistic_lm"
window_size <- 12
sample_size <- 3000

variable_name <- "BinNum"
variable_range <- c(50, 100, 200)

prob_cut_off <- 0.01
granularity <- 0.78125
state_num <- NA

other_param <- list(prob_cut_off=prob_cut_off, granularity=granularity, state_num=state_num)
conames <- c("Model", "StateNum", "Probability.Cut.Off", "Granularity", "Window.Size", "Sample.Size", "BinNum")

model_performance <- read_runs(model_name, window_size, sample_size, prob_cut_off, state_num, granularity, cpu_usage, result_folder_path, mode="raw")

## Check traces performance

## Check histogram of runs
distribution_of_runs <- compute_distribution_of_runs(model_performance$survival_info)[1:50]
distribution_of_runs.df <- data.frame(x=1:length(distribution_of_runs), y=distribution_of_runs)
make_plot(distribution_of_runs.df, sample_size, window_size)
