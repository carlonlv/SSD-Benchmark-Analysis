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


read_runs <- function(result_folder_path, model_name, window_size, sample_size, variable_name, variable_range, other_param) {
  
  if (variable_name == "prob_cut_off") {
  } else if (variable_name == "granularity") {
  } else if (variable_name == "state_num") {
  } else if (variable_name == "bin_num") {
  }
}


generate_info_df <- function()


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
