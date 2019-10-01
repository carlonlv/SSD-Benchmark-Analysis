library("xlsx")
library("readxl")
library("ggplot2")
library("dplyr")


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


make_2d_density <- function(info_df, sample_size, window_size) {
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
sample_size <- 100

bin_num <- c(100, 200)
prob_cut_offs <- c(0.005, 0.01, 0.02, 0.1, 0.5, 0.75)
granularity <- c(10, 100/32, 100/128, 0)

state_num <- NA

parameter.df <- expand.grid(window_size, prob_cut_offs, granularity, bin_num)
colnames(parameter.df) <- c("window_size", "prob_cut_off", "granularity", "bin_num")
parameter.df <- parameter.df

total_runs <- sapply(1:nrow(parameter.df), read_runs, parameter.df, model_name, sample_size, result_folder_path, simplify=FALSE)

models <- c()
for (i in 1:nrow(parameter.df)) {
  param_list <- parameter.df[i,]
  window_size <- param_list[1]
  prob_cut_off <- param_list[2]
  granularity <- param_list[3]
  bin_num <- param_list[4]
  model <- paste(window_size, prob_cut_off, granularity, bin_num)
  models <- c(models, model)
}

total_runs.df <- expand.grid(models, 1:20) %>%
  rename(Model = Var1, Run = Var2) %>%
  mutate(Counts = NA) %>%
  arrange(Model)

for (i in 1:nrow(parameter.df)) {
  param_list <- parameter.df[i,]
  window_size <- param_list[1]
  prob_cut_off <- param_list[2]
  granularity <- param_list[3]
  bin_num <- param_list[4]
  
  model <- paste(window_size, prob_cut_off, granularity, bin_num) 
  for (j in 1:20) {
    total_runs.df %>%
      mutate(Counts = ifelse(Model == model & Run == j, as.numeric(total_runs[[i]][j]), Counts))
  }
}
