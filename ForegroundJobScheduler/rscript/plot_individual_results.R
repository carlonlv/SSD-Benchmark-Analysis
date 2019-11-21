library("ggplot")
library("dpylr")

model_name <- "AR1_Markov"
schedule_policy <- "dynamic"
state_num <- 32
prob_cut_off <- 0.01
granularity <- 0
window_size <- 12
bin_num <- 0

simulation <- "online"

train_size <- 4000
update_freq <- 36

data_path <- NULL
if (simulation == "online") {
  if (Sys.info()["sysname"] == "Windows") {
    data_path <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//online results//ts_results//"
  } else {
    data_path <- "/Users/carlonlv/Documents/Github/Research-Projects/ForegroundJobScheduler/results/online results/ts_results/"
  }
} else {
  if (Sys.info()["sysname"] == "Windows") {
    data_path <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//offline results//ts_results//"
  } else {
    data_path <- "/Users/carlonlv/Documents/Github/Research-Projects/ForegroundJobScheduler/results/offline results/ts_results/"
  }
}

file_path <- NULL
if (simulation == "online")  {
  file_name <- paste(model_name, schedule_policy, state_num, prob_cut_off, granularity, window_size, bin_num, train_size, update_freq)
  file_path <- paste0(data_path, file_name)
} else {
  file_name <- paste(model_name, schedule_policy, state_num, prob_cut_off, granularity, window_size, bin_num)
  file_path <- paste0(data_path, file_name)
}

