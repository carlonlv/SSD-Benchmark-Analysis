library("dplyr")

models <- c("AR1", "VAR1", "AR1_logistic_glm", "AR1_logistic_lm", "AR1_state_based_logistic", "AR1_Markov", "Markov")
statenum <- c(8, 16, 32, 64)
prob_cut_offs <- c(0.0025, 0.005, 0.01, 0.1)
granularity <- c(100/32, 100/64, 100/128, 0)
window_size <- c(12, 36)
sample_size <- c(100, 3000)
bin_num <- c(1000, 500)

train_size <- c(2000, 4000)
update_freq <- 3 * window_size

mode <- "offline"
schedule_policy <- "dynamic"

result.dp1 <- NULL
result.dp2 <- NULL
if (Sys.info()["sysname"] == "Windows") {
  result.dp1 <- paste0("C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//", paste(mode, "results"), "//", paste("summary", schedule_policy), ".csv")
  result.dp2 <- paste0("C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//", paste(mode, "results"), "//", paste("summary", schedule_policy, "post adj"), ".csv")
} else if (Sys.info()["sysname"] == "Darwin") {
  result.dp1 <- paste0("/Users/carlonlv/Documents/GitHub/Research-Projects/ForegroundJobScheduler/results/", paste(mode, "results"), "/", paste("summary", schedule_policy), ".csv")
  result.dp2 <- paste0("/Users/carlonlv/Documents/GitHub/Research-Projects/ForegroundJobScheduler/results/", paste(mode, "results"), "/", paste("summary", schedule_policy, "post adj"), ".csv")
} else {
  result.dp1 <- paste0("/home/jialun/Research-Projects/ForegroundJobScheduler/results/", paste(mode, "results"), "/", paste("summary", schedule_policy), ".csv")
  result.dp2 <- paste0("/home/jialun/Research-Projects/ForegroundJobScheduler/results/", paste(mode, "results"), "/", paste("summary", schedule_policy, "post adj"), ".csv")
}


if (mode == "online") {
  result.df <- expand.grid(models, statenum, prob_cut_offs, granularity, window_size, sample_size, bin_num, train_size, update_freq, KEEP.OUT.ATTRS=FALSE, stringsAsFactors=FALSE)
  colnames(result.df) <- c("Model", "StateNum", "Probability.Cut.Off", "Granularity", "Window.Size", "Sample.Size", "BinNum", "Training.Size", "Update.Freq")
} else {
  result.df <- expand.grid(models, statenum, prob_cut_offs, granularity, window_size, sample_size, bin_num, KEEP.OUT.ATTRS=FALSE, stringsAsFactors=FALSE)
  colnames(result.df) <- c("Model", "StateNum", "Probability.Cut.Off", "Granularity", "Window.Size", "Sample.Size", "BinNum")
}


result.df$Avg.Cycle.Usage <- NA
result.df$Agg.Cycle.Usage <- NA
result.df$Avg.Survival.Rate <- NA
result.df$Agg.Survival.Rate <- NA
result.df$Correctly.Scheduled <- NA
result.df$Correctly.Unscheduled <- NA

result.df <- result.df %>%
  mutate(StateNum=ifelse(Model %in% c("AR1_state_based_logistic", "Markov", "AR1_Markov"), StateNum, 0)) %>%
  mutate(BinNum=ifelse(Model %in% c("AR1_logistic_lm", "AR1_logistic_glm"), BinNum, 0)) %>%
  distinct() %>%
  arrange(Update.Freq >= 2 * Window.Size)

if (mode == "offline") {
  result.df <- result.df %>%
    filter()
}

write.csv(result.df, file = result.dp1, row.names = FALSE)
write.csv(result.df, file = result.dp2, row.names = FALSE)