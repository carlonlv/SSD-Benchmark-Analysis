library("dplyr")

models <- c("AR1", "VAR1", "AR1_logistic_glm", "AR1_logistic_lm", "AR1_state_based_logistic", "AR1_Markov", "Markov")
statenum <- c(8, 16, 32, 64)
prob_cut_offs <- c(0.01, 0.1)
granularity <- c(100/32, 0)
window_size <- c(12, 36)
sample_size <- c(100, 3000)
bin_num <- c(1000, 500)
train_size <- c(2000, 4000)

schedule_policy <- "disjoint"

result.dp1 <- NULL
result.dp2 <- NULL
if (Sys.info()["sysname"] == "Windows") {
  if (schedule_policy == "dynamic") {
    result.dp1 <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//online results//summary dynamic (windows,granularity).csv"
    result.dp2 <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//online results//summary dynamic (windows,granularity) post adj.csv"
  } else {
    result.dp1 <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//online results//summary disjoint (windows,granularity).csv"
    result.dp2 <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//online results//summary disjoint (windows,granularity) post adj.csv"
  }
} else {
  if (schedule_policy == "dynamic") {
    result.dp1 <- "/Users/carlonlv/Documents/GitHub/Research-Projects/ForegroundJobScheduler/results/online results/summary dynamic (windows,granularity).csv"
    result.dp2 <- "/Users/carlonlv/Documents/GitHub/Research-Projects/ForegroundJobScheduler/results/online results/summary dynamic (windows,granularity) post adj.csv"
  } else {
    result.dp1 <- "/Users/carlonlv/Documents/GitHub/Research-Projects/ForegroundJobScheduler/results/online results/summary disjoint (windows,granularity).csv"
    result.dp2 <- "/Users/carlonlv/Documents/GitHub/Research-Projects/ForegroundJobScheduler/results/online results/summary disjoint (windows,granularity) post adj.csv"
  }
}

result.df <- expand.grid(models, statenum, prob_cut_offs, granularity, window_size, sample_size, bin_num, train_size, KEEP.OUT.ATTRS=FALSE, stringsAsFactors=FALSE)
colnames(result.df) <- c("Model", "StateNum", "Probability.Cut.Off", "Granularity", "Window.Size", "Sample.Size", "BinNum", "Training Size")
result.df$Update.Freq <- 3 * result.df$Window.Size
result.df$Avg.Cycle.Usage1 <- NA
result.df$Avg.Cycle.Usage2 <- NA
result.df$Survival.Rate <- NA
result.df$Correctly.Scheduled <- NA
result.df$Correctly.Unscheduled <- NA

result.df <- result.df %>%
  mutate(StateNum=ifelse(Model=="AR1_state_based_logistic"| Model=="Markov" | Model=="AR1_Markov", StateNum, 0)) %>%
  mutate(BinNum=ifelse(Model=="AR1_logistic_lm" | Model == "AR1_logistic_glm", BinNum, 0)) %>%
  filter(!is.na(Probability.Cut.Off)) %>%
  distinct() %>%
  arrange(Model, StateNum, Probability.Cut.Off, Granularity, Window.Size, Sample.Size)

write.csv(result.df, file = result.dp1, row.names = FALSE)
write.csv(result.df, file = result.dp2, row.names = FALSE)