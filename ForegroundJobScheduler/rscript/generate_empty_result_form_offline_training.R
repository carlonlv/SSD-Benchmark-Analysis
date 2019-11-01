library("dplyr")
library("xlsx")

models <- c("AR1", "VAR1", "AR1_logistic_glm", "AR1_logistic_lm", "AR1_state_based_logistic", "AR1_Markov", "Markov")
statenum <- c(32, 64, 128)
prob_cut_offs <- c(0.005, 0.01, 0.1, 0.75)
granularity <- c(100/32, 100/64, 100/128, 0)
window_size <- c(12, 36)
sample_size <- c(100, 3000)
bin_num <- c(1000, 500)

prob_ban_pool <- c(0.75)

result.dp1 <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//Nonoverlapping windows//offline results//summary disjoint (windows,granularity).xlsx"
result.dp2 <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//Nonoverlapping windows//offline results//summary disjoint (windows,granularity) post adj.xlsx"
result.df <- expand.grid(models, statenum, prob_cut_offs, granularity, window_size, sample_size, bin_num, KEEP.OUT.ATTRS=FALSE, stringsAsFactors=FALSE)
colnames(result.df) <- c("Model", "StateNum", "Probability.Cut.Off", "Granularity", "Window.Size", "Sample.Size", "BinNum")
result.df$Avg.Cycle.Usage1 <- NA
result.df$Avg.Cycle.Usage2 <- NA
result.df$Survival.Rate <- NA
result.df$Correctly.Scheduled <- NA
result.df$Correctly.Unscheduled <- NA

result.df <- result.df %>%
  mutate(StateNum=ifelse(Model=="AR1_state_based_logistic", StateNum, 0)) %>%
  mutate(BinNum=ifelse(Model=="AR1_logistic_lm" | Model == "AR1_logistic_glm", BinNum, 0)) %>%
  mutate(Probability.Cut.Off=ifelse(Model!="AR1_logistic_lm" & Model!="AR1_logistic_glm" & Probability.Cut.Off %in% prob_ban_pool, NA, Probability.Cut.Off)) %>%
  filter(!is.na(Probability.Cut.Off)) %>%
  distinct() %>%
  arrange(Model, StateNum, Probability.Cut.Off, Granularity, Window.Size, Sample.Size)

write.xlsx(result.df, file = result.dp1, row.names = FALSE, showNA = FALSE)
write.xlsx(result.df, file = result.dp2, row.names = FALSE, showNA = FALSE)