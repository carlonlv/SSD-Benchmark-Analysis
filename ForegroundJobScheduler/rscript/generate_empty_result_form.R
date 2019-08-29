library("dplyr")
library("xlsx")

Model <- c("AR1", "VAR1", "AR1_logistic_kmeans", "AR1_logistic_lm", "AR1_state_based_logistic")
StateNum <- c(10, 20, 30, 50)
Probability.Cut.Off <- c(0.005, 0.01, 0.02, 0.1, 0.125, 0.15, 0.175, 0.2)
Granularity <- c(100/32, 100/64, 100/128)
Window.Size <- c(12, 36)
Sample.Size <- c(100, 3000)

result.dp1 <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//Nonoverlapping windows//summary (windows,granularity).xlsx"
result.dp2 <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//Nonoverlapping windows//summary (windows,granularity) post adj.xlsx"
result.df <- expand.grid(Model, StateNum, Probability.Cut.Off, Granularity, Window.Size, Sample.Size, KEEP.OUT.ATTRS=FALSE, stringsAsFactors=FALSE)
colnames(result.df) <- c("Model", "StateNum", "Probability.Cut.Off", "Granularity", "Window.Size", "Sample.Size")

result.df <- result.df %>%
  mutate(StateNum=ifelse(Model=="AR1_state_based_logistic", StateNum, NA)) %>%
  mutate(Probability.Cut.Off=ifelse(Model!="AR1_logistic_kmeans" & Model!="AR1_logistic_lm" & (Probability.Cut.Off==0.125 | Probability.Cut.Off==0.15 | Probability.Cut.Off==0.175 | Probability.Cut.Off==0.2), NA, Probability.Cut.Off)) %>%
  filter(!is.na(Probability.Cut.Off)) %>%
  distinct() %>%
  arrange(Model, StateNum, Probability.Cut.Off, Granularity, Window.Size, Sample.Size) %>%
  mutate(Avg.Cycle.Usage=NA, Survival.Rate=NA, Correctly.Scheduled=NA, Correctly.Unscheduled=NA) 

write.xlsx(result.df, file = result.dp1, row.names = FALSE, showNA = FALSE)
write.xlsx(result.df, file = result.dp2, row.names = FALSE, showNA = FALSE)