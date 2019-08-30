library("xlsx")
library("readxl")
library("ggplot2")
library("dplyr")

plot_results <- function(model_results, sample_size, window_size) {
  model_results <- model_results %>% 
    filter(Sample.Size== sample_size & Window.Size == window_size)
  ggplot(model_results, aes(x=Survival.Rate, y=Avg.Cycle.Usage)) +
    geom_point(na.rm=TRUE, aes(color=factor(Model))) + 
    scale_shape_manual(values=c(16,17,18,21,22,23,24)) +
    geom_vline(xintercept=0.99, linetype="dashed", color="red") + 
    ylab("Utilization") +
    xlab("Survival Rate") + 
    ggtitle(paste("Model Performance With Sample Size", sample_size, "and Window Size", window_size))
  ggsave(paste("Model Performance With Sample Size", sample_size, "and Window Size", window_size, ".png"))
}

#ar_data_path <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//Nonoverlapping windows//summary (windows,granularity).xlsx"
#ar_data_path <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//Nonoverlapping windows//summary (windows,granularity) post adj.xlsx"
mc_data_path <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//Nonoverlapping windows//mc summary granularity.csv"
sample_size <- 3000
window_size <- 12

ar_xlsx <- read.xlsx(ar_data_path, sheetIndex = 1)
ar_xlsx <- ar_xlsx %>%
  mutate(StateNum=ifelse(is.na(StateNum), 0, StateNum))

mc_xlsx <- read.csv(mc_data_path)

#model_results <- data.frame(matrix(nrow = 0, ncol = 5), stringsAsFactors = FALSE)
#colnames(model_results) <- c("Model", "Prob_Cut_Off",  "StateNum", "Utilization", "Survival")
#model_results <- read_from_models_xlsx(model_results, ar_xlsx, sample_size, window_size)

plot_results(mc_xlsx, sample_size, window_size)
