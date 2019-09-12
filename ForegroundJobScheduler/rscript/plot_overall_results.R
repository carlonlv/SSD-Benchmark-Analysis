library("xlsx")
library("readxl")
library("ggplot2")
library("dplyr")

plot_results <- function(model_results, sample_size, window_size, model_name="NULL") {
  model_results <- model_results %>% 
    filter(Sample.Size== sample_size & Window.Size == window_size)
  if (model_name == "ar_logistic_lm") {
    ggplot(model_results, aes(x=Survival.Rate, y=Avg.Cycle.Usage)) +
      geom_point(na.rm=TRUE, aes(color=factor(Model), shape=factor(Cpu_usage), alpha=factor(Granularity))) + 
      geom_vline(xintercept=0.99, linetype="dashed", color="red") + 
      ylab("Utilization") +
      xlab("Survival Rate") + 
      ggtitle(paste("Model Performance With Sample Size", sample_size, "and Window Size", window_size))
    ggsave(paste("Model Performance of AR_Logistic_Lm With Sample Size", sample_size, "and Window Size", window_size, ".png"))
  } else {
    ggplot(model_results, aes(x=Survival.Rate, y=Avg.Cycle.Usage)) +
      geom_point(na.rm=TRUE, aes(color=factor(Model), shape=factor(StateNum), alpha=factor(Granularity))) + 
      scale_shape_manual(values=c(15, 16,17,18,21,22,23,24)) +
      geom_vline(xintercept=0.99, linetype="dashed", color="red") + 
      ylab("Utilization") +
      xlab("Survival Rate") + 
      ggtitle(paste("Model Performance With Sample Size", sample_size, "and Window Size", window_size))
    ggsave(paste("Model Performance With Sample Size", sample_size, "and Window Size", window_size, ".png"))
  }
}

ar_data_path <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//Nonoverlapping windows//summary (windows,granularity).xlsx"
#ar_data_path <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//Nonoverlapping windows//summary (windows,granularity) post adj.xlsx"
#mc_data_path <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//Nonoverlapping windows//mc summary granularity.csv"
sample_size <- 3000
window_size <- 36

ar_xlsx <- read.xlsx(ar_data_path, sheetIndex = 1)
#mc_xlsx <- read.csv(mc_data_path)
#colnames(mc_xlsx) <- c("Model", "Survival.Rate", "Avg.Cycle.Usage", "Sample.Size", "Window.Size", "StateNum","Probability.Cut.Off")

plot_results(ar_xlsx, sample_size, window_size)
#plot_results(mc_xlsx, sample_size, window_size)

ar_logistic_lm_data_path <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//Nonoverlapping windows//summary AR1_logistic_lm.xlsx"
ar_logistic_lm_xlsx <- read.xlsx(ar_logistic_lm_data_path, sheetIndex = 1)
plot_results(ar_logistic_lm_xlsx, sample_size, window_size, "ar_logistic_lm")