library("xlsx")
library("readxl")
library("ggplot2")
library("dplyr")

plot_results <- function(model_results, sample_size, window_size, model_name, usage_calculation=1) {
  model_results <- model_results %>% 
    filter(Sample.Size== sample_size & Window.Size == window_size & Model %in% model_name)  
  ggplot(model_results, aes(x=Survival.Rate, y=Avg.Cycle.Usage1)) +
    geom_point(na.rm=TRUE, aes(color=factor(BinNum), fill=factor(Model), shape=factor(StateNum), alpha=factor(Granularity))) + 
    scale_shape_manual(values=c(21,22,23,24,25)) +
    geom_vline(xintercept=0.99, linetype="dashed", color="red") + 
    ylab("Utilization") +
    xlab("Survival Rate") + 
    ggtitle(paste("Model Performance With Sample Size", sample_size, "and Window Size", window_size))
  ggsave(paste("Model Performance With Sample Size", sample_size, "and Window Size", window_size, ".png"))
}

data_path <- NULL
if (Sys.info()["sysname"] == "Windows") {
  data_path <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//offline results//summary dynamic (windows,granularity).xlsx"
} else {
  data_path <- "/Users/carlonlv/Documents/GitHub/Research-Projects/ForegroundJobScheduler/results/offline results/summary dynamic (windows,granularity).xlsx"
}

sample_size <- 100
window_size <- 12

ar_xlsx <- read.xlsx(data_path, sheetIndex = 1)
plot_results(ar_xlsx, sample_size, window_size, model_name = c("AR1_logistic_glm", "AR1", "VAR1"))

