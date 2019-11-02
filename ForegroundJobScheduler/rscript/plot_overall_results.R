library("xlsx")
library("readxl")
library("ggplot2")
library("dplyr")

plot_results <- function(model_results, sample_size, window_size, model_name, utilization_type=2) {
  model_results <- model_results %>% 
    filter(Sample.Size== sample_size & Window.Size == window_size & Model %in% model_name)  
  if (utilization_type == 1) {
    ggplot(model_results, aes(x=Survival.Rate, y=Avg.Cycle.Usage1)) +
      geom_point(na.rm=TRUE, aes(color=factor(Model), size=factor(BinNum), shape=factor(StateNum), alpha=factor(Granularity))) + 
      scale_shape_manual(values=c(21,22,23,24,25)) +
      geom_vline(xintercept=0.99, linetype="dashed", color="red") + 
      ylab("Utilization") +
      xlab("Survival Rate") + 
      ggtitle(paste("Model Performance With Sample Size", sample_size, "and Window Size", window_size))
  } else {
    ggplot(model_results, aes(x=Survival.Rate, y=Avg.Cycle.Usage2)) +
      geom_point(na.rm=TRUE, aes(color=factor(Model), size=factor(BinNum), shape=factor(StateNum), alpha=factor(Granularity))) + 
      scale_shape_manual(values=c(21,22,23,24,25)) +
      geom_vline(xintercept=0.99, linetype="dashed", color="red") + 
      ylab("Utilization") +
      xlab("Survival Rate") + 
      ggtitle(paste("Model Performance With Sample Size", sample_size, "and Window Size", window_size))
  }
  ggsave(paste("Model Performance With Sample Size", sample_size, "and Window Size", window_size, "dynamic adjustment.png"))
}

sample_size <- 100
window_size <- 12
adjustment <- TRUE
schedule_policy <- "dynamic"

data_path <- NULL
if (adjustment) {
  if (schedule_policy == "dynamic") {
    if (Sys.info()["sysname"] == "Windows") {
      data_path <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//offline results//summary dynamic (windows,granularity) post adj.xlsx"
    } else {
      data_path <- "/Users/carlonlv/Documents/Github/Research-Projects/ForegroundJobScheduler/results/offline results/summary dynamic (windows,granularity) post adj.xlsx"
    }
  } else {
    if (Sys.info()["sysname"] == "Windows") {
      data_path <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//offline results//summary disjoint (windows,granularity) post adj.xlsx"
    } else {
      data_path <- "/Users/carlonlv/Documents/Github/Research-Projects/ForegroundJobScheduler/results/offline results/summary disjoint (windows,granularity) post adj.xlsx"
    }
  }
} else {
  if (schedule_policy == "dynamic") {
    if (Sys.info()["sysname"] == "Windows") {
      data_path <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//offline results//summary dynamic (windows,granularity).xlsx"
    } else {
      data_path <- "/Users/carlonlv/Documents/Github/Research-Projects/ForegroundJobScheduler/results/offline results/summary dynamic (windows,granularity).xlsx"
    }
  } else {
    if (Sys.info()["sysname"] == "Windows") {
      data_path <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//offline results//summary disjoint (windows,granularity).xlsx"
    } else {
      data_path <- "/Users/carlonlv/Documents/Github/Research-Projects/ForegroundJobScheduler/results/offline results/summary disjoint (windows,granularity).xlsx"
    }
  }
}

model_names <- c("AR1", "VAR1", "Markov", "AR1_Markov", "AR1_state_based_logistic", "AR1_logistic_glm")

ar_xlsx <- read.xlsx(data_path, sheetIndex = 1)
plot_results(ar_xlsx, sample_size, window_size, model_name = model_names[c(1,3,4,6)])