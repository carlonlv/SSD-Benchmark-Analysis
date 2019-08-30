library("xlsx")
library("readxl")
library("ggplot2")
library("dplyr")

read_from_models_xlsx <- function(model_results, xlsx, sample_size, window_size) {
  xlsx <- xlsx %>%
    filter(Sample.Size== sample_size & Window.Size == window_size)
  model_names <- unique(xlsx$Model)
  for (model in model_names) {
    current_model <- xlsx %>%
      filter(Model == model)
    for (alpha in current_model$Probability.Cut.Off) {
      current_model_prob <- current_model %>%
        filter(Probability.Cut.Off == alpha)
      for (granularity in current_model_prob$Granularity) {
        current_model_prob_gran <- current_model_prob %>%
          filter(Granularity == granularity)
        
        if (all(is.na(current_model_prob_gran$StateNum))) {
          
          utilization <- current_model_prob_gran$Avg.Cycle.Usage
          survival_rate <- current_model_prob_gran$Survival.Rate
          
          result <- data.frame("Model" = model, "Prob_Cut_Off" = alpha, "StateNum" = 0, "Granularity" = granularity, "Utilization" = utilization, "Survival" = survival_rate)
          model_results <- rbind(model_results, result)
        } else {
          
          for (stateNum in current_model_prob_gran$StateNum) {
            current_model_prob_gran_stateNum <- current_model_prob_gran %>%
              filter(StateNum == stateNum)
            utilization <- current_model_prob_gran_stateNum$Avg.Cycle.Usage
            survival_rate <- current_model_prob_gran_stateNum$Survival.Rate
            
            result <- data.frame("Model" = model, "Prob_Cut_Off" = alpha, "StateNum" = stateNum, "Granularity" = granularity, "Utilization" = utilization, "Survival" = survival_rate)
            model_results <- rbind(model_results, result)
          }
        }
        
      }
    }
  }
  return(model_results)
}

plot_results <- function(model_results, sample_size, window_size) {
  ggplot(model_results, aes(x=Survival, y=Utilization, group=factor(Model), colour=factor(Model))) +
    geom_point(na.rm=TRUE, aes(shape=factor(StateNum), alpha=factor(Granularity))) + 
    scale_shape_manual(values=c(16,17,18,0,1,2,6)) +
    geom_vline(xintercept=0.99, linetype="dashed", color="red") + 
    ylab("Utilization") +
    xlab("Survival Rate") + 
    ggtitle(paste("Model Performance With Sample Size", sample_size, "and Window Size", window_size))
  ggsave(paste("Model Performance With Sample Size", sample_size, "and Window Size", window_size, ".png"))
}

ar_data_path <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//Nonoverlapping windows//summary (windows,granularity).xlsx"
#ar_data_path <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//Nonoverlapping windows//summary (windows,granularity) post adj.xlsx"
#mc_data_path <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//Nonoverlapping windows//mc summary.xlsx"
sample_size <- 100
window_size <- 12

ar_xlsx <- read.xlsx(ar_data_path, sheetIndex = 1)
#mc_xlsx <- read.xlsx(mc_data_path, sheetIndex = 1)

model_results <- data.frame(matrix(nrow = 0, ncol = 5), stringsAsFactors = FALSE)
colnames(model_results) <- c("Model", "Prob_Cut_Off",  "StateNum", "Utilization", "Survival")
model_results <- read_from_models_xlsx(model_results, ar_xlsx, sample_size, window_size)

plot_results(model_results, sample_size, window_size)
