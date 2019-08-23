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
      if (all(is.na(current_model_prob$StateNum))) {
        
        utilization <- current_model_prob$Avg.Cycle.Usage
        survival_rate <- current_model_prob$Survival.Rate
        
        result <- data.frame("Model" = model, "Prob_Cut_Off" = alpha, "StateNum" = 0, "Utilization" = utilization, "Survival" = survival_rate)
        model_results <- rbind(model_results, result)
      } else {
        
        for (stateNum in current_model_prob$StateNum) {
          current_model_prob_stateNum <- current_model_prob %>%
            filter(StateNum == stateNum)
          utilization <- current_model_prob_stateNum$Avg.Cycle.Usage
          survival_rate <- current_model_prob_stateNum$Survival.Rate
          
          result <- data.frame("Model" = model, "Prob_Cut_Off" = alpha, "StateNum" = stateNum, "Utilization" = utilization, "Survival" = survival_rate)
          model_results <- rbind(model_results, result)
        }
      }
    }
  }
  return(model_results)
}

plot_results <- function(model_results, sample_size, window_size) {
  ggplot(model_results, aes(x=Survival, y=Utilization, group=factor(Model))) +
    geom_line(na.rm = TRUE, aes(linetype=as.factor(StateNum), colour=factor(Model))) +
    geom_point(na.rm = TRUE, aes(shape=as.factor(StateNum), colour=factor(Model))) + 
    ylab("Utilization") +
    xlab("Survival Rate") + 
    ggtitle(paste("Model Performance With Sample Size", sample_size, "and Window Size", window_size))
  ggsave(paste("Model Performance With Sample Size", sample_size, "and Window Size", window_size, ".png"))
}

ar_data_path <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//Nonoverlapping windows//summary (windows) max.xlsx"
#mc_data_path <- ""
sample_size <- 100
window_size <- 12

ar_xlsx <- read.xlsx(ar_data_path, sheetIndex = 1)
#mc_xlsx <- read.xlsx(mc_data_path, sheetIndex = 1)

model_results <- data.frame(matrix(nrow = 0, ncol = 5), stringsAsFactors = FALSE)
colnames(model_results) <- c("Model", "Prob_Cut_Off",  "StateNum", "Utilization", "Survival")
model_results <- read_from_models_xlsx(model_results, ar_xlsx, sample_size, window_size)
#model_results <- read_from_models_xlsx(model_results, mc_xlsx, sample_size, window_size)

plot_results(model_results, sample_size, window_size)
