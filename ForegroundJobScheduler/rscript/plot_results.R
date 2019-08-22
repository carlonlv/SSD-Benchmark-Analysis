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
        
        result <- c(model, alpha, NA, utilization, survival_rate)
        model_results <- rbind(model_results, result)
      } else {
        
        for (stateNum in current_model_prob$StateNum) {
          current_model_prob_stateNum <- current_model_prob %>%
            filter(StateNum == stateNum)
          utilization <- current_model_prob_stateNum$Avg.Cycle.Usage
          survival_rate <- current_model_prob_stateNum$Survival.Rate
          
          result <- c(model, alpha, stateNum, utilization, survival_rate)
          model_results <- rbind(model_results, result)
        }
      }
    }
  }
  colnames(model_results) <- c("Model", "Prob_Cut_Off",  "StateNum", "Utilization", "Survival")
  return(model_results)
}

adjust_results <- function(model_results, quantile_thresh, sample_size, window_size, result_path) {
  
  for (model in model_results$Model) {
    if (!is.na(model_results$Utilization[model_results$Model == model]) & !is.na(model_results$Survival[model_results$Model == model])) {
      filename <- NULL
      model_name <- strsplit(model, " ")[[1]][1]
      prob_cut_off <- as.numeric(strsplit(model, " ")[[1]][2])
      if (grepl("AR1_logistic_state", model_name)) {
        num_of_states <- as.numeric(regmatches(model_name, gregexpr("[[:digit:]]+", model_name))[[1]][2])
        filename <- paste(strsplit(model_name, "(")[[1]][1], window_size, num_of_states, sample_size, prob_cut_off)
      } else {
        filename <- paste(model_name, window_size, sample_size, prob_cut_off)
      }
      end_pat <- c(" avg_usage.csv", " job_survival.csv")
      
      utilization <- read.csv(paste(result_path, filename, end_pat[1], sep = ""))
      survival <- read.csv(paste(result_path, filename, end_pat[2], sep = ""))
      
      utilization %>%
        summarize_at(.cols=1:ncol(utilization), funs(sum(., na.rm=TRUE)))
    }
  }
}

plot_results <- function(model_results, sample_size, window_size) {
  ggplot(model_results, aes(x=Survival, y=Utilization, colour = factor(Model))) +
    geom_point(na.rm = TRUE) +
    xlim(0, 1) +
    ylim(0, 1) + 
    ylab("Utilization") +
    xlab("Survival Rate") + 
    ggtitle(paste("Model Performance With Sample Size", sample_size, "and Window Size", window_size))
  ggsave(paste("Model Performance With Sample Size", sample_size, "and Window Size", window_size, ".png"))
}

ar_data_path <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//Nonoverlapping windows//summary (windows) max.xlsx"
mc_data_path <- ""
sample_size <- 100
window_size <- 36

ar_xlsx <- read.xlsx(ar_data_path, sheetIndex = 1)
mc_xlsx <- read.xlsx(mc_data_path, sheetIndex = 1)

model_results <- data.frame(matrix(nrow = 0, ncol = 5), stringsAsFactors = FALSE)
model_results <- read_from_models_xlsx(model_results, ar_xlsx, sample_size, window_size)
model_results <- read_from_models_xlsx(model_results, mc_xlsx, sample_size, window_size)

plot_results(model_results, sample_size, window_size)

result_path <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//Nonoverlapping windows//maxes//"
