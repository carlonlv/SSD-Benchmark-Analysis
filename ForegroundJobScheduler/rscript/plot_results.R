library("xlsx")
library("readxl")
library("ggplot2")
library("dplyr")

read_from_ar_models_xlsx <- function(model_results, ar_xlsx, sample_size, window_size) {
  ar_xlsx <- ar_xlsx %>%
    filter(Sample.Size== sample_size & Job.Length.Window.Size == window_size)
  model_names <- unique(ar_xlsx$Model)
  for (model in model_names) {
    current_model <- ar_xlsx %>%
      filter(Model == model)
    for (alpha in current_model$Probability.Cut.Off) {
      utilization <- current_model$Avg.Cycle.Usage[current_model$Probability.Cut.Off == alpha]
      survival_rate <- current_model$Survival.Rate[current_model$Probability.Cut.Off == alpha]
      model_results <- cbind(model_results, c(utilization, survival_rate))
      colnames(model_results)[ncol(model_results)] <- paste(model, alpha)
    }
  }
  return(model_results)
}


read_from_mc_models_xlsx <- function() {
  
}


adjust_results <- function() {
  
}

plot_results <- function() {
  
}

data_path <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//Nonoverlapping windows//summary (windows) max.xlsx"
sample_size <- 3000
window_size <- 12

ar_xlsx <- read.xlsx(data_path, sheetIndex = 1)

model_results <- data.frame(matrix(nrow = 2, ncol = 0))

model_results <- read_from_ar_models_xlsx(model_results, ar_xlsx, sample_size, window_size)
