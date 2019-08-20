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
      model_results <- rbind(model_results, c(paste(model, alpha), utilization, survival_rate))
    }
  }
  colnames(model_results) <- c("Model", "Utilization", "Survival")
  return(model_results)
}


read_from_mc_models_xlsx <- function() {
  
}


adjust_results <- function(model_results, quantile_thresh, sample_size, window_size, result_path) {
  
  for (model in model_results$Model) {
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
    
    
  }
}

plot_results <- function(model_results, sample_size, window_size) {
  ggplot(model_results, aes(x=Survival, y=Utilization, colour = factor(Model))) +
    geom_point(na.rm = TRUE) +
    xlim(0, 100) +
    ylim(0, 100)
    ylab("Utilization") +
    xlab("Survival Rate") + 
    ggtitle(paste("Model Performance With Sample Size", sample_size, "and Window Size", window_size))
  ggsave(paste("Model Performance With Sample Size", sample_size, "and Window Size", window_size, ".png"))
}

data_path <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//Nonoverlapping windows//summary (windows) max.xlsx"
sample_size <- 3000
window_size <- 12

ar_xlsx <- read.xlsx(data_path, sheetIndex = 1)

model_results <- data.frame(matrix(nrow = 0, ncol = 3))

model_results <- read_from_ar_models_xlsx(model_results, ar_xlsx, sample_size, window_size)

plot_results(model_results)

result_path <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//Nonoverlapping windows//maxes//"
