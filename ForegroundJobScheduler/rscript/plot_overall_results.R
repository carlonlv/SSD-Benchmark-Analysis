library("ggplot2")
library("dplyr")

plot_results <- function(model_results, sample_size, window_size, model_name, utilization_type=2, schedule_policy, simulation, adjustment) {
  model_results <- model_results %>% 
    filter(Sample.Size== sample_size & Window.Size == window_size & Model %in% model_name)
  
  model_results$Model <- factor(model_results$Model)
  model_results$Granularity <- factor(model_results$Granularity)
  model_results$StateNum <- factor(model_results$StateNum)
  model_results$BinNum <- factor(model_results$BinNum)
  
  ## Check contains bin_num
  contains_bin_num <- ifelse(all(model_results$BinNum == 0), FALSE, TRUE)
  ## Check contains state_num
  contains_state_num <- ifelse(all(model_results$StateNum == 0), FALSE, TRUE)
  
  plt <- NULL
  if (simulation == "online") {
    
    model_results$Training.Size <- factor(model_results$Training.Size)
    model_results$Update.Freq <- factor(model_results$Update.Freq)
    if (utilization_type == 1) {
      if (contains_bin_num & contains_state_num) {
        stop("Not enough spaces.")
      } else if (contains_bin_num) {
        plt <- ggplot(model_results, aes(x=Survival.Rate, y=Avg.Cycle.Usage1)) +
          geom_point(na.rm=TRUE, aes(fill=factor(Model), shape=factor(BinNum), alpha=factor(Granularity)), colour=factor(Training.Size))
      } else if (contains_state_num) {
        plt <- ggplot(model_results, aes(x=Survival.Rate, y=Avg.Cycle.Usage1)) +
          geom_point(na.rm=TRUE, aes(fill=factor(Model), shape=factor(StateNum), alpha=factor(Granularity)), colour=factor(Training.Size))
      } else {
        plt <- ggplot(model_results, aes(x=Survival.Rate, y=Avg.Cycle.Usage1)) +
          geom_point(na.rm=TRUE, aes(fill=factor(Model), alpha=factor(Granularity)), colour=factor(Training.Size))
      }
    } else {
      if (contains_bin_num & contains_state_num) {
        stop("Not enough spaces.")
      } else if (contains_bin_num) {
        plt <- ggplot(model_results, aes(x=Survival.Rate, y=Avg.Cycle.Usage2)) +
          geom_point(na.rm=TRUE, aes(fill=factor(Model), shape=factor(BinNum), alpha=factor(Granularity), colour=factor(Training.Size)))
      } else if (contains_state_num) {
        plt <- ggplot(model_results, aes(x=Survival.Rate, y=Avg.Cycle.Usage2)) +
          geom_point(na.rm=TRUE, aes(fill=factor(Model), shape=factor(StateNum), alpha=factor(Granularity), colour=factor(Training.Size)))
      } else {
        plt <- ggplot(model_results, aes(x=Survival.Rate, y=Avg.Cycle.Usage2)) +
          geom_point(na.rm=TRUE, aes(fill=factor(Model), alpha=factor(Granularity), colour=factor(Training.Size)))
      }
    }
    plt <- plt + 
      scale_shape_manual(values=21:25) +
      scale_colour_manual(values=c("black", "white")) +
      guides(fill = guide_legend(override.aes=list(shape=21))) +
      geom_vline(xintercept=0.99, linetype="dashed", color="red") + 
      ylab("Utilization") +
      xlab("Survival Rate") + 
      ggtitle(paste("Model Performance With Sample Size", sample_size, "and Window Size", window_size))
    
  } else {
  
    if (utilization_type == 1) {
      if (contains_bin_num & contains_state_num) {
        stop("Not enough spaces.")
      } else if (contains_bin_num) {
        plt <- ggplot(model_results, aes(x=Survival.Rate, y=Avg.Cycle.Usage1)) +
          geom_point(na.rm=TRUE, aes(fill=factor(Model), shape=factor(BinNum), alpha=factor(Granularity)))
      } else if (contains_state_num) {
        plt <- ggplot(model_results, aes(x=Survival.Rate, y=Avg.Cycle.Usage1)) +
          geom_point(na.rm=TRUE, aes(fill=factor(Model), shape=factor(StateNum), alpha=factor(Granularity)))
      } else {
        plt <- ggplot(model_results, aes(x=Survival.Rate, y=Avg.Cycle.Usage1)) +
          geom_point(na.rm=TRUE, aes(fill=factor(Model), alpha=factor(Granularity)))
      }
    } else {
      if (contains_bin_num & contains_state_num) {
        stop("Not enough spaces.")
      } else if (contains_bin_num) {
        plt <- ggplot(model_results, aes(x=Survival.Rate, y=Avg.Cycle.Usage2)) +
          geom_point(na.rm=TRUE, aes(fill=factor(Model), shape=factor(BinNum), alpha=factor(Granularity)))
      } else if (contains_state_num) {
        plt <- ggplot(model_results, aes(x=Survival.Rate, y=Avg.Cycle.Usage2)) +
          geom_point(na.rm=TRUE, aes(fill=factor(Model), shape=factor(StateNum), alpha=factor(Granularity)))
      } else {
        plt <- ggplot(model_results, aes(x=Survival.Rate, y=Avg.Cycle.Usage2)) +
          geom_point(na.rm=TRUE, aes(fill=factor(Model), alpha=factor(Granularity)))
      }
    }
    plt <- plt + 
      scale_shape_manual(values=21:25) +
      guides(fill = guide_legend(override.aes=list(shape=21))) +
      geom_vline(xintercept=0.99, linetype="dashed", color="red") + 
      ylab("Utilization") +
      xlab("Survival Rate") + 
      ggtitle(paste("Model Performance With Sample Size", sample_size, "and Window Size", window_size))
  }
  
  ggsave(paste0(simulation, " Sample Size ", sample_size, " Window Size ", window_size, " ", schedule_policy, " ", ifelse(adjustment, " adjustment", ""), ".png"))
}


sample_size <- 100
window_size <- 12
adjustment <- FALSE
schedule_policy <- "dynamic"
simulation <- "offline"

data_path <- NULL
if (adjustment) {
  if (schedule_policy == "dynamic") {
    if (simulation == "online") {
      if (Sys.info()["sysname"] == "Windows") {
        data_path <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//online results//summary dynamic (windows,granularity) post adj.csv"
      } else {
        data_path <- "/Users/carlonlv/Documents/Github/Research-Projects/ForegroundJobScheduler/results/online results/summary dynamic (windows,granularity) post adj.csv"
      }
    } else {
      if (Sys.info()["sysname"] == "Windows") {
        data_path <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//offline results//summary dynamic (windows,granularity) post adj.csv"
      } else {
        data_path <- "/Users/carlonlv/Documents/Github/Research-Projects/ForegroundJobScheduler/results/offline results/summary dynamic (windows,granularity) post adj.csv"
      }
    }
  } else {
    if (simulation == "online") {
      if (Sys.info()["sysname"] == "Windows") {
        data_path <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//online results//summary disjoint (windows,granularity) post adj.csv"
      } else {
        data_path <- "/Users/carlonlv/Documents/Github/Research-Projects/ForegroundJobScheduler/results/online results/summary disjoint (windows,granularity) post adj.csv"
      }
    } else {
      if (Sys.info()["sysname"] == "Windows") {
        data_path <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//offline results//summary disjoint (windows,granularity) post adj.csv"
      } else {
        data_path <- "/Users/carlonlv/Documents/Github/Research-Projects/ForegroundJobScheduler/results/offline results/summary disjoint (windows,granularity) post adj.csv"
      }
    }
  }
} else {
  if (simulation == "online") {
    if (schedule_policy == "dynamic") {
      if (Sys.info()["sysname"] == "Windows") {
        data_path <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//online results//summary dynamic (windows,granularity).csv"
      } else {
        data_path <- "/Users/carlonlv/Documents/Github/Research-Projects/ForegroundJobScheduler/results/online results/summary dynamic (windows,granularity).csv"
      }
    } else {
      if (Sys.info()["sysname"] == "Windows") {
        data_path <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//online results//summary disjoint (windows,granularity).csv"
      } else {
        data_path <- "/Users/carlonlv/Documents/Github/Research-Projects/ForegroundJobScheduler/results/online results/summary disjoint (windows,granularity).csv"
      }
    }
  } else {
    if (schedule_policy == "dynamic") {
      if (Sys.info()["sysname"] == "Windows") {
        data_path <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//offline results//summary dynamic (windows,granularity).csv"
      } else {
        data_path <- "/Users/carlonlv/Documents/Github/Research-Projects/ForegroundJobScheduler/results/offline results/summary dynamic (windows,granularity).csv"
      }
    } else {
      if (Sys.info()["sysname"] == "Windows") {
        data_path <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//offline results//summary disjoint (windows,granularity).csv"
      } else {
        data_path <- "/Users/carlonlv/Documents/Github/Research-Projects/ForegroundJobScheduler/results/offline results/summary disjoint (windows,granularity).csv"
      }
    }
  }
}


model_names <- c("AR1", "VAR1", "Markov", "AR1_Markov", "AR1_state_based_logistic", "AR1_logistic_lm", "AR1_logistic_glm")

ar_results <- read.csv(data_path)
plot_results(ar_results, sample_size, window_size, model_name = model_names[c(1,2,3,4,5)], 2, schedule_policy, simulation, adjustment)

