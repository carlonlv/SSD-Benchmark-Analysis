library("ggplot2")
library("dplyr")

plot_results <- function(model_results, sample_size, window_size, model_name, utilization_type=2, schedule_policy, simulation, adjustment) {
  model_results <- model_results %>% 
    filter(Sample.Size== sample_size & Window.Size == window_size & Model %in% model_name)
  
  if (simulation == "online") {
    if (utilization_type == 1) {
      ## Check contains bin_num
      contains_bin_num <- ifelse(all(model_results$BinNum == 0), FALSE, TRUE)
      ## Check contains state_num
      contains_state_num <- ifelse(all(model_results$StateNum == 0), FALSE, TRUE)
      
      plt <- NULL
      if (contains_bin_num & contains_state_num) {
        stop("Not enough spaces.")
      } else if (contains_bin_num) {
        plt <- ggplot(model_results, aes(x=Survival.Rate, y=Avg.Cycle.Usage1)) +
          geom_point(na.rm=TRUE, aes(color=factor(Model), shape=factor(StateNum), alpha=factor(Granularity)), size=factor(Training.Size))
      } else if (contains_state_num) {
        plt <- ggplot(model_results, aes(x=Survival.Rate, y=Avg.Cycle.Usage1)) +
          geom_point(na.rm=TRUE, aes(color=factor(Model), shape=factor(BinNum), alpha=factor(Granularity)))
      } else {
        plt <- ggplot(model_results, aes(x=Survival.Rate, y=Avg.Cycle.Usage1)) +
          geom_point(na.rm=TRUE, aes(color=factor(Model), alpha=factor(Granularity)))
      }
      plt <- plt + 
        scale_shape_manual(values=20:25) +
        geom_vline(xintercept=0.99, linetype="dashed", color="red") + 
        ylab("Utilization") +
        xlab("Survival Rate") + 
        ggtitle(paste("Model Performance With Sample Size", sample_size, "and Window Size", window_size))
      
    } else {
      ## Check contains bin_num
      contains_bin_num <- ifelse(all(model_results$BinNum == 0), FALSE, TRUE)
      ## Check contains state_num
      contains_state_num <- ifelse(all(model_results$StateNum == 0), FALSE, TRUE)
      
      plt <- NULL
      if (contains_bin_num & contains_state_num) {
        stop("Not enough spaces.")
      } else if (contains_bin_num) {
        plt <- ggplot(model_results, aes(x=Survival.Rate, y=Avg.Cycle.Usage2)) +
          geom_point(na.rm=TRUE, aes(color=factor(Model), shape=factor(StateNum), alpha=factor(Granularity)), size=factor(Training.Size))
      } else if (contains_state_num) {
        plt <- ggplot(model_results, aes(x=Survival.Rate, y=Avg.Cycle.Usage2)) +
          geom_point(na.rm=TRUE, aes(color=factor(Model), shape=factor(BinNum), alpha=factor(Granularity)))
      } else {
        plt <- ggplot(model_results, aes(x=Survival.Rate, y=Avg.Cycle.Usage2)) +
          geom_point(na.rm=TRUE, aes(color=factor(Model), alpha=factor(Granularity)))
      }
      plt <- plt + 
        scale_shape_manual(values=20:25) +
        geom_vline(xintercept=0.99, linetype="dashed", color="red") + 
        ylab("Utilization") +
        xlab("Survival Rate") + 
        ggtitle(paste("Model Performance With Sample Size", sample_size, "and Window Size", window_size))
    }
    
  } else {
    if (utilization_type == 1) {
      ## Check contains bin_num
      contains_bin_num <- ifelse(all(model_results$BinNum == 0), FALSE, TRUE)
      ## Check contains state_num
      contains_state_num <- ifelse(all(model_results$StateNum == 0), FALSE, TRUE)
      
      plt <- NULL
      if (contains_bin_num & contains_state_num) {
        stop("Not enough spaces.")
      } else if (contains_bin_num) {
        plt <- ggplot(model_results, aes(x=Survival.Rate, y=Avg.Cycle.Usage1)) +
          geom_point(na.rm=TRUE, aes(color=factor(Model), shape=factor(StateNum), alpha=factor(Granularity)))
      } else if (contains_state_num) {
        plt <- ggplot(model_results, aes(x=Survival.Rate, y=Avg.Cycle.Usage1)) +
          geom_point(na.rm=TRUE, aes(color=factor(Model), shape=factor(BinNum), alpha=factor(Granularity)))
      } else {
        plt <- ggplot(model_results, aes(x=Survival.Rate, y=Avg.Cycle.Usage1)) +
          geom_point(na.rm=TRUE, aes(color=factor(Model), alpha=factor(Granularity)))
      }
      plt <- plt + 
        scale_shape_manual(values=20:25) +
        geom_vline(xintercept=0.99, linetype="dashed", color="red") + 
        ylab("Utilization") +
        xlab("Survival Rate") + 
        ggtitle(paste("Model Performance With Sample Size", sample_size, "and Window Size", window_size))
      
    } else {
      ## Check contains bin_num
      contains_bin_num <- ifelse(all(model_results$BinNum == 0), FALSE, TRUE)
      ## Check contains state_num
      contains_state_num <- ifelse(all(model_results$StateNum == 0), FALSE, TRUE)
      
      plt <- NULL
      if (contains_bin_num & contains_state_num) {
        stop("Not enough spaces.")
      } else if (contains_bin_num) {
        plt <- ggplot(model_results, aes(x=Survival.Rate, y=Avg.Cycle.Usage2)) +
          geom_point(na.rm=TRUE, aes(color=factor(Model), shape=factor(StateNum), alpha=factor(Granularity)))
      } else if (contains_state_num) {
        plt <- ggplot(model_results, aes(x=Survival.Rate, y=Avg.Cycle.Usage2)) +
          geom_point(na.rm=TRUE, aes(color=factor(Model), shape=factor(BinNum), alpha=factor(Granularity)))
      } else {
        plt <- ggplot(model_results, aes(x=Survival.Rate, y=Avg.Cycle.Usage2)) +
          geom_point(na.rm=TRUE, aes(color=factor(Model), alpha=factor(Granularity)))
      }
      plt <- plt + 
        scale_shape_manual(values=20:25) +
        geom_vline(xintercept=0.99, linetype="dashed", color="red") + 
        ylab("Utilization") +
        xlab("Survival Rate") + 
        ggtitle(paste("Model Performance With Sample Size", sample_size, "and Window Size", window_size))
    }
    
  }
  ggsave(paste0("Sample Size ", plot = plt, sample_size, " Window Size ", window_size, " ", schedule_policy, " ", simulation, ifelse(adjustment, " adjustment", ""), ".png"))
}

sample_size <- 100
window_size <- 12
adjustment <- FALSE
schedule_policy <- "dynamic"
simulation <- "online"

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
plot_results(ar_results, sample_size, window_size, model_name = model_names[c(1,2,3)], 2, schedule_policy, simulation, adjustment)

