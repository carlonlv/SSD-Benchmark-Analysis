
generate_default_df <- function(include.bin, include.state) {
  window_sizes <- c(12, 36)
  prob_cut_offs <- c(0.005, 0.01, 0.1)
  granularity <- c(100/32, 100/64, 100/128, 0)
  
  num_of_bins <- c(1000, 500)
  num_of_states <- c(8, 16, 32, 64)
  
  if (include.bin & include.state) {
    parameter.df <- expand.grid(window_sizes, prob_cut_offs, granularity, num_of_states, num_of_bins)
    colnames(parameter.df) <- c("window_size", "prob_cut_off", "granularity", "num_of_states", "num_of_bins")
  } else if (include.state & !include.bin) {
    parameter.df <- expand.grid(window_sizes, prob_cut_offs, granularity)
    colnames(parameter.df) <- c("window_size", "prob_cut_off", "granularity", "num_of_states")
  } else if (include.bin & !include.state) {
    parameter.df <- expand.grid(window_sizes, prob_cut_offs, granularity, num_of_bins)
    colnames(parameter.df) <- c("window_size", "prob_cut_off", "granularity", "num_of_bins")
  }  else {
    parameter.df <- expand.grid(window_sizes, prob_cut_offs, granularity)
    colnames(parameter.df) <- c("window_size", "prob_cut_off", "granularity")
  }
  
  parameter.df <- parameter.df %>%
    arrange()
  return(parameter.df)
}

define.inputs <- function(model_name, param, sample_size, adjustment, write_result, schedule_policy, cpu_usage=0.85, max_run_length=37, total_trace_length=8000, initial_train_size=6000) {
  
  ## Read background job pool
  write_result_path <- NULL
  if (Sys.info()["sysname"] == "Windows") {
    write_result_path <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//offline results//ts_results//"
  } else {
    write_result_path <- "/Users/carlonlv/Documents/Github/Research-Projects/ForegroundJobScheduler/results/offline results/ts_results/"
  }
  
  bg_jobs_path <- NULL
  if (Sys.info()["sysname"] == "Windows") {
    bg_jobs_path <- "C://Users//carlo//Documents//sample background jobs//"
  } else {
    bg_jobs_path <- "/Users/carlonlv/Documents/microsoft traces/"
  }
  
  bg_job_pool <- NULL
  if (sample_size == 100 ) {
    if (Sys.info()["sysname"] == "Windows") {
      bg_job_pool <- read.csv("C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//pythonscripts//list of sampled 100 background jobs.csv")[,1]
    } else {
      bg_job_pool <- read.csv("/Users/carlonlv/Documents/GitHub/Research-Projects/ForegroundJobScheduler/pythonscripts/list of sampled 100 background jobs.csv")[,1]
    }
    bg_job_pool <- sub(".pd", "", bg_job_pool)
  } else {
    if (Sys.info()["sysname"] == "Windows") {
      bg_job_pool <- read.csv("C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//pythonscripts//list of sampled background jobs.csv")[,1]
    } else {
      bg_job_pool <- read.csv("/Users/carlonlv/Documents/GitHub/Research-Projects/ForegroundJobScheduler/pythonscripts/list of sampled background jobs.csv")[,1]
    }
    bg_job_pool <- sub(".pd", "", bg_job_pool)
  }
  
  data_matrix_avg <- matrix(nrow = total_trace_length, ncol = 0)
  data_matrix_max <- matrix(nrow = total_trace_length, ncol = 0)
  for (job_num in bg_job_pool) {
    bg_job <- read.csv(paste(bg_jobs_path, job_num, ".csv", sep = ""))
    data_matrix_avg <- cbind(data_matrix_avg, bg_job$avg_cpu[1:total_trace_length])
    data_matrix_max <- cbind(data_matrix_max, bg_job$max_cpu[1:total_trace_length])
  }
  rownames(data_matrix_avg) <- seq(1, nrow(data_matrix_avg) ,1)
  colnames(data_matrix_avg) <- bg_job_pool
  rownames(data_matrix_max) <- seq(1, nrow(data_matrix_max) ,1)
  colnames(data_matrix_max) <- bg_job_pool
  
  cpu_required <- rep(0, ncol(data_matrix))
  for (j in 1:ncol(data_matrix)) {
    cpu_required[j] <- as.numeric(quantile(data_matrix[,j], cpu_usage, type = 4)[cpu_usage])
  }
  
  output_dp <- NULL
  if (adjustment) {
    if (schedule_policy == "dynamic") {
      if (Sys.info()["sysname"] == "Windows") {
        output_dp <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//offline results//summary dynamic (windows,granularity) post adj.csv"
      } else {
        output_dp <- "/Users/carlonlv/Documents/Github/Research-Projects/ForegroundJobScheduler/results/offline results/summary dynamic (windows,granularity) post adj.csv"
      }
    } else {
      if (Sys.info()["sysname"] == "Windows") {
        output_dp <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//offline results//summary disjoint (windows,granularity) post adj.csv"
      } else {
        output_dp <- "/Users/carlonlv/Documents/Github/Research-Projects/ForegroundJobScheduler/results/offline results/summary disjoint (windows,granularity) post adj.csv"
      }
    }
  } else {
    if (schedule_policy == "dynamic") {
      if (Sys.info()["sysname"] == "Windows") {
        output_dp <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//offline results//summary dynamic (windows,granularity).csv"
      } else {
        output_dp <- "/Users/carlonlv/Documents/Github/Research-Projects/ForegroundJobScheduler/results/offline results/summary dynamic (windows,granularity).csv"
      }
    } else {
      if (Sys.info()["sysname"] == "Windows") {
        output_dp <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//offline results//summary disjoint (windows,granularity).csv"
      } else {
        output_dp <- "/Users/carlonlv/Documents/Github/Research-Projects/ForegroundJobScheduler/results/offline results/summary disjoint (windows,granularity).csv"
      }
    }
  }
  
  if (model_name == "AR1") {
    if (Sys.info()["sysname"] == "Windows") {
      source("C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//rscript//offline scripts//svt_ar1_model.R")
    } else {
      source("/Users/carlonlv/Documents/GitHub/Research-Projects/ForegroundJobScheduler/rscript/offline scripts/svt_ar1_model.R")
    }
    param <- ifelse(is.null(param), generate_default_df(FALSE, FALSE), param)
    slt <- apply(param, 1, wrapper.epoche, data_matrix_max, (100-cpu_required), initial_train_size, max_run_length, output_dp, schedule_policy, adjustment, write_result, write_result_path)
  } else if (model_name == "VAR1") {
    if (Sys.info()["sysname"] == "Windows") {
      source("C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//rscript//offline scripts//mvt_stationary_model.R")
    } else {
      source("/Users/carlonlv/Documents/GitHub/Research-Projects/ForegroundJobScheduler/rscript/offline scripts/mvt_stationary_model.R")
    }
    param <- ifelse(is.null(param), generate_default_df(FALSE, FALSE), param)
    slt <- apply(param, 1, wrapper.epoche, data_matrix_avg, data_matrix_max, (100-cpu_required), initial_train_size, max_run_length, output_dp, schedule_policy, adjustment, write_result, write_result_path)
  } else if (model_name == "Markov") {
    if (Sys.info()["sysname"] == "Windows") {
      source("C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//rscript//offline scripts//markov_model.R")
    } else {
      source("/Users/carlonlv/Documents/GitHub/Research-Projects/ForegroundJobScheduler/rscript/offline scripts/markov_model.R")
    }
    param <- ifelse(is.null(param), generate_default_df(FALSE, TRUE), param)
    slt <- apply(param, 1, wrapper.epoche, data_matrix_max, (100-cpu_required), initial_train_size, max_run_length, output_dp, schedule_policy, adjustment, write_result, write_result_path)
  } else if (model_name == "AR1_Markov") {
    if (Sys.info()["sysname"] == "Windows") {
      source("C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//rscript//offline scripts//ar1_markov_model.R")
    } else {
      source("/Users/carlonlv/Documents/GitHub/Research-Projects/ForegroundJobScheduler/rscript/offline scripts/ar1_markov_model.R")
    }
    param <- ifelse(is.null(param), generate_default_df(FALSE, TRUE), param)
    slt <- apply(param, 1, wrapper.epoche, data_matrix_avg, data_matrix_max, (100-cpu_required), initial_train_size, max_run_length, output_dp, schedule_policy, adjustment, write_result, write_result_path)
  } else if (model_name == "AR1_logistic_lm") {
    if (Sys.info()["sysname"] == "Windows") {
      source("C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//rscript//offline scripts//svt_ar1_logistic_model.R")
    } else {
      source("/Users/carlonlv/Documents/GitHub/Research-Projects/ForegroundJobScheduler/rscript/offline scripts/svt_ar1_logistic_model.R")
    }
    param <- ifelse(is.null(param), generate_default_df(TRUE, FALSE), param)
    slt <- apply(param, 1, wrapper.epoche, data_matrix_avg, data_matrix_max, (100-cpu_required), initial_train_size, max_run_length, "lm", output_dp, schedule_policy, adjustment, write_result, write_result_path)
  } else if (model_name == "AR1_logistic_glm") {
    if (Sys.info()["sysname"] == "Windows") {
      source("C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//rscript//offline scripts//svt_ar1_logistic_model.R")
    } else {
      source("/Users/carlonlv/Documents/GitHub/Research-Projects/ForegroundJobScheduler/rscript/offline scripts/svt_ar1_logistic_model.R")
    }
    param <- ifelse(is.null(param), generate_default_df(TRUE, FALSE), param)
    slt <- apply(param, 1, wrapper.epoche, data_matrix_avg, data_matrix_max, (100-cpu_required), initial_train_size, max_run_length, "glm", output_dp, schedule_policy, adjustment, write_result, write_result_path)
  } else if (model_name == "AR1_state_based_logistic") {
    if (Sys.info()["sysname"] == "Windows") {
      source("C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//rscript//offline scripts//svt_ar1_state_based_logistic_model.R")
    } else {
      source("/Users/carlonlv/Documents/GitHub/Research-Projects/ForegroundJobScheduler/rscript/offline scripts/svt_ar1_state_based_logistic_model.R")
    }
    param <- ifelse(is.null(param), generate_default_df(FALSE, TRUE), param)
    slt <- apply(param, 1, wrapper.epoche, data_matrix_avg, data_matrix_max, (100-cpu_required), initial_train_size, max_run_length, output_dp, schedule_policy, adjustment, write_result, write_result_path)
  }
}

