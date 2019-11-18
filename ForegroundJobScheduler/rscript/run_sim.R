arg_checker <- function(check, args, mandatory=TRUE, default=NULL) {
  idx <- which(check == args)
  if (length(idx) == 0) {
    if (mandatory) {
      stop(paste("Error: flag", check, "not found."))
    } else {
      return(default)
    }
  }
  content <- args[idx + 1]
  if (is.na(content) | "--" %in% content) {
    stop(paste("Error: content", check, "not found, but flag is provided."))
  }
  return(content)
}

true_false_checker <- function(check, flag) {
  if (any(check == c("T", "True", "TRUE"))) {
    adjustment <- TRUE
  } else if (any(check == c("F", "False", "FALSE"))) {
    adjustment <- FALSE
  } else {
    stop(paste("Usage:", flag, "<T/F>"))
  }
}

args <- commandArgs(trailingOnly = TRUE)

action <- arg_checker("--action", args)
simulation <- arg_checker("--sim", args)
model <- arg_checker("--model", args)
sample_size <- arg_checker("--sample", args)
if (!any(sample_size == c(100, 3000))) {
  stop("Usage: --sample <100/3000>")
}
write_result <- true_false_checker(arg_checker("--result", args), "--result")
schedule_policy <- ifelse(arg_checker("--schedule", args))
if (!any(schedule_policy == c("disjoint", "dynamic"))) {
  stop("Usage: -- sample <disjoint/dynamic>")
}
  
if (action == "file") {
  ## File mandatory flags
  file_path <- arg_checker("--file", args)
  if (!any(model == c("AR1", "VAR1", "AR1_logistic_lm", "AR1_logistic_glm", "AR1_Markov", "Markov"))) {
    stop("Usage: --model <model name>")
  }
  param <- ifelse(file_path == "default", NULL, file_path)
  ## File alternative flags
  cpu_usage <- arg_checker("--cpu_use", args, FALSE, 0.85)
  total_trace_length <- arg_checker("--total_trace", args, FALSE, 8000)
  
  if (simulation == "online") {
    if (Sys.info()["sysname"] == "Windows") {
      source("C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//rscript//online_simulation.R")
    } else {
      source("/Users/carlonlv/Documents/GitHub/Research-Projects/ForegroundJobScheduler/rscript/online_simulation.R")
    }
    define.inputs(model, param, sample_size, write_result, schedule_policy, cpu_usage=0.85, total_trace_length=8000)
  } else if (simulation == "offline") {
    ## Offline mandatory flags
    adjustment <- true_false_checker(arg_checker("--adjust", args), "--adjust")
    ## Offline alternative flags
    max_run_length <- arg_checker("--max_run", args, FALSE, 37)
    initial_train_size <- arg_checker("--initial_train_size", args, FALSE, 6000)
    
    if (Sys.info()["sysname"] == "Windows") {
      source("C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//rscript//offline_simulation.R")
    } else {
      source("/Users/carlonlv/Documents/GitHub/Research-Projects/ForegroundJobScheduler/rscript/offline_simulation.R")
    }
    define.inputs(model, param, sample_size, adjustment, write_result, schedule_policy, cpu_usage, max_run_length, total_trace_length, initial_train_size)
  } else {
    stop("Usage: --sim <offline/online>")
  }
} else if (action == "param") {
  window_size <- arg_checker("--window", args)
  prob_cut_off <- arg_checker("--prob", args)
  granularity <- arg_checker("--gran", args)
  
  cpu_usage <- arg_checker("--cpu_use", args, FALSE, 0.85)
  total_trace_length <- arg_checker("--total_trace", args, FALSE, 8000)
  
  if (simulation == "online") {
    train_size <- arg_checker("--train", args)
    update_freq <- arg_checker("--update", args)
    
    param <- NULL 
    if (model %in% c("AR1", "VAR1")) {
      param <- data.frame("window_size"=window_size, "prob_cut_off"=prob_cut_off, "granularity"=granularity, "train_size"=train_size, "update_freq"=update_freq)
    } else if (model %in% c("AR1_logistic_lm", "AR1_logistic_glm")) {
      num_of_bins <- arg_checker("--bin", args)
      param <- data.frame("window_size"=window_size, "prob_cut_off"=prob_cut_off, "granularity"=granularity, "train_size"=train_size, "update_freq"=update_freq, "num_of_bins"=num_of_bins)
    } else if (model %in% c("Markov", "AR1_Markov", "AR1_state_based_logistic")) {
      num_of_states <- arg_checker("--state", args)
      param <- data.frame("window_size"=window_size, "prob_cut_off"=prob_cut_off, "granularity"=granularity, "train_size"=train_size, "update_freq"=update_freq, "num_of_states"=num_of_states)
    } else {
      stop("Usage: --model <model name>")
    }
    
    if (Sys.info()["sysname"] == "Windows") {
      source("C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//rscript//online_simulation.R")
    } else {
      source("/Users/carlonlv/Documents/GitHub/Research-Projects/ForegroundJobScheduler/rscript/online_simulation.R")
    }
    define.inputs(model_name, param, sample_size, write_result, schedule_policy, cpu_usage, total_trace_length)
  } else if (simulation == "offline") {
    max_run_length <- arg_checker("--max_run", args, FALSE, 37)
    initial_train_size <- arg_checker("--initial_train_size", args, FALSE, 6000)
    
    param <- NULL 
    if (model %in% c("AR1", "VAR1")) {
      param <- data.frame("window_size"=window_size, "prob_cut_off"=prob_cut_off, "granularity"=granularity)
    } else if (model %in% c("AR1_logistic_lm", "AR1_logistic_glm")) {
      num_of_bins <- arg_checker("--bin", args)
      param <- data.frame("window_size"=window_size, "prob_cut_off"=prob_cut_off, "granularity"=granularity, "num_of_bins"=num_of_bins)
    } else if (model %in% c("Markov", "AR1_Markov", "AR1_state_based_logistic")) {
      num_of_states <- arg_checker("--state", args)
      param <- data.frame("window_size"=window_size, "prob_cut_off"=prob_cut_off, "granularity"=granularity, "num_of_states"=num_of_states)
    } else {
      stop("Usage: --model <model name>")
    }
    
    if (Sys.info()["sysname"] == "Windows") {
      source("C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//rscript//offline_simulation.R")
    } else {
      source("/Users/carlonlv/Documents/GitHub/Research-Projects/ForegroundJobScheduler/rscript/offline_simulation.R")
    }
    define.inputs(model, param, sample_size, adjustment, write_result, schedule_policy, cpu_usage, max_run_length, total_trace_length, initial_train_size)
  } else {
    stop("Usage: --sim <offline/online>")
  }
} else {
  stop("Usage: --action file/param.")
}
