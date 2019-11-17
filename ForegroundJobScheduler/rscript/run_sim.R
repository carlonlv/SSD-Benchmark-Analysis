arg_checker <- function(check, args, type="mandatory") {
  idx <- which(check == args)
  if (length(idx) == 0) {
    if (type == "mandatory") {
      stop(paste("Error: flag", check, "not found."))
    } else {
      return(NULL)
    }
  }
  content <- args[idx + 1]
  if (is.na(content) | "--" %in% content) {
    stop(paste("Error: content", check, "not found, but flag is provided."))
  }
  return(content)
}

args <- commandArgs(trailingOnly = TRUE)

action <- arg_checker("--action", args)
model <- arg_checker("--model", args)
simulation <- arg_checker("--sim", args)
sample_size <- arg_checker("--sample", args)
adjustment <- arg_checker("--adjust", args)

if (action == "file") {
  file_path <- arg_checker("--file", args)
  
  if (simulation == "online") {
    if (Sys.info()["sysname"] == "Windows") {
      output_dp <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//offline results//summary dynamic (windows,granularity) post adj.csv"
    } else {
      output_dp <- "/Users/carlonlv/Documents/Github/Research-Projects/ForegroundJobScheduler/results/offline results/summary dynamic (windows,granularity) post adj.csv"
    }
  } else if (simulation == "offline") {
    if (Sys.info()["sysname"] == "Windows") {
      output_dp <- "C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//results//offline results//summary dynamic (windows,granularity) post adj.csv"
    } else {
      output_dp <- "/Users/carlonlv/Documents/Github/Research-Projects/ForegroundJobScheduler/results/offline results/summary dynamic (windows,granularity) post adj.csv"
    }
  } else {
    stop("Usage: --sim <offline/online>")
  }
} else if (action == "param") {
  window_size <- arg_checker("--window", args)
  prob_cut_off <- arg_checker("--prob", args)
  granularity <- arg_checker("--gran", args)

  if (model %in% c("AR1", "VAR1")) {
    
  } else if (model %in% c("AR1_logistic_lm", "AR1_logistic_glm")) {
    bin_num <- arg_checker("--bin", args)
    
  } else if (model %in% c("Markov", "AR1_Markov", "AR1_state_based_logistic")) {
    num_of_states <- arg_checker("--state", args)
    
  } else {
    stop("Usage: --model <model name>")
  }
    
  if (simulation == "online") {
    train_size <- arg_checker("--train", args)
    update_freq <- arg_checker("--update", args)
    
  } else if (simulation == "offline") {
    
  } else {
    stop("Usage: --sim <offline/online>")
  }
} else {
  stop("Usage: --action file/param.")
}
