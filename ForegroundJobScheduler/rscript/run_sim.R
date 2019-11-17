
arg_checker <- function(check, args) {
  idx <- which(check == args)
  if (length(idx) == 0) {
    stop(paste("Error: flag", check, "not found."))
  }
  content <- args[idx + 1]
  if (is.na(content) | "--" %in% content) {
    stop(paste("Error: content", check, "not found."))
  }
  return(content)
}

## Read command line argument
args <- commandArgs(trailingOnly = TRUE)

action <- arg_checker("--action", args)
## Use file/command as input
if (action == "file") {
  file_path <- arg_checker("--file", args)
  
} else if (action == "param") {
  
} else {
  
  stop("Usage --action file/param.")
}


## Identify online/offline simulation
simulation <- arg_checker("--sim", args)
if (simulation == "online") {
  
} else if (simulation == "offline") {
  
} else {
  
}
