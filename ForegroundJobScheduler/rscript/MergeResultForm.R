library("dplyr")


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
  if (is.na(content) | grepl("--", content)) {
    stop(paste("Error: content", check, "not found, but flag is provided."))
  }
  return(content)
}


merge_row <- function(rownum, file_from, file_to) {
  row_from <- file_from[rownum,]
  row_to <- file_to[rownum,]
  if (all(!is.na(row_from))) {
    row_to <- row_from
  }
  return(row_to)
}

 
merge_result <- function(file_from, file_to) {
  result <- sapply(1:nrow(file_to), merge_row, file_from, file_to, simplify = FALSE)
  result.df <- data.frame()
  for (i in 1:nrow(file_to)) {
    result.df <- rbind(result.df, result[[i]])
  }
  rownames(result.df) <- rownames(file_to)
  colnames(result.df) <- colnames(file_to)
  return(result.df)
}


## Script
args <- commandArgs(trailingOnly = TRUE)
from <- arg_checker("--from", args)
to <- arg_checker("--to", args)

file_from <- read.csv(from)
file_to <- read.csv(to)

result_to <- merge_result(file_from, file_to)
write.csv(result_to, file = to, row.names = FALSE)
