library("xlsx")
library("readxl")
library("dplyr")


merge_row <- function(rownum, file_from, file_to) {
  row_from <- file_from[rownum,]
  row_to <- file_to[rownum,]
  if (any(is.na(row_to))) {
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
file_from.dp <- file.choose()
file_to.dp <- file.choose()

file_from <- read.xlsx(file = file_from.dp, sheetIndex = 1)
file_to <- read.xlsx(file = file_to.dp, sheetIndex = 1)

result_to <- merge_result(file_from, file_to)
write.xlsx(result_to, showNA = FALSE, file = file_to.dp, row.names = FALSE)
