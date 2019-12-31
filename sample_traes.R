library(tidyverse)
library(purrr)
library(dplyr)

setwd("~/Desktop/CS_Research/sample_background_jobs")

names <- c(1010545, 1407067,	1732028,	382681, 780806,
           1010567,	1407210,	1732032,	382683,	781233,
           1010577,	1407217,	1732086,	382692,	781252,
           1010583,	1407268,	1737700,	382694,	781255,
           1010637,	1407280,	1737718,	382930,	781257,
           1010678,	1407379,	1739024,	382931,	7817,
           1010687,	1407382,	1742874,	385058,	7818,
           1014129,	1407396,	1743011,	385352,	7839,
           1014169,	1407497,	1747047,	385360,	783927,
           1014549,	1407605,	1747109,	385481,	788990,
           1014600,	1407804,	1747112,	385554,	789885,
           1014705,	1408100,	1754,	385611,	7903,
           1014709,	1408314,	1755661,	385645,	7928,
           1014746,	1418,	1755681,	385666,	792842,
           1014773,	1444,	1755699,	385712,	793205,
           1014846,	145,		1755705,	385823,	793247,
           1014877,	1456349,	1756130,	385972,	793276,
           1015066,	1456353,	1756140,	39,		794324,
           1015250,	1471309,	1756186,	408,		794482,
           1015285,	1471874,	1756188,	41988,	7966)

names <- as.character(names)

names <- paste(names,".csv", sep = "")


try <- read.csv(names[1])
try1 <- slice(try,1:8000)
try2 <- try1$max_cpu

# For Max:
result_max <- data_frame(timestamp = try1$timestamp)
aux <- NULL

for (i in 1:length(names)) {
  name <- names[i]
  aux <- read.csv(name)
  aux <- slice(aux,1:8000)
  aux <- aux$max_cpu
  result_max <- cbind(result_max,aux)
  names(result_max)[length(names(result_max))] <- paste("trace",i,sep = "_")
}

result_max <- as_tibble(result_max)
save(result_max,file = "~/Desktop/CS_Research/Max.Rdata")






# For Avg:

result_avg <- data_frame(timestamp = try1$timestamp)
aux <- NULL

for (i in 1:length(names)) {
  name <- names[i]
  aux <- read.csv(name)
  aux <- slice(aux,1:8000)
  aux <- aux$avg_cpu
  result_avg <- cbind(result_avg,aux)
  names(result_avg)[length(names(result_avg))] <- paste("trace",i,sep = "_")
}

result_max <- as_tibble(result_avg)
save(result_max,file = "~/Desktop/CS_Research/Avg.Rdata")