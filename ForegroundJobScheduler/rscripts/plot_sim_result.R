library("DataCenterSim")
library("dplyr")

overall_df <- data.frame()
feature <- "granularity"
for (i in 1:4) {
  feature_df <- read.csv(file.choose())
  paramwise_df <- read.csv(file.choose())
  overall_df <- rbind(overall_df, cbind(paramwise_df, feature = feature_df[1, feature]))
}

plot_ecdf_traces_performance(overall_df, "Granularity", FALSE, "AR1 Model Offline Training Policy", "~/Documents/")


overall_df <- data.frame()
feature <- "granularity"
feature_df <- read.csv(file.choose())
for (i in 1:4) {
  paramwise_df <- read.csv(file.choose())
  overall_df <- rbind(overall_df, cbind(paramwise_df, feature = feature_df[i, feature]))
}

plot_ecdf_traces_performance(overall_df, "granularity", FALSE, "AR1 Model Offline Training Policy", "~/Documents/")
