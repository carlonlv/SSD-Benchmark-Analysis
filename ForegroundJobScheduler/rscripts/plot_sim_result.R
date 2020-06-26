library("DataCenterSim")
library("dplyr")

overall_df <- data.frame()
feature <- "name"
for (i in 1:4) {
  feature_df <- read.csv(file.choose())
  paramwise_df <- read.csv(file.choose())
  overall_df <- rbind(overall_df, cbind(paramwise_df, feature = feature_df[1, feature]))
}

plot_ecdf_traces_performance(overall_df, "Model", FALSE, "Markov_fixed,Markov_quantile,MarkovX_fixed,MarkovX_quantile", "~/Documents/")


overall_df <- data.frame()
feature <- "cut_off_prob"
feature_df <- read.csv(file.choose())
for (i in 1:5) {
  paramwise_df <- read.csv(file.choose())
  overall_df <- rbind(overall_df, cbind(paramwise_df, feature = feature_df[i, feature]))
}

plot_ecdf_traces_performance(overall_df, "Cut Off Prob", FALSE, "ARI11,fixed", "~/Documents/")
