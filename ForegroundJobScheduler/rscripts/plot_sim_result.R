library("DataCenterSim")
library("dplyr")

path = "~/Documents/PDSF Dataset/WindowSize/"

lsfiles <- list.files(path = path, pattern = "Charwise*", recursive = TRUE)

overall_df <- data.frame()
for (i in lsfiles) {
  temp_df <- read.csv(paste0(path, i))
  temp_df <- temp_df[temp_df$name %in% c("AR1", "LM"), c("name", "window_size", "cut_off_prob", "score1.n", "score1.w", "score1_adj.n", "score1_adj.w", "score1_adj.w",  "score2.n", "score2.w", "score2_adj.n", "score2_adj.w")]
  overall_df <- rbind(overall_df, temp_df)
}
DataCenterSim:::plot_sim_charwise(overall_df, mapping = list("color" = "name", "linetype" = "window_size"), adjusted = FALSE, point_or_line = NA, name = "Different Models Under Different Window Sizes", path)
