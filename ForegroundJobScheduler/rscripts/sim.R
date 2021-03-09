library("DataCenterSim")
library("dplyr")

load("~/Documents/PDSF Dataset/microsoft_10000.rda")

microsoft_max_10000 <- microsoft_max_10000[1:3000, c(1:2016)[-c(286,290,328,380,387,398,399,704,706,718,720,738,813,1571,1637,1638)]]
microsoft_avg_10000 <- microsoft_avg_10000[1:3000, c(1:2016)[-c(286,290,328,380,387,398,399,704,706,718,720,738,813,1571,1637,1638)]]


## Finding best predictor using ARIMA, LM, Markov
window_size <- c(1, 5, 10, 15, 20, 25)

cut_off_prob <- c(0.0001, 0.0003, 0.0005, 0.001, 0.003, 0.005, 0.01, 0.03, 0.05)
additional_setting <- list("cut_off_prob" = cut_off_prob)
## Baseline max to max, same window sizes
bg_param_setting <- data.frame(class = "ARIMA", name = "AR1", window_size = window_size, granularity = 0, train_policy = "fixed", train_size = 2000, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, additional_setting, microsoft_max_10000, NULL, cores = parallel::detectCores(), write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/FindBestPredictor/AR1/")

bg_param_setting <- data.frame(class = "MARKOV", name = "Markov", window_size = window_size, granularity = 0, train_policy = "fixed", train_size = 2000, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, additional_setting, microsoft_max_10000, NULL, cores = parallel::detectCores(), write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/FindBestPredictor/Markov/")

## Adding Averages
bg_param_setting <- data.frame(class = "ARIMA", name = "AR1X(Avg)", window_size = window_size, window_size_for_reg = window_size, window_type_for_reg = "avg", granularity = 0, train_policy = "fixed", train_size = 2000, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, additional_setting, microsoft_max_10000, microsoft_avg_10000, cores = parallel::detectCores(), write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/FindBestPredictor/AR1/")

bg_param_setting <- data.frame(class = "MARKOV", name = "MarkovX(Avg)", window_size = window_size, window_size_for_reg = window_size, window_type_for_reg = "avg", granularity = 0, train_policy = "fixed", train_size = 2000, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, additional_setting, microsoft_max_10000, microsoft_avg_10000, cores = parallel::detectCores(), write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/FindBestPredictor/Markov/")

bg_param_setting <- data.frame(class = "LM", name = "LM(Avg)", window_size = window_size, window_size_for_reg = window_size, window_type_for_reg = "avg", granularity = 0, train_policy = "fixed", train_size = 2000, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, additional_setting, microsoft_max_10000, microsoft_avg_10000, cores = parallel::detectCores(), write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/FindBestPredictor/LM/")

## Adding past window size of 12
additional_setting <- list("cut_off_prob" = cut_off_prob, "window_size_for_reg" = 12, "window_type_for_reg" = "max")
bg_param_setting <- data.frame(class = "ARIMA", name = "AR1X(Avg/Max12)", window_size = window_size, window_size_for_reg = window_size, window_type_for_reg = "avg", granularity = 0, train_policy = "fixed", train_size = 2000, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, additional_setting, microsoft_max_10000, list("avg" = microsoft_avg_10000, "max_12" = microsoft_max_10000), cores = parallel::detectCores(), write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/FindBestPredictor/AR1/")

bg_param_setting <- data.frame(class = "MARKOV", name = "MarkovX(Avg/Max12)", window_size = window_size, window_size_for_reg = window_size, window_type_for_reg = "avg", granularity = 0, train_policy = "fixed", train_size = 2000, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, additional_setting, microsoft_max_10000, list("avg" = microsoft_avg_10000, "max_12" = microsoft_max_10000), cores = parallel::detectCores(), write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/FindBestPredictor/Markov/")

bg_param_setting <- data.frame(class = "LM", name = "LM(Avg/Max12)", window_size = window_size, window_size_for_reg = window_size, window_type_for_reg = "avg", granularity = 0, train_policy = "fixed", train_size = 2000, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, additional_setting, microsoft_max_10000, list("avg" = microsoft_avg_10000, "max_12" = microsoft_max_10000), cores = parallel::detectCores(), write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/FindBestPredictor/LM/")

## ARIMA-like

cut_off_prob <- c(0.001, 0.003, 0.005, 0.01, 0.03, 0.05)
bg_param_setting <- data.frame(name = "ARIMA", window_size = 1, cut_off_prob = cut_off_prob, granularity = 0, train_policy = "offline", train_size = 1000, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, microsoft_max_10000, NULL, start_point = 2, cores = parallel::detectCores(), write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/Model/AR1/")

bg_param_setting <- data.frame(name = "ARIMA", window_size = 1, cut_off_prob = cut_off_prob, granularity = 0, train_policy = "offline", train_size = 1000, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, microsoft_max_10000, as.matrix(dplyr::mutate_all(as.data.frame(microsoft_avg_10000), dplyr::lag, 1)), start_point = 2, cores = parallel::detectCores(), write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/Model/AR1X/")

bg_param_setting <- data.frame(name = "ARIMA", window_size = 1, cut_off_prob = cut_off_prob, granularity = 0, train_policy = "offline", train_size = 1000, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
bg_param_setting$train_args <- list(order = c(1,1,0))
d <- run_sim(bg_param_setting, microsoft_max_10000, NULL, start_point = 2, cores = parallel::detectCores(), write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/Model/ARI11/")

bg_param_setting <- data.frame(name = "ARIMA", window_size = 1, cut_off_prob = cut_off_prob, granularity = 0, train_policy = "offline", train_size = 1000, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
bg_param_setting$train_args <- list(order = c(1,1,0))
d <- run_sim(bg_param_setting, microsoft_max_10000, as.matrix(dplyr::mutate_all(as.data.frame(microsoft_avg_10000), dplyr::lag, 1)), start_point = 2, cores = parallel::detectCores(), write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/Model/ARI11X/")

bg_param_setting <- data.frame(name = "AUTO_ARIMA", window_size = 1, cut_off_prob = cut_off_prob, granularity = 0, train_policy = "offline", train_size = 1000, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, microsoft_max_10000, NULL, start_point = 2, cores = parallel::detectCores(), write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/Model/AUTOARIMA/")

bg_param_setting <- data.frame(name = "AUTO_ARIMA", window_size = 1, cut_off_prob = cut_off_prob, granularity = 0, train_policy = "offline", train_size = 1000, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, microsoft_max_10000, as.matrix(dplyr::mutate_all(as.data.frame(microsoft_avg_10000), dplyr::lag, 1)), start_point = 2, cores = parallel::detectCores(), write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/Model/AUTOARIMAX/")


### Residual distribution

cut_off_prob <- c(0.001, 0.003, 0.005, 0.01, 0.03, 0.05)
res_dist <- c("normal","skew_norm","empirical")
bg_param_setting <- expand.grid(cut_off_prob = cut_off_prob, res_dist = res_dist,  stringsAsFactors = FALSE)
bg_param_setting <- cbind(bg_param_setting, data.frame(name = "ARIMA", window_size = 1, granularity = 0, train_policy = "offline", train_size = 1000, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE))
d <- run_sim(bg_param_setting, microsoft_max_10000, NULL, start_point = 2, cores = parallel::detectCores(), write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/ResDist/")

## Markov-like

cut_off_prob <- c(0.001, 0.003, 0.005, 0.01, 0.03, 0.05)
bg_param_setting <- data.frame(name = "MARKOV", window_size = 1, cut_off_prob = cut_off_prob, granularity = 0, train_policy = "offline", train_size = 1000, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, microsoft_max_10000, NULL, start_point = 2, cores = parallel::detectCores(), write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/Model/Clustertype/Markov/")

bg_param_setting <- data.frame(name = "MARKOV", window_size = 1, cut_off_prob = cut_off_prob, granularity = 0, train_policy = "offline", train_size = 1000, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, microsoft_max_10000, as.matrix(dplyr::mutate_all(as.data.frame(microsoft_avg_10000), dplyr::lag, 1)), start_point = 2, cores = parallel::detectCores(), write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/Model/Clustertype/MarkovX/")

bg_param_setting <- data.frame(name = "MARKOV", window_size = 1, cut_off_prob = cut_off_prob, granularity = 0, train_policy = "offline", train_size = 1000, update_freq = 3, react_speed = "1,2", extrap_step = 1, cluster_type = "quantile", stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, microsoft_max_10000, NULL, start_point = 2, cores = parallel::detectCores(), write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/Model/Clustertype/Markov/")

bg_param_setting <- data.frame(name = "MARKOV", window_size = 1, cut_off_prob = cut_off_prob, granularity = 0, train_policy = "offline", train_size = 1000, update_freq = 3, react_speed = "1,2", extrap_step = 1, cluster_type = "quantile", stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, microsoft_max_10000, as.matrix(dplyr::mutate_all(as.data.frame(microsoft_avg_10000), dplyr::lag, 1)), start_point = 2, cores = parallel::detectCores(), write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/Model/Clustertype/MarkovX/")

### State Number
cut_off_prob <- c(0.001, 0.003, 0.005, 0.01, 0.03, 0.05)
state_num <- c(8, 10, 16, 20)
bg_param_setting <- expand.grid(cut_off_prob = cut_off_prob, state_num = state_num,  stringsAsFactors = FALSE)
bg_param_setting <- cbind(bg_param_setting, data.frame(name = "MARKOV", window_size = 1, granularity = 0, train_policy = "offline", train_size = 1000, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE))
d <- run_sim(bg_param_setting, microsoft_max_10000, NULL, start_point = 2, cores = parallel::detectCores(), write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/Model/Markov/StateNum/")

## VAR-like
cut_off_prob <- c(0.001, 0.003, 0.005, 0.01, 0.03, 0.05)
p <- c(1, 2, 3)
bg_param_setting <- expand.grid(cut_off_prob = cut_off_prob, p = p,  stringsAsFactors = FALSE)
bg_param_setting <- cbind(bg_param_setting, data.frame(name = "VAR", window_size = 1, granularity = 0, train_policy = "offline", train_size = 1000, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE))
d <- run_sim(bg_param_setting, microsoft_max_10000, DataCenterSim::microsoft_avg_100[-c(1:12),], start_point = 2, cores = parallel::detectCores(), write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/Model/VAR/p/")

## NN-like
load("~/Documents/PDSF Dataset/microsoft_10000.rda")

microsoft_max_10000 <- microsoft_max_10000[1:700, c(1:2016)[-c(286,290,328,380,387,398,399,704,706,718,720,738,813,1571,1637,1638)]]
microsoft_avg_10000 <- microsoft_avg_10000[1:700, c(1:2016)[-c(286,290,328,380,387,398,399,704,706,718,720,738,813,1571,1637,1638)]]

cut_off_prob <- c(0.001, 0.003, 0.01, 0.03, 0.05)
p <- c(1, 2, 3, NA_real_)
bg_param_setting <- expand.grid(cut_off_prob = cut_off_prob, p = p,  stringsAsFactors = FALSE)
bg_param_setting <- cbind(bg_param_setting, data.frame(name = "NN", P = 0, window_size = 1, granularity = 0, train_policy = "offline", train_size = 600, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE))
d <- run_sim(bg_param_setting, microsoft_max_10000, NULL, start_point = 2, cores = parallel::detectCores(), write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/Model/NN/p/")

load("~/Documents/PDSF Dataset/microsoft_10000.rda")

microsoft_max_10000 <- microsoft_max_10000[1:3000, c(1:2016)[-c(286,290,328,380,387,398,399,704,706,718,720,738,813,1571,1637,1638)]]
microsoft_avg_10000 <- microsoft_avg_10000[1:3000, c(1:2016)[-c(286,290,328,380,387,398,399,704,706,718,720,738,813,1571,1637,1638)]]

## Aggregation VS Extrapolation

cut_off_prob <- c(0.001, 0.003, 0.005, 0.01, 0.03, 0.05)
window_size <- c(12, 6, 4, 3, 2, 1)
bg_param_setting <- expand.grid(cut_off_prob = cut_off_prob, window_size = window_size, stringsAsFactors = FALSE)
bg_param_setting$extrap_step <- 12 / bg_param_setting$window_size
bg_param_setting <- cbind(bg_param_setting, data.frame(name = "ARIMA", granularity = 0, train_policy = "offline", train_size = 1000, update_freq = 3, react_speed = "1,2", stringsAsFactors = FALSE))
d <- run_sim(bg_param_setting, microsoft_max_10000, NULL, start_point = 2, cores = parallel::detectCores(), write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/AggregationVSExtrapolation/")

## Update Frequency
cut_off_prob <- c(0.001, 0.003, 0.005, 0.01, 0.03, 0.05)
update_freq <- c(3, 9, 27, 81)
bg_param_setting <- expand.grid(cut_off_prob = cut_off_prob, update_freq = update_freq, stringsAsFactors = FALSE)
bg_param_setting <- cbind(bg_param_setting, data.frame(name = "ARIMA", window_size = 1, granularity = 0, train_policy = "fixed", train_size = 1000, model_num = 1, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE))
d <- run_sim(bg_param_setting, microsoft_max_10000, NULL, start_point = 2, cores = parallel::detectCores(), write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/BatchSize/AR1/")

## Granularities
granularity <- c(0, 100 / 128, 100 / 64, 100 / 32)
bg_param_setting <- data.frame(name = "ARIMA", window_size = 1, cut_off_prob = 0.01, granularity = granularity, train_policy = "offline", train_size = 1000, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, microsoft_max_10000, NULL, start_point = 2, cores = parallel::detectCores(), write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/Granularity/")

## React Speed
cut_off_prob <- c(0.001, 0.003, 0.005, 0.01, 0.03, 0.05)
react_speed <- c("1,1", "1,2", "1,3", "2,1", "2,2", "2,3")
bg_param_setting <- expand.grid(cut_off_prob = cut_off_prob, react_speed = react_speed, stringsAsFactors = FALSE)
bg_param_setting <- cbind(bg_param_setting, data.frame(name = "ARIMA", window_size = 1, granularity = 0, train_policy = "offline", train_size = 1000, update_freq = 3, extrap_step = 1, stringsAsFactors = FALSE))
d <- run_sim(bg_param_setting, microsoft_max_10000, NULL, start_point = 2, cores = parallel::detectCores(), write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/ReactSpeed/")


## Autopilot

### Half life
load("~/microsoft_generated_data_2000.rda")
microsoft_generated_data_2000 <- microsoft_generated_data_2000[1:(3000 * 30),]

cut_off_prob <- c(0.001, 0.003, 0.005, 0.010, 0.050)
half_life <- c(1, 3, 6, 12, 36, 72)
bg_param_setting <- expand.grid(cut_off_prob = cut_off_prob, half_life = half_life, stringsAsFactors = FALSE)
bg_param_setting <- cbind(bg_param_setting, data.frame(name = "AUTOPILOT", window_size = 30, granularity = 0, train_policy = "fixed", train_size = 1000 * 30, statistics = "j-quantile", cut_off_weight = 0.01, model_num = 1, update_freq = 1, react_speed = "1,2", stringsAsFactors = FALSE))
d <- run_sim(bg_param_setting, microsoft_generated_data_2000, NULL, start_point = 31, cores = parallel::detectCores() - 1, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/Compare/half_life/")

### Half life and Window Sizes
cut_off_prob <- c(0.001, 0.003, 0.007, 0.009, 0.010, 0.020, 0.050)
half_life <- c(12, 36, 72)
window_size <- c(10, 20, 30, 50) * 30
bg_param_setting <- expand.grid(window_size = window_size, half_life = half_life, cut_off_prob = cut_off_prob, stringsAsFactors = FALSE)
bg_param_setting <- cbind(bg_param_setting, data.frame(name = "AUTOPILOT", granularity = 0, train_policy = "fixed", train_size = 1000 * 30, statistics = "j-quantile", cut_off_weight = 0.01, model_num = 1, update_freq = 1, react_speed = "1,2", stringsAsFactors = FALSE))
d <- run_sim(bg_param_setting, microsoft_generated_data_2000, NULL, start_point = 31, cores = parallel::detectCores() - 1, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/Compare/half_life/")

### Breaks
cut_off_prob <- c(0.001, 0.003, 0.005, 0.010, 0.050)
breaks <- c(5, 8, 10, 20, 30)
bg_param_setting <- expand.grid(cut_off_prob = cut_off_prob, breaks = breaks, stringsAsFactors = FALSE)
bg_param_setting <- cbind(bg_param_setting, data.frame(name = "AUTOPILOT", window_size = 30, granularity = 0, train_policy = "fixed", train_size = 1000 * 30, statistics = "j-quantile", cut_off_weight = 0.01, model_num = 1, update_freq = 1, react_speed = "1,2", stringsAsFactors = FALSE))
d <- run_sim(bg_param_setting, microsoft_generated_data_2000, NULL, start_point = 31, cores = parallel::detectCores() - 1, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/Compare/breaks/")

## Group Comparisons-Online vs Offline Training

cut_off_prob <- c(0.001, 0.003, 0.005, 0.01, 0.03, 0.05)
train_policy <- c("offline", "fixed")
bg_param_setting <- expand.grid(cut_off_prob = cut_off_prob, train_policy = train_policy, stringsAsFactors = FALSE)
bg_param_setting <- cbind(bg_param_setting, data.frame(name = "ARIMA", window_size = 1, granularity = 0, train_size = 1000, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE))
d <- run_sim(bg_param_setting, microsoft_max_10000, NULL, start_point = 2, cores = parallel::detectCores(), write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/TrainPolicy/AR1/")

bg_param_setting <- expand.grid(cut_off_prob = cut_off_prob, train_policy = train_policy, stringsAsFactors = FALSE)
bg_param_setting <- cbind(bg_param_setting, data.frame(name = "ARIMA", window_size = 1, granularity = 0, train_size = 1000, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE))
d <- run_sim(bg_param_setting, microsoft_max_10000, as.matrix(dplyr::mutate_all(as.data.frame(microsoft_avg_10000), dplyr::lag, 1)), start_point = 2, cores = parallel::detectCores(), write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/TrainPolicy/AR1X/")

bg_param_setting <- expand.grid(cut_off_prob = cut_off_prob, train_policy = train_policy, stringsAsFactors = FALSE)
bg_param_setting <- cbind(bg_param_setting, data.frame(name = "MARKOV", window_size = 1, granularity = 0, train_size = 1000, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE))
d <- run_sim(bg_param_setting, microsoft_max_10000, NULL, start_point = 2, cores = parallel::detectCores(), write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/TrainPolicy/Markov/")

bg_param_setting <- expand.grid(cut_off_prob = cut_off_prob, train_policy = train_policy, cluster_type = c("fixed", "quantile"), stringsAsFactors = FALSE)
bg_param_setting <- cbind(bg_param_setting, data.frame(name = "MARKOV", window_size = 1, cluster_type = "quantile", granularity = 0, train_size = 1000, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE))
d <- run_sim(bg_param_setting, microsoft_max_10000, as.matrix(dplyr::mutate_all(as.data.frame(microsoft_avg_10000), dplyr::lag, 1)), start_point = 2, cores = parallel::detectCores(), write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/TrainPolicy/MarkovX/")

bg_param_setting <- expand.grid(cut_off_prob = cut_off_prob, train_policy = train_policy, stringsAsFactors = FALSE)
bg_param_setting <- cbind(bg_param_setting, data.frame(name = "VAR", p = 1, window_size = 1, granularity = 0, train_size = 1000, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE))
d <- run_sim(bg_param_setting, microsoft_max_10000, DataCenterSim::microsoft_avg_100[-c(1:12),], start_point = 2, cores = parallel::detectCores(), write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/TrainPolicy/VAR1/")

bg_param_setting <- expand.grid(cut_off_prob = cut_off_prob, train_policy = train_policy, stringsAsFactors = FALSE)
bg_param_setting <- cbind(bg_param_setting, data.frame(name = "NN", p = 1, P = 0, window_size = 1, granularity = 0, train_size = 1000, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE))
d <- run_sim(bg_param_setting, microsoft_max_10000, NULL, start_point = 2, cores = parallel::detectCores(), write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/TrainPolicy/NN/")

## Group Comparisons-Training Size

cut_off_prob <- c(0.001, 0.003, 0.005, 0.01, 0.03, 0.05)
train_size <- c(1000, 1500, 2000)
bg_param_setting <- expand.grid(cut_off_prob = cut_off_prob, train_size = train_size, stringsAsFactors = FALSE)
bg_param_setting <- cbind(bg_param_setting, data.frame(name = "ARIMA", window_size = 1, granularity = 0, train_size = 1000, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE))
d <- run_sim(bg_param_setting, microsoft_max_10000, NULL, start_point = 2, cores = parallel::detectCores(), write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/TrainPolicy/AR1/")

bg_param_setting <- expand.grid(cut_off_prob = cut_off_prob, train_size = train_size, stringsAsFactors = FALSE)
bg_param_setting <- cbind(bg_param_setting, data.frame(name = "ARIMA", window_size = 1, granularity = 0, train_size = 1000, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE))
d <- run_sim(bg_param_setting, microsoft_max_10000, as.matrix(dplyr::mutate_all(as.data.frame(microsoft_avg_10000), dplyr::lag, 1)), start_point = 2, cores = parallel::detectCores(), write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/TrainPolicy/AR1X/")

bg_param_setting <- expand.grid(cut_off_prob = cut_off_prob, train_size = train_size, stringsAsFactors = FALSE)
bg_param_setting <- cbind(bg_param_setting, data.frame(name = "MARKOV", window_size = 1, granularity = 0, train_size = 1000, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE))
d <- run_sim(bg_param_setting, microsoft_max_10000, NULL, start_point = 2, cores = parallel::detectCores(), write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/TrainPolicy/Markov/")

bg_param_setting <- expand.grid(cut_off_prob = cut_off_prob, train_size = train_size, cluster_type = c("fixed", "quantile"), stringsAsFactors = FALSE)
bg_param_setting <- cbind(bg_param_setting, data.frame(name = "MARKOV", window_size = 1, cluster_type = "quantile", granularity = 0, train_size = 1000, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE))
d <- run_sim(bg_param_setting, microsoft_max_10000, as.matrix(dplyr::mutate_all(as.data.frame(microsoft_avg_10000), dplyr::lag, 1)), start_point = 2, cores = parallel::detectCores(), write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/TrainPolicy/MarkovX/")

bg_param_setting <- expand.grid(cut_off_prob = cut_off_prob, train_size = train_size, stringsAsFactors = FALSE)
bg_param_setting <- cbind(bg_param_setting, data.frame(name = "VAR", p = 1, window_size = 1, granularity = 0, train_size = 1000, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE))
d <- run_sim(bg_param_setting, microsoft_max_10000, DataCenterSim::microsoft_avg_100[-c(1:12),], start_point = 2, cores = parallel::detectCores(), write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/TrainPolicy/VAR1/")

## Group Comparisons- Window Sizes
library(DataCenterSim)

load("~/microsoft_10000.rda")

microsoft_max_10000 <- microsoft_max_10000[1:3000, c(1:3019)[-c(286,290,328,380,387,398,399,704,706,718,720,738,813,1571,1637,1638,2021,3012,3018)]]
microsoft_avg_10000 <- microsoft_avg_10000[1:3000, c(1:3019)[-c(286,290,328,380,387,398,399,704,706,718,720,738,813,1571,1637,1638,2021,3012,3018)]]

cut_off_prob <- c(0.001, 0.003, 0.005, 0.007, 0.01, 0.03, 0.05, 0.07)
window_size <- c(10, 20, 30, 50)

## AR1
bg_param_setting <- expand.grid(cut_off_prob = cut_off_prob, window_size = window_size, stringsAsFactors = FALSE)
bg_param_setting <- cbind(bg_param_setting, data.frame(name = "ARIMA", train_size = 2000, train_policy = "fixed", granularity = 0, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE))
d <- run_sim(bg_param_setting, microsoft_max_10000, NULL, start_point = 2, cores = parallel::detectCores() , write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/WindowSize/AR1/")

## AR1X
bg_param_setting <- expand.grid(cut_off_prob = cut_off_prob, window_size = window_size, stringsAsFactors = FALSE)
bg_param_setting <- cbind(bg_param_setting, data.frame(name = "ARIMA", train_size = 2000, train_policy = "fixed", granularity = 0, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE))
d <- run_sim(bg_param_setting, microsoft_max_10000, as.matrix(dplyr::mutate_all(as.data.frame(microsoft_avg_10000), dplyr::lag, 1)), start_point = 2, cores = parallel::detectCores() , write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/WindowSize/AR1X/")

## LM
bg_param_setting <- expand.grid(cut_off_prob = cut_off_prob, window_size = window_size, stringsAsFactors = FALSE)
bg_param_setting <- cbind(bg_param_setting, data.frame(name = "LM", train_size = 1989, train_policy = "fixed", granularity = 0, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, window_size_for_reg = 12, window_type_for_reg = "max", stringsAsFactors = FALSE))
d <- run_sim(bg_param_setting, microsoft_max_10000, as.matrix(dplyr::mutate_all(as.data.frame(microsoft_max_10000), dplyr::lag, 12)), start_point = 13, wait_time = 11, cores = parallel::detectCores(), write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/WindowSize/AR1X/")

## Markov
bg_param_setting <- expand.grid(cut_off_prob = cut_off_prob, window_size = window_size, stringsAsFactors = FALSE)
bg_param_setting <- cbind(bg_param_setting, data.frame(name = "MARKOV", train_size = 2000, train_policy = "fixed", granularity = 0, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE))
d <- run_sim(bg_param_setting, microsoft_max_10000, NULL, start_point = 2, cores = parallel::detectCores() , write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/WindowSize/Markov/")

## MarkoVX
bg_param_setting <- expand.grid(cut_off_prob = cut_off_prob, window_size = window_size, cluster_type = c("fixed", "quantile"), stringsAsFactors = FALSE)
bg_param_setting <- cbind(bg_param_setting, data.frame(name = "MARKOV", train_size = 2000, train_policy = "fixed", granularity = 0, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE))
d <- run_sim(bg_param_setting, microsoft_max_10000, as.matrix(dplyr::mutate_all(as.data.frame(microsoft_avg_10000), dplyr::lag, 1)), start_point = 2, cores = parallel::detectCores() , write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/WindowSize/MarkovX/")

## VAR1
bg_param_setting <- expand.grid(cut_off_prob = cut_off_prob, window_size = window_size, stringsAsFactors = FALSE)
bg_param_setting <- cbind(bg_param_setting, data.frame(name = "VAR", p = 1, train_size = 2000, train_policy = "fixed", granularity = 0, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE))
d <- run_sim(bg_param_setting, microsoft_max_10000, microsoft_avg_10000, cores = parallel::detectCores() , write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/WindowSize/VAR1/")

## NN1
bg_param_setting <- expand.grid(cut_off_prob = cut_off_prob, window_size = window_size, stringsAsFactors = FALSE)
bg_param_setting <- cbind(bg_param_setting, data.frame(name = "NN", p = 1, P = 0, train_size = 2000, train_policy = "fixed", granularity = 0, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE))
d <- run_sim(bg_param_setting, microsoft_max_10000, NULL, start_point = 2, cores = parallel::detectCores() , write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/WindowSize/NN/")

## Autopilot
load("~/microsoft_generated_data_2000.rda")
microsoft_generated_data_2000.rda <- microsoft_generated_data_2000.rda[1:(3000 * 30),]
bg_param_setting <- expand.grid(cut_off_prob = cut_off_prob, window_size = window_size, stringsAsFactors = FALSE)
bg_param_setting <- cbind(bg_param_setting, data.frame(name = "AUTOPILOT", granularity = 0, train_policy = "fixed", train_size = 1000 * 30, statistics = "j-quantile", cut_off_weight = 0.01, model_num = 1, update_freq = 1, react_speed = "1,2", stringsAsFactors = FALSE))
d <- run_sim(bg_param_setting, microsoft_generated_data_2000, NULL, start_point = 31, cores = parallel::detectCores() - 1, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/Compare/half_life/")
