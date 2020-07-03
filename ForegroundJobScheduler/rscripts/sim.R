library("DataCenterSim")
library("dplyr")

## 1.3

### 1.3.1
bg_param_setting <- data.frame(name = "ARIMA", window_size = 12, cut_off_prob = 0.01, granularity = 0, train_policy = "offline", train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], NULL, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "paramwise", result_loc = "~/Documents/Model/AR1/")

bg_param_setting <- data.frame(name = "ARIMA", window_size = 12, cut_off_prob = 0.01, granularity = 0, train_policy = "offline", train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], as.matrix(as.matrix(dplyr::mutate_all(as.data.frame(DataCenterSim::microsoft_avg_100), dplyr::lag, 12)[-c(1:12),])), cores = 8, write_type = c("charwise", "paramwise"), plot_type = "paramwise", result_loc = "~/Documents/Model/AR1X/")

### 1.3.2
bg_param_setting <- data.frame(name = "ARIMA", window_size = 12, cut_off_prob = 0.01, granularity = 0, train_policy = "offline", train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
bg_param_setting$train_args <- list(order = c(1,1,0))
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], NULL, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "paramwise", result_loc = "~/Documents/Model/ARI11/")

bg_param_setting <- data.frame(name = "ARIMA", window_size = 12, cut_off_prob = 0.01, granularity = 0, train_policy = "offline", train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
bg_param_setting$train_args <- list(order = c(1,1,0))
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], as.matrix(dplyr::mutate_all(as.data.frame(DataCenterSim::microsoft_avg_100), dplyr::lag, 12)[-c(1:12),]), cores = 8, write_type = c("charwise", "paramwise"), plot_type = "paramwise", result_loc = "~/Documents/Model/ARI11X/")

bg_param_setting <- data.frame(name = "AUTO_ARIMA", window_size = 12, cut_off_prob = 0.01, granularity = 0, train_policy = "offline", train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], NULL, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "paramwise", result_loc = "~/Documents/Model/AUTOARIMA/")

bg_param_setting <- data.frame(name = "AUTO_ARIMA", window_size = 12, cut_off_prob = 0.01, granularity = 0, train_policy = "offline", train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], as.matrix(dplyr::mutate_all(as.data.frame(DataCenterSim::microsoft_avg_100), dplyr::lag, 12)[-c(1:12),]), cores = 8, write_type = c("charwise", "paramwise"), plot_type = "paramwise", result_loc = "~/Documents/Model/AUTOARIMAX/")

### 1.3.3
bg_param_setting <- data.frame(name = "ARIMA", window_size = 12, cut_off_prob = 0.01, granularity = 0, train_policy = "offline", train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, outlier_type = c("AO", "IO", "None", "All", "LS"), stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], NULL, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "paramwise", result_loc = "~/Documents/Outlier/")

### 1.3.4
bg_param_setting <- data.frame(name = "ARIMA", window_size = 12, cut_off_prob = 0.01, granularity = 0, train_policy = "offline", train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, res_dist = c("normal","skew_norm","empirical"), stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], NULL, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "paramwise", result_loc = "~/Documents/ResDist/")

### 1.3.5
bg_param_setting <- data.frame(name = "MARKOV", window_size = 12, cut_off_prob = 0.01, granularity = 0, train_policy = "offline", train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], NULL, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "paramwise", result_loc = "~/Documents/Model/Markov/")

bg_param_setting <- data.frame(name = "MARKOV", window_size = 12, cut_off_prob = 0.01, granularity = 0, train_policy = "offline", train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], as.matrix(dplyr::mutate_all(as.data.frame(DataCenterSim::microsoft_avg_100), dplyr::lag, 12)[-c(1:12),]), cores = 8, write_type = c("charwise", "paramwise"), plot_type = "paramwise", result_loc = "~/Documents/Model/MarkovX/")

bg_param_setting <- data.frame(name = "MARKOV", window_size = 12, cut_off_prob = 0.01, granularity = 0, train_policy = "offline", train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, cluster_type = "quantile", stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], NULL, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "paramwise", result_loc = "~/Documents/Model/Markov/")

bg_param_setting <- data.frame(name = "MARKOV", window_size = 12, cut_off_prob = 0.01, granularity = 0, train_policy = "offline", train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, cluster_type = "quantile", stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], as.matrix(as.matrix(dplyr::mutate_all(as.data.frame(DataCenterSim::microsoft_avg_100), dplyr::lag, 12)[-c(1:12),])), cores = 8, write_type = c("charwise", "paramwise"), plot_type = "paramwise", result_loc = "~/Documents/Model/MarkovX/")

### 1.3.6
bg_param_setting <- data.frame(name = "MARKOV", window_size = 12, cut_off_prob = 0.01, granularity = 0, train_policy = "offline", train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", state_num = c(8, 10, 16, 20), extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], NULL, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "paramwise", result_loc = "~/Documents/StateNum/")

### 1.3.7
bg_param_setting <- data.frame(name = "VAR", p = 1, window_size = 12, cut_off_prob = 0.01, granularity = 0, train_policy = "offline", train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], DataCenterSim::microsoft_avg_100[-c(1:12),], cores = 1, write_type = c("charwise", "paramwise"), plot_type = "paramwise", result_loc = "~/Documents/Model/VAR1/")

bg_param_setting <- data.frame(name = "VAR", p = 2, window_size = 12, cut_off_prob = 0.01, granularity = 0, train_policy = "offline", train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], DataCenterSim::microsoft_avg_100[-c(1:12),], cores = 8, write_type = c("charwise", "paramwise"), plot_type = "paramwise", result_loc = "~/Documents/Model/VAR2/")


## 1.4

### 1.4.1
bg_param_setting <- data.frame(name = "ARIMA", window_size = c(12, 6, 4, 3, 2, 1), cut_off_prob = 0.01, granularity = 0, train_policy = "offline", train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = c(1, 2, 3, 4, 6, 12), stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], NULL, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "paramwise", result_loc = "~/Documents/AggregationVSExtrapolation/")

## 1.5

### 1.5.1
bg_param_setting <- data.frame(name = "ARIMA", window_size = 12, cut_off_prob = 0.01, granularity = 0, train_policy = c("offline", "fixed", "dynamic"), train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], NULL, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "paramwise", result_loc = "~/Documents/TrainPolicy/")

### 1.5.2
bg_param_setting <- data.frame(name = "ARIMA", window_size = 12, cut_off_prob = 0.01, granularity = 0, train_policy = "dynamic", train_size = 840, model_num = 1, update_freq = c(3, 9, 27, 81), react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], NULL, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "paramwise", result_loc = "~/Documents/BatchSize/")

## 1.6

### 1.6.1
bg_param_setting <- data.frame(name = "ARIMA", window_size = 12, cut_off_prob = 0.01, granularity = 0, train_policy = "offline", train_size = 840, model_num = 1:4, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], NULL, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "paramwise", result_loc = "~/Documents/ModelNum/")

### 1.6.2
bg_param_setting <- data.frame(name = "ARIMA", window_size = 12, cut_off_prob = 0.01, granularity = 0, train_policy = "dynamic", train_size = 840, model_num = 1:4, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], NULL, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "paramwise", result_loc = "~/Documents/ModelNum/")

## 1.7

### 1.7.1
bg_param_setting <- data.frame(name = "ARIMA", window_size = 12, cut_off_prob = 0.01, granularity = c(0, 100 / 128, 100 / 64, 100 / 32), train_policy = "offline", train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], NULL, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "paramwise", result_loc = "~/Documents/Granularity/")

## 1.8

### 1.8.1
bg_param_setting <- data.frame(name = "ARIMA", window_size = 12, cut_off_prob = 0.01, granularity = 0, train_policy = "offline", train_size = 840, model_num = 1, update_freq = 3, react_speed = c("1,1", "1,2", "1,3", "2,1", "2,2", "2,3"), extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], NULL, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "paramwise", result_loc = "~/Documents/ReactSpeed/")

## 1.9

### 1.9.1
bg_param_setting <- data.frame(name = "ARIMA", window_size = 12, cut_off_prob = c(0.005, 0.01, 0.03, 0.05, 0.07, 0.1), granularity = 0, train_policy = "offline", train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], NULL, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "paramwise", result_loc = "~/Documents/CutOffProb/")


## 1.10

### 1.10.1
bg_param_setting <- data.frame(name = "ARIMA", window_size = 12, cut_off_prob = 0.01, granularity = 0, train_policy = "offline", train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, response = "avg", stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_avg_100[-c(1:12),], NULL, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "paramwise", result_loc = "~/Documents/Avg/Model/AR1/")

bg_param_setting <- data.frame(name = "ARIMA", window_size = 12, cut_off_prob = 0.01, granularity = 0, train_policy = "offline", train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, response = "avg", stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_avg_100[-c(1:12),], as.matrix(dplyr::mutate_all(as.data.frame(DataCenterSim::microsoft_max_100), dplyr::lag, 12)[-c(1:12),]), cores = 8, write_type = c("charwise", "paramwise"), plot_type = "paramwise", result_loc = "~/Documents/Avg/Model/AR1X/")

bg_param_setting <- data.frame(name = "ARIMA", window_size = 12, cut_off_prob = 0.01, granularity = 0, train_policy = "offline", train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, response = "avg", stringsAsFactors = FALSE)
bg_param_setting$train_args <- list(order = c(1,1,0))
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_avg_100[-c(1:12),], NULL, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "paramwise", result_loc = "~/Documents/Avg/Model/ARI11/")

bg_param_setting <- data.frame(name = "VAR", p = 1, window_size = 12, cut_off_prob = 0.01, granularity = 0, train_policy = "offline", train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, response = "avg", stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_avg_100[-c(1:12),], DataCenterSim::microsoft_max_100[-c(1:12),], cores = 1, write_type = c("charwise", "paramwise"), plot_type = "paramwise", result_loc = "~/Documents/Avg/Model/VAR1/")

### 1.10.2
bg_param_setting <- data.frame(name = "ARIMA", window_size = 12, cut_off_prob = 0.01, granularity = 0, train_policy = "fixed", train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, response = "avg", stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_avg_100[-c(1:12),], NULL, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "paramwise", result_loc = "~/Documents/Avg/TrainPolicy/AR1/")

bg_param_setting <- data.frame(name = "ARIMA", window_size = 12, cut_off_prob = 0.01, granularity = 0, train_policy = "dynamic", train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, response = "avg", stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_avg_100[-c(1:12),], NULL, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "paramwise", result_loc = "~/Documents/Avg/TrainPolicy/AR1/")

bg_param_setting <- data.frame(name = "ARIMA", window_size = 12, cut_off_prob = 0.01, granularity = 0, train_policy = "fixed", train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, response = "avg", stringsAsFactors = FALSE)
bg_param_setting$train_args <- list(order = c(1,1,0))
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_avg_100[-c(1:12),], NULL, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "paramwise", result_loc = "~/Documents/Avg/TrainPolicy/ARI11/")

bg_param_setting <- data.frame(name = "ARIMA", window_size = 12, cut_off_prob = 0.01, granularity = 0, train_policy = "dynamic", train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, response = "avg", stringsAsFactors = FALSE)
bg_param_setting$train_args <- list(order = c(1,1,0))
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_avg_100[-c(1:12),], NULL, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "paramwise", result_loc = "~/Documents/Avg/TrainPolicy/ARI11/")

### 1.10.3
bg_param_setting <- data.frame(name = "ARIMA", window_size = 12, cut_off_prob = c(0.001, 0.002, 0.003, 0.004, 0.005), granularity = 0, train_policy = "fixed", train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, response = "avg", stringsAsFactors = FALSE)
bg_param_setting$train_args <- list(order = c(1,1,0))
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_avg_100[-c(1:12),], NULL, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "paramwise", result_loc = "~/Documents/Avg/CutOffProb/ARI11/")

## 1.11
load("~/Documents/microsoft_generated_data.rda")

### 1.11.2
bg_param_setting <- data.frame(name = "AUTOPILOT", window_size = 300, cut_off_prob = c(0.001, 0.01, 0.05), granularity = 0, train_policy = "offline", train_size = 840 * 300, statistics = "j-quantile", cut_off_weight = 0.01, model_num = 1, react_speed = "1,2", stringsAsFactors = FALSE)
bg_param_setting$update_freq <- 7164
d <- run_sim(bg_param_setting, microsoft_generated_data, NULL, cores = 3, write_type = c("charwise", "paramwise"), plot_type = "paramwise", result_loc = "~/Documents/Compare/")

### 1.11.3
bg_param_setting <- data.frame(name = "AUTOPILOT", window_size = 300, n = c(36, 72, 144, 288), granularity = 0, train_policy = "offline", train_size = 840 * 300, statistics = "peak", cut_off_weight = 0.01, model_num = 1, react_speed = "1,2", stringsAsFactors = FALSE)
bg_param_setting$update_freq <- 7164
d <- run_sim(bg_param_setting, microsoft_generated_data, NULL, cores = 3, write_type = c("charwise", "paramwise"), plot_type = "paramwise", result_loc = "~/Documents/Compare/")

### 1.11.4
bg_param_setting <- data.frame(name = "AUTOPILOT", window_size = 300, cut_off_prob = 0.01, granularity = 0, train_policy = "offline", train_size = 840 * 300, statistics = "j-quantile", cut_off_weight = 0.01, model_num = 1, react_speed = "1,2", half_life = c(36, 72, 144), stringsAsFactors = FALSE)
bg_param_setting$update_freq <- 7164
d <- run_sim(bg_param_setting, microsoft_generated_data, NULL, cores = 3, write_type = c("charwise", "paramwise"), plot_type = "paramwise", result_loc = "~/Documents/Compare/")

### 1.11.5
bg_param_setting <- data.frame(name = "AUTOPILOT", window_size = 300, cut_off_prob = 0.01, granularity = 0, train_policy = "offline", train_size = 840 * 300, statistics = "j-quantile", cut_off_weight = 0.01, model_num = 1, react_speed = "1,2", half_life = 144, breaks = c(10, 20, 50), stringsAsFactors = FALSE)
bg_param_setting$update_freq <- 7164
d <- run_sim(bg_param_setting, microsoft_generated_data, NULL, cores = 3, write_type = c("charwise", "paramwise"), plot_type = "paramwise", result_loc = "~/Documents/Compare/")

### 1.11.6
bg_param_setting <- data.frame(name = "ARIMA", window_size = 300, cut_off_prob = c(0.001, 0.01, 0.05), granularity = 0, train_policy = "offline", train_size = 840 * 300, cut_off_weight = 0.01, model_num = 1, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, microsoft_generated_data, NULL, cores = 4, write_type = "none", plot_type = "none", result_loc = "~/Documents/Compare/")
bg_param_setting <- data.frame(name = "ARIMA", window_size = 300, cut_off_prob = c(0.001, 0.01, 0.05), granularity = 0, train_policy = "dynamic", train_size = 840 * 300, cut_off_weight = 0.01, model_num = 1, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, microsoft_generated_data, NULL, cores = 4, write_type = "none", plot_type = "none", result_loc = "~/Documents/Compare/")
bg_param_setting <- data.frame(name = "ARIMA", window_size = 300, cut_off_prob = c(0.001, 0.01, 0.05), granularity = 0, train_policy = "fixed", train_size = 840 * 300, cut_off_weight = 0.01, model_num = 1, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, microsoft_generated_data, NULL, cores = 4, write_type = "none", plot_type = "none", result_loc = "~/Documents/Compare/")
