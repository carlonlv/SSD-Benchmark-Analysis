library("DataCenterSim")
library("dplyr")

## 1.3

### 1.3.1
cut_off_prob <- c(0.001, 0.003, 0.005, 0.01, 0.03, 0.05)
bg_param_setting <- data.frame(name = "ARIMA", window_size = 12, cut_off_prob = cut_off_prob, granularity = 0, train_policy = "offline", train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], NULL, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/Model/AR1/")

bg_param_setting <- data.frame(name = "ARIMA", window_size = 12, cut_off_prob = cut_off_prob, granularity = 0, train_policy = "offline", train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], as.matrix(dplyr::mutate_all(as.data.frame(DataCenterSim::microsoft_avg_100), dplyr::lag, 12)[-c(1:12),]), cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/Model/AR1X/")

### 1.3.2
bg_param_setting <- data.frame(name = "ARIMA", window_size = 12, cut_off_prob = cut_off_prob, granularity = 0, train_policy = "offline", train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
bg_param_setting$train_args <- list(order = c(1,1,0))
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], NULL, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/Model/ARI11/")

bg_param_setting <- data.frame(name = "ARIMA", window_size = 12, cut_off_prob = cut_off_prob, granularity = 0, train_policy = "offline", train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
bg_param_setting$train_args <- list(order = c(1,1,0))
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], as.matrix(dplyr::mutate_all(as.data.frame(DataCenterSim::microsoft_avg_100), dplyr::lag, 12)[-c(1:12),]), cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/Model/ARI11X/")

bg_param_setting <- data.frame(name = "AUTO_ARIMA", window_size = 12, cut_off_prob = cut_off_prob, granularity = 0, train_policy = "offline", train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], NULL, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/Model/AUTOARIMA/")

bg_param_setting <- data.frame(name = "AUTO_ARIMA", window_size = 12, cut_off_prob = cut_off_prob, granularity = 0, train_policy = "offline", train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], as.matrix(dplyr::mutate_all(as.data.frame(DataCenterSim::microsoft_avg_100), dplyr::lag, 12)[-c(1:12),]), cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/Model/AUTOARIMAX/")

### 1.3.3
cut_off_prob <- c(0.001, 0.003, 0.005, 0.01, 0.03, 0.05)
outlier_type <- c("AO", "IO", "None", "All", "LS")
bg_param_setting <- expand.grid(cut_off_prob = cut_off_prob, outlier_type = outlier_type, stringsAsFactors = FALSE)
bg_param_setting <- cbind(bg_param_setting, data.frame(name = "ARIMA", window_size = 12, granularity = 0, train_policy = "offline", train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE))
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], NULL, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/Outlier/")

### 1.3.4
cut_off_prob <- c(0.001, 0.003, 0.005, 0.01, 0.03, 0.05)
res_dist <- c("normal","skew_norm","empirical")
bg_param_setting <- expand.grid(cut_off_prob = cut_off_prob, res_dist = res_dist)
bg_param_setting <- cbind(bg_param_setting, data.frame(name = "ARIMA", window_size = 12, granularity = 0, train_policy = "offline", train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE))
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], NULL, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/ResDist/")

### 1.3.5
cut_off_prob <- c(0.001, 0.003, 0.005, 0.01, 0.03, 0.05)
bg_param_setting <- data.frame(name = "MARKOV", window_size = 12, cut_off_prob = cut_off_prob, granularity = 0, train_policy = "offline", train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], NULL, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/Model/Clustertype/Markov/")

bg_param_setting <- data.frame(name = "MARKOV", window_size = 12, cut_off_prob = cut_off_prob, granularity = 0, train_policy = "offline", train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], as.matrix(dplyr::mutate_all(as.data.frame(DataCenterSim::microsoft_avg_100), dplyr::lag, 12)[-c(1:12),]), cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/Model/Clustertype/MarkovX/")

bg_param_setting <- data.frame(name = "MARKOV", window_size = 12, cut_off_prob = cut_off_prob, granularity = 0, train_policy = "offline", train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, cluster_type = "quantile", stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], NULL, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/Model/Clustertype/Markov/")

bg_param_setting <- data.frame(name = "MARKOV", window_size = 12, cut_off_prob = cut_off_prob, granularity = 0, train_policy = "offline", train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, cluster_type = "quantile", stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], as.matrix(dplyr::mutate_all(as.data.frame(DataCenterSim::microsoft_avg_100), dplyr::lag, 12)[-c(1:12),]), cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/Model/Clustertype/MarkovX/")

### 1.3.6
cut_off_prob <- c(0.001, 0.003, 0.005, 0.01, 0.03, 0.05)
state_num <- c(8, 10, 16, 20)
bg_param_setting <- expand.grid(cut_off_prob = cut_off_prob, state_num = state_num)
bg_param_setting <- cbind(bg_param_setting, data.frame(name = "MARKOV", window_size = 12, granularity = 0, train_policy = "offline", train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE))
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], NULL, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/Model/Markov/StateNum/")

### 1.3.7
cut_off_prob <- c(0.001, 0.003, 0.005, 0.01, 0.03, 0.05)
p <- c(1, 2, 3)
bg_param_setting <- expand.grid(cut_off_prob = cut_off_prob, p = p)
bg_param_setting <- cbind(bg_param_setting, data.frame(name = "VAR", window_size = 12, granularity = 0, train_policy = "offline", train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE))
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], DataCenterSim::microsoft_avg_100[-c(1:12),], cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/Model/VAR/p/")

### 1.3.8
cut_off_prob <- c(0.001, 0.003, 0.005, 0.01, 0.03, 0.05)
p <- c(1, 2, 3, 4, NA_real_)
bg_param_setting <- expand.grid(cut_off_prob = cut_off_prob, p = p)
bg_param_setting <- cbind(bg_param_setting, data.frame(name = "NN", P = 0, window_size = 12, granularity = 0, train_policy = "offline", train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE))
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], NULL, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/Model/NN/p/")

bg_param_setting <- expand.grid(cut_off_prob = cut_off_prob, p = p)
bg_param_setting <- cbind(bg_param_setting, data.frame(name = "NN", P = 0, window_size = 12, granularity = 0, train_policy = "offline", train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE))
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], as.matrix(dplyr::mutate_all(as.data.frame(DataCenterSim::microsoft_avg_100), dplyr::lag, 12)[-c(1:12),]), cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/Model/NNX/p/")

## 1.4

### 1.4.1
cut_off_prob <- c(0.001, 0.003, 0.005, 0.01, 0.03, 0.05)
window_size <- c(12, 6, 4, 3, 2, 1)
bg_param_setting <- expand.grid(cut_off_prob = cut_off_prob, window_size = window_size)
bg_param_setting$extrap_step <- 12 / bg_param_setting$window_size
bg_param_setting <- cbind(bg_param_setting, data.frame(name = "ARIMA", granularity = 0, train_policy = "offline", train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", stringsAsFactors = FALSE))
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], NULL, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/AggregationVSExtrapolation/")

## 1.5

### 1.5.1
cut_off_prob <- c(0.001, 0.003, 0.005, 0.01, 0.03, 0.05)
train_policy <- c("offline", "fixed", "dynamic")
bg_param_setting <- expand.grid(cut_off_prob = cut_off_prob, train_policy = train_policy, stringsAsFactors = FALSE)
bg_param_setting <- cbind(bg_param_setting, data.frame(name = "ARIMA", window_size = 12, granularity = 0, train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE))
bg_param_setting$target <- 1 - bg_param_setting$cut_off_prob
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], NULL, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/TrainPolicy/")

### 1.5.2
cut_off_prob <- c(0.001, 0.003, 0.005, 0.01, 0.03, 0.05)
update_freq <- c(3, 9, 27, 81)
bg_param_setting <- expand.grid(cut_off_prob = cut_off_prob, update_freq = update_freq)
bg_param_setting <- cbind(bg_param_setting, data.frame(name = "ARIMA", window_size = 12, granularity = 0, train_policy = "dynamic", train_size = 840, model_num = 1, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE))
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], NULL, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/BatchSize/AR1/")

bg_param_setting$train_policy <- "fixed"
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], NULL, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/BatchSize/AR1/")

### 1.5.3
cut_off_prob <- c(0.001, 0.003, 0.005, 0.01, 0.03, 0.05)
train_policy <- c("offline", "fixed")
bg_param_setting <- expand.grid(cut_off_prob = cut_off_prob, train_policy = train_policy, stringsAsFactors = FALSE)
bg_param_setting <- cbind(bg_param_setting, data.frame(name = "ARIMA", window_size = 12, granularity = 0, train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE))
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], NULL, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/TrainPolicy/AR1/")

bg_param_setting <- expand.grid(cut_off_prob = cut_off_prob, train_policy = train_policy, stringsAsFactors = FALSE)
bg_param_setting <- cbind(bg_param_setting, data.frame(name = "ARIMA", window_size = 12, granularity = 0, train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE))
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], as.matrix(dplyr::mutate_all(as.data.frame(DataCenterSim::microsoft_avg_100), dplyr::lag, 12)[-c(1:12),]), cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/TrainPolicy/AR1X/")

bg_param_setting <- expand.grid(cut_off_prob = cut_off_prob, train_policy = train_policy, stringsAsFactors = FALSE)
bg_param_setting <- cbind(bg_param_setting, data.frame(name = "MARKOV", window_size = 12, granularity = 0, train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE))
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], NULL, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/TrainPolicy/Markov/")

bg_param_setting <- expand.grid(cut_off_prob = cut_off_prob, train_policy = train_policy, cluster_type = c("fixed", "quantile"), stringsAsFactors = FALSE)
bg_param_setting <- cbind(bg_param_setting, data.frame(name = "MARKOV", window_size = 12, cluster_type = "quantile", granularity = 0, train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE))
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], as.matrix(dplyr::mutate_all(as.data.frame(DataCenterSim::microsoft_avg_100), dplyr::lag, 12)[-c(1:12),]), cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/TrainPolicy/MarkovX/")

bg_param_setting <- expand.grid(cut_off_prob = cut_off_prob, train_policy = train_policy, stringsAsFactors = FALSE)
bg_param_setting <- cbind(bg_param_setting, data.frame(name = "VAR", p = 1, window_size = 12, granularity = 0, train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE))
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], DataCenterSim::microsoft_avg_100[-c(1:12),], cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/TrainPolicy/VAR1/")

bg_param_setting <- expand.grid(cut_off_prob = cut_off_prob, train_policy = train_policy, stringsAsFactors = FALSE)
bg_param_setting <- cbind(bg_param_setting, data.frame(name = "NN", p = 1, P = 0, window_size = 12, granularity = 0, train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE))
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], NULL, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/TrainPolicy/NN/")

## 1.6

### 1.6.1
cut_off_prob <- c(0.001, 0.003, 0.005, 0.01, 0.03, 0.05)
model_num <- 1:4
bg_param_setting <- expand.grid(cut_off_prob = cut_off_prob, model_num = model_num)
bg_param_setting <- cbind(bg_param_setting, data.frame(name = "ARIMA", window_size = 12, granularity = 0, train_policy = "offline", train_size = 840, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE))
bg_param_setting$target <- 1 - bg_param_setting$cut_off_prob
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], NULL, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/ModelNum/")

### 1.6.2
cut_off_prob <- c(0.001, 0.003, 0.005, 0.01, 0.03, 0.05)
model_num <- 1:4
bg_param_setting <- expand.grid(cut_off_prob = cut_off_prob, model_num = model_num)
bg_param_setting <- cbind(bg_param_setting, data.frame(name = "ARIMA", window_size = 12, granularity = 0, train_policy = "dynamic", train_size = 840, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE))
bg_param_setting$target <- 1 - bg_param_setting$cut_off_prob
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], NULL, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/ModelNum/")

## 1.7

### 1.7.1
granularity <- c(0, 100 / 128, 100 / 64, 100 / 32)
bg_param_setting <- data.frame(name = "ARIMA", window_size = 12, cut_off_prob = 0.01, granularity = granularity, train_policy = "offline", train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], NULL, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/Granularity/")

## 1.8

### 1.8.1
cut_off_prob <- c(0.001, 0.003, 0.005, 0.01, 0.03, 0.05)
react_speed <- c("1,1", "1,2", "1,3", "2,1", "2,2", "2,3")
bg_param_setting <- expand.grid(cut_off_prob = cut_off_prob, react_speed = react_speed, stringsAsFactors = FALSE)
bg_param_setting <- cbind(bg_param_setting, data.frame(name = "ARIMA", window_size = 12, granularity = 0, train_policy = "offline", train_size = 840, model_num = 1, update_freq = 3, extrap_step = 1, stringsAsFactors = FALSE))
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], NULL, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/ReactSpeed/")

cut_off_prob <- c(0.001, 0.003, 0.005, 0.01, 0.03, 0.05)
react_speed <- c("1,1", "1,2", "1,3", "2,1", "2,2", "2,3")
bg_param_setting <- expand.grid(cut_off_prob = cut_off_prob, react_speed = react_speed, stringsAsFactors = FALSE)
bg_param_setting <- cbind(bg_param_setting, data.frame(name = "ARIMA", window_size = 12, granularity = 0, train_policy = "fixed", train_size = 840, model_num = 1, update_freq = 3, extrap_step = 1, stringsAsFactors = FALSE))
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], NULL, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/ReactSpeed/")

## 1.9

### 1.9.1
bg_param_setting <- data.frame(name = "ARIMA", window_size = 12, cut_off_prob = c(0.005, 0.01, 0.03, 0.05, 0.07, 0.1), granularity = 0, train_policy = "offline", train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], NULL, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/CutOffProb/")

## 1.10

### 1.10.1
cut_off_prob <- c(0.001, 0.003, 0.005, 0.01, 0.03, 0.05)
train_policy <- c("offline","fixed")
bg_param_setting <- expand.grid(cut_off_prob = cut_off_prob, train_policy = train_policy, stringsAsFactors = FALSE)
bg_param_setting <- cbind(bg_param_setting, data.frame(name = "ARIMA", window_size = 12, granularity = 0, train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, response = "avg", stringsAsFactors = FALSE))
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_avg_100[-c(1:12),], NULL, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/Avg/TrainPolicy/AR1/")

cut_off_prob <- c(0.001, 0.003, 0.005, 0.01, 0.03, 0.05)
train_policy <- c("offline","fixed")
bg_param_setting <- expand.grid(cut_off_prob = cut_off_prob, train_policy = train_policy, stringsAsFactors = FALSE)
bg_param_setting <- cbind(bg_param_setting, data.frame(name = "ARIMA", window_size = 12, granularity = 0, train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, response = "avg", stringsAsFactors = FALSE))
bg_param_setting$train_args <- list(order = c(1,1,0))
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_avg_100[-c(1:12),], NULL, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/Avg/TrainPolicy/ARI11/")

### 1.10.2
cut_off_prob <- c(0.001, 0.003, 0.005, 0.01, 0.03, 0.05)
bg_param_setting <- data.frame(name = "ARIMA", window_size = 12, cut_off_prob = cut_off_prob, granularity = 0, train_policy = "offline", train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, response = "avg", stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_avg_100[-c(1:12),], NULL, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/Avg/Model/AR1/")

bg_param_setting <- data.frame(name = "ARIMA", window_size = 12, cut_off_prob = cut_off_prob, granularity = 0, train_policy = "offline", train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, response = "avg", stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_avg_100[-c(1:12),], as.matrix(dplyr::mutate_all(as.data.frame(DataCenterSim::microsoft_max_100), dplyr::lag, 12)[-c(1:12),]), cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/Avg/Model/AR1X/")

bg_param_setting <- data.frame(name = "ARIMA", window_size = 12, cut_off_prob = cut_off_prob, granularity = 0, train_policy = "offline", train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, response = "avg", stringsAsFactors = FALSE)
bg_param_setting$train_args <- list(order = c(1,1,0))
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_avg_100[-c(1:12),], NULL, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/Avg/Model/ARI11/")

bg_param_setting <- data.frame(name = "VAR", p = 1, window_size = 12, cut_off_prob = cut_off_prob, granularity = 0, train_policy = "offline", train_size = 840, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, response = "avg", stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_avg_100[-c(1:12),], DataCenterSim::microsoft_max_100[-c(1:12),], cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/Avg/Model/VAR1/")

## 1.11

### 1.11.1
load("~/Documents/Generateddata_V2/microsoft_generated_data_V2.rda")
cut_off_prob <- c(0.001, 0.010, 0.050, 0.07, 0.08, 0.1)
bg_param_setting <- data.frame(name = "AUTOPILOT", window_size = 300, cut_off_prob = cut_off_prob, granularity = 0, train_policy = "fixed", train_size = 840 * 300, statistics = "j-quantile", cut_off_weight = 0.01, model_num = 1, update_freq = 1, react_speed = "1,2", stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, microsoft_generated_data, NULL, cores = 3, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/Compare/Generateddata_V2/j/")

load("~/Documents/Generateddata_V3/microsoft_generated_data_V3.rda")
d <- run_sim(bg_param_setting, microsoft_generated_data, NULL, cores = 3, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/Compare/Generateddata_V3/j/")

cut_off_prob <- c(0.0001, 0.0003, 0.0005, 0.001, 0.010, 0.050)
load("~/Documents/Generateddata_V4/microsoft_generated_data_V4.rda")
bg_param_setting <- data.frame(name = "AUTOPILOT", window_size = 300, cut_off_prob = cut_off_prob, granularity = 0, train_policy = "fixed", train_size = 840 * 300, statistics = "j-quantile", cut_off_weight = 0.01, model_num = 1, update_freq = 1, react_speed = "1,2", stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, microsoft_generated_data, NULL, cores = 3, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/Compare/Generateddata_V4/j/")

### 1.11.2
load("~/Documents/Generateddata_V2/microsoft_generated_data_V2.rda")
cut_off_prob <- c(0.001, 0.003, 0.005, 0.007, 0.008, 0.009, 0.010, 0.020, 0.030, 0.050)
half_life <- c(1, 3, 6, 12, 36, 72)
bg_param_setting <- expand.grid(cut_off_prob = cut_off_prob, half_life = half_life)
bg_param_setting <- cbind(bg_param_setting, data.frame(name = "AUTOPILOT", window_size = 300, granularity = 0, train_policy = "fixed", train_size = 840 * 300, statistics = "j-quantile", cut_off_weight = 0.01, model_num = 1, update_freq = 1, react_speed = "1,2", stringsAsFactors = FALSE))
d <- run_sim(bg_param_setting, microsoft_generated_data, NULL, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/Compare/Generateddata_V2/half_life/")

### 1.11.3
load("~/Documents/Generateddata_V2/microsoft_generated_data_V2.rda")
cut_off_prob <- c(0.001, 0.003, 0.005, 0.010, 0.050)
breaks <- c(10, 20, 50)
bg_param_setting <- expand.grid(cut_off_prob = cut_off_prob, breaks = breaks)
bg_param_setting <- cbind(bg_param_setting, data.frame(name = "AUTOPILOT", window_size = 300, half_life = 36, granularity = 0, train_policy = "fixed", train_size = 840 * 300, statistics = "j-quantile", cut_off_weight = 0.01, model_num = 1, update_freq = 1, react_speed = "1,2", stringsAsFactors = FALSE))
d <- run_sim(bg_param_setting, microsoft_generated_data, NULL, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/Compare/Generateddata_V2/breaks/")

### 1.11.5
name = "ARIMA"
window_size = c(2, 3, 6, 12, 36)
cut_off_prob = c(0.01, 0.03)
granularity = 0
model_num = 1
train_policy = "fixed"
train_size = 840
update_freq = 3
extrap_step = 1
react_speed = c("1,1", "1,2", "2,1")
bg_param_setting <- expand.grid(name = name, window_size = window_size, cut_off_prob = cut_off_prob, granularity = granularity, model_num = model_num, train_policy = train_policy, train_size = train_size, update_freq = update_freq, extrap_step = extrap_step, react_speed = react_speed, stringsAsFactors = FALSE)
bg_param_setting$target <- 1 - bg_param_setting$cut_off_prob

bg_param_setting$train_policy <- "fixed"
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], NULL, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/WindowSize/AR1/")

bg_param_setting$train_policy <- "fixed"
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], as.matrix(dplyr::mutate_all(as.data.frame(DataCenterSim::microsoft_avg_100), dplyr::lag, 12)[-c(1:12),]), cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/WindowSize/AR1X/")

bg_param_setting$train_policy <- "fixed"
bg_param_setting$train_args <- list("order" = c(1,1,0))
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], NULL, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/WindowSize/ARI11/")

bg_param_setting$train_policy <- "fixed"
bg_param_setting$train_args <- list("order" = c(1,1,0))
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], as.matrix(dplyr::mutate_all(as.data.frame(DataCenterSim::microsoft_avg_100), dplyr::lag, 12)[-c(1:12),]), cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/WindowSize/ARI11X/")

bg_param_setting$name <- "NN"
bg_param_setting$p <- 1
bg_param_setting$train_policy <- "fixed"
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], as.matrix(dplyr::mutate_all(as.data.frame(DataCenterSim::microsoft_avg_100), dplyr::lag, 12)[-c(1:12),]), cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/Documents/WindowSize/NN/")

load("~/microsoft_generated_data_V2.rda")
bg_param_setting$name <- "AUTOPILOT"
bg_param_setting$statistics <- "j-quantile"
bg_param_setting$window_size <- bg_param_setting$window_size * 300
bg_param_setting$update_freq <- 1
bg_param_setting$train_policy <- "fixed"
bg_param_setting$half_life <- 36
bg_param_setting$train_args <- NULL
bg_param_setting$cut_off_weight <- 0.01
bg_param_setting$train_size <- 840 * 300
d <- run_sim(bg_param_setting, microsoft_generated_data[-c(1:3600),], NULL, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/WindowSize/Autopilot/")

### 1.11.7
load("~/microsoft_generated_data_V2.rda")

bg_param_setting <- data.frame(name = "ARIMA", window_size = 1, cut_off_prob = 0.01, granularity = 0, train_policy = c("offline", "fixed"), train_size = 120, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], NULL, start_point = 1801, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/TrainSize/TrainSize120/AR1/")

bg_param_setting <- data.frame(name = "MARKOV", window_size = 1, cut_off_prob = 0.01, granularity = 0, train_policy = c("offline", "fixed"), train_size = 120, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], NULL, start_point = 1801, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/TrainSize/TrainSize120/Markov/")

bg_param_setting <- data.frame(name = "VAR", p = 1, window_size = 1, cut_off_prob = 0.01, granularity = 0, train_policy = c("offline", "fixed"), train_size = 120, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], DataCenterSim::microsoft_avg_100[-c(1:12),], start_point = 1801, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/TrainSize/TrainSize120/VAR1/")

bg_param_setting <- data.frame(name = "NN", P = 0, window_size = 1, cut_off_prob = 0.01, granularity = 0, train_policy = c("offline", "fixed"), train_size = 120, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], NULL, start_point = 1801, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/TrainSize/TrainSize120/NN/")

bg_param_setting <- data.frame(name = "AUTOPILOT", window_size = 300, cut_off_prob = 0.01, granularity = 0, train_policy = c("offline", "fixed"), train_size = 120 * 300, statistics = "j-quantile", cut_off_weight = 0.01, model_num = 1, react_speed = "1,2", stringsAsFactors = FALSE)
bg_param_setting$update_freq <- 1
d <- run_sim(bg_param_setting, microsoft_generated_data, NULL, start_point = 1800 * 300 + 1, cores = 3, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/TrainSize/TrainSize120/Autopilot/")

bg_param_setting <- data.frame(name = "ARIMA", window_size = 1, cut_off_prob = 0.01, granularity = 0, train_policy = c("offline", "fixed"), train_size = 240, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], NULL, start_point = 1681, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/TrainSize/TrainSize240/AR1/")

bg_param_setting <- data.frame(name = "MARKOV", window_size = 1, cut_off_prob = 0.01, granularity = 0, train_policy = c("offline", "fixed"), train_size = 240, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], NULL, start_point = 1681, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/TrainSize/TrainSize240/Markov/")

bg_param_setting <- data.frame(name = "VAR", p = 1, window_size = 1, cut_off_prob = 0.01, granularity = 0, train_policy = c("offline", "fixed"), train_size = 240, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], DataCenterSim::microsoft_avg_100[-c(1:12),], start_point = 1681, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/TrainSize/TrainSize240/VAR1/")

bg_param_setting <- data.frame(name = "NN", P = 0, window_size = 1, cut_off_prob = 0.01, granularity = 0, train_policy = c("offline", "fixed"), train_size = 240, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], NULL, start_point = 1681, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/TrainSize/TrainSize240/NN/")

bg_param_setting <- data.frame(name = "AUTOPILOT", window_size = 300, cut_off_prob = 0.01, granularity = 0, train_policy = c("offline", "fixed"), train_size = 240 * 300, statistics = "j-quantile", cut_off_weight = 0.01, model_num = 1, react_speed = "1,2", stringsAsFactors = FALSE)
bg_param_setting$update_freq <- 1
d <- run_sim(bg_param_setting, microsoft_generated_data, NULL, start_point = 1680 * 300 + 1, cores = 3, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/TrainSize/TrainSize240/Autopilot/")

bg_param_setting <- data.frame(name = "ARIMA", window_size = 1, cut_off_prob = 0.01, granularity = 0, train_policy = c("offline", "fixed"), train_size = 480, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], NULL, start_point = 1441, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/TrainSize/TrainSize480/AR1/")

bg_param_setting <- data.frame(name = "MARKOV", window_size = 1, cut_off_prob = 0.01, granularity = 0, train_policy = c("offline", "fixed"), train_size = 480, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], NULL, start_point = 1441, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/TrainSize/TrainSize480/Markov/")

bg_param_setting <- data.frame(name = "VAR", p = 1, window_size = 1, cut_off_prob = 0.01, granularity = 0, train_policy = c("offline", "fixed"), train_size = 480, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], DataCenterSim::microsoft_avg_100[-c(1:12),], start_point = 1441, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/TrainSize/TrainSize480/VAR1/")

bg_param_setting <- data.frame(name = "NN", P = 0, window_size = 1, cut_off_prob = 0.01, granularity = 0, train_policy = c("offline", "fixed"), train_size = 480, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], NULL, start_point = 1441, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/TrainSize/TrainSize480/NN/")

bg_param_setting <- data.frame(name = "AUTOPILOT", window_size = 300, cut_off_prob = 0.01, granularity = 0, train_policy = c("offline", "fixed"), train_size = 480 * 300, statistics = "j-quantile", cut_off_weight = 0.01, model_num = 1, react_speed = "1,2", stringsAsFactors = FALSE)
bg_param_setting$update_freq <- 1
d <- run_sim(bg_param_setting, microsoft_generated_data, NULL, start_point = 1440 * 300 + 1, cores = 3, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/TrainSize/TrainSize480/Autopilot/")

bg_param_setting <- data.frame(name = "ARIMA", window_size = 1, cut_off_prob = 0.01, granularity = 0, train_policy = c("offline", "fixed"), train_size = 960, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], NULL, start_point = 961, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/TrainSize/TrainSize960/AR1/")

bg_param_setting <- data.frame(name = "MARKOV", window_size = 1, cut_off_prob = 0.01, granularity = 0, train_policy = c("offline", "fixed"), train_size = 960, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], NULL, start_point = 961, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/TrainSize/TrainSize960/Markov/")

bg_param_setting <- data.frame(name = "VAR", p = 1, window_size = 1, cut_off_prob = 0.01, granularity = 0, train_policy = c("offline", "fixed"), train_size = 960, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], DataCenterSim::microsoft_avg_100[-c(1:12),], start_point = 961, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/TrainSize/TrainSize960/VAR1/")

bg_param_setting <- data.frame(name = "NN", P = 0, window_size = 1, cut_off_prob = 0.01, granularity = 0, train_policy = c("offline", "fixed"), train_size = 960, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], NULL, start_point = 961, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/TrainSize/TrainSize960/NN/")

bg_param_setting <- data.frame(name = "AUTOPILOT", window_size = 300, cut_off_prob = 0.01, granularity = 0, train_policy = c("offline", "fixed"), train_size = 960 * 300, statistics = "j-quantile", cut_off_weight = 0.01, model_num = 1, react_speed = "1,2", stringsAsFactors = FALSE)
bg_param_setting$update_freq <- 1
d <- run_sim(bg_param_setting, microsoft_generated_data, NULL, start_point = 960 * 300 + 1, cores = 3, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/TrainSize/TrainSize960/Autopilot/")

bg_param_setting <- data.frame(name = "ARIMA", window_size = 1, cut_off_prob = 0.01, granularity = 0, train_policy = c("offline", "fixed"), train_size = 1920, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], NULL, start_point = 1, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/TrainSize/TrainSize1920/AR1/")

bg_param_setting <- data.frame(name = "MARKOV", window_size = 1, cut_off_prob = 0.01, granularity = 0, train_policy = c("offline", "fixed"), train_size = 1920, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], NULL, start_point = 1, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/TrainSize/TrainSize1920/Markov/")

bg_param_setting <- data.frame(name = "VAR", p = 1, window_size = 1, cut_off_prob = 0.01, granularity = 0, train_policy = c("offline", "fixed"), train_size = 1920, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], DataCenterSim::microsoft_avg_100[-c(1:12),], start_point = 1, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/TrainSize/TrainSize1920/VAR1/")

bg_param_setting <- data.frame(name = "NN", P = 0, window_size = 1, cut_off_prob = 0.01, granularity = 0, train_policy = c("offline", "fixed"), train_size = 1920, model_num = 1, update_freq = 3, react_speed = "1,2", extrap_step = 1, stringsAsFactors = FALSE)
d <- run_sim(bg_param_setting, DataCenterSim::microsoft_max_100[-c(1:12),], NULL, start_point = 1, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/TrainSize/TrainSize1920/NN/")

bg_param_setting <- data.frame(name = "AUTOPILOT", window_size = 300, cut_off_prob = 0.01, granularity = 0, train_policy = c("offline", "fixed"), train_size = 1920 * 300, statistics = "j-quantile", cut_off_weight = 0.01, model_num = 1, react_speed = "1,2", stringsAsFactors = FALSE)
bg_param_setting$update_freq <- 1
d <- run_sim(bg_param_setting, microsoft_generated_data, NULL, start_point = 1, cores = 8, write_type = c("charwise", "paramwise"), plot_type = "none", result_loc = "~/TrainSize/TrainSize1920/Autopilot/")

## 2.3
load("~/Documents/Generateddata_V2/microsoft_generated_data_V2.rda")

total_num_jobs <- 10000

bins <- c(0, 1:6, 14, 18, 22, 26, 30, 50, 80, 205)

sim_length <- 200

machine_num <- 3

model_name <- c("ANOVATREE", "SURTREE")

cut_off_prob <- c(1 - sqrt(0.99), 0.01, 0.02, 0.05)

for (i in model_name) {
  print(paste("Model Name:", i))

  for (k in cut_off_prob) {
    print(paste("Cut off prob:", k))

    set.seed(10)
    param_setting_sim <- data.frame(name = "ARIMA", window_size = 1, train_size = 3000, update_freq = 1, train_policy = "offline", cut_off_prob = k, stringsAsFactors = FALSE)
    param_setting_pred <- data.frame(name = i, train_size = 5000, stringsAsFactors = FALSE)

    dd <- run_sim_pred(param_setting_sim, param_setting_pred, microsoft_max_100, NULL, FALSE, sim_length, machine_num, FALSE, google_runtime_data[, 9], google_runtime_data[, -9], TRUE, total_num_jobs, bins, repeats = 20, cores = 8, write_type = "summary", result_loc = "~/Documents/CombinedFgBg/BgEfficacy/")
  }
}

## 3.2

### 3.2.1
load("~/Documents/Generateddata_V2/microsoft_generated_data_V2.rda")

total_num_jobs <- c(6000, 7000, 8000, 10000)

bins <- c(0, 1:6, 14, 18, 22, 26, 30, 50, 80, 205)

sim_length <- 200

machine_num <- 3

for (i in total_num_jobs) {
  print(paste("Number of Jobs:", i))

  set.seed(10)
  param_setting_sim <- data.frame(name = "ARIMA", window_size = 1, train_size = 3000, update_freq = 1, train_policy = "offline", cut_off_prob = 1 - sqrt(0.99), stringsAsFactors = FALSE)
  param_setting_pred <- data.frame(name = "ANOVATREE", train_size = 5000, stringsAsFactors = FALSE)

  dd <- run_sim_pred(param_setting_sim, param_setting_pred, microsoft_max_100, NULL, FALSE, sim_length, machine_num, FALSE, google_runtime_data[, 9], google_runtime_data[, -9], FALSE, i, bins, repeats = 20, cores = 8, write_type = "summary", result_loc = "~/Documents/CombinedFgBg/NumJobs/")
}

total_num_jobs <- 10000

bins <- c(0, 1:6, 14, 18, 22, 26, 30, 50, 80, 205)

sim_length <- c(100, 200, 500, 1000)

machine_num <- 3

param_setting_sim <- data.frame(name = "ARIMA", window_size = 1, train_size = 3000, update_freq = 1, train_policy = "offline", cut_off_prob = 1 - sqrt(0.99), stringsAsFactors = FALSE)
param_setting_pred <- data.frame(name = "ANOVATREE", train_size = 5000, stringsAsFactors = FALSE)

for (i in sim_length) {
  print(paste("Sim Length:", i))
  set.seed(10)
  param_setting_sim <- data.frame(name = "ARIMA", window_size = 1, train_size = 3000, update_freq = 1, train_policy = "offline", cut_off_prob = 1 - sqrt(0.99), stringsAsFactors = FALSE)
  param_setting_pred <- data.frame(name = "ANOVATREE", train_size = 5000, stringsAsFactors = FALSE)

  dd <- run_sim_pred(param_setting_sim, param_setting_pred, microsoft_max_100, NULL, FALSE, i, machine_num, FALSE, google_runtime_data[, 9], google_runtime_data[, -9], FALSE, total_num_jobs, bins, repeats = 20, cores = 8, write_type = "summary", result_loc = "~/Documents/CombinedFgBg/SimLength/")
}

### 3.2.2
load("~/Documents/Generateddata_V2/microsoft_generated_data_V2.rda")

total_num_jobs <- 10000

bins <- c(0, 1:6, 14, 18, 22, 26, 30, 50, 80, 205)

sim_length <- 200

machine_num <- 3

model_name <- c("ANOVATREE", "SURTREE")

cut_off_prob <- c(1 - sqrt(0.99), 0.01, 0.02, 0.05)

for (i in model_name) {
  print(paste("Model Name:", i))

    for (k in cut_off_prob) {
    print(paste("Cut off prob:", k))
    set.seed(10)
    param_setting_sim <- data.frame(name = "ARIMA", window_size = 1, train_size = 3000, update_freq = 1, train_policy = "offline", cut_off_prob = k, stringsAsFactors = FALSE)
    param_setting_pred <- data.frame(name = i, train_size = 5000, stringsAsFactors = FALSE)

    dd <- run_sim_pred(param_setting_sim, param_setting_pred, microsoft_max_100, NULL, FALSE, sim_length, machine_num, FALSE, google_runtime_data[, 9], google_runtime_data[, -9], FALSE, total_num_jobs, bins, repeats = 20, cores = 8, write_type = "summary", result_loc = "~/Documents/CombinedFgBg/BgModels/")
  }
}

### 3.2.3
load("~/Documents/Generateddata_V2/microsoft_generated_data_V2.rda")

total_num_jobs <- 10000

bins <- c(0, 1:6, 14, 18, 22, 26, 30, 50, 80, 205)

sim_length <- 200

machine_num <- 3

cut_off_probs <- c(1 - sqrt(0.99), 0.01, 0.02, 0.05, 0.07, 0.08, 0.09, 0.10)

sim_setting_list <- list("Autopilot" = data.frame(name = "AUTOPILOT", window_size = 300, train_size = 3000, update_freq = 1, train_policy = "fixed", half_life = 36, stringsAsFactors = FALSE),
                         "AR1" = data.frame(name = "ARIMA", window_size = 1, train_size = 3000, update_freq = 1, train_policy = "fixed", stringsAsFactors = FALSE),
                         "ARI11" = data.frame(name = "ARIMA", window_size = 1, train_size = 3000, update_freq = 1, train_policy = "fixed", stringsAsFactors = FALSE),
                         "ARI11X" = data.frame(name = "ARIMA", window_size = 1, train_size = 3000, update_freq = 1, train_policy = "fixed", stringsAsFactors = FALSE),
                         "AR1X" = data.frame(name = "ARIMA", window_size = 1, train_size = 3000, update_freq = 1, train_policy = "fixed", stringsAsFactors = FALSE),
                         "VAR1" = data.frame(name = "VAR", window_size = 1, train_size = 3000, update_freq = 1, train_policy = "fixed", stringsAsFactors = FALSE))

sim_setting_list[["ARI11"]]$train_args <- list(order = c(1,1,0))
sim_setting_list[["ARI11X"]]$train_args <- list(order = c(1,1,0))

for (i in names(sim_setting_list)) {
  print(paste("Model name", i))

  for (j in cut_off_probs) {
    print(paste("Cut off prob", j))
    set.seed(10)

    param_setting_sim <- sim_setting_list[[i]]
    param_setting_sim$cut_off_prob <- j
    param_setting_pred <- data.frame(name = "ANOVATREE", train_size = 5000, stringsAsFactors = FALSE)

    if (grepl("X", i)) {
      dd <- run_sim_pred(param_setting_sim, param_setting_pred, microsoft_max_100, microsoft_avg_100, TRUE, sim_length, machine_num, FALSE, google_runtime_data[, 9], google_runtime_data[, -9], FALSE, total_num_jobs, bins, repeats = 20, cores = 8, write_type = "summary", result_loc = "~/Documents/CombinedFgBg/FgModels/")
    } else if (i == "VAR1") {
      dd <- run_sim_pred(param_setting_sim, param_setting_pred, microsoft_max_100, microsoft_avg_100, FALSE, sim_length, machine_num, FALSE, google_runtime_data[, 9], google_runtime_data[, -9], FALSE, total_num_jobs, bins, repeats = 20, cores = 8, write_type = "summary", result_loc = "~/Documents/CombinedFgBg/FgModels/")
    } else if (i == "Autopilot") {
      dd <- run_sim_pred(param_setting_sim, param_setting_pred, microsoft_generated_data, NULL, FALSE, sim_length, machine_num, FALSE, google_runtime_data[, 9], google_runtime_data[, -9], FALSE, total_num_jobs, bins, repeats = 20, cores = 8, write_type = "summary", result_loc = "~/Documents/CombinedFgBg/FgModels/")
    } else {
      dd <- run_sim_pred(param_setting_sim, param_setting_pred, microsoft_max_100, NULL, FALSE, sim_length, machine_num, FALSE, google_runtime_data[, 9], google_runtime_data[, -9], FALSE, total_num_jobs, bins, repeats = 20, cores = 8, write_type = "summary", result_loc = "~/Documents/CombinedFgBg/FgModels/")
    }
  }
}


### 3.2.4
load("~/Documents/Generateddata_V2/microsoft_generated_data_V2.rda")

total_num_jobs <- 10000

bins <- c(0, 1:6, 14, 18, 22, 26, 30, 50, 80, 205)

sim_length <- 200

machine_num <- 3

cut_off_probs <- c(1 - sqrt(0.99), 0.01, 0.02, 0.05, 0.07, 0.08, 0.09, 0.10)

sim_setting_list <- list("Autopilot" = data.frame(name = "AUTOPILOT", window_size = 300, train_size = 3000, update_freq = 1, train_policy = "fixed", half_life = 36, stringsAsFactors = FALSE),
                         "AR1" = data.frame(name = "ARIMA", window_size = 1, train_size = 3000, update_freq = 1, train_policy = "fixed", stringsAsFactors = FALSE),
                         "ARI11" = data.frame(name = "ARIMA", window_size = 1, train_size = 3000, update_freq = 1, train_policy = "fixed", stringsAsFactors = FALSE),
                         "ARI11X" = data.frame(name = "ARIMA", window_size = 1, train_size = 3000, update_freq = 1, train_policy = "fixed", stringsAsFactors = FALSE),
                         "AR1X" = data.frame(name = "ARIMA", window_size = 1, train_size = 3000, update_freq = 1, train_policy = "fixed", stringsAsFactors = FALSE),
                         "NN" = data.frame(name = "NN", window_size = 1, train_size = 3000, update_freq = 1, train_policy = "fixed", p = 1, stringsAsFactors = FALSE))

sim_setting_list[["ARI11"]]$train_args <- list(order = c(1,1,0))
sim_setting_list[["ARI11X"]]$train_args <- list(order = c(1,1,0))

for (i in names(sim_setting_list)) {
  print(paste("Model name", i))

  for (j in cut_off_probs) {
    print(paste("Cut off prob", j))
    set.seed(10)

    param_setting_sim <- sim_setting_list[[i]]
    param_setting_sim$cut_off_prob <- j
    param_setting_pred <- data.frame(name = "ANOVATREE", train_size = 5000, stringsAsFactors = FALSE)

    if (grepl("X", i)) {
      dd <- run_sim_pred(param_setting_sim, param_setting_pred, microsoft_max_100, microsoft_avg_100, TRUE, sim_length, machine_num, TRUE, google_runtime_data[, 9], google_runtime_data[, -9], FALSE, total_num_jobs, bins, repeats = 20, cores = 8, write_type = "summary", result_loc = "~/Documents/CombinedFgBg/FgModels/")
    } else if (i == "VAR1") {
      dd <- run_sim_pred(param_setting_sim, param_setting_pred, microsoft_max_100, microsoft_avg_100, FALSE, sim_length, machine_num, TRUE, google_runtime_data[, 9], google_runtime_data[, -9], FALSE, total_num_jobs, bins, repeats = 20, cores = 8, write_type = "summary", result_loc = "~/Documents/CombinedFgBg/FgModels/")
    } else if (i == "Autopilot") {
      dd <- run_sim_pred(param_setting_sim, param_setting_pred, microsoft_generated_data, NULL, FALSE, sim_length, machine_num, TRUE, google_runtime_data[, 9], google_runtime_data[, -9], FALSE, total_num_jobs, bins, repeats = 20, cores = 8, write_type = "summary", result_loc = "~/Documents/CombinedFgBg/FgModels/")
    } else {
      dd <- run_sim_pred(param_setting_sim, param_setting_pred, microsoft_max_100, NULL, FALSE, sim_length, machine_num, TRUE, google_runtime_data[, 9], google_runtime_data[, -9], FALSE, total_num_jobs, bins, repeats = 20, cores = 8, write_type = "summary", result_loc = "~/Documents/CombinedFgBg/FgModels/")
    }
  }
}
