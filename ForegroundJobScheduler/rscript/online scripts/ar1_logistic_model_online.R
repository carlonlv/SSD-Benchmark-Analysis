library("parallel")
library("dplyr")
library("forecast")
library("mvtnorm")
library("cluster")

if (Sys.info()["sysname"] == "Windows") {
  source("C://Users//carlo//Documents//GitHub//Research-Projects//ForegroundJobScheduler//rscript//helper_functions.R")
} else {
  source("/Users/carlonlv/Documents/Github/Research-Projects/ForegroundJobScheduler/rscript/helper_functions.R")
}

cores <- ifelse(Sys.info()["sysname"] == "Windows", 1, detectCores(all.tests = FALSE, logical = TRUE))


train_ar1_model <- function(ts_num, train_dataset) {
  
  ts_model <- tryCatch({
    arima(x=train_dataset[, ts_num], order = c(1,0,0), include.mean = TRUE, method = "CSS-ML", optim.control = list(maxit=2000))
  }, error = function(cond) {
    return(arima(x=train_dataset[, ts_num], order = c(1,0,0), include.mean = TRUE, method = "ML", optim.control = list(maxit=2000)))
  })
  return(list("coeffs"=as.numeric(ts_model$coef[1]), "means"= as.numeric(ts_model$coef[2]), "vars"=ts_model$sigma2))
}


generate_expected_conditional_var <- function(expected_avgs, variance_model) {
  
  expected_var <- NULL
  if (is.numeric(variance_model)) {
    expected_var <- rep(variance_model^2, length(expected_avgs))
  } else {
    expected_var <- predict(variance_model, newdata=data.frame("bin"=expected_avgs), type = "response")^2
  }
  return(max(expected_var, 0))
}


parser_for_logistic_model <- function(train_set_max, train_set_avg, cpu_required) {
  
  df <- data.frame("avg"=train_set_avg, "max"=train_set_max)
  df$survived <- ifelse(df$max <= (100 - cpu_required), 1, 0)
  return(df)
}


train_logistic_model <- function(ts_num, train_dataset_max, train_dataset_avg, cpu_required) {
  
  logistic_input <- parser_for_logistic_model(train_dataset_max[,ts_num], train_dataset_avg[,ts_num], cpu_required[ts_num])
  log.lm <- glm(survived~avg, data = logistic_input, family = "binomial", control=glm.control(maxit=2000))
  return(log.lm) 
}


do_prediction <- function(last_obs, phi, mean, variance) {
  
  # Construct mean
  mu <- last_obs * phi + (1 - phi) * mean
  # Construct Var-cov matrix
  var <- variance
  result <- list('mu' = mu, 'var'=var)
  return(result)
}


find_bin_obs <- function(avg, binsize) {
  
  bin <- NULL
  if (avg == 0) {
    bin <- 1
  } else {
    bin <- ifelse(avg %% binsize == 0, avg %/% binsize - 1, ceiling(avg / binsize))
  }
  return(bin)
}


train_cond_var_model <- function(ts_num, train_set_max, train_set_avg, bin_num, method) {
  
  new_parsed_dat <- data.frame(matrix(nrow=nrow(train_set_avg), ncol=3))
  binsize <- 100 / bin_num
  bin <- as.numeric(sapply(train_set_avg[,ts_num], find_bin_obs, binsize))
  bin <- bin * binsize
  for (i in 1:nrow(train_set_avg)) {
    new_parsed_dat[i,] = c(train_set_avg[i, ts_num], train_set_max[i, ts_num], bin[i])
  }
  
  colnames(new_parsed_dat) <- c('avg', 'max', 'bin')
  selected_bins <- new_parsed_dat %>%
    group_by(bin) %>%
    count()
  
  selected_bins <- subset(selected_bins, selected_bins$n >= 3)$bin
  selected_bins <- new_parsed_dat$bin
  new_parsed_dat <- new_parsed_dat %>%
    filter(bin %in% selected_bins) %>%
    group_by(bin) %>% 
    summarise(sd=sqrt(var(max))) %>%
    filter(!is.na(sd)) %>%
    filter(sd != 0)
  
  sd.lm <- NULL
  if (nrow(new_parsed_dat) >= 3) {
    if (method == "lm") {
      sd.lm <- lm(sd~bin+I(bin^2), data = new_parsed_dat)
    } else {
      sd.lm <- glm(sd~bin, data = new_parsed_dat, family = Gamma(link="log"))
    }
  } else if (nrow(new_parsed_dat) == 2) {
    if (method == "lm") {
      sd.lm <- lm(sd~bin, data = new_parsed_dat)
    } else {
      sd.lm <- glm(sd~bin, data = new_parsed_dat)
    }
  } else {
    sd.lm <- new_parsed_dat$sd
  }
  return(sd.lm)
}