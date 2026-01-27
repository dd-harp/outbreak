

percentile_threshold <- function(past, current, percentile = 0.75) {
  return(as.integer(current > quantile(past, percentile)))
}

mean_sd_threshold <- function(past, current, n_sd = 2) {
  return(as.integer(current > (mean(past) + n_sd * sd(past))))
}

c_sum_threshold <- function(past, current, n_weeks) {
  
}

past <- runif(50)
current <- 0.74
percentile <- 0.75
percentile_threshold(past, current, percentile)
mean_sd_threshold(past, current)
