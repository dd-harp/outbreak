# Convenience function
logit <- function(x) exp(x) / (1 + exp(x))

get_simulated_data <- function () {
  ## Generate synthetic data
  n_years <- 5
  n_steps <- n_years * 12 * 3
  x = seq(1, n_steps, length.out = n_steps / 3)
  data <- torch_tensor(cbind(
    x = x,
    y = logit(-2 + 0.5 * sin(x*pi / (36 / 2)) + rnorm(length(x), 0, 0.5) + 3 * (x >= 45 & x <= 75) * (1 - abs(x - 60) / 15))
  ))
  return(data)
}

# Grab pfpr time series from BoX
get_pfpr <- function(loc) {
  box_dir <- "/Users/aucarter/Library/CloudStorage/Box-Box/RAMP"
  pfpr_path <- file.path(box_dir, "Clean Data/pfpr_tpr_work/district_level_pfpr_time_series.csv")
  dt <- fread(pfpr_path)[district_name == loc]
  dt[, date := as.Date(paste(year, month, 1, sep = "-"))]
  data <- torch_tensor(cbind(
    x = 1:nrow(dt) * 3,
    y = dt[order(date)]$pfpr_pred
  ))
  return(data)
}

get_cases <- function(loc) {
  box_dir <- "/Users/aucarter/Library/CloudStorage/Box-Box/RAMP"
  cases_path <- file.path(box_dir, "data/dhis/monthly/clean/prod/clean_data.csv")
  pop_path <- file.path(box_dir, "External Data/dist_pop.csv")
  dt <- fread(cases_path)[location_name == loc & code_name == "conf_malaria"][order(period)]
  dt[, year := as.integer(substr(period, 1, 4))]
  dt <- dt[year > 2015]
  pop <- fread(pop_path)[admin3 == loc]$population
  data <- torch_tensor(cbind(
    x = 1:nrow(dt) * 3,
    y = dt$imputed_value / pop / (0.4) # Proportion seeking care in a public facility
  ))
  return(data)
}

get_pop <- function(loc) {
  box_dir <- "/Users/aucarter/Library/CloudStorage/Box-Box/RAMP"
  pop_path <- file.path(box_dir, "External Data/dist_pop.csv")
  pop <- fread(pop_path)[admin3 == loc]$population
  return(pop)
}


get_data <- function(loc = NULL) {
  if (is.null(loc)) {
    data <- get_simulated_data()
  } else {
    data <- get_cases(loc)
  } 
  return(data)
}

