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

# Read PfPR time series from BigQuery
get_pfpr <- function(loc) {
  # TODO: migrate pfpr data to BigQuery
  message("get_pfpr: PfPR data not yet available in BigQuery")
  stop("PfPR data migration pending")
}

#' Get malaria case data for a district from BigQuery
#'
#' @param loc District name
#' @return torch tensor with x (time index) and y (cases / population)
get_cases <- function(loc) {
  library(ramptools)
  dt <- bq_get_clean_data(
    frequency = "monthly",
    code_names = "conf_malaria",
    levels = 3L
  )
  dt <- dt[location_name == loc][order(period)]
  dt[, year := as.integer(substr(period, 1, 4))]
  dt <- dt[year > 2015]
  pop <- district_pop[district_pop$admin3 == loc, ]$population
  data <- torch_tensor(cbind(
    x = 1:nrow(dt) * 3,
    y = dt$imputed_value / pop / (0.4)
  ))
  return(data)
}

#' Get population for a district
#'
#' @param loc District name
#' @return Population count
get_pop <- function(loc) {
  library(ramptools)
  pop <- district_pop[district_pop$admin3 == loc, ]$population
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

