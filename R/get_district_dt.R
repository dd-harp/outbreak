#' Get district-level monthly data for outbreak detection
#'
#' Reads clean monthly data from BigQuery, subsets to district level,
#' and prepares it for smoothing and outbreak detection.
#'
#' @return data.table with district-level malaria data
get_district_dt <- function() {
  library(data.table)
  library(lubridate)
  library(ramptools)
  source("R/gen_smooths.R")

  # Read from BigQuery
  district_dt <- bq_get_clean_data(
    frequency = "monthly",
    code_names = c("conf_malaria", "ip_conf_cases"),
    levels = 3L
  )
  district_dt[, period := as.character(period)]
  district_dt <- merge(district_dt, make_month_map())
  district_dt[, date := date_mid]
  setnames(district_dt, "value", "raw_value")
  setnames(district_dt, "imputed_value", "value")

  return(district_dt)
}