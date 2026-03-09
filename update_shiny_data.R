####
# Update Shiny App Data from BigQuery
#
# Reads clean monthly and weekly data from BigQuery, computes outbreak indices,
# joins with shapefiles, and saves the RDS for the Shiny app.
####

library(ggplot2); library(lubridate)
library(sf); library(scales); library(data.table)
source("R/gen_smooths.R")
library(ramptools)

# Arguments
district_bandwidths <- c(60, 100, 365, 0) # Zero means median

# -- Helper: compute outbreak index for a given frequency --------------------
compute_outbreak <- function(input_dt, period_map, bandwidths) {
  district_dt <- copy(input_dt)
  district_dt[, period := as.character(period)]
  district_dt <- merge(district_dt, period_map, by = "period")
  district_dt[, date := date_mid]
  setnames(district_dt, "value", "raw_value")
  setnames(district_dt, "imputed_value", "value")

  # Calculate smooths
  district_smooth_dt <- gen_smooths(district_dt, bandwidths)

  # Calculate relative values
  cast_dt <- dcast(district_smooth_dt,
                   location_name + date + code_name + value ~ bandwidth,
                   value.var = "smooth")
  cast_dt[, year_to_average := b_365 / b_0]
  cast_dt[, monthly_to_year := b_100 / b_365]
  cast_dt[, residual := value - b_100]

  # Get median monthly-to-year to establish a seasonal pattern
  all_dt <- cast_dt[, .(monthly_to_year, location_name, date, code_name)]
  all_dt[, month := month(date)]
  month_dt <- all_dt[, .(median = median(monthly_to_year),
                         mean = mean(monthly_to_year)),
                     by = .(location_name, code_name, month)]
  month_dt[, smooth := ksmooth(month, median, kernel = "normal",
                               x.points = month, bandwidth = 3)$y,
           by = .(location_name, code_name)]
  month_dt <- melt(month_dt, id.vars = c("location_name", "month", "code_name"),
                   value.name = "rel_seasonality")

  district_smooth_dt[, month := month(date)]
  district_smooth_dt <- merge(
    district_smooth_dt,
    month_dt[variable == "median",
             .(location_name, code_name, month, rel_seasonality)],
    by = c("location_name", "code_name", "month"))
  district_smooth_dt[bandwidth == "b_0", smooth := smooth * rel_seasonality]

  # Calculate outbreak index
  dt <- dcast(district_smooth_dt,
              location_name + code_name + date + value ~ bandwidth,
              value.var = "smooth")
  dt[, excess_rel_baseline := b_60 / b_0]

  return(dt)
}

# ============================================================================
# 1. Monthly outbreak index
# ============================================================================
message("Processing monthly data...")
monthly_input <- bq_get_clean_data(frequency = "monthly")
monthly_input <- monthly_input[level == 3 & code_name %in% c("conf_malaria", "ip_conf_cases")]

monthly_dt <- compute_outbreak(monthly_input, make_month_map(), district_bandwidths)

# Combined indicator (average across code_names)
monthly_avg <- monthly_dt[, .(excess_rel_baseline = mean(excess_rel_baseline)),
                          by = .(date, location_name)]
monthly_avg[, code_name := "combined"]
monthly_dt <- rbind(monthly_dt, monthly_avg, fill = TRUE)

monthly_dt[, Year := year(date)]
monthly_dt[, Month := lubridate::month(date, label = TRUE)]
monthly_dt[, frequency := "monthly"]
setnames(monthly_dt, "location_name", "name")

# ============================================================================
# 2. Weekly outbreak index
# ============================================================================
message("Processing weekly data...")
weekly_input <- bq_get_clean_data(frequency = "weekly")
weekly_input <- weekly_input[level == 3 & code_name == "cases"]

weekly_dt <- compute_outbreak(weekly_input, make_week_map(), district_bandwidths)

# Weekly only has 'cases'; rename to match monthly convention for the app
weekly_dt[code_name == "cases", code_name := "conf_malaria"]
# Create 'combined' as a copy (only one indicator)
weekly_avg <- copy(weekly_dt[code_name == "conf_malaria",
                             .(date, name = location_name, excess_rel_baseline)])
weekly_avg[, code_name := "combined"]
weekly_dt[, Year := year(date)]
weekly_dt[, Month := lubridate::month(date, label = TRUE)]
weekly_dt[, frequency := "weekly"]
setnames(weekly_dt, "location_name", "name")
weekly_avg[, Year := year(date)]
weekly_avg[, Month := lubridate::month(date, label = TRUE)]
weekly_avg[, frequency := "weekly"]
weekly_dt <- rbind(weekly_dt, weekly_avg, fill = TRUE)

# ============================================================================
# 3. Combine and save
# ============================================================================
# Map-ready data (monthly, as before)
map_dt <- monthly_dt[Year >= 2017,
  .(date, code_name, name, value = excess_rel_baseline, Year, Month)]

# Time series data (both frequencies, for the new time series panel)
ts_cols <- c("date", "code_name", "name", "excess_rel_baseline",
             "Year", "Month", "frequency")
ts_dt <- rbind(
  monthly_dt[Year >= 2017, ..ts_cols],
  weekly_dt[Year >= 2017, ..ts_cols]
)

shiny_data <- list(
  map_dt = map_dt,
  ts_dt = ts_dt,
  dist_shp = uga_district_shp
)

saveRDS(shiny_data, "outbreak_detection/data.rds")
message("Shiny data updated: outbreak_detection/data.rds")
