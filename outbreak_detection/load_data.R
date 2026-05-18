####
# Shiny app data loader
#
# Loads clean monthly + weekly data from BigQuery and computes the outbreak
# index expected by app.R. Returns a list compatible with the bundled
# data.rds schema (map_dt, ts_dt, dist_shp).
#
# Mirrors the logic in ../update_shiny_data.R but designed to run inside the
# Shiny app at startup. Falls back to data.rds on any error so the app stays
# usable when BigQuery is unreachable.
####

library(data.table)

gen_smooths <- function(dt, bandwidths) {
  smooth_dt <- rbindlist(lapply(bandwidths, function(i) {
    copy_dt <- copy(dt)
    copy_dt[, value := as.numeric(value)]
    if (i == 0) {
      copy_dt[, smooth := median(value), by = .(location_name, code_name)]
    } else {
      copy_dt[, smooth := ksmooth(date, value, kernel = "normal",
                                  x.points = date, bandwidth = i)$y,
              by = .(location_name, code_name)]
    }
    copy_dt[, bandwidth := paste0("b_", i)]
    return(copy_dt)
  }))
  return(smooth_dt)
}

compute_outbreak <- function(input_dt, period_map, bandwidths) {
  district_dt <- copy(input_dt)
  district_dt[, period := as.character(period)]
  district_dt <- merge(district_dt, period_map, by = "period")
  district_dt[, date := date_mid]
  setnames(district_dt, "value", "raw_value")
  setnames(district_dt, "imputed_value", "value")

  district_smooth_dt <- gen_smooths(district_dt, bandwidths)

  cast_dt <- dcast(district_smooth_dt,
                   location_name + date + code_name + value ~ bandwidth,
                   value.var = "smooth")
  cast_dt[, year_to_average := b_365 / b_0]
  cast_dt[, monthly_to_year := b_100 / b_365]
  cast_dt[, residual := value - b_100]

  all_dt <- cast_dt[, .(monthly_to_year, location_name, date, code_name)]
  all_dt[, month := lubridate::month(date)]
  month_dt <- all_dt[, .(median = median(monthly_to_year),
                         mean = mean(monthly_to_year)),
                     by = .(location_name, code_name, month)]
  month_dt[, smooth := ksmooth(month, median, kernel = "normal",
                               x.points = month, bandwidth = 3)$y,
           by = .(location_name, code_name)]
  month_dt <- melt(month_dt, id.vars = c("location_name", "month", "code_name"),
                   value.name = "rel_seasonality")

  district_smooth_dt[, month := lubridate::month(date)]
  district_smooth_dt <- merge(
    district_smooth_dt,
    month_dt[variable == "median",
             .(location_name, code_name, month, rel_seasonality)],
    by = c("location_name", "code_name", "month"))
  district_smooth_dt[bandwidth == "b_0", smooth := smooth * rel_seasonality]

  dt <- dcast(district_smooth_dt,
              location_name + code_name + date + value ~ bandwidth,
              value.var = "smooth")
  dt[, excess_rel_baseline := b_60 / b_0]
  return(dt)
}

build_shiny_data <- function() {
  if (file.exists("sa-key.json")) {
    bigrquery::bq_auth(path = "sa-key.json")
  }

  district_bandwidths <- c(60, 100, 365, 0)

  message("Loading clean monthly data from BigQuery...")
  monthly_input <- ramptools::bq_get_clean_data(
    frequency = "monthly",
    code_names = c("conf_malaria", "ip_conf_cases"),
    levels = 3)
  monthly_dt <- compute_outbreak(monthly_input, ramptools::make_month_map(),
                                 district_bandwidths)

  monthly_avg <- monthly_dt[
    , .(excess_rel_baseline = mean(excess_rel_baseline)),
    by = .(date, location_name)]
  monthly_avg[, code_name := "combined"]
  monthly_dt <- rbind(monthly_dt, monthly_avg, fill = TRUE)
  monthly_dt[, Year := lubridate::year(date)]
  monthly_dt[, Month := lubridate::month(date, label = TRUE)]
  monthly_dt[, frequency := "monthly"]
  setnames(monthly_dt, "location_name", "name")

  message("Loading clean weekly data from BigQuery...")
  weekly_input <- ramptools::bq_get_clean_data(
    frequency = "weekly",
    code_names = "cases",
    levels = 3)
  weekly_dt <- compute_outbreak(weekly_input, ramptools::make_week_map(),
                                district_bandwidths)
  weekly_dt[code_name == "cases", code_name := "conf_malaria"]
  weekly_avg <- copy(weekly_dt[code_name == "conf_malaria",
    .(date, name = location_name, excess_rel_baseline)])
  weekly_avg[, code_name := "combined"]
  weekly_dt[, Year := lubridate::year(date)]
  weekly_dt[, Month := lubridate::month(date, label = TRUE)]
  weekly_dt[, frequency := "weekly"]
  setnames(weekly_dt, "location_name", "name")
  weekly_avg[, Year := lubridate::year(date)]
  weekly_avg[, Month := lubridate::month(date, label = TRUE)]
  weekly_avg[, frequency := "weekly"]
  weekly_dt <- rbind(weekly_dt, weekly_avg, fill = TRUE)

  map_dt <- monthly_dt[Year >= 2017,
    .(date, code_name, name, value = excess_rel_baseline, Year, Month)]

  ts_cols <- c("date", "code_name", "name", "excess_rel_baseline",
               "Year", "Month", "frequency")
  ts_dt <- rbind(
    monthly_dt[Year >= 2017, ..ts_cols],
    weekly_dt[Year >= 2017, ..ts_cols]
  )

  list(map_dt = map_dt, ts_dt = ts_dt, dist_shp = ramptools::uga_district_shp)
}

load_shiny_data <- function(rds_path = "data.rds") {
  fresh <- tryCatch(
    build_shiny_data(),
    error = function(e) {
      message("BigQuery load failed (", e$message,
              "). Falling back to bundled data.rds.")
      NULL
    }
  )
  if (!is.null(fresh)) return(fresh)
  readRDS(rds_path)
}
