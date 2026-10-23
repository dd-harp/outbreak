####
# Shiny app data loader
#
# Fast path: read pre-computed long-format outbreak rows from the BigQuery
# `outbreak_data` table (written by uga-etl-facility-data/scripts/
# cloud_compute_outbreak.R). Schema:
#   date, code_name, name, frequency, method,
#   obs, value, threshold, Year, Month
#
# `obs` is the observed case count (imputed_value); `value` is the
# normalized outbreak metric used by the map (RAMP: b_60/b_0; others:
# obs/threshold); `threshold` is in case-count units (RAMP: b_0
# seasonal baseline; others: percentile or mean+sd of obs within the
# same period-of-year). The time-series plot draws `obs` and overlays
# `threshold × user-slider` per selected method.
#
# Compute path: if the pre-computed table is missing the new schema, pull
# clean monthly + weekly data and compute the four methods in process.
#
# Disk path: if BigQuery is unreachable, fall back to the bundled data.rds.
####

library(data.table)

# Vendored from `ramptools` so the deployed app doesn't depend on a
# private package. Sources expose: bq_connect, bq_get_clean_data,
# bq_get_outbreak_data, make_month_map, make_week_map.
source("vendor_bq_utils.R", local = TRUE)
source("vendor_period.R",   local = TRUE)
load("uga_district_shp.rda")  # provides `uga_district_shp` in the parent env

METHODS <- c("ramp", "who_p75", "mean_2sd", "mean_1sd")

# -- Smooths and RAMP index --------------------------------------------------

gen_smooths <- function(dt, bandwidths) {
  rbindlist(lapply(bandwidths, function(i) {
    copy_dt <- copy(dt)
    copy_dt[, value := as.numeric(value)]
    # ksmooth returns $y in sorted-x order; assignment writes row-for-row.
    # Sort within each group so the smooth column aligns with its dates.
    setorder(copy_dt, location_name, code_name, date)
    if (i == 0) {
      copy_dt[, smooth := median(value), by = .(location_name, code_name)]
    } else {
      copy_dt[, smooth := ksmooth(date, value, kernel = "normal",
                                  x.points = date, bandwidth = i)$y,
              by = .(location_name, code_name)]
    }
    copy_dt[, bandwidth := paste0("b_", i)]
    copy_dt
  }))
}

compute_ramp <- function(input_dt, period_map, bandwidths) {
  district_dt <- copy(input_dt)
  district_dt[, period := as.character(period)]
  district_dt <- merge(district_dt, period_map, by = "period")
  district_dt[, date := date_mid]
  setnames(district_dt, "value", "raw_value")
  setnames(district_dt, "imputed_value", "value")

  smooth_dt <- gen_smooths(district_dt, bandwidths)

  cast_dt <- dcast(smooth_dt,
                   location_name + date + code_name + value ~ bandwidth,
                   value.var = "smooth")
  cast_dt[, monthly_to_year := b_100 / b_365]

  season_dt <- cast_dt[, .(monthly_to_year, location_name, date, code_name)]
  season_dt[, month := lubridate::month(date)]
  season_dt <- season_dt[, .(median = median(monthly_to_year, na.rm = TRUE)),
                         by = .(location_name, code_name, month)]
  # `by` returns rows in first-appearance order of `month`, not 1→12.
  # Sort before ksmooth so the smooth aligns with the months it claims.
  setorder(season_dt, location_name, code_name, month)
  season_dt[, smooth := ksmooth(month, median, kernel = "normal",
                                x.points = month, bandwidth = 3)$y,
            by = .(location_name, code_name)]

  smooth_dt[, month := lubridate::month(date)]
  smooth_dt <- merge(
    smooth_dt,
    season_dt[, .(location_name, code_name, month, rel_seasonality = smooth)],
    by = c("location_name", "code_name", "month"))
  smooth_dt[bandwidth == "b_0", smooth := smooth * rel_seasonality]

  dt <- dcast(smooth_dt,
              location_name + code_name + date + value ~ bandwidth,
              value.var = "smooth")
  dt[, excess_rel_baseline := b_60 / b_0]
  dt[, .(date, code_name, name = location_name,
         obs = value, value = excess_rel_baseline, threshold = b_0)]
}

# -- WHO p75 and mean+SD methods ---------------------------------------------
# Each method computes a per-(location, code_name, period-of-year) threshold
# from the historical imputed_value series, then expresses each period's
# observation as observation / threshold. value > 1 ⇒ outbreak.

compute_period_method <- function(input_dt, period_map, period_of_year,
                                  threshold_fn) {
  dt <- copy(input_dt)
  dt[, period := as.character(period)]
  dt <- merge(dt, period_map, by = "period")
  dt[, date := date_mid]
  dt[, obs := as.numeric(imputed_value)]
  dt[, poy := period_of_year(date)]

  thresholds <- dt[, .(threshold = threshold_fn(obs)),
                   by = .(location_name, code_name, poy)]
  dt <- merge(dt, thresholds, by = c("location_name", "code_name", "poy"))
  dt[, value := obs / threshold]
  dt[!is.finite(value), value := NA_real_]
  dt[, .(date, code_name, name = location_name, obs, value, threshold)]
}

threshold_p75   <- function(x) as.numeric(quantile(x, 0.75, na.rm = TRUE))
threshold_2sd   <- function(x) mean(x, na.rm = TRUE) + 2 * sd(x, na.rm = TRUE)
threshold_1sd   <- function(x) mean(x, na.rm = TRUE) +     sd(x, na.rm = TRUE)

# -- Combine indicators (conf_malaria + ip_conf_cases → combined) ------------

add_combined <- function(dt) {
  if (uniqueN(dt$code_name) <= 1) {
    combined <- copy(dt)
    combined[, code_name := "combined"]
  } else {
    by_cols <- setdiff(names(dt),
                       c("code_name", "obs", "value", "threshold"))
    combined <- dt[, .(obs       = mean(obs,       na.rm = TRUE),
                       value     = mean(value,     na.rm = TRUE),
                       threshold = mean(threshold, na.rm = TRUE)),
                   by = by_cols]
    combined[, code_name := "combined"]
  }
  rbind(dt, combined, fill = TRUE)
}

# -- Per-frequency wrapper ---------------------------------------------------

compute_all_methods <- function(input_dt, frequency_label) {
  if (frequency_label == "monthly") {
    period_map <- make_month_map()
    period_of_year <- function(d) lubridate::month(d)
  } else {
    period_map <- make_week_map()
    period_of_year <- function(d) lubridate::isoweek(d)
  }

  ramp_dt <- compute_ramp(input_dt, period_map,
                          bandwidths = c(60, 100, 365, 0))
  ramp_dt[, method := "ramp"]

  p75_dt  <- compute_period_method(input_dt, period_map, period_of_year,
                                   threshold_p75)
  p75_dt[, method := "who_p75"]

  s2_dt   <- compute_period_method(input_dt, period_map, period_of_year,
                                   threshold_2sd)
  s2_dt[, method := "mean_2sd"]

  s1_dt   <- compute_period_method(input_dt, period_map, period_of_year,
                                   threshold_1sd)
  s1_dt[, method := "mean_1sd"]

  out <- rbind(ramp_dt, p75_dt, s2_dt, s1_dt, fill = TRUE)
  out <- add_combined(out)
  out[, frequency := frequency_label]
  out[, Year := lubridate::year(date)]
  out[, Month := as.character(lubridate::month(date, label = TRUE))]
  out[Year >= 2017]
}

# -- Top-level builds --------------------------------------------------------

build_from_clean_bq <- function() {
  if (file.exists("sa-key.json")) bigrquery::bq_auth(path = "sa-key.json")

  message("Loading clean monthly data from BigQuery...")
  monthly_input <- bq_get_clean_data(
    frequency = "monthly",
    code_names = c("conf_malaria", "ip_conf_cases"),
    levels = 3)
  monthly_dt <- compute_all_methods(monthly_input, "monthly")

  message("Loading clean weekly data from BigQuery...")
  weekly_input <- bq_get_clean_data(
    frequency = "weekly",
    code_names = "cases",
    levels = 3)
  weekly_input[code_name == "cases", code_name := "conf_malaria"]
  weekly_dt <- compute_all_methods(weekly_input, "weekly")

  rbind(monthly_dt, weekly_dt, fill = TRUE)
}

build_from_precomputed_bq <- function(frequency = NULL) {
  if (file.exists("sa-key.json")) bigrquery::bq_auth(path = "sa-key.json")
  dt <- bq_get_outbreak_data(frequency = frequency)
  required <- c("date", "code_name", "name", "frequency", "method",
                "obs", "value", "threshold", "Year", "Month")
  if (!all(required %in% names(dt))) {
    stop("outbreak_data BQ table missing new-schema columns; ",
         "re-run cloud_compute_outbreak.R.")
  }
  data.table::as.data.table(dt)
}

shape_for_app <- function(long_dt) {
  long_dt <- copy(long_dt)
  long_dt[, date := as.Date(date)]

  map_dt <- long_dt[frequency == "monthly",
                    .(date, code_name, name, method, obs, value, threshold,
                      Year, Month)]
  ts_dt  <- long_dt[, .(date, code_name, name, method, obs, value, threshold,
                        Year, Month, frequency)]

  list(map_dt = map_dt, ts_dt = ts_dt,
       dist_shp = uga_district_shp)
}

# Cold-start loader: pulls monthly only (75% smaller payload than the full
# table). Weekly is lazy-loaded on first Time-Series tab render via
# `load_weekly_ts_data()`. If BQ is reachable, `has_weekly = FALSE` flags
# the app that weekly still needs fetching. The data.rds and recompute
# fallbacks return both frequencies and set `has_weekly = TRUE`.
load_shiny_data <- function(rds_path = "data.rds") {
  fast <- tryCatch(build_from_precomputed_bq(frequency = "monthly"),
                   error = function(e) {
    message("Pre-computed outbreak_data unavailable (", e$message, ").")
    NULL
  })
  if (!is.null(fast)) {
    out <- shape_for_app(fast)
    out$has_weekly <- FALSE
    return(out)
  }

  fresh <- tryCatch(build_from_clean_bq(), error = function(e) {
    message("Clean-data fallback failed (", e$message, ").")
    NULL
  })
  if (!is.null(fresh)) {
    out <- shape_for_app(fresh)
    out$has_weekly <- TRUE
    return(out)
  }

  message("Falling back to bundled ", rds_path, ".")
  out <- readRDS(rds_path)
  out$has_weekly <- TRUE
  out
}

# Lazy weekly fetch. Tries the pre-computed BQ table; on failure returns
# an empty data.table so the time-series plot still renders the monthly
# series. Called once per session via a Shiny `reactive()`.
load_weekly_ts_data <- function() {
  tryCatch({
    dt <- build_from_precomputed_bq(frequency = "weekly")
    dt[, date := as.Date(date)]
    dt[, .(date, code_name, name, method, obs, value, threshold,
           Year, Month, frequency)]
  }, error = function(e) {
    message("Weekly lazy-load failed (", e$message, "); ",
            "time series will show monthly only.")
    data.table()
  })
}
