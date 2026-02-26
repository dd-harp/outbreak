get_outbreak_index <- function(dt, bandwidths = c(60, 100, 365, 0)) {
  district_smooth_dt <- gen_smooths(dt, bandwidths)
  
  # Calculate relative values
  cast_district_dt <- dcast(district_smooth_dt, location_name + date + code_name + value ~ bandwidth, value.var = "smooth")
  cast_district_dt[, year_to_average := b_365 / b_0]
  cast_district_dt[, monthly_to_year := b_100 / b_365]
  cast_district_dt[, residual := value - b_100]
  
  # Get median monthly-to-year to establish a seasonal pattern
  all_month_dt <- cast_district_dt[, .(monthly_to_year, location_name, date, code_name)]
  all_month_dt[, month := month(date)]
  month_dt <- all_month_dt[, .(median = median(monthly_to_year), mean = mean(monthly_to_year)), by = .(location_name, code_name, month)]
  month_dt[, smooth := ksmooth(month, median, kernel = "normal", x.points = month, bandwidth = 3)$y, by = .(location_name, code_name)]
  month_dt <- melt(month_dt, id.vars = c("location_name", "month", "code_name"), value.name = "rel_seasonality")
  
  district_smooth_dt[, month := month(date)]
  district_smooth_dt <- merge(district_smooth_dt, 
                              month_dt[variable == "median", 
                                       .(location_name, code_name, month, rel_seasonality)], 
                              by = c("location_name", "code_name", "month"))
  district_smooth_dt[bandwidth == "b_0", smooth := smooth * rel_seasonality]
  
  # Calculate relative values
  cast_district_dt <- dcast(district_smooth_dt, location_id + code_name + date + value ~ bandwidth, value.var = "smooth")
  cast_district_dt[, year_to_average := b_365 / b_0]
  cast_district_dt[, monthly_to_year := b_100 / b_365]
  cast_district_dt[, residual := value - b_100]
  cast_district_dt[, excess_rel_baseline := b_60 / b_0]
  
  return(cast_district_dt)
}