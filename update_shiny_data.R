####
# Update Shiny App Data from BigQuery
#
# Reads clean monthly data from BigQuery, computes outbreak indices,
# joins with shapefiles, and saves the RDS for the Shiny app.
####

library(ggplot2); library(lubridate)
library(sf); library(scales); library(data.table)
source("R/gen_smooths.R")
library(ramptools)

# Arguments
district_bandwidths <- c(60, 100, 365, 0) # Zero means median

# Read from BigQuery
input_dt <- bq_get_clean_data(frequency = "monthly")

district_dt <- input_dt[level == 3 & code_name %in% c("conf_malaria", "ip_conf_cases")]
district_dt[, period := as.character(period)]
district_dt <- merge(district_dt, make_month_map())
district_dt[, date := date_mid]
setnames(district_dt, "value", "raw_value")
setnames(district_dt, "imputed_value", "value")

# Calculate smooths
district_smooth_dt <- gen_smooths(district_dt, district_bandwidths)

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

# Calculate outbreak index
dt <- dcast(district_smooth_dt, location_name + code_name + date + value ~ bandwidth, value.var = "smooth")
dt[, excess_rel_baseline := b_60 / b_0]

# Create combined indicator (average across code_names)
avg_dt <- dt[, .(excess_rel_baseline = mean(excess_rel_baseline)), by = .(date, location_name)]
avg_dt[, code_name := "combined"]
dt <- rbind(dt, avg_dt, fill = TRUE)

dt[, Year := year(date)]
dt[, Month := lubridate::month(date, label = TRUE)]
setnames(dt, "location_name", "name")

subset_dt <- dt[Year >= 2017, c("date", "code_name", "name", "excess_rel_baseline", "Year", "Month"), with = FALSE]
setnames(subset_dt, "excess_rel_baseline", "value")
dist_shp2 <- merge(uga_district_shp, subset_dt, by = "name")

saveRDS(dist_shp2, "outbreak_detection2/data2.rds")
message("Shiny data updated: outbreak_detection2/data2.rds")
