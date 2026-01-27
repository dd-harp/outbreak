library(ggplot2); library(ggforce); library(ggpubr); library(ggrepel); library(lubridate);
library(sf); library(scales)
source("gen_smooths.R")
library(ramptools)

# Arguments
read_cached_data <- F
district_bandwidths <- c(60, 100, 365, 0) # Zero means median
region_bandwidths <-  c(60, 100, 365, 0) 

# Paths
box_dir <- "/Users/aucarter/Library/CloudStorage/Box-Box/RAMP"
case_path <- file.path(box_dir, "data/dhis/monthly/clean/prod/clean_data.csv")

# Prep case data
input_dt <- fread(case_path)
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

# Calculate relative values
dt <- dcast(district_smooth_dt, location_name + code_name + date + value ~ bandwidth, value.var = "smooth")
dt[, year_to_average := b_365 / b_0]
dt[, monthly_to_year := b_100 / b_365]
dt[, residual := value - b_100]
dt[, excess_rel_baseline := b_60 / b_0]


avg_dt <- dt[, .(excess_rel_baseline = mean(excess_rel_baseline)), by = .(date, location_name)]
avg_dt[, code_name := "combined"]
dt <- rbind(dt, avg_dt, fill = T)

dt[, Year := year(date)]
dt[, Month := lubridate::month(date, label = T)]
setnames(dt, "location_name", "name")

  
subset_dt <- dt[Year >= 2017, c("date", "code_name", "name", "excess_rel_baseline", "Year", "Month"), with = F]
setnames(subset_dt, "excess_rel_baseline", "value")
dist_shp2 <- merge(uga_district_shp, subset_dt, by = "name")
c <- "conf_malaria"

gg <- ggplot(data = dist_shp2[dist_shp2$code_name == c,]) + 
  geom_sf(aes(fill = value), lwd = 0) + 
  theme_void()  + 
  scale_fill_gradientn(
    colors = c("forestgreen", "yellowgreen", "yellow", "red", "darkred"), 
    values = rescale(log(c(0.1, 1, 1.5, 2, 5))), 
    guide = "colorbar", limits=c(0.1, 5), 
    trans = "log", breaks = c(0.1, 0.5, 1, 2, 5)) + 
  facet_grid(Year~Month) +
  theme(legend.position = "bottom") +
  labs(fill = "Outbreak Index")
gg

saveRDS(dist_shp2, "shiny_apps/outbreak_detection2/data2.rds")
