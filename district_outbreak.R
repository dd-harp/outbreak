library(yaml);library(data.table); library(ramptools); library(ggplot2)
library(scales); library(sf)

## Orient to local machine ----
paths <- read_yaml("my_paths.yaml")

clean_data_root <- file.path(
  paths[["box_dir"]], "data", "dhis", "monthly", "clean", "prod"
)
clean_data_path <- file.path(clean_data_root, "clean_data.csv")

# Read in monthly confirmed case data
dt <- fread(clean_data_path)
dt$period <- as.character(dt$period)
dt <- merge(dt, make_month_map(), by = "period")

# Iterate through and apply detection algorithms
district_dt <- dt[level == 3]
district_dt[, date := date_mid]
dt[, value := imputed_value]

district_bandwidths <- c(60, 100, 365, 0) # Zero means median
outbreak_dt <- get_outbreak_index(district_dt[value > 0], district_bandwidths)
setnames(outbreak_dt, c("location_id"), c("id"))
plot_dt <- merge(
  outbreak_dt[code_name == "conf_malaria", .(id, excess_rel_baseline, date)], 
  st_simplify(uga_district_shp, preserveTopology = TRUE, dTolerance = 1000), 
  by = "id"
)

plot_dt[, Year := year(date)]
plot_dt[, Month := lubridate::month(date, label = T)]

gg <- ggplot(data = plot_dt) + 
  geom_sf(aes(fill = excess_rel_baseline, geometry = geometry), lwd = 0) + 
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
