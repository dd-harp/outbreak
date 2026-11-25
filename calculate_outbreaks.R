## Read in data and calculate outbreak two ways
library(data.table)
source("R/get_district_dt.R")
source("R/gen_smooths.R")
source("R/get_outbreak_index.R")
source("R/get_who_outbreak.R")
# Read district data
district_dt <- get_district_dt()

# Subset to districts of interest
d_list <- c(
  "Tororo District",
  "Rukiga District",
  "Bundibugyo District"
)

district_dt <- district_dt[code_name == "conf_malaria" & location_name %in% d_list]

# Iterate through time, assessing outbreak based on available data
out_dt <- rbindlist(lapply(sort(unique(district_dt[date >= as.Date("2018-01-01")]$date)), function(d) {
  date_dt <- district_dt[date <= d]
  # Get outbreak index
  outbreak_index <- get_outbreak_index(date_dt)
  outbreak_val <- outbreak_index[date == d, .(location_id, date, excess_rel_baseline, value)]
  outbreak_val[, ramp_outbreak := ifelse(excess_rel_baseline > 2, 1, 0)]
  # Get WHO index
  who_index <- get_who_outbreak(date_dt)
  who_index[, who_outbreak := ifelse(raw_value > perc_75, 1, 0)]
  # Merge
  outbreak_dt <- merge(outbreak_val, who_index, by = c("location_id", "date"))
  return(outbreak_dt)
}))

out_dt <- merge(out_dt, unique(district_dt[, .(location_id, location_name)]))
out_dt[, ramp_threshold := value / excess_rel_baseline * 2]

## Plot
setnames(district_dt, "value", "imputed_value")

pdf("plots/outbreak.pdf", width = 10, height = 7)
# for (d in sort(unique(district_dt$location_name))) {
  
  data_dt <- melt(district_dt[, .(location_name, date, imputed_value, raw_value)], id.vars = c("location_name", "date"))
  data_dt[, variable_name := ifelse(variable == "imputed_value", "Clean data", "Reported data")]
  threshold_dt <- melt(out_dt[, .(location_name, date, ramp_threshold, perc_75)], id.vars = c("location_name", "date"))
  threshold_dt[, variable_name := ifelse(variable == "ramp_threshold", "RAMP outbreak threshold", "75th percentile threshold")]
  outbreak_dt <- melt(out_dt[, .(location_name, date, ramp_outbreak, who_outbreak)], id.vars = c("location_name", "date"))
  outbreak_dt <- outbreak_dt[value == 1]
  outbreak_dt[, variable_name := ifelse(variable == "ramp_outbreak", "RAMP outbreak", "75th percentile outbreak")]
  gg <- ggplot() +
    geom_point(data = data_dt, aes(x = date, y = value, shape = variable_name)) + 
    geom_line(data = threshold_dt, aes(x = date, y = value, color = variable_name)) +
    geom_vline(data = outbreak_dt, aes(xintercept = date, color = variable_name), alpha = 0.5, linewidth = 2) +
    theme_bw() + expand_limits(y = 0) + facet_wrap(.~ location_name, nrow = 3, scales = "free_y") +
    theme(legend.title = element_blank()) + xlab("Date") + ylab("Confirmed malaria cases")
  print(gg)
# }
dev.off()

write.csv(out_dt, "outputs/outbreaks.csv", row.names = F)
