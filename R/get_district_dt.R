 
get_district_dt <- function() {
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
  
  return(district_dt)
}