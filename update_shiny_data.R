####
# Update Shiny App Data from BigQuery
#
# Refreshes the bundled-RDS fallback (outbreak_detection/data.rds) used
# when BigQuery is unreachable at app startup. Calls the same loader the
# Shiny app uses so the snapshot matches what users will see.
####

library(data.table)
library(lubridate)

main <- function() {
  old_wd <- setwd("outbreak_detection")
  on.exit(setwd(old_wd))
  source("load_data.R", local = TRUE)
  shiny_data <- load_shiny_data("data.rds")
  saveRDS(shiny_data, "data.rds")
  message("Shiny data updated: outbreak_detection/data.rds (",
          nrow(shiny_data$ts_dt), " ts rows, ",
          nrow(shiny_data$map_dt), " map rows)")
}
main()
