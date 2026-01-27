get_who_outbreak <- function(dt) {
  dt[, month := month(date)]
  m <- unique(dt[date == max(date)]$month)
  summ_dt <- dt[month == m, .(perc_75 = quantile(raw_value, 0.75)), by = .(location_id)] 
  raw_value <- dt[date == max(date), .(location_id, date, raw_value)]
  out_dt <- merge(raw_value, summ_dt, by = "location_id")
  return(out_dt)
}

