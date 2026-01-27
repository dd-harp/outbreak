# Generate smooths
gen_smooths <- function(dt, bandwidths) {
  smooth_dt <- rbindlist(lapply(bandwidths, function(i) {
    copy_dt <- copy(dt)
    copy_dt[, value := as.numeric(value)]
    if (i == 0) {
      # Calculate median value
      copy_dt[, smooth := median(value), by = .(location_name, code_name)]
    } else {
      copy_dt[, smooth := ksmooth(date, value, kernel = "normal", x.points = date, bandwidth = i)$y, by = .(location_name, code_name)]
    }
    copy_dt[, bandwidth := paste0("b_", i)]
    return(copy_dt)
  }))
  return(smooth_dt)
}
