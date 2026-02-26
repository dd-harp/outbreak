#' Generate kernel smooths at multiple bandwidths
#'
#' For each bandwidth, computes a kernel smooth of the time series. A bandwidth
#' of 0 computes the median instead.
#'
#' @param dt data.table with columns: location_name, code_name, date, value
#' @param bandwidths Numeric vector of bandwidths (0 = median)
#' @return data.table with added \code{smooth} and \code{bandwidth} columns
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
