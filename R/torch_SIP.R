# Setup
library(torch)
library(splines)
library(data.table)
rm(list = ls())
set.seed(7)
torch_manual_seed(2)

# Source functions
source("R/get_data.R")
source("R/fit_model.R")
source("R/model_torch.R")
source("R/model_R.R")

# Simulate all possible timings of MDA and assess averted burden from each
resim_mda <- function(fit, mda_cov, loc) {
  params <- fit$params
  data <- fit$data
  pop <- get_pop(loc)
  sim_list_0 <- sim_tm(params, 0, mda_cov)
  d_date <- as.Date(as.integer(data[, 1]) * 10, origin = "2016-01-01")
  s_date <- as.Date(1:length(sim_list_0$new_inf) * 10, origin = "2016-01-01")
  max_val <- max(c(as.numeric(sim_list_0$new_inf), as.numeric(data[, 2])))
  averted_inf <- c()
  for (response_idx in 2:(params$fixed$n_steps - 10)) {
    
    # plot(as.matrix(data[, 1:2]), pch = 19, ylim = c(0, 1))
    # matplot(t(as.matrix(sim_list_0$x_all)), type = 'l', add = T, lwd = 5)
    
    sim_list_1 <- sim_tm(params, response_idx, mda_cov)
    plot(d_date, as.numeric(data[,2])[] * pop / 1e3, pch = 19, ylim = c(0, max(sim_list_0$new_inf) * pop / 1e3),
         xlab = "Date", ylab = "Infections (in thousands)")
    lines(s_date, sim_list_1$new_inf * pop / 1e3, col = 'red')
    lines(s_date, sim_list_0$new_inf * pop / 1e3)
    abline(v = s_date[response_idx], lty = "dashed")
    legend("topleft", legend = c("Observed", "Outbreak response"), 
           col = c("black", "red"), lty = c(1, 1))

    # plot(sim_list_0$new_inf, type = 'l', ylim = c(0, max(sim_list_0$new_inf)))
    # lines(sim_list_1$new_inf, col = 'red')
    
    diff_inf <- (sim_list_0$new_inf - sim_list_1$new_inf) * pop
    # plot(diff_inf, type = 'l')
    
    averted_inf <- c(averted_inf, sum(diff_inf))
  }
  
  

  
  par(mar = c(5, 4, 4, 4) + 0.3)
  plot(d_date, as.numeric(data[,2])[], pch = 19, 
       ylim = c(0, max_val), 
       xlab = "Date", ylab = "Incidence")
  lines(s_date, sim_list_0$new_inf, lwd = 3, col = "blue")
  abline(v = s_date[which(averted_inf == max(averted_inf))], lty = "dashed")
  abline(h = median(averted_inf/ max(averted_inf)) * max_val, lty = "dashed", col = "red")
  
  par(new = TRUE)
  plot(s_date, c(averted_inf / 1e3, rep(NA, length(s_date) - length(averted_inf))), type = 'l', axes = F,
       xlab = "", ylab = "", ylim = c(0, max(averted_inf / 1e3)), bty = "n", col = 'red')
  axis(side = 4, at = pretty(range(averted_inf / 1e3)))
  mtext("Cases averted (in thousands)", side = 4, line = 3)
  
  return(averted_inf)
}

main <- function(loc_name) {
  # Set up data and update params
  data <- get_data(loc = loc_name)
  
  # Organize params object
  params <- list(
    fixed = list(
      n_steps = nrow(data) * 3 - 1,
      gamma = 0.05,
      rhos = rep(0.7, nrow(data) * 3) # Treating once a year
    ),
    fitted = list(
      knot_vals = torch_zeros(round(nrow(data) / 2), requires_grad = T)
    ),
    fitting = list(
      learning_rate = 10,
      tol = 1e-7
    )
  )
  params$fixed$basis <- construct_basis(
    params$fitted$knot_vals, 
    params$fixed$n_steps + 1
  )
  
  # Fit model to data
  fit <- fit_model(params, data)
  
  # Re-simulate all possible MDA delivery time points
  mda_cov <- 0.8
  averted_inf <- resim_mda(fit, mda_cov, loc_name)
  
  return(
    list(
      fit = fit,
      averted_inf = averted_inf
    )
  )
}

loc <- "Bundibugyo District"
out_list <- main(loc_name = loc)
fit <- out_list$fit
averted_inf <- out_list$averted_inf

# Make figure two plot
d_date <- as.Date(as.integer(fit$data[, 1]) * 10, origin = "2016-01-01")
s_date <- as.Date(1:length(fit$new_inf) * 10, origin = "2016-01-01")
plot(d_date, as.numeric(fit$data[,2])[], pch = 19, 
     ylim = c(0, max(c(as.numeric(fit$new_inf), as.numeric(fit$data[, 2])))), 
     xlab = "Date", ylab = "Incidence", col = "red")
lines(s_date, fit$new_inf, lwd = 4)

# Read in outbreaks and grab dates of outbreak
outbreak_dt <- fread("outputs/outbreaks.csv")
outbreak_dt[, year := year(date)]
outbreak_dt[, month := month(date)]


outbreak_dt[location_name == loc]

ramp_o_date <- outbreak_dt[location_name == loc & ramp_outbreak == 1]$date
who_o_date <- outbreak_dt[location_name == loc & who_outbreak == 1]$date

# Group dates and keep minimum
o_start <- function(dates) {
  o_start <- c(min(dates))
  date_diff <- diff(dates)
  o_start <- c(o_start, dates[which(date_diff > 31) + 1])
  return(o_start)
}

ramp_o_starts <- o_start(ramp_o_date)
who_o_starts <- o_start(who_o_date)

ramp_o_idx <- c()
for (i in ramp_o_starts) {
  diffs <- abs(as.Date(i) - s_date)
  ramp_o_idx <- c(ramp_o_idx, which(diffs == min(diffs)) + 3)
}

mean_ramp <- mean(averted_inf[ramp_o_idx])

who_o_idx <- c()
for (i in who_o_starts) {
  diffs <- abs(as.Date(i) - s_date)
  who_o_idx <- c(who_o_idx, which(diffs == min(diffs)) + 3)
}

mean_who <- mean(averted_inf[who_o_idx])

# Compare to median
median_averted <- median(averted_inf)


mean_ramp / 1e3

mean_who / 1e3

mean_ramp / median_averted

mean_who / median_averted
