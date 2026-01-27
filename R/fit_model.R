# Calculate RMSE between data and simulation output
error <- function(data, sim_vals) {
  sim_vals <- sim_vals[as.integer(data[,1])]
  error <- torch_sum(sim_vals$sub_(data[, 2])$pow(2)$mean()$pow(0.5))
  error <- error + torch_mul(0.05, torch_sum(torch_abs(torch_diff(sim_vals, n = 2))))
  return(error)
}

# Fit model with specified parameters to supplied data
fit_model <- function(params, data) {
  ## fit model
  t <- 1
  prev_loss <- 1
  loss <- 0
  while(as.numeric(abs(loss - prev_loss)) > params$fitting$tol) {
    prev_loss <- loss
    sim <- run_sim(params)
    loss <- error(data, sim$new_inf)
    
    # if (t %% 5 == 1) { # Print out info and plot
      cat("Epoch:", t, " Loss: ", loss$item(), "\n")
      # " Knot values:", paste(round(as_array(params$fitted$knot_vals), 2), collapse = ','), "\n")
      d_date <- as.Date(as.integer(data[, 1]) * 10, origin = "2016-01-01")
      s_date <- as.Date(1:length(sim$new_inf) * 10, origin = "2016-01-01")
      plot(d_date, as.numeric(data[,2])[], pch = 19, 
           ylim = c(0, max(c(as.numeric(sim$new_inf), as.numeric(data[, 2])))), 
           xlab = "Date", ylab = "Incidence")
      lines(s_date, sim$new_inf, lwd = 5)
    # }
    
    # Update gradient and modify parameter values accordingly
    loss$backward()
    with_no_grad({
      params$fitted$knot_vals$sub_(params$fitting$learning_rate * params$fitted$knot_vals$grad)
      params$fitted$knot_vals$grad$zero_()
    })
    t <- t + 1
  }
  
  ## Look at estimated betas time series
  params$fitted$betas <- betas <- nnf_sigmoid(torch_mv(params$fixed$basis, params$fitted$knot_vals))
  params$fitted$betas2 <- betas / sim$x_all[2, ] # Convert from foi to transmission rate
  # plot(params$fitted$betas, type = 'l', ylim = c(0, as.numeric(max(params$fitted$betas2))))
  # lines(params$fitted$betas2, col = 'red')
  # 
  fit <- list(
    params = params,
    x_all = sim$x_all,
    new_inf = sim$new_inf,
    data = data
  )
  
  return(fit)
}