# Construct discrete-time transition probability matrix using parameter values
construct_P_foi <- function(beta, gamma, rho) {
  P <- torch_eye(4)
  # beta - force of infection
  P[1, 1]$sub_(beta)
  P[2, 1]$add_(beta)
  # gamma - recovery rate
  P[2, 2]$sub_(gamma)
  P[1, 2]$add_(gamma)
  # rho - treatment
  P[2, 2]$sub_(rho)
  P[3, 2]$add_(rho)
  # stay in treatment for one step
  P[3, 3]$sub_(1)
  P[4, 3]$add_(1)
  # recover from treatment
  P[4, 4]$sub_(1)
  P[1, 4]$add_(1)
  return(P)
}

# Generate beta time-series using knot values for a bspline
construct_basis <- function(knot_vals, n_steps) {
  # Set up spline for beta
  n_knots <- length(knot_vals)  + 4
  step <- n_steps / (n_knots - 1 - 6)
  knots <- seq(0 - 3 * step, n_steps + 3 * step, step)
  x <- seq(1, n_steps)
  basis <- torch_tensor(splines::splineDesign(knots = knots, x = x, outer.ok = TRUE))
  return(basis)
}

# Run a single forward simulation of the model
run_sim <- function(params) {
  # Grab parameter values
  knot_vals <- params$fitted$knot_vals
  gamma <- params$fixed$gamma
  rhos <- params$fixed$rhos
  basis <- params$fixed$basis
  n_steps <- params$fixed$n_steps
  
  # Generate betas
  betas <- nnf_sigmoid(torch_mv(basis, knot_vals))
  
  # Initiate initial state
  P <- construct_P_foi(betas[1], gamma, rhos[1])
  ev <- eigen(as.matrix(P))$vectors[, 1]
  x0 <- abs(ev / sum(ev))
  x_all <- torch_zeros(length(x0), n_steps + 1)
  x_all[, 1]$add_(x0)
  x <- x0
  new_inf <- torch_zeros(n_steps + 1)
  new_inf[1]$add_(P[2, 1]$mul(x[1]))
  
  # Step forward, updating TPM, updating state
  for (i in 1:n_steps) {
    P <- construct_P_foi(betas[i + 1], gamma, rhos[i])
    new_inf[i + 1]$add_(P[2, 1]$mul(x[1]))
    x <- torch_mv(P, x)
    x_all[, i + 1]$add_(x)
    
  }
  return(list(x_all = x_all, new_inf = new_inf))
}