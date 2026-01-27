# Base (non-torch) model with MDA
construct_P_tm <- function(beta, gamma, rho, phi) {
  P <- diag(4)
  # beta - force of infection
  P[1, 1] <- P[1, 1] - beta * (1 - phi)
  P[2, 1] <- P[2, 1] + beta * (1 - phi)
  # gamma - recovery rate
  P[2, 2] <- P[2, 2] - gamma * (1 - phi)
  P[1, 2] <- P[1, 2] + gamma * (1 - phi)
  # rho - treatment
  P[2, 2] <- P[2, 2] - rho * (1 - phi)
  P[3, 2] <- P[3, 2] + rho * (1 - phi)
  # stay in treatment for a timestep
  P[3, 3] <- P[3, 3] - 1 * (1 - phi)
  P[4, 3] <- P[4, 3] + 1 * (1 - phi)
  # recover from treatment
  P[4, 4] <- P[4, 4] - 1 * (1 - phi)
  P[1, 4] <- P[1, 4] + 1 * (1 - phi)
  # MDA - phi
  P[1, 1] <- P[1, 1] - phi
  P[3, 1] <- P[3, 1] + phi
  P[2, 2] <- P[2, 2] - phi
  P[3, 2] <- P[3, 2] + phi
  P[4, 4] <- P[4, 4] - phi
  P[3, 4] <- P[3, 4] + phi
  
  return(P)
}

# Run a single forward simulation with a transmission rate input
sim_tm <- function(params, response_idx, mda_cov) {
  # Grab parameters
  gamma <- params$fixed$gamma
  rhos <- params$fixed$rhos
  n_steps <- params$fixed$n_steps
  betas <- as.numeric(params$fitted$betas)
  
  # Initiate initial state
  P <- construct_P_foi(betas[1], gamma, rhos[1])
  ev <- eigen(as.matrix(P))$vectors[, 1]
  x0 <- abs(ev / sum(ev))
  x_all <- matrix(0, length(x0), n_steps + 1)
  x_all[, 1] <- x <- x0
  new_inf <- c()
  
  # Step forward, updating TPM, updating state
  for (i in 1:n_steps) {
    P <- construct_P_tm(
      betas[i + 1] - betas[i + 1] * (response_idx > 0) * as.integer(i %in% response_idx:(response_idx + 36)) * 0.5, 
      gamma, rhos[i], 
      as.integer(i %in% response_idx) * mda_cov)
    new_inf <- c(new_inf, x[1] * P[2, 1])
    x <- P %*% x
    x_all[, i + 1] <- x
  }
  return(list(x_all = x_all, new_inf = new_inf))
}