# Setup
library(torch)
library(splines)

# source("R/SIP_model.R")

set.seed(1)
torch_manual_seed(2)
n_knots <- 40
learning_rate <- 10
tol <- 1e-6

## Pull in district data
library(yaml);library(data.table); library(ramptools)

## Orient to local machine ----
paths <- read_yaml("my_paths.yaml")

clean_data_root <- file.path(
  paths[["box_dir"]], "data", "dhis", "monthly", "clean", "prod"
)
clean_data_path <- file.path(clean_data_root, "clean_data.csv")

# Read in monthly confirmed case data
dt <- fread(clean_data_path)
dt$period <- as.character(dt$period)
dt <- merge(dt, make_month_map(), by = "period")

# Get district data
district_dt <- dt[level == 3]
d <- unique(district_dt$location_id)[1]
d_data <- district_dt[location_id == d & code_name == "conf_malaria", .(date_mid, imputed_value)]
data <- torch_tensor(cbind(
  x = seq(1, nrow(d_data) * 3, by = 3),
  y = d_data$imputed_value / (max(d_data$imputed_value) * 2)
))
plot(as.matrix(data), ylim = c(0, 1), pch = 19)
n_steps = max(as.matrix(data)[,1])
# Fit model
# Organize params object
params <- list(
  fixed = list(
    n_steps = n_steps,
    gamma = 0.05,
    rhos = rep(0.1, n_steps)
  ),
  fitted = list(
    knot_vals = torch_zeros(n_knots, requires_grad = T)
  )
)


x_all <- fit_model(data, params, tol)

## Look at estimated betas time series
betas <- construct_betas(params$fitted$knot_vals, n_steps)
betas2 <- betas / x_all[2, 1:334]
plot(betas, type = 'l', ylim = c(0, as.numeric(max(betas2))))
lines(betas2, col = 'red')

