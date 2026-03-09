# outbreak

Malaria outbreak detection and transmission modeling for Uganda. Reads clean monthly and weekly DHIS2 facility data from BigQuery (produced by `uga-etl-facility-data`) and detects district-level outbreaks using kernel-smoothed time series analysis.

## Outbreak Detection

The detection algorithm:
1. Smooths district-level confirmed case time series at multiple bandwidths (60, 100, 365 days, median)
2. Estimates seasonal patterns from the ratio of short-term to long-term smooths
3. Computes an **outbreak index** = short-term smooth (b_60) / seasonally-adjusted baseline (b_0)
4. Districts with outbreak index > 2.0 are flagged as experiencing an outbreak

A WHO-style 75th percentile threshold method is also implemented for comparison.

## Shiny Dashboard

The Shiny app (`outbreak_detection/app.R`) provides:
- An **interactive leaflet choropleth map** with hover tooltips showing district name, outbreak index, and severity status
- A **searchable, sortable table** of all districts with color-coded outbreak severity
- A **time series view** comparing monthly and weekly outbreak indices against the baseline for any selected district
- Clicking a district on the map auto-selects it for the time series view

Deployed at: https://aucarter.shinyapps.io/outbreak_detection

### Testing Locally

```r
# From the outbreak/ directory:
shiny::runApp("outbreak_detection")
```

The app reads from BigQuery at startup. If BigQuery is unavailable, it falls back to the bundled `data.rds`. To regenerate the local data file:

```r
# From the outbreak/ directory:
source("update_shiny_data.R")
```

This pulls both monthly and weekly clean data from BigQuery, computes outbreak indices, and saves to `outbreak_detection/data.rds`.

### Deploying to shinyapps.io

Deployment uses the `rsconnect` package. One-time setup:

```r
rsconnect::setAccountInfo(
  name = "aucarter",
  token = "<YOUR_TOKEN>",
  secret = "<YOUR_SECRET>"
)
```

To deploy:

```r
rsconnect::deployApp(
  appDir = "outbreak_detection",
  appName = "outbreak_detection",
  account = "aucarter"
)
```

The deployed app uses a bundled `sa-key.json` service account key for BigQuery access. Ensure this file is present in the `outbreak_detection/` directory before deploying.

## Transmission Modeling (Experimental)

A discrete-time SIP (Susceptible-Infected-Protected) compartmental model fitted using:
- PyTorch via R `torch` (`R/model_torch.R`, `R/fit_model.R`)
- Base R forward simulation with MDA scenarios (`R/model_R.R`)
- NumPyro/JAX for Bayesian MCMC inference (`R/model_numpyro.py`)

## Data Source

All case data comes from BigQuery dataset `uga_facility_data` in project `uganda-malaria`, populated by the `uga-etl-facility-data` pipeline.

## Dependencies

- `ramptools` — Metadata tables, shapefiles, BigQuery utilities
- `data.table`, `ggplot2`, `sf`, `scales`, `lubridate`
- `shiny`, `leaflet`, `DT`, `htmltools` (for the dashboard)
- `torch` (for transmission modeling only)

## Files

| File | Purpose |
|---|---|
| `R/gen_smooths.R` | Kernel smoothing at multiple bandwidths |
| `R/get_district_dt.R` | Load district-level data from BigQuery |
| `R/get_outbreak_index.R` | Compute outbreak index |
| `R/get_who_outbreak.R` | WHO 75th percentile threshold method |
| `calculate_outbreaks.R` | Iterative outbreak assessment, saves CSV |
| `district_outbreak.R` | District-level outbreak maps |
| `update_shiny_data.R` | Pre-compute and save RDS for Shiny app |
| `outbreak_detection/app.R` | Shiny dashboard (interactive map + time series) |
