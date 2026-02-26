# outbreak

Malaria outbreak detection and transmission modeling for Uganda. Reads clean monthly DHIS2 facility data from BigQuery (produced by `uga-etl-facility-data`) and detects district-level outbreaks using kernel-smoothed time series analysis.

## Outbreak Detection

The detection algorithm:
1. Smooths district-level confirmed case time series at multiple bandwidths (60, 100, 365 days, median)
2. Estimates seasonal patterns from the ratio of short-term to long-term smooths
3. Computes an **outbreak index** = short-term smooth (b_60) / seasonally-adjusted baseline (b_0)
4. Districts with outbreak index > 2.0 are flagged as experiencing an outbreak

A WHO-style 75th percentile threshold method is also implemented for comparison.

## Shiny Dashboard

The Shiny app (`outbreak_detection2/app.R`) provides an interactive choropleth map of outbreak status across Uganda. It reads directly from BigQuery on startup, so new data from the ETL pipeline is picked up automatically on any app restart.

Deployed at: [shinyapps.io/aucarter/outbreak_detection](https://aucarter.shinyapps.io/outbreak_detection)

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
- `shiny` (for the dashboard)
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
| `outbreak_detection2/app.R` | Shiny dashboard (reads from BigQuery) |
