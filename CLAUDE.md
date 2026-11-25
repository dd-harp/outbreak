# outbreak — Claude guidance

Per-repo notes for the Uganda malaria outbreak-detection app. The
umbrella file at `~/projects/dd-harp-outbreak/CLAUDE.md` applies
here too — read it for cross-repo guardrails (cloud auth, deploy
discipline, period-string sorting). This file documents the
specifics of `outbreak/`.

## What this repo is

- **`outbreak_detection/`** — Shiny dashboard deployed to
  `https://aucarter.shinyapps.io/outbreak_detection/`. Map + table +
  time series, sourced from BigQuery.
- **`R/`** — Outbreak-index helpers (`gen_smooths.R`,
  `get_outbreak_index.R`, `get_who_outbreak.R`) and an experimental
  SIP transmission model (`fit_model.R`, `model_torch.R`,
  `model_numpyro.py`).
- **`update_shiny_data.R`** — Local script that rebuilds
  `outbreak_detection/data.rds` from BigQuery for the bundled
  fallback path.
- **`calculate_outbreaks.R`** — Iterative back-test comparing RAMP
  index vs WHO 75th-percentile outbreak flags for a few districts;
  writes `outputs/outbreaks.csv` + `plots/outbreak.pdf`.

## Data flow

```
DHIS2 (uga-etl-facility-data Cloud Run job)
  → BigQuery: uganda-malaria.uga_facility_data.{raw,clean}_{monthly,weekly}_data
  → cloud_compute_outbreak.R (Cloud Run, monthly)
  → BigQuery: outbreak_data (pre-computed, long format)
  → Shiny app reads outbreak_data at startup
```

The pre-computed `outbreak_data` table is the **fast path**. Shiny
falls back to (a) computing smooths from clean BQ data and (b) the
bundled `data.rds` if BQ is unreachable. Always prefer fixing the
upstream pre-compute over making Shiny do more work at startup —
shinyapps.io re-runs `app.R` on every cold start.

## Outbreak methods

Four methods are exposed in the app, all comparable on a single
"value / threshold" axis (>1 = outbreak):

| Method     | `value` definition                                     | Intrinsic threshold |
|------------|--------------------------------------------------------|---------------------|
| `ramp`     | `b_60 / b_0` (short-term smooth ÷ seasonal baseline)   | 2.0 (user slider)   |
| `who_p75`  | `imputed_value / quantile(imputed_value, 0.75)` within same month-of-year | 1.0 |
| `mean_2sd` | `imputed_value / (mean + 2·sd)` within same month-of-year | 1.0              |
| `mean_1sd` | `imputed_value / (mean + sd)` within same month-of-year   | 1.0              |

The WHO/SD method per-period statistics use **same month-of-year**
(monthly) or **same week-of-year** (weekly) across all available
years. They use `imputed_value` (the clean column), not raw, to
avoid one-off DHIS2 outliers blowing up the threshold.

## Schema: BigQuery `outbreak_data`

Long-format, written via `ramptools::bq_write_outbreak_data` with
`overwrite = TRUE`. Columns:

- `date` DATE — period mid-date
- `code_name` STRING — `conf_malaria`, `ip_conf_cases`, or `combined`
- `name` STRING — district name (level 3)
- `frequency` STRING — `monthly` or `weekly`
- `method` STRING — one of the four above
- `value` FLOAT64 — already normalized so >1 = outbreak (except `ramp`)
- `threshold` FLOAT64 — natural threshold (2.0 for `ramp`, 1.0 elsewhere)
- `Year` INT64, `Month` STRING

Re-running `cloud_compute_outbreak.R` overwrites the whole table —
no schema migration needed when columns are added or removed.

## Local testing

```r
# From the outbreak/ directory:
shiny::runApp("outbreak_detection")
```

Cold start tries `bq_get_outbreak_data()` first. If that fails or
returns an old schema (missing the `method` column), it falls back
to computing the four methods from `bq_get_clean_data()`. If BQ is
unreachable entirely, it reads `outbreak_detection/data.rds`.

To regenerate the local fallback:

```r
source("update_shiny_data.R")
```

This mirrors the cloud pre-compute and writes
`outbreak_detection/data.rds`.

## Deploying

Don't deploy without explicit go-ahead (see umbrella). When asked:

```r
rsconnect::deployApp(
  appDir   = "outbreak_detection",
  appName  = "outbreak_detection",
  account  = "aucarter"
)
```

The deployed app reads BQ via the bundled `outbreak_detection/sa-key.json`
service-account key. Confirm it's present (and `.gitignored`) before
deploying.

## Vendored `ramptools`

`outbreak_detection/vendor_bq_utils.R`, `vendor_period.R`, and
`uga_district_shp.rda` are vendored copies from
`~/projects/dd-harp/ramptools/`. The Shiny app sources them directly
instead of calling `library(ramptools)` because shinyapps.io's
manifest parser rejects packages installed from non-CRAN/non-GitHub
sources and we don't want to depend on a private package for a
public app. Resync these files when ramptools changes the BQ helpers
or the district shapefile (no automatic sync — re-copy from
`~/projects/dd-harp/ramptools/{R,data}/`).

## Operational notes

- **The Shiny app's load time is dominated by the BQ read + smooth
  computation, not by Shiny rendering.** Pre-computed reads
  (`bq_get_outbreak_data`) finish in seconds; on-the-fly compute
  takes 30s–2min depending on the BQ region. If the app feels slow,
  check whether the pre-compute job ran recently (`bq query "SELECT
  COUNT(*), MAX(date) FROM outbreak_data"`).
- **`uga_district_shp` is in `ramptools`** — load via
  `ramptools::uga_district_shp` and `st_transform(..., 4326)` for
  leaflet. Don't bundle the shapefile elsewhere.
- **`make_month_map()` / `make_week_map()`** in `ramptools` map BQ
  `period` strings to mid-period `Date` values. Always merge through
  these maps — don't `as.Date()` a period string directly.
- **`bq_get_clean_data` is `SELECT *`** — pass `code_names` and
  `levels` to filter at the BQ side. `level == 3` is district.

## Pointers

- Sister repo: `~/projects/dd-harp/uga-etl-facility-data/` (ETL +
  `scripts/cloud_compute_outbreak.R` — the pre-compute job).
- `ramptools` (`~/projects/dd-harp/ramptools/`) — BQ helpers,
  period maps, district shapefile.
- Umbrella: `~/projects/dd-harp-outbreak/CLAUDE.md`.
