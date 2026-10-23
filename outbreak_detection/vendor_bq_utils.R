#' @title BigQuery Utilities for RAMP Data Pipeline
#' @description Read/write functions for interacting with Google BigQuery as the
#'   primary data backend for DHIS2 facility data.

# -- Connection helpers -------------------------------------------------------

#' Create a BigQuery connection
#'
#' @param project GCP project ID (default: \code{"uganda-malaria"})
#' @param dataset BigQuery dataset name (default: \code{"uga_facility_data"})
#' @param billing GCP project to bill for queries (defaults to \code{project})
#' @return A DBI connection object
#' @export
bq_connect <- function(project = "uganda-malaria",
                       dataset = "uga_facility_data",
                       billing = project) {
  DBI::dbConnect(
    bigrquery::bigquery(),
    project = project,
    dataset = dataset,
    billing = billing
  )
}

# -- Read functions -----------------------------------------------------------

#' Get the latest version number from BigQuery
#'
#' @param con A DBI BigQuery connection (or NULL to create one)
#' @param frequency Either \code{"weekly"} or \code{"monthly"}
#' @return Integer value of the latest version
#' @export
bq_get_latest_version <- function(con = NULL, frequency = "monthly") {

  own_con <- is.null(con)
  if (own_con) con <- bq_connect()
  on.exit(if (own_con) DBI::dbDisconnect(con))

  table_name <- paste0("raw_", frequency, "_version_metadata")
  sql <- sprintf("SELECT MAX(version) AS latest FROM `%s`", table_name)
  result <- DBI::dbGetQuery(con, sql)
  latest <- result$latest
  if (is.null(latest) || is.na(latest)) return(0L)
  return(as.integer(latest))
}

#' Get version metadata from BigQuery
#'
#' @param con A DBI BigQuery connection (or NULL to create one)
#' @param frequency Either \code{"weekly"} or \code{"monthly"}
#' @param version_id Version to retrieve (NULL = latest)
#' @return data.table with version metadata
#' @export
bq_get_version_metadata <- function(con = NULL, frequency = "monthly",
                                    version_id = NULL) {
  own_con <- is.null(con)
  if (own_con) con <- bq_connect()
  on.exit(if (own_con) DBI::dbDisconnect(con))

  if (is.null(version_id)) {
    version_id <- bq_get_latest_version(con, frequency)
  }
  table_name <- paste0("raw_", frequency, "_version_metadata")
  sql <- sprintf("SELECT * FROM `%s` WHERE version = %d", table_name, version_id)
  result <- DBI::dbGetQuery(con, sql)
  return(data.table::as.data.table(result))
}

#' Read raw DHIS data from BigQuery
#'
#' Retrieves the latest version of each observation, analogous to
#' \code{\link{get_data}} for SQLite.
#'
#' @param con A DBI BigQuery connection (or NULL to create one)
#' @param frequency Either \code{"weekly"} or \code{"monthly"}
#' @param id_list Named list of ID columns and values to filter on (optional)
#' @param version_id Maximum version to include (NULL = latest)
#' @return data.table with the requested data
#' @export
bq_get_data <- function(con = NULL, frequency = "monthly",
                        id_list = NULL, version_id = NULL) {
  own_con <- is.null(con)
  if (own_con) con <- bq_connect()
  on.exit(if (own_con) DBI::dbDisconnect(con))

  if (is.null(version_id)) {
    version_id <- bq_get_latest_version(con, frequency)
  }

  table_name <- paste0("raw_", frequency, "_data")
  id_cols <- c("dataElement", "period", "orgUnit")

  # Build WHERE clauses

  where_parts <- sprintf("version <= %d", version_id)
  if (!is.null(id_list)) {
    for (col in names(id_list)) {
      vals <- paste0("'", id_list[[col]], "'", collapse = ", ")
      where_parts <- c(where_parts, sprintf("%s IN (%s)", col, vals))
    }
  }
  where_clause <- paste(where_parts, collapse = " AND ")

  # Use window function to get latest version per observation
  sql <- sprintf(
    "SELECT * EXCEPT(rn) FROM (
       SELECT *, ROW_NUMBER() OVER (
         PARTITION BY %s ORDER BY version DESC
       ) AS rn
       FROM `%s`
       WHERE %s
     ) WHERE rn = 1",
    paste(id_cols, collapse = ", "),
    table_name,
    where_clause
  )

  result <- DBI::dbGetQuery(con, sql)
  return(data.table::as.data.table(result))
}

#' Get new/changed rows not yet in BigQuery
#'
#' Compares \code{new_data} against what is currently stored and returns
#' only the diff (new or changed values).
#'
#' @param new_data data.frame with freshly pulled DHIS data
#' @param con A DBI BigQuery connection (or NULL to create one)
#' @param frequency Either \code{"weekly"} or \code{"monthly"}
#' @return data.table of rows that are new or have changed values
#' @export
bq_get_db_diff <- function(new_data, con = NULL, frequency = "monthly") {
  own_con <- is.null(con)
  if (own_con) con <- bq_connect()
  on.exit(if (own_con) DBI::dbDisconnect(con))

  id_vars <- c("dataElement", "period", "orgUnit")
  value_var <- "value"

  id_list <- lapply(id_vars, function(id) unique(new_data[[id]]))
  names(id_list) <- id_vars

  db_data <- bq_get_data(con, frequency, id_list)

  # If no existing data, everything is new
  if (nrow(db_data) == 0) {
    return(data.table::as.data.table(new_data))
  }

  db_diff <- dplyr::anti_join(
    data.table::as.data.table(new_data),
    db_data,
    by = c(id_vars, value_var)
  )
  return(db_diff)
}

#' Read clean aggregated data from BigQuery
#'
#' @param con A DBI BigQuery connection (or NULL to create one)
#' @param frequency Either \code{"weekly"} or \code{"monthly"}
#' @param code_names Character vector of indicator code_names to filter (optional)
#' @param levels Integer vector of admin levels to filter (optional)
#' @return data.table with clean aggregated data
#' @export
bq_get_clean_data <- function(con = NULL, frequency = "monthly",
                              code_names = NULL, levels = NULL) {
  own_con <- is.null(con)
  if (own_con) con <- bq_connect()
  on.exit(if (own_con) DBI::dbDisconnect(con))

  table_name <- paste0("clean_", frequency, "_data")

  where_parts <- character(0)
  if (!is.null(code_names)) {
    vals <- paste0("'", code_names, "'", collapse = ", ")
    where_parts <- c(where_parts, sprintf("code_name IN (%s)", vals))
  }
  if (!is.null(levels)) {
    vals <- paste(levels, collapse = ", ")
    where_parts <- c(where_parts, sprintf("level IN (%s)", vals))
  }

  if (length(where_parts) > 0) {
    sql <- sprintf("SELECT * FROM `%s` WHERE %s",
                   table_name, paste(where_parts, collapse = " AND "))
  } else {
    sql <- sprintf("SELECT * FROM `%s`", table_name)
  }

  result <- DBI::dbGetQuery(con, sql)
  return(data.table::as.data.table(result))
}

#' Sample imputed facility data from BigQuery
#'
#' Returns a random sample of rows from the imputed facility data table.
#' Useful for computing outlier summaries without reading the full table.
#'
#' @param con A DBI BigQuery connection (or NULL to create one)
#' @param frequency Either \code{"weekly"} or \code{"monthly"}
#' @param n Maximum number of rows to sample
#' @return data.table with sampled imputed data
#' @export
bq_get_imputed_sample <- function(con = NULL, frequency = "monthly", n = 500000L) {
  own_con <- is.null(con)
  if (own_con) con <- bq_connect()
  on.exit(if (own_con) DBI::dbDisconnect(con))

  table_name <- paste0("imputed_", frequency, "_facility_data")
  sql <- sprintf(
    "SELECT * FROM `%s` WHERE is_outlier IS NOT NULL ORDER BY RAND() LIMIT %d",
    table_name, as.integer(n)
  )
  result <- DBI::dbGetQuery(con, sql)
  return(data.table::as.data.table(result))
}

# -- Write functions ----------------------------------------------------------

#' Append raw data to BigQuery
#'
#' @param dt data.table to append (must have: dataElement, period, orgUnit,
#'   value, timestamp, version)
#' @param con A DBI BigQuery connection (or NULL to create one)
#' @param frequency Either \code{"weekly"} or \code{"monthly"}
#' @param chunk_size Upper limit on download chunk
#' @export
bq_append_raw_data <- function(dt, con = NULL, frequency = "monthly",
                              chunk_size = 500000L) {
  own_con <- is.null(con)
  if (own_con) con <- bq_connect()
  on.exit(if (own_con) DBI::dbDisconnect(con))

  table_name <- paste0("raw_", frequency, "_data")

  # Keep only columns that match the BQ schema and ensure correct types
  expected_cols <- c("dataElement", "period", "orgUnit", "value",
                     "timestamp", "version")
  dt <- data.table::copy(dt[, intersect(expected_cols, names(dt)), with = FALSE])
  if ("value" %in% names(dt)) {
    dt[, value := as.numeric(value)]
    dt <- dt[is.finite(value)]
  }

  n <- nrow(dt)

  if (n <= chunk_size) {
    DBI::dbWriteTable(con, table_name, dt, append = TRUE)
  } else {
    n_chunks <- ceiling(n / chunk_size)
    message(sprintf("Uploading %d rows in %d chunks...", n, n_chunks))
    for (i in seq_len(n_chunks)) {
      start_row <- (i - 1L) * chunk_size + 1L
      end_row <- min(i * chunk_size, n)
      chunk <- dt[start_row:end_row, ]
      DBI::dbWriteTable(con, table_name, chunk, append = TRUE)
      message(sprintf("  Chunk %d/%d: rows %d-%d", i, n_chunks, start_row, end_row))
    }
  }
  message(sprintf("Appended %d rows to %s", n, table_name))
}

#' Write version metadata to BigQuery
#'
#' @param version_df data.frame with version metadata
#' @param con A DBI BigQuery connection (or NULL to create one)
#' @param frequency Either \code{"weekly"} or \code{"monthly"}
#' @export
bq_append_version_metadata <- function(version_df, con = NULL,
                                       frequency = "monthly") {
  own_con <- is.null(con)
  if (own_con) con <- bq_connect()
  on.exit(if (own_con) DBI::dbDisconnect(con))

  table_name <- paste0("raw_", frequency, "_version_metadata")
  DBI::dbWriteTable(con, table_name, version_df, append = TRUE)
  message(sprintf("Wrote version %s metadata to %s",
                  version_df$version[1], table_name))
}

#' Write clean aggregated data to BigQuery (full replace)
#'
#' Overwrites the clean data table with the latest processed output.
#'
#' @param dt data.table with clean aggregated data
#' @param con A DBI BigQuery connection (or NULL to create one)
#' @param frequency Either \code{"weekly"} or \code{"monthly"}
#' @param append_mode FALSE=overwrite, TRUE=append
#' @param chunk_size Upper limit on upload
#' @export
bq_write_clean_data <- function(dt, con = NULL, frequency = "monthly",
                                chunk_size = 500000L, append_mode = FALSE) {
  own_con <- is.null(con)
  if (own_con) con <- bq_connect()
  on.exit(if (own_con) DBI::dbDisconnect(con))

  table_name <- paste0("clean_", frequency, "_data")
  n <- nrow(dt)

  if (n <= chunk_size) {
    DBI::dbWriteTable(con, table_name, dt,
                      overwrite = !append_mode, append = append_mode)
  } else {
    n_chunks <- ceiling(n / chunk_size)
    message(sprintf("Uploading %d rows in %d chunks...", n, n_chunks))
    for (i in seq_len(n_chunks)) {
      start_row <- (i - 1L) * chunk_size + 1L
      end_row <- min(i * chunk_size, n)
      chunk <- dt[start_row:end_row, ]
      # First chunk of first call overwrites; everything else appends
      first_chunk_overwrite <- (i == 1L) && !append_mode
      DBI::dbWriteTable(con, table_name, chunk,
                        overwrite = first_chunk_overwrite,
                        append = !first_chunk_overwrite)
      message(sprintf("  Chunk %d/%d: rows %d-%d", i, n_chunks, start_row, end_row))
    }
  }
  message(sprintf("Wrote %d rows to %s", nrow(dt), table_name))
}

#' Write imputed facility data to BigQuery (full replace)
#'
#' @param dt data.table with imputed facility-level data
#' @param con A DBI BigQuery connection (or NULL to create one)
#' @param frequency Either \code{"weekly"} or \code{"monthly"}
#' @param append_mode FALSE=overwrite, TRUE=append
#' @param chunk_size Upper limit on upload
#' @export
bq_write_imputed_data <- function(dt, con = NULL, frequency = "monthly",
                                  chunk_size = 500000L, append_mode = FALSE) {
  own_con <- is.null(con)
  if (own_con) con <- bq_connect()
  on.exit(if (own_con) DBI::dbDisconnect(con))

  table_name <- paste0("imputed_", frequency, "_facility_data")
  n <- nrow(dt)

  if (n <= chunk_size) {
    DBI::dbWriteTable(con, table_name, dt,
                      overwrite = !append_mode, append = append_mode)
  } else {
    n_chunks <- ceiling(n / chunk_size)
    message(sprintf("Uploading %d rows in %d chunks...", n, n_chunks))
    for (i in seq_len(n_chunks)) {
      start_row <- (i - 1L) * chunk_size + 1L
      end_row <- min(i * chunk_size, n)
      chunk <- dt[start_row:end_row, ]
      first_chunk_overwrite <- (i == 1L) && !append_mode
      DBI::dbWriteTable(con, table_name, chunk,
                        overwrite = first_chunk_overwrite,
                        append = !first_chunk_overwrite)
      message(sprintf("  Chunk %d/%d: rows %d-%d", i, n_chunks, start_row, end_row))
    }
  }
  message(sprintf("Wrote %d rows to %s", nrow(dt), table_name))
}

# -- Schema setup -------------------------------------------------------------

#' Initialize BigQuery tables for the DHIS data pipeline
#'
#' Creates the required tables in BigQuery if they don't exist. Run once during
#' initial setup.
#'
#' @param con A DBI BigQuery connection (or NULL to create one)
#' @param frequency Either \code{"weekly"}, \code{"monthly"}, or \code{"both"}
#' @export
bq_init_tables <- function(con = NULL, frequency = "both") {
  own_con <- is.null(con)
  if (own_con) con <- bq_connect()
  on.exit(if (own_con) DBI::dbDisconnect(con))

  frequencies <- if (frequency == "both") c("weekly", "monthly") else frequency

  for (freq in frequencies) {
    # Raw data table
    raw_table <- paste0("raw_", freq, "_data")
    if (!DBI::dbExistsTable(con, raw_table)) {
      sql <- sprintf(
        "CREATE TABLE `%s` (
          dataElement STRING NOT NULL,
          period STRING NOT NULL,
          orgUnit STRING NOT NULL,
          value FLOAT64,
          timestamp STRING,
          version INT64 NOT NULL
        )", raw_table)
      DBI::dbExecute(con, sql)
      message(sprintf("Created table: %s", raw_table))
    }

    # Version metadata table
    meta_table <- paste0("raw_", freq, "_version_metadata")
    if (!DBI::dbExistsTable(con, meta_table)) {
      sql <- sprintf(
        "CREATE TABLE `%s` (
          version INT64 NOT NULL,
          timestamp STRING,
          user STRING,
          commit STRING,
          branch STRING,
          period_start STRING,
          period_end STRING,
          api_call STRING
        )", meta_table)
      DBI::dbExecute(con, sql)
      message(sprintf("Created table: %s", meta_table))
    }

    # Clean data table
    clean_table <- paste0("clean_", freq, "_data")
    if (!DBI::dbExistsTable(con, clean_table)) {
      sql <- sprintf(
        "CREATE TABLE `%s` (
          location_id STRING,
          location_name STRING,
          period STRING,
          code_name STRING,
          value FLOAT64,
          imputed_value FLOAT64,
          level INT64
        )", clean_table)
      DBI::dbExecute(con, sql)
      message(sprintf("Created table: %s", clean_table))
    }

    # Imputed facility data table
    imputed_table <- paste0("imputed_", freq, "_facility_data")
    if (!DBI::dbExistsTable(con, imputed_table)) {
      sql <- sprintf(
        "CREATE TABLE `%s` (
          location_id STRING,
          date_mid DATE,
          value FLOAT64,
          imputed_value FLOAT64,
          period STRING,
          is_outlier INT64,
          code_name STRING,
          dhis_id STRING,
          timestamp STRING,
          version INT64,
          location_name STRING,
          level INT64
        )", imputed_table)
      DBI::dbExecute(con, sql)
      message(sprintf("Created table: %s", imputed_table))
    }
    # Outbreak data table (long format, one row per location × date ×
    # code_name × frequency × method). cloud_compute_outbreak.R overwrites
    # this on each run, so this CREATE is only used on first init.
    if (freq == "monthly") {
      outbreak_table <- "outbreak_data"
      if (!DBI::dbExistsTable(con, outbreak_table)) {
        sql <- sprintf(
          "CREATE TABLE `%s` (
            date DATE,
            code_name STRING,
            name STRING,
            frequency STRING,
            method STRING,
            value FLOAT64,
            threshold FLOAT64,
            Year INT64,
            Month STRING
          )", outbreak_table)
        DBI::dbExecute(con, sql)
        message(sprintf("Created table: %s", outbreak_table))
      }
    }
  }
  message("BigQuery initialization complete.")
}

# -- Outbreak data functions --------------------------------------------------

#' Write pre-computed outbreak index data to BigQuery
#'
#' Overwrites the \code{outbreak_data} table with the latest computed outbreak
#' indices. Designed to be called after clean/aggregate to keep the Shiny app
#' read-only.
#'
#' @param dt data.table with columns: date, code_name, name, value, Year, Month
#' @param con A DBI BigQuery connection (or NULL to create one)
#' @export
bq_write_outbreak_data <- function(dt, con = NULL) {
  own_con <- is.null(con)
  if (own_con) con <- bq_connect()
  on.exit(if (own_con) DBI::dbDisconnect(con))

  table_name <- "outbreak_data"
  DBI::dbWriteTable(con, table_name, dt, overwrite = TRUE)
  message(sprintf("Wrote %d rows to %s", nrow(dt), table_name))
}

#' Read pre-computed outbreak index data from BigQuery
#'
#' @param con A DBI BigQuery connection (or NULL to create one)
#' @param frequency Optional filter: "monthly" or "weekly". NULL returns
#'   all rows. Pushed into the SELECT to reduce wire payload.
#' @return data.table with outbreak index data
#' @export
bq_get_outbreak_data <- function(con = NULL, frequency = NULL) {
  own_con <- is.null(con)
  if (own_con) con <- bq_connect()
  on.exit(if (own_con) DBI::dbDisconnect(con))

  if (is.null(frequency)) {
    sql <- "SELECT * FROM `outbreak_data`"
  } else {
    sql <- sprintf("SELECT * FROM `outbreak_data` WHERE frequency = '%s'",
                   frequency)
  }
  result <- DBI::dbGetQuery(con, sql)
  return(data.table::as.data.table(result))
}
