library(shiny)
library(ggplot2)
library(lubridate)
library(sf)
library(scales)
library(leaflet)
library(DT)
library(htmltools)
library(data.table)
library(bigrquery)

sf_use_s2(FALSE)

indicator_map <- list(
  "Confirmed malaria cases" = "conf_malaria",
  "Malaria admissions" = "ip_conf_cases",
  "Combined" = "combined"
)

method_labels <- c(
  "RAMP outbreak index"  = "ramp",
  "WHO 75th percentile"  = "who_p75",
  "Mean + 2SD"           = "mean_2sd",
  "Mean + 1SD"           = "mean_1sd"
)
method_default_threshold <- c(ramp = 2.0, who_p75 = 1.0,
                              mean_2sd = 1.0, mean_1sd = 1.0)

# -- Data loading -------------------------------------------------------------
# Fast path: BigQuery `outbreak_data` (pre-computed). Falls through to a
# clean-data re-compute and finally to bundled data.rds. See load_data.R.
source("load_data.R")
shiny_data <- load_shiny_data("data.rds")

map_dt <- shiny_data$map_dt
map_dt[, date := as.Date(date)]
map_dt[, Month := factor(Month, levels = month.abb, ordered = TRUE)]

ts_dt  <- shiny_data$ts_dt
has_ts <- !is.null(ts_dt) && nrow(ts_dt) > 0
if (has_ts) ts_dt[, date := as.Date(date)]

# True when the loader already supplied weekly rows (data.rds fallback or
# recompute path). False on the fast BQ path — weekly is lazy-loaded.
has_weekly_preloaded <- isTRUE(shiny_data$has_weekly)

dist_shp <- st_transform(shiny_data$dist_shp, 4326)
district_names <- sort(unique(dist_shp$name))

all_dates <- sort(unique(map_dt$date))

snap_date <- function(d) {
  d <- as.Date(d)
  all_dates[which.min(abs(all_dates - d))]
}

severity_label <- function(val, threshold) {
  ifelse(val > threshold * 2, "Critical",
  ifelse(val > threshold * 1.5, "Severe",
  ifelse(val > threshold, "Elevated", "Normal")))
}

# 5-stop palette anchored so that `threshold` (= the user slider value)
# falls on the orange-red transition (position 0.5 of domain). Domain
# stretches to 2 × threshold so the deepest red corresponds to "Critical".
make_outbreak_pal <- function(threshold) {
  colorNumeric(
    palette = c("#228B22", "#FFD700", "#FF6600", "#CC0000", "#660000"),
    domain  = c(0, 2 * threshold),
    na.color = "#CCCCCC"
  )
}

method_line_colors <- c(
  "RAMP outbreak index" = "#2166AC",
  "WHO 75th percentile" = "#B2182B",
  "Mean + 2SD"          = "#1A9850",
  "Mean + 1SD"          = "#F49717"
)

# -- UI -----------------------------------------------------------------------
ui <- fluidPage(
  tags$head(tags$style(HTML("
    .severity-critical { color: #8B0000; font-weight: bold; }
    .severity-severe   { color: #FF0000; font-weight: bold; }
    .severity-elevated { color: #FF8C00; }
    .severity-normal   { color: #228B22; }
  "))),
  titlePanel("Uganda Malaria Outbreak Detection"),
  sidebarLayout(
    sidebarPanel(
      selectInput("element", "Indicator:",
                  c("Confirmed malaria cases", "Malaria admissions", "Combined"),
                  selected = "Confirmed malaria cases"),
      selectInput("method", "Outbreak method (map):",
                  choices = method_labels,
                  selected = "ramp"),
      sliderInput("date_val", "Date:",
                  min = min(all_dates) + years(1),
                  max = max(all_dates),
                  value = max(all_dates),
                  timeFormat = "%b %Y", step = 30,
                  animate = animationOptions(loop = TRUE)),
      sliderInput("threshold_val", "Outbreak threshold:",
                  min = 0.5, max = 5.0, value = 2.0, step = 0.1),
      selectInput("district", "District (time series):",
                  choices = district_names,
                  selected = district_names[1]),
      checkboxGroupInput("ts_methods", "Overlay on time series:",
                         choices = method_labels,
                         selected = "ramp"),
      hr(),
      helpText("All methods are scaled so values > 1 indicate the metric",
               "exceeds its natural threshold (RAMP uses the slider).")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Map",
                 leafletOutput("map", height = "500px"),
                 hr(),
                 DT::DTOutput("table")),
        tabPanel("Time Series",
                 plotOutput("tsPlot", height = "500px"))
      )
    )
  )
)

# -- Server -------------------------------------------------------------------
server <- function(input, output, session) {

  # Reset the threshold slider to the method's natural default when changed.
  observeEvent(input$method, {
    updateSliderInput(session, "threshold_val",
                      value = method_default_threshold[[input$method]])
  })

  map_data <- reactive({
    code <- indicator_map[[input$element]]
    map_dt[code_name == code & method == input$method]
  })

  # Weekly TS data is loaded on demand. The reactive only evaluates the
  # first time `weekly_ts()` is called (i.e., first time the Time-Series
  # tab renders) and the result is cached for the session.
  weekly_ts <- reactive({
    if (has_weekly_preloaded) {
      return(ts_dt[frequency == "weekly"])
    }
    showNotification("Loading weekly data...", id = "load_weekly",
                     duration = NULL, type = "message")
    on.exit(removeNotification("load_weekly"), add = TRUE)
    load_weekly_ts_data()
  })

  outbreak_pal <- reactive(make_outbreak_pal(input$threshold_val))

  map_sf <- reactive({
    date_val <- snap_date(input$date_val)
    sub <- map_data()[date == date_val]
    cap <- 2 * input$threshold_val
    plot_sf <- merge(dist_shp, sub, by = "name", all.x = TRUE)
    plot_sf$value[is.na(plot_sf$value)] <- 0
    plot_sf$display_value <- pmin(plot_sf$value, cap)
    plot_sf$district <- gsub(" District$", "", plot_sf$name)
    plot_sf$label <- sprintf(
      "<strong>%s</strong><br/>%s: %.2f<br/>Status: %s",
      plot_sf$district,
      names(method_labels)[method_labels == input$method],
      plot_sf$value,
      severity_label(plot_sf$value, input$threshold_val)
    )
    plot_sf
  })

  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = TRUE)) %>%
      setView(lng = 32.3, lat = 1.4, zoom = 7) %>%
      # Esri gray canvas is keyless; CARTO basemaps now require an API key.
      addProviderTiles(providers$Esri.WorldGrayCanvas,
                       options = providerTileOptions(opacity = 0.6))
  })

  observe({
    sf <- map_sf()
    pal <- outbreak_pal()
    thr <- input$threshold_val
    leafletProxy("map", data = sf) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        fillColor = ~pal(display_value),
        fillOpacity = 0.8,
        weight = 0.5, color = "#444444", opacity = 0.6,
        label = lapply(sf$label, HTML),
        labelOptions = labelOptions(
          style = list("font-size" = "13px", "padding" = "6px 10px"),
          direction = "auto"),
        highlightOptions = highlightOptions(
          weight = 2, color = "#000000", fillOpacity = 0.9,
          bringToFront = TRUE)
      ) %>%
      addLegend(
        position = "bottomright", pal = pal,
        values = c(0, 2 * thr),
        bins = c(0, thr / 2, thr, 1.5 * thr, 2 * thr),
        title = sprintf("%s<br/>(threshold %.1f)",
                        names(method_labels)[method_labels == input$method],
                        thr),
        opacity = 0.8,
        labFormat = labelFormat(digits = 2)
      )
  })

  output$table <- DT::renderDT({
    date_val <- snap_date(input$date_val)
    # Not named threshold: outbreak_data has a threshold column (the
    # baseline in case counts), which would shadow it inside sub[, ...].
    thr_val <- input$threshold_val
    sub <- map_data()[date == date_val]
    if (nrow(sub) == 0) return(NULL)

    tbl <- sub[, .(
      District = gsub(" District$", "", name),
      Value = round(value, 2),
      Status = severity_label(value, thr_val)
    )]
    tbl <- tbl[order(-Value)]

    DT::datatable(
      tbl,
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 15,
        dom = "ftip",
        order = list(list(1, "desc")),
        columnDefs = list(list(className = "dt-center", targets = 1:2))
      ),
      caption = htmltools::tags$caption(
        style = "caption-side: top; text-align: left; font-weight: bold; font-size: 1.1em;",
        paste0(names(method_labels)[method_labels == input$method],
               " — ", format(date_val, "%B %Y"))
      )
    ) %>%
      DT::formatStyle(
        "Status",
        color = DT::styleEqual(
          c("Critical", "Severe", "Elevated", "Normal"),
          c("#8B0000", "#FF0000", "#FF8C00", "#228B22")
        ),
        fontWeight = DT::styleEqual(
          c("Critical", "Severe", "Elevated", "Normal"),
          c("bold", "bold", "normal", "normal")
        )
      ) %>%
      DT::formatStyle(
        "Value",
        background = DT::styleColorBar(c(0, 2 * thr_val), "#FFE0E0"),
        backgroundSize = "98% 80%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      )
  })

  output$tsPlot <- renderPlot({
    code <- indicator_map[[input$element]]
    dist <- input$district
    sel_methods <- input$ts_methods
    if (!has_ts) {
      plot.new()
      text(0.5, 0.5, "Time series data not available.",
           cex = 1.2, col = "grey50")
      return()
    }

    monthly_part <- ts_dt[code_name == code & name == dist &
                          frequency == "monthly"]
    weekly_part  <- weekly_ts()
    if (!is.null(weekly_part) && nrow(weekly_part) > 0) {
      weekly_part <- weekly_part[code_name == code & name == dist]
    } else {
      weekly_part <- data.table()
    }
    plot_dt <- rbind(monthly_part, weekly_part, fill = TRUE)

    if (nrow(plot_dt) == 0) {
      plot.new()
      text(0.5, 0.5, paste("No data for", dist), cex = 1.2, col = "grey50")
      return()
    }

    # Observation line: identical across methods for a given (date,
    # frequency), so dedupe.
    data_dt <- unique(plot_dt[, .(date, frequency, obs)])

    # Threshold curves in case-count units. The RAMP threshold is
    # `b_0 × user slider` (slider default 2.0 = the conventional RAMP
    # outbreak cutoff). The other methods have intrinsic thresholds
    # (75th percentile / mean + k·sd), so the slider does not apply.
    thr_dt <- plot_dt[method %in% sel_methods,
                      .(date, frequency, method, threshold)]
    if (nrow(thr_dt) > 0) {
      thr_dt[, threshold_cases := ifelse(method == "ramp",
                                         threshold * input$threshold_val,
                                         threshold)]
      thr_dt[, method_label := factor(method, levels = method_labels,
                                      labels = names(method_labels))]
    }

    gg <- ggplot() +
      geom_line(data = data_dt, aes(x = date, y = obs),
                color = "grey20", linewidth = 0.7) +
      geom_point(data = data_dt, aes(x = date, y = obs),
                 color = "grey20", size = 0.6, alpha = 0.5)
    if (nrow(thr_dt) > 0) {
      gg <- gg + geom_line(
        data = thr_dt,
        aes(x = date, y = threshold_cases,
            color = method_label, group = method_label),
        linewidth = 0.8, alpha = 0.9)
    }
    gg +
      facet_wrap(~ frequency, scales = "free_y", ncol = 1) +
      scale_color_manual(values = method_line_colors, drop = TRUE) +
      theme_bw(base_size = 14) +
      labs(title = paste(input$element, "—", dist),
           x = "Date", y = "Cases per period",
           color = "Threshold (method)") +
      expand_limits(y = 0) +
      theme(legend.position = "bottom")
  })

  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    if (!is.null(click)) {
      pt <- st_point(c(click$lng, click$lat))
      hit <- st_intersects(st_sfc(pt, crs = 4326), dist_shp)
      if (length(hit[[1]]) > 0) {
        clicked_name <- dist_shp$name[hit[[1]][1]]
        updateSelectInput(session, "district", selected = clicked_name)
      }
    }
  })
}

shinyApp(ui = ui, server = server)
