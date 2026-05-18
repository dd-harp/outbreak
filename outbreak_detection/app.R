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
library(ramptools)

sf_use_s2(FALSE)

indicator_map <- list(
  "Confirmed malaria cases" = "conf_malaria",
  "Malaria admissions" = "ip_conf_cases",
  "Combined" = "combined"
)

# -- Data loading -------------------------------------------------------------
# Pulls clean monthly + weekly data from BigQuery and computes outbreak
# indices at startup; falls back to bundled data.rds if BigQuery is
# unreachable. shinyapps.io restarts the container after idle timeout, so
# each restart picks up the latest data.
source("load_data.R")
shiny_data <- load_shiny_data("data.rds")

map_dt <- shiny_data$map_dt
map_dt[, date := as.Date(date)]
map_dt[, Month := factor(Month, levels = month.abb, ordered = TRUE)]

ts_dt  <- shiny_data$ts_dt
has_ts <- !is.null(ts_dt) && nrow(ts_dt) > 0
if (has_ts) ts_dt[, date := as.Date(date)]

# Shapefile bundled in the RDS, transformed to WGS84 for leaflet
dist_shp <- st_transform(shiny_data$dist_shp, 4326)
district_names <- sort(unique(dist_shp$name))

all_dates <- sort(unique(map_dt$date))

snap_date <- function(d) {
  d <- as.Date(d)
  idx <- which.min(abs(all_dates - d))
  all_dates[idx]
}

# Outbreak severity label for the table
severity_label <- function(val, threshold) {
  ifelse(val > threshold * 2, "Critical",
  ifelse(val > threshold * 1.5, "Severe",
  ifelse(val > threshold, "Elevated", "Normal")))
}

# Color palette matching the original gradient
outbreak_pal <- colorNumeric(
  palette = c("#228B22", "#9ACD32", "#FFFF00", "#FF0000", "#8B0000"),
  domain = c(0, 5), na.color = "#CCCCCC"
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
      sliderInput("date_val", "Date:",
                  min = min(all_dates) + years(1),
                  max = max(all_dates),
                  value = max(all_dates),
                  timeFormat = "%b %Y", step = 30,
                  animate = animationOptions(loop = TRUE)),
      sliderInput("threshold_val", "Outbreak threshold:",
                  min = 1.0, max = 5.0, value = 2.0, step = 0.1),
      selectInput("district", "District (time series):",
                  choices = district_names,
                  selected = district_names[1]),
      hr(),
      helpText("Outbreak index = ratio of short-term trend to",
               "seasonally-adjusted baseline. Values above the",
               "threshold indicate potential outbreak activity.")
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

  map_data <- reactive({
    code <- indicator_map[[input$element]]
    map_dt[code_name == code]
  })

  # Reactive: sf object for the selected date
  map_sf <- reactive({
    date_val <- snap_date(input$date_val)
    sub <- map_data()[date == date_val]
    plot_sf <- merge(dist_shp, sub, by = "name", all.x = TRUE)
    plot_sf$value[is.na(plot_sf$value)] <- 0
    plot_sf$display_value <- pmin(plot_sf$value, 5)
    plot_sf$district <- gsub(" District$", "", plot_sf$name)
    plot_sf$label <- sprintf(
      "<strong>%s</strong><br/>Outbreak index: %.2f<br/>Status: %s",
      plot_sf$district,
      plot_sf$value,
      severity_label(plot_sf$value, input$threshold_val)
    )
    plot_sf
  })

  # -- Interactive leaflet map ------------------------------------------------
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = TRUE)) %>%
      setView(lng = 32.3, lat = 1.4, zoom = 7) %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(opacity = 0.6))
  })

  observe({
    sf <- map_sf()
    date_val <- snap_date(input$date_val)
    threshold <- input$threshold_val

    leafletProxy("map", data = sf) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        fillColor = ~outbreak_pal(display_value),
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
        position = "bottomright", pal = outbreak_pal,
        values = c(0, 5), title = "Outbreak Index",
        opacity = 0.8,
        labFormat = labelFormat(digits = 1)
      )
  })

  # -- Interactive table with sorting / searching -----------------------------
  output$table <- DT::renderDT({
    date_val <- snap_date(input$date_val)
    threshold <- input$threshold_val
    sub <- map_data()[date == date_val]
    if (nrow(sub) == 0) return(NULL)

    tbl <- sub[, .(
      District = gsub(" District$", "", name),
      `Outbreak Index` = round(value, 2),
      Status = severity_label(value, threshold)
    )]
    tbl <- tbl[order(-`Outbreak Index`)]

    DT::datatable(
      tbl,
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 15,
        dom = "ftip",
        order = list(list(1, "desc")),
        columnDefs = list(
          list(className = "dt-center", targets = 1:2)
        )
      ),
      caption = htmltools::tags$caption(
        style = "caption-side: top; text-align: left; font-weight: bold; font-size: 1.1em;",
        paste0("District Outbreak Status — ", format(date_val, "%B %Y"))
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
        "Outbreak Index",
        background = DT::styleColorBar(range(0, 5), "#FFE0E0"),
        backgroundSize = "98% 80%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      )
  })

  # -- Time series plot -------------------------------------------------------
  output$tsPlot <- renderPlot({
    code <- indicator_map[[input$element]]
    dist <- input$district
    if (!has_ts) {
      plot.new()
      text(0.5, 0.5, "Weekly data not available.\nRun update_shiny_data.R to generate.",
           cex = 1.2, col = "grey50")
      return()
    }

    plot_dt <- ts_dt[code_name == code & name == dist]
    if (nrow(plot_dt) == 0) {
      plot.new()
      text(0.5, 0.5, paste("No data for", dist), cex = 1.2, col = "grey50")
      return()
    }

    plot_dt[, index := pmin(excess_rel_baseline, 10)]

    gg <- ggplot(plot_dt, aes(x = date, y = index, color = frequency)) +
      geom_line(linewidth = 0.7) +
      geom_hline(aes(yintercept = 1, linetype = "Baseline"),
                 color = "grey30", linewidth = 0.8) +
      geom_hline(aes(yintercept = input$threshold_val,
                     linetype = "Outbreak threshold"),
                 color = "red3", linewidth = 0.6) +
      scale_color_manual(
        values = c(monthly = "#2166AC", weekly = "#B2182B"),
        labels = c(monthly = "Monthly", weekly = "Weekly")) +
      scale_linetype_manual(
        name = NULL,
        values = c("Baseline" = "dashed", "Outbreak threshold" = "dotted")) +
      theme_bw(base_size = 14) +
      labs(title = paste(input$element, "\u2014", dist),
           x = "Date", y = "Outbreak Index",
           color = "Frequency") +
      expand_limits(y = 0) +
      theme(legend.position = "bottom")
    gg
  })

  # Click on map → update district selector for time series
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
