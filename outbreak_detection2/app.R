library(shiny)
library(ggplot2)
library(lubridate)
library(sf)
library(scales)
library(bigrquery)
library(DBI)
library(data.table)
library(ramptools)

# Avoid s2 spherical geometry — prevents PROJ database lookup warnings
# that can cascade into C stack overflows during geom_sf rendering.
sf_use_s2(FALSE)

indicator_map <- list(
  "Confirmed malaria cases" = "conf_malaria",
  "Malaria admissions" = "ip_conf_cases",
  "Combined" = "combined"
)

# -- BigQuery auth (service account key bundled with app) ---------------------
if (file.exists("sa-key.json")) {
  bigrquery::bq_auth(path = "sa-key.json")
}

# -- Data loading: read pre-computed outbreak table from BigQuery -------------
load_outbreak_data <- function() {
  message("Loading pre-computed outbreak data from BigQuery...")
  outbreak_dt <- bq_get_outbreak_data()
  outbreak_dt[, date := as.Date(date)]
  outbreak_dt[, Month := factor(Month, levels = month.abb, ordered = TRUE)]
  message("Data loaded successfully.")
  return(outbreak_dt)
}

# Load data at startup — keep as lightweight data.table (no geometry)
dt <- tryCatch(
  load_outbreak_data(),
  error = function(e) {
    message("BigQuery load failed, falling back to local RDS: ", e$message)
    readRDS("data2.rds")
  }
)

# Keep the district shapefile separate — only 146 rows with geometry
dist_shp <- uga_district_shp

all_dates <- sort(unique(dt$date))

# Snap a slider value to the nearest actual date in the data.
# Data dates are mid-month (15th/16th), not first-of-month,
# so floor_date("month") would miss them entirely.
snap_date <- function(d) {
  d <- as.Date(d)
  idx <- which.min(abs(all_dates - d))
  all_dates[idx]
}

# Define UI
ui <- fluidPage(
  titlePanel("Uganda malaria"),
  sidebarLayout(
    sidebarPanel(
      selectInput("element",
                  "Select data element",
                  c("Confirmed malaria cases", "Malaria admissions", "Combined"),
                  selected = "Confirmed malaria cases"),
      sliderInput("date_val",
                  "Date:",
                  min = min(all_dates) + years(1),
                  max = max(all_dates),
                  value = max(all_dates),
                  timeFormat = "%b %Y",
                  step = 30,
                  animate = animationOptions(loop = TRUE)),
      sliderInput("threshold_val",
                  "Outbreak threshold:",
                  min = 1.0,
                  max = 5.0,
                  value = 2.0,
                  step = 0.1)
    ),
    mainPanel(
      plotOutput("rankPlot"),
      tableOutput("table")
    )
  )
)

# Define server logic
server <- function(input, output) {
  data <- reactive({
    code <- indicator_map[[input$element]]
    dt[code_name == code]
  })

  output$rankPlot <- renderPlot({
    date_val <- snap_date(input$date_val)
    sub <- data()[date == date_val]
    # Join geometry only for the ~146 rows we actually plot
    plot_sf <- dplyr::left_join(dist_shp, sub, by = "name")
    plot_sf$value[is.na(plot_sf$value)] <- 0
    plot_sf$value[plot_sf$value > 5] <- 5
    gg <- ggplot(data = plot_sf) +
      geom_sf(aes(fill = value), lwd = 0) +
      theme_void() +
      scale_fill_gradientn(
        colors = c("forestgreen", "yellowgreen", "yellow", "red", "darkred"),
        values = rescale(log(c(0.1, input$threshold_val - 1,
                               input$threshold_val - 0.5,
                               input$threshold_val, 5))),
        guide = "colorbar", limits = c(0.1, 5),
        trans = "log", breaks = c(0.2, 0.5, 1, 2, 5)) +
      theme(legend.position = "bottom") +
      labs(fill = "Outbreak Index") +
      ggtitle(paste(unique(sub$Month), unique(sub$Year)))
    gg
  })

  output$table <- renderTable({
    date_val <- snap_date(input$date_val)
    table_dt <- data()[date == date_val & value > input$threshold_val]
    if (nrow(table_dt) == 0) return(NULL)
    table_dt[, District := gsub(" District$", "", name)]
    setnames(table_dt, "value", "Outbreak Index")
    table_dt[, .(District, `Outbreak Index`)][rev(order(`Outbreak Index`))]
  }, bordered = TRUE, caption = "Outbreak Districts", caption.placement = "top")
}

shinyApp(ui = ui, server = server)
