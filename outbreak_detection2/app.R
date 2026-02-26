library(shiny); library(data.table); library(ggplot2); library(lubridate)
library(sf); library(scales); library(ramptools)

indicator_map <- list(
  "Confirmed malaria cases" = "conf_malaria",
  "Malaria admissions" = "ip_conf_cases",
  "Combined" = "combined"
)

# -- Data loading function ---------------------------------------------------
# Computes outbreak indices from BigQuery clean data on app startup.
# This runs once when the app launches, so new data is picked up automatically
# whenever the app restarts or is re-deployed.

load_outbreak_data <- function() {
  message("Loading data from BigQuery...")
  source("../R/gen_smooths.R", local = TRUE)

  input_dt <- bq_get_clean_data(frequency = "monthly")
  district_dt <- input_dt[level == 3 & code_name %in% c("conf_malaria", "ip_conf_cases")]
  district_dt[, period := as.character(period)]
  district_dt <- merge(district_dt, make_month_map())
  district_dt[, date := date_mid]
  setnames(district_dt, "value", "raw_value")
  setnames(district_dt, "imputed_value", "value")

  bandwidths <- c(60, 100, 365, 0)
  district_smooth_dt <- gen_smooths(district_dt, bandwidths)

  cast_dt <- dcast(district_smooth_dt, location_name + date + code_name + value ~ bandwidth,
                   value.var = "smooth")
  cast_dt[, monthly_to_year := b_100 / b_365]

  all_month_dt <- cast_dt[, .(monthly_to_year, location_name, date, code_name)]
  all_month_dt[, month := month(date)]
  month_dt <- all_month_dt[, .(median = median(monthly_to_year)), by = .(location_name, code_name, month)]
  month_dt[, smooth := ksmooth(month, median, kernel = "normal", x.points = month, bandwidth = 3)$y,
           by = .(location_name, code_name)]

  district_smooth_dt[, month := month(date)]
  district_smooth_dt <- merge(district_smooth_dt,
                              month_dt[, .(location_name, code_name, month, rel_seasonality = smooth)],
                              by = c("location_name", "code_name", "month"))
  district_smooth_dt[bandwidth == "b_0", smooth := smooth * rel_seasonality]

  dt <- dcast(district_smooth_dt, location_name + code_name + date + value ~ bandwidth,
              value.var = "smooth")
  dt[, excess_rel_baseline := b_60 / b_0]

  avg_dt <- dt[, .(excess_rel_baseline = mean(excess_rel_baseline)), by = .(date, location_name)]
  avg_dt[, code_name := "combined"]
  dt <- rbind(dt, avg_dt, fill = TRUE)

  dt[, Year := year(date)]
  dt[, Month := lubridate::month(date, label = TRUE)]
  setnames(dt, "location_name", "name")

  subset_dt <- dt[Year >= 2017, c("date", "code_name", "name", "excess_rel_baseline", "Year", "Month"), with = FALSE]
  setnames(subset_dt, "excess_rel_baseline", "value")
  dist_shp <- merge(uga_district_shp, subset_dt, by = "name")
  message("Data loaded successfully.")
  return(dist_shp)
}

# Load data at startup
dt <- tryCatch(
  load_outbreak_data(),
  error = function(e) {
    message("BigQuery load failed, falling back to local RDS: ", e$message)
    readRDS("data2.rds")
  }
)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Uganda malaria"),
    
    sidebarLayout(
      sidebarPanel(
        selectInput("element",
                    "Select data element",
                    c("Confirmed malaria cases", "Malaria admissions", "Combined"),
                    selected = "Confirmed malaria cases"),
        sliderInput("date_val",
                    "Date:",
                    min = min(unique(dt$date)) + years(1),
                    max = max(unique(dt$date)),
                    value = max(unique(dt$date)),
                    timeFormat="%b %Y",
                    step = 30,
                    animate = animationOptions(loop = TRUE)),
        sliderInput("threshold_val",
                    "Outbreak threshold:",
                    min = 1.0,
                    max = 5.0,
                    value = 2.0,
                    step = 0.1)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("rankPlot"),
        tableOutput("table")
      )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    data <- reactive({
      element <- input$element
      # Subset to equal to and before end_date
      subset_dt <- dt[dt$code_name == indicator_map[[element]],]
    })

    output$rankPlot <- renderPlot({
      date_val <- floor_date(as.Date(input$date_val), "month")
      dist_shp2 <- data()
      dist_shp2 <- dist_shp2[dist_shp2$date == date_val,]
      dist_shp2[dist_shp2$value > 5, "value"] <- 5
      gg <- ggplot(data = dist_shp2) + 
        geom_sf(aes(fill = value), lwd = 0) + 
        theme_void()  + 
        scale_fill_gradientn(
          colors = c("forestgreen", "yellowgreen", "yellow", "red", "darkred"), 
          values = rescale(log(c(0.1, input$threshold_val - 1, input$threshold_val - 0.5, input$threshold_val, 5))), 
          guide = "colorbar", limits=c(0.1, 5), 
          trans = "log", breaks = c(0.2, 0.5, 1, 2, 5)) + 
        theme(legend.position = "bottom") +
        labs(fill = "Outbreak Index") +
        ggtitle(paste(dist_shp2$Month, dist_shp2$Year))
      gg
    })
    
    output$table <- renderTable({
      date_val <- floor_date(as.Date(input$date_val), "month")
      table_dt <- as.data.table(data())[date == date_val & value > input$threshold_val,]
      table_dt[, District := tolower(DName2019)]
      table_dt[, District := paste(toupper(substr(District, 1, 1)), substr(District, 2, nchar(District)), sep="")]
      setnames(table_dt, "value", "Outbreak Index")
      table_dt[, .(District, `Outbreak Index`)][rev(order(`Outbreak Index`))]
    }, bordered = T, caption = "Outbreak Districts", caption.placement = "top")
}

# Run the application 
shinyApp(ui = ui, server = server)
