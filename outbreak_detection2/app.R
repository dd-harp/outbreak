library(shiny); library(data.table); library(ggplot2); library(lubridate)
library(sf); library(scales)

indicator_map <- list(
  "Confirmed malaria cases" = "conf_malaria",
  "Malaria admissions" = "ip_conf_cases",
  "Combined" = "combined"
)

# Read data
dt <- readRDS("data2.rds")

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
