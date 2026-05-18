library(shiny)
library(bslib)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(magrittr)
library(scales)
library(stringr)
library(lubridate)
library(ggiraph)
library(sf)
require(rmapshaper)
require(ggrepel)
require(shinyWidgets) 

# UI params
PLOT_TITLE_WRAP <- 45
BASE_FONT_SIZE <- 11

# Utils
source("utils.R")

# Read data
ae_impacts <- readRDS("data/ae_impacts.RDS")
region_plot <- readRDS("data/region_plot.RDS")
cluster_shp <- readRDS("data/cluster_shp_simple.RDS")

ae_impacts$period <- as.Date(ae_impacts$period)
available_dates <- sort(unique(ae_impacts$period))
min_date <- min(available_dates, na.rm = TRUE)
max_date <- max(available_dates, na.rm = TRUE)

ui <- page_navbar(
  title = "NHS National A&E Delay-Related Impacts Dashboard",
  theme = bs_theme(version = 5, bg = "#ffffff", fg = "#333333", primary = "#003087"),
  
  tags$head(
    tags$style(HTML("
      .container-fluid { padding: 0.5rem 1rem !important; }
      .custom-plot-block { display: flex; flex-direction: column; padding: 0 !important; }
      .shiny-input-container { margin-bottom: 0.25rem !important; width: 100% !important; }
      .shiny-input-container label { font-weight: 600; margin-bottom: 0.2rem; font-size: 11px; color: #666; text-transform: uppercase; letter-spacing: 0.5px; }
      .girafe_container_std { width: 100% !important; margin: 0 !important; padding: 0 !important; }
      .navbar { padding-top: 0.5rem !important; padding-bottom: 0.5rem !important; border-bottom: 1px solid #e9ecef !important; margin-bottom: 0.5rem !important; }
    "))
  ),
  
  div(class = "container-fluid",
    div(class = "row gx-4",
      
      div(class = "col-md-4 custom-plot-block",
        # Custom Month-Only Range Picker
        airDatepickerInput(
          inputId = "ts_date_range",
          label = "Select Trend Window:",
          value = c(max_date - months(6), max_date),
          minDate = min_date,
          maxDate = max_date,
          range = TRUE,          # Allows picking a start and end point
          view = "months",       # Opens directly to month selection matrix
          minView = "months",    # Disables drilling down to individual days
          dateFormat = "yyyy-MM" # Formats display cleanly stringwise
        ),
        girafeOutput("time_series_plot", height = "auto")
      ),
      
      div(class = "col-md-4 custom-plot-block",
        selectInput(
          inputId = "cluster_date",
          label = "Select Map Target Month:",
          choices = format(available_dates, "%B %Y"),
          selected = format(max_date, "%B %Y")
        ),
        girafeOutput("choropleth", height = "auto")
      ),
      
      div(class = "col-md-4 custom-plot-block",
        selectInput(
          inputId = "trust_date",
          label = "Select Funnel Target Month:",
          choices = format(available_dates, "%B %Y"),
          selected = format(max_date, "%B %Y")
        ),
        girafeOutput("funnel_plot", height = "auto")
      )
    )
  )
)

server <- function(input, output, session) {
  tooltip_css <- "background-color:white;color:black;padding:8px 12px;border-radius:4px;font-family:Inter,sans-serif;font-size:12px;box-shadow:0 2px 8px rgba(0,0,0,0.15);border:1px solid #e9ecef;"

  target_month_cluster <- reactive({
    req(input$cluster_date) 
    as.Date(paste("01", input$cluster_date), format = "%d %B %Y")
  })

  target_month_trust <- reactive({
    req(input$trust_date) 
    as.Date(paste("01", input$trust_date), format = "%d %B %Y")
  })

  # 1. Time Series
  output$time_series_plot <- renderGirafe({
    req(input$ts_date_range)
    
    # airDatepicker outputs strings/dates natively, extract bounding items safely
    start_dt <- as.Date(input$ts_date_range[1])
    end_dt <- if(length(input$ts_date_range) > 1) as.Date(input$ts_date_range[2]) else start_dt

    filtered_ts_data <- ae_impacts %>%
      filter(period >= start_dt & period <= end_dt)

    p <- time_series_plot(filtered_ts_data, region_plot, BASE_FONT_SIZE, PLOT_TITLE_WRAP)
    girafe(
      ggobj = p,
      options = list(
        opts_hover(css = "opacity:1.0; stroke-width:3px;"),
        opts_tooltip(css = tooltip_css),
        opts_hover_inv(css = "opacity:0.1;"),
        opts_toolbar(saveaspng = FALSE),
        opts_sizing(rescale = TRUE, width = 1)
      ),
      width_svg = 6.0, height_svg = 5.0
    )
  })

  # 2. Choropleth Map
  output$choropleth <- renderGirafe({
    filtered_map_data <- ae_impacts %>% filter(period == target_month_cluster())
    p <- choropleth_plot(filtered_map_data, cluster_shp, BASE_FONT_SIZE, PLOT_TITLE_WRAP)
    girafe(
      ggobj = p,
      options = list(
        opts_hover(css = "stroke-width:1.5px; stroke:white;"),
        opts_hover_inv(css = "opacity:0.2;"),
        opts_tooltip(css = tooltip_css),
        opts_toolbar(saveaspng = FALSE),
        opts_sizing(rescale = TRUE, width = 1)
      ),
      width_svg = 6.0, height_svg = 5.0
    )
  })

  # 3. Funnel Plot
  output$funnel_plot <- renderGirafe({
    filtered_funnel_data <- ae_impacts %>% filter(period == target_month_trust())
    p <- funnel_plot(filtered_funnel_data, BASE_FONT_SIZE, PLOT_TITLE_WRAP)
    girafe(
      ggobj = p,
      options = list(
        opts_hover(css = "stroke-width:1.5px; stroke:white;"),
        opts_hover_inv(css = "opacity:0.2;"),
        opts_tooltip(css = tooltip_css),
        opts_toolbar(saveaspng = FALSE),
        opts_sizing(rescale = TRUE, width = 1) 
      ),
      width_svg = 6.0, height_svg = 5.0   
    )
  })
}

shinyApp(ui, server)