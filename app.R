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

ui <- page_sidebar(
  title = "NHS National A&E Delay-Related Impacts Dashboard",
  
  theme = bs_theme(
    version = 5,
    bg = "#ffffff",
    fg = "#333333",
    primary = "#003087",
    base_font = font_google("Inter", wght = c(400, 500))
  ),
  
  tags$head(
    tags$style(HTML("
      /* Sidebar styling */
      .bslib-sidebar-layout > .sidebar {
        background-color: #f8f9fa !important;
        border-right: 1px solid #e9ecef !important;
      }
      
      /* Remove all card styling - completely flat look */
      .card {
        border: none !important;
        box-shadow: none !important;
        background: transparent !important;
        border-radius: 0 !important;
        margin-bottom: 0 !important;
      }
      .card-body {
        padding: 0 !important;
        display: flex;
        flex-direction: column;
      }
      
      /* Tighten the main content area */
      .bslib-sidebar-layout > .main {
        padding: 1rem !important;
      }
      
      /* Fine tune space below the chart, above the input label */
      .shiny-input-container {
        margin-top: -0.25rem !important;
        padding-top: 0 !important;
      }
      
      /* Clean up selector labels */
      .shiny-input-container label { 
        font-weight: 600;
        margin-bottom: 0.25rem;
        font-size: 12px;
        color: #555;
      }
      
      /* Ensure girafe containers fill up maximum available panel space */
      .girafe_container_std { 
        width: 100% !important; 
      }
    "))
  ),
  
  sidebar = sidebar(
    width = 220,
    bg = "#f8f9fa",
    padding = "1rem",
    p("Filter controls have been integrated inside the charts for context-driven adjustments.", style = "color: #666; font-size: 13px;")
  ),
  
  # Replacing layout_column_wrap with explicit column sizing grids
  layout_columns(
    col_widths = c(4, 4, 4), # Split screen into equal 1/3 layout fractions
    gap = "1.5rem",
    
    card(
      girafeOutput("time_series_plot", height = "440px"),
      dateRangeInput(
        inputId = "ts_date_range",
        label = "Select Trend Window:",
        start = max_date - months(6), 
        end = max_date,
        min = min_date,
        max = max_date,
        format = "yyyy-mm",
        startview = "year"
      )
    ),
    card(
      girafeOutput("choropleth", height = "440px"),
      selectInput(
        inputId = "cluster_date",
        label = "Select Map Target Month:",
        choices = format(available_dates, "%B %Y"),
        selected = format(max_date, "%B %Y")
      )
    ),
    card(
      girafeOutput("funnel_plot", height = "440px"),
      selectInput(
        inputId = "trust_date",
        label = "Select Funnel Target Month:",
        choices = format(available_dates, "%B %Y"),
        selected = format(max_date, "%B %Y")
      )
    )
  )
)

server <- function(input, output, session) {
  
  tooltip_css <- "background-color:white;color:black;padding:8px 12px;border-radius:4px;font-family:Inter,sans-serif;font-size:12px;box-shadow:0 2px 8px rgba(0,0,0,0.15);border:1px solid #e9ecef;"

  # FIXED: Pointed reactive dependencies directly to your updated UI input IDs
  target_month_cluster <- reactive({
    req(input$cluster_date) 
    as.Date(paste("01", input$cluster_date), format = "%d %B %Y")
  })

  # FIXED: Pointed reactive dependencies directly to your updated UI input IDs
  target_month_trust <- reactive({
    req(input$trust_date) 
    as.Date(paste("01", input$trust_date), format = "%d %B %Y")
  })

  # 1. Time Series
  output$time_series_plot <- renderGirafe({
    req(input$ts_date_range)
    
    filtered_ts_data <- ae_impacts %>%
      filter(period >= input$ts_date_range[1] & period <= input$ts_date_range[2])

    p <- time_series_plot(filtered_ts_data, region_plot, BASE_FONT_SIZE, PLOT_TITLE_WRAP)
    girafe(
      ggobj = p,
      options = list(
        opts_hover(css = "opacity:1.0; fill-opacity:1.0; stroke-width:3px; transition: all 0.2s ease-in-out;"),
        opts_tooltip(css = tooltip_css),
        opts_hover_inv(css = "opacity:0.1;"),
        opts_toolbar(saveaspng = FALSE),
        opts_selection(type = "none"),
        opts_sizing(rescale = TRUE, width = 1)
      ),
      width_svg = 6.0,
      height_svg = 5.5
    )
  })

  # 2. Choropleth Map
  output$choropleth <- renderGirafe({
    filtered_map_data <- ae_impacts %>%
      filter(period == target_month_cluster())

    p <- choropleth_plot(filtered_map_data, cluster_shp, BASE_FONT_SIZE, PLOT_TITLE_WRAP)
    girafe(
      ggobj = p,
      options = list(
        opts_hover(css = "stroke-width:1.5px; stroke:white;"),
        opts_hover_inv(css = "opacity:0.2; transition: opacity 0.3s;"),
        opts_tooltip(css = tooltip_css),
        opts_toolbar(saveaspng = FALSE),
        opts_selection(type = "none"),
        opts_sizing(rescale = TRUE, width = 1)
      ),
      width_svg = 6.0,
      height_svg = 5.5
    )
  })

  # 3. Funnel Plot
  output$funnel_plot <- renderGirafe({
    filtered_funnel_data <- ae_impacts %>%
      filter(period == target_month_trust())

    p <- funnel_plot(filtered_funnel_data, BASE_FONT_SIZE, PLOT_TITLE_WRAP)
    girafe(
      ggobj = p,
      options = list(
        opts_hover(css = "stroke-width:1.5px; stroke:white;"),
        opts_hover_inv(css = "opacity:0.2; transition: opacity 0.3s;"),
        opts_tooltip(css = tooltip_css),
        opts_toolbar(saveaspng = FALSE),
        opts_selection(type = "none"),
        opts_sizing(rescale = TRUE, width = 1) 
      ),
      width_svg = 6.0, 
      height_svg = 5.5   
    )
  })
}

shinyApp(ui, server)