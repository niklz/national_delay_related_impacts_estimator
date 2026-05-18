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

# Utils
source("utils.R")

# Read data
ae_impacts <- readRDS("data/ae_impacts.RDS")
region_plot <- readRDS("data/region_plot.RDS")
cluster_shp <- readRDS("data/cluster_shp_simple.RDS")

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
      
      /* Remove all card styling - make them invisible containers */
      .card {
        border: none !important;
        box-shadow: none !important;
        background: transparent !important;
        border-radius: 0 !important;
      }
      .card-body {
        padding: 0 !important;
      }
      
      /* Tighten the main content area */
      .bslib-sidebar-layout > .main {
        padding: 0.5rem !important;
      }
      
      /* Reduce gap between plot columns */
      .bslib-grid {
        gap: 0.5rem !important;
      }
      
      /* Ensure girafe plots fill space */
      .girafe_container_std { 
        width: 100% !important; 
      }
      
      /* Clean up sidebar inputs */
      .shiny-input-container label { 
        font-weight: 500; 
      }
      .radio label { 
        font-size: 14px; 
        color: #333; 
      }
      
      /* Title bar cleanup */
      .navbar {
        padding-top: 0.5rem !important;
        padding-bottom: 0.5rem !important;
      }
    "))
  ),
  
  sidebar = sidebar(
    width = 250,
    bg = "#f8f9fa",
    padding = "1rem"
  ),
  
  layout_column_wrap(
    width = 1/3,
    gap = "0.5rem",
    heights_equal = "row",
    girafeOutput("time_series_plot", height = "450px"),
    girafeOutput("choropleth", height = "450px"),
    girafeOutput("funnel_plot", height = "450px")
  )
)

server <- function(input, output, session) {
  output$funnel_plot <- renderGirafe({
    p <- funnel_plot(ae_impacts)
    girafe(
      ggobj = p,
      options = list(
        opts_hover(css = "stroke-width:1.5px; stroke:white;"),
        opts_hover_inv(css = "opacity:0.2; transition: opacity 0.3s;"),
        opts_tooltip(
          css = "background-color:white;color:black;padding:8px 12px;border-radius:4px;font-family:Inter,sans-serif;font-size:13px;box-shadow:0 2px 8px rgba(0,0,0,0.15);"
        ),
        opts_toolbar(saveaspng = FALSE),
        opts_selection(type = "none")
      ),
      width_svg = 8,
      height_svg = 6
    )
  })
  
  output$time_series_plot <- renderGirafe({
    p <- time_series_plot(ae_impacts, region_plot)
    girafe(
      ggobj = p,
      inset_element(
        region_plot,
        on_top = FALSE,
        left = -0.15,
        bottom = 0,
        right = 0.9,
        top = 1
      ),
      options = list(
        opts_hover(
          css = "opacity:1.0; fill-opacity:1.0; stroke-width:3px; transition: all 0.3s ease-in-out;"
        ),
        opts_tooltip(
          css = "background-color:white;color:black;padding:8px 12px;border-radius:4px;font-family:Inter,sans-serif;font-size:13px;box-shadow:0 2px 8px rgba(0,0,0,0.15);"
        ),
        opts_hover_inv(css = "opacity:0.05;"),
        opts_toolbar(saveaspng = FALSE),
        opts_selection(type = "none")
      ),
      width_svg = 8,
      height_svg = 6
    )
  })
  
  output$choropleth <- renderGirafe({
    p <- choropleth_plot(ae_impacts, cluster_shp)
    girafe(
      ggobj = p,
      options = list(
        opts_hover(css = "stroke-width:1.5px; stroke:white;"),
        opts_hover_inv(css = "opacity:0.2; transition: opacity 0.3s;"),
        opts_tooltip(
          css = "background-color:white;color:black;padding:8px 12px;border-radius:4px;font-family:Inter,sans-serif;font-size:13px;box-shadow:0 2px 8px rgba(0,0,0,0.15);"
        ),
        opts_toolbar(saveaspng = FALSE),
        opts_selection(type = "none")
      ),
      width_svg = 8,
      height_svg = 6
    )
  })
}

shinyApp(ui, server)
