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
PLOT_TITLE_WRAP <- 50
BASE_FONT_SIZE <- 14

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
      
      /* Remove all card styling - completely flat look */
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
        padding: 0.75rem !important;
      }
      
      /* Bring the columns closer together */
      .bslib-grid {
        gap: 1rem !important;
      }
      
      /* Ensure girafe containers aggressively scale to container width */
      .girafe_container_std { 
        width: 100% !important; 
        height: 100% !important;
      }
      
      /* Clean up sidebar inputs */
      .shiny-input-container label { 
        font-weight: 500; 
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
  
  # Keeping the strict single-row layout requested
  layout_column_wrap(
    width = 1/3,
    heights_equal = "row",
    
    card(
      girafeOutput("time_series_plot", height = "100%")
    ),
    card(
      girafeOutput("choropleth", height = "100%")
    ),
    card(
      girafeOutput("funnel_plot", height = "100%")
    )
  )
)

server <- function(input, output, session) {
  
  # Shared tooltip CSS styling
  tooltip_css <- "background-color:white;color:black;padding:8px 12px;border-radius:4px;font-family:Inter,sans-serif;font-size:12px;box-shadow:0 2px 8px rgba(0,0,0,0.15);border:1px solid #e9ecef;"

  output$funnel_plot <- renderGirafe({
    p <- funnel_plot(ae_impacts, BASE_FONT_SIZE, PLOT_TITLE_WRAP)
    girafe(
      ggobj = p,
      options = list(
        opts_hover(css = "stroke-width:1.5px; stroke:white;"),
        opts_hover_inv(css = "opacity:0.2; transition: opacity 0.3s;"),
        opts_tooltip(css = tooltip_css),
        opts_toolbar(saveaspng = FALSE),
        opts_selection(type = "none"),
        # Forces the SVG to flex naturally into changing screen ratios
        opts_sizing(rescale = TRUE, width = 1) 
      ),
      width_svg = 7.5,  # Tighter width to account for narrow dashboard column blocks
      height_svg = 6.0   # Tighter aspect ratio stops horizontal squishing
    )
  })
  
  output$time_series_plot <- renderGirafe({
    # Fixed: Removed the repetitive inset_element call here. 
    # the function time_series_plot already includes this step natively.
    p <- time_series_plot(ae_impacts, region_plot, BASE_FONT_SIZE, PLOT_TITLE_WRAP)
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
      width_svg = 7.5,
      height_svg = 6.0
    )
  })
  
  output$choropleth <- renderGirafe({
    p <- choropleth_plot(ae_impacts, cluster_shp, BASE_FONT_SIZE, PLOT_TITLE_WRAP)
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
      width_svg = 7.5,
      height_svg = 6.0
    )
  })
}

shinyApp(ui, server)