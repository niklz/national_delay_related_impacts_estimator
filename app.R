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
# Impacts
ae_impacts <- readRDS("data/ae_impacts.RDS")
# Shape files
region_plot <- readRDS("data/region_plot.RDS")
cluster_shp <- readRDS("data/cluster_shp_simple.RDS")




ui <- page_sidebar(
  options = list(shiny.busyIndicators = FALSE),

  title = "NHS National A&E Delay-Related Impacts Dashboard",

  theme = bs_theme(
    version = 5,
    bg = "#ffffff",
    fg = "#333333",
    primary = "#003087",
    base_font = font_google("Inter", wght = c(400, 500))
  ),

  tags$head(
    tags$style(HTML(
      "
      /* Sidebar */
      .bslib-sidebar-layout > .sidebar {
        background-color: #f4f4f4 !important;
        border-right: 1px solid #e9ecef !important;
      }

      /* Flat, borderless cards */
      .no-border {
        border: none !important;
        box-shadow: none !important;
      }
      .card {
        border: 0.5px solid #e0e0e0 !important;
        box-shadow: none !important;
        border-radius: 8px !important;
      }

      /* Tidy up radio buttons in sidebar */
      .shiny-input-container label { font-weight: 500; }
      .radio label { font-size: 14px; color: #333; }

      /* girafe fills its container */
      .girafe_container_std { width: 100% !important; }
      
      $(document).on('shiny:busy', function(e) { e.preventDefault(); });
    "
    )),
  ),

  sidebar = sidebar(
    width = 250,
    bg = "#f4f4f4",
    padding = "20px"
  ),


  layout_column_wrap(
    width = 1/3,
    card(
      girafeOutput("time_series_plot", height = "400px")
      # card_header(""),
    ),
    card(
      # card_header(""),
      girafeOutput("choropleth", height = "400px")
    ),
    card(
      # card_header(""),
      girafeOutput("funnel_plot", height = "400px")
    )
  )
)


server <- function(input, output, session) {
  output$funnel_plot <- renderGirafe({
    p <- funnel_plot(ae_impacts)
    girafe(
      ggobj = p,
      options = list(
        # Keep the hovered item normal
        opts_hover(css = "stroke-width:1.5px; stroke:white;"),
        # Fade everything else out (lower opacity)
        opts_hover_inv(css = "opacity:0.2; transition: opacity 0.3s;"),
        opts_tooltip(
          css = "background-color:white;color:black;padding:5px;border-radius:5px;font-family:sans-serif;"
        ),
        opts_toolbar(saveaspng = FALSE),
        opts_selection(
          css = NULL,
          type = c("none"),
          only_shiny = TRUE,
          selected = character(0),
          linked = FALSE
        )
      ),
      width_svg = 9,
      height_svg = 7
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
        # This CSS makes the hovered region/line fully opaque and "pops" it
        opts_hover(
          css = "opacity:1.0; fill-opacity:1.0; stroke-width:3px; transition: all 0.3s ease-in-out;"
        ),
        opts_tooltip(
          css = "background-color:white;color:black;padding:5px;border-radius:5px;font-family:sans-serif;"
        ),
        # This dims everything else so the hovered region stands out
        opts_hover_inv(css = "opacity:0.05;"),
        opts_toolbar(saveaspng = FALSE),
        opts_selection(
          css = NULL,
          type = c("none"),
          only_shiny = TRUE,
          selected = character(0),
          linked = FALSE
        )
      ),
      width_svg = 9,
      height_svg = 7
    )
  })

  output$choropleth <- renderGirafe({
    p <- choropleth_plot(ae_impacts, cluster_shp)
    girafe(
      ggobj = p,
      options = list(
        # Keep the hovered item normal
        opts_hover(css = "stroke-width:1.5px; stroke:white;"),
        # Fade everything else out (lower opacity)
        opts_hover_inv(css = "opacity:0.2; transition: opacity 0.3s;"),
        opts_tooltip(
          css = "background-color:white;color:black;padding:5px;border-radius:5px;font-family:sans-serif;"
        ),
        opts_toolbar(saveaspng = FALSE),
        opts_selection(
          css = NULL,
          type = c("none"),
          only_shiny = TRUE,
          selected = character(0),
          linked = FALSE
        )
      ),
      width_svg = 9,
      height_svg = 7
    )
  })
}

shinyApp(ui, server)