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
library(shinycssloaders)

# UI params
PLOT_TITLE_WRAP <- 65
BASE_FONT_SIZE <- 11

# Utils
source("utils.R")

# Read data
ae_impacts <- read_csv(
  "https://raw.githubusercontent.com/niklz/excess_impacts_national/refs/heads/main/data/ae_impacts.csv"
)
region_plot <- readRDS("data/region_plot.RDS")
cluster_shp <- readRDS("data/cluster_shp_simple.RDS")

ae_impacts$period <- as.Date(ae_impacts$period)
available_dates <- sort(unique(ae_impacts$period))
min_date <- min(available_dates, na.rm = TRUE)
max_date <- max(available_dates, na.rm = TRUE)

# ==============================================================================
# PERF FIX: PRE-COMPUTE MATHEMATICAL GRID MATRIX ONCE
# ==============================================================================
# Generate a definitive lookup framework covering every conceivable hospital size
over_dispersion <- 3
sigmas_global <- seq(0.5, 3.0, by = 0.5)
x_grid_max <- max(ae_impacts$tot_ae_adm, na.rm = TRUE) * 1.5

# Calculate an overarching global national average baseline
mu_global <- sum(ae_impacts$excess_mort, na.rm = TRUE) /
  sum(ae_impacts$tot_ae_adm, na.rm = TRUE)

funnel_base_grid <- tibble(
  tot_ae_adm = seq(10, x_grid_max, length.out = 600)
) %>%
  mutate(
    logit_mu = log(mu_global / (1 - mu_global)),
    logit_se = sqrt(over_dispersion) *
      sqrt(1 / (tot_ae_adm * mu_global * (1 - mu_global)))
  )

# Reshape into ready-to-layer lines and ribbons frames
global_funnel_lines <- purrr::map_df(sigmas_global, function(z) {
  tibble(
    tot_ae_adm = funnel_base_grid$tot_ae_adm,
    upper = 1 /
      (1 + exp(-(funnel_base_grid$logit_mu + z * funnel_base_grid$logit_se))),
    sigma = as.character(z),
    z_val = z
  )
})



global_funnel_ribbons <- tibble()
if (length(sigmas_global) >= 2) {
  stripe_indices <- seq(1, length(sigmas_global) - 1, by = 2)
  global_funnel_ribbons <- purrr::map_df(stripe_indices, function(i) {
    z_lower <- sigmas_global[i]
    z_upper <- sigmas_global[i + 1]
    tibble(
      tot_ae_adm = funnel_base_grid$tot_ae_adm,
      ymin = 1 /
        (1 +
          exp(
            -(funnel_base_grid$logit_mu + z_lower * funnel_base_grid$logit_se)
          )),
      ymax = 1 /
        (1 +
          exp(
            -(funnel_base_grid$logit_mu + z_upper * funnel_base_grid$logit_se)
          )),
      group_id = paste0(z_lower, "-", z_upper)
    )
  })
}
# ==============================================================================

ui <- page_navbar(
  title = tags$p(
    style = "margin: 0; padding-top: 0.25rem; font-size: 24px; color: #000000; max-width: 900px; line-height: 1.4;",
    "NHS National A&E Delay-Related Impacts Dashboard"
  ),

  header = tags$p(
    style = "margin: 0; padding-top: 0.25rem; font-size: 18px; color: #555; max-width: 1080px; line-height: 1.4;",
    "This dashboard displays estimated excess deaths associated with prolonged waits for A&E admission, ",
    "applying the risk associations established in ",
    tags$a(
      href = "https://doi.org/10.1136/emermed-2025-214983",
      target = "_blank",
      "Howlett et al.",
      style = "color: #003087; text-decoration: underline;"
    ),
    ". Waiting times and admission volumes data are sourced directly from ",
    tags$a(
      href = "https://www.england.nhs.uk/statistics/statistical-work-areas/ae-waiting-times-and-activity/",
      target = "_blank",
      "NHS England Statistics",
      style = "color: #003087; text-decoration: underline;"
    ),
    ". All metrics are expressed as rates per 1,000 Type-1 A&E admissions."
  ),

  theme = bs_theme(
    version = 5,
    bg = "#ffffff",
    fg = "#333333",
    primary = "#003087"
  ),

  tags$head(
    tags$style(HTML(
      "
      .container-fluid { padding: 0.5rem 1rem !important; }
      .custom-plot-block {
        display: flex !important;
        flex-direction: column !important;
        justify-content: space-between !important;
        height: 100% !important;
      }
      .custom-plot-block .shiny-html-output {
        display: flex !important;
        flex-direction: column !important;
        justify-content: space-between !important;
        flex-grow: 1 !important;
      }
      .girafe_container_std {
        display: flex !important;
        flex-direction: column !important;
        justify-content: space-between !important;
        height: 100% !important;
        flex-grow: 1 !important;
      }
      #ts_date_range { max-width: 290px !important; }
      #cluster_date, #trust_date { max-width: 180px !important; }
      .shiny-input-container { margin-bottom: 0.25rem !important; width: 100% !important; }
      .shiny-input-container label { font-weight: 600; margin-bottom: 0.2rem; font-size: 11px; color: #666; text-transform: uppercase; letter-spacing: 0.5px; }
      .girafe_container_std { width: 100% !important; margin: 0 !important; padding: 0 !important; }
      .navbar { padding-top: 0.5rem !important; padding-bottom: 0.5rem !important; border-bottom: 1px solid #e9ecef !important; margin-bottom: 0.5rem !important; }
      
      .funnel-control-header {
        display: flex !important;
        align-items: flex-end !important;
        justify-content: space-between !important;
        gap: 15px;
        width: 100%;
      }
      .funnel-control-header .shiny-input-container { margin-bottom: 0 !important; }

      /* Fixes for prettySwitch visual alignment bugs */
      .pretty input:checked~.state.p-primary label:after {
        background-color: #003087 !important;
      }
      .pretty.p-switch .state label:before {
        border-radius: 60px !important;
        background-color: #e9ecef !important;
        border-color: #ced4da !important;
        top: 0px !important;
        height: 22px !important;
        width: 44px !important;
      }
      .pretty.p-switch .state label:after {
        border-radius: 50% !important;
        background-color: #fff !important;
        top: 3px !important;
        left: 3px !important;
        height: 16px !important;
        width: 16px !important;
        box-shadow: 0 1px 3px rgba(0,0,0,0.4);
        transition: left 0.25s ease;
      }
      .pretty.p-switch input:checked~.state label:after {
        left: 25px !important;
      }
      .pretty .state label {
        text-indent: 52px !important;
        font-weight: 600 !important;
        font-size: 11px !important;
        color: #666 !important;
        text-transform: uppercase !important;
        letter-spacing: 0.5px !important;
        line-height: 22px !important;
      }
      /* Soft loading wrapper overlay adjustment styles */
      .shiny-spinner-output-container {
        flex-grow: 1 !important;
        display: flex !important;
        flex-direction: column !important;
      }
      "
    ))
  ),

  div(
    class = "container-fluid",
    div(
      class = "row gx-4",

      # Column 1: Time Series Trend Window
      div(
        class = "col-md-4 custom-plot-block",
        airDatepickerInput(
          inputId = "ts_date_range",
          label = "Select Trend Window:",
          value = c(max_date - months(6), max_date),
          minDate = min_date,
          maxDate = max_date,
          range = TRUE,
          view = "months",
          minView = "months",
          dateFormat = "yyyy MMMM",
          monthsField = "months"
        ),
        # Wrapped with shinycssloaders
        withSpinner(
          girafeOutput("time_series_plot", height = "auto"),
          type = 8, color = "#003087", size = 0.7
        )
      ),

      # Column 2: Choropleth Map Snapshot Selector
      div(
        class = "col-md-4 custom-plot-block",
        airDatepickerInput(
          inputId = "cluster_date",
          label = "Select Map Target Month:",
          value = max_date,
          minDate = min_date,
          maxDate = max_date,
          view = "months",
          minView = "months",
          dateFormat = "yyyy MMMM",
          monthsField = "months"
        ),
        # Wrapped with shinycssloaders
        withSpinner(
          girafeOutput("choropleth", height = "auto"),
          type = 8, color = "#003087", size = 0.7
        )
      ),

      # Column 3: Funnel Plot
      div(
        class = "col-md-4 custom-plot-block",
        div(
          class = "funnel-control-header d-flex align-items-end justify-content-between",
          airDatepickerInput(
            inputId = "trust_date",
            label = "Select Funnel Target Month:",
            value = max_date,
            minDate = min_date,
            maxDate = max_date,
            view = "months",
            minView = "months",
            dateFormat = "yyyy MMMM",
            monthsField = "months"
          ),
          div(
            style = "margin-bottom: 6px; padding-right: 5px; height: 22px;", 
            prettySwitch(
              inputId = "log_x",
              label = "Log X-Axis",
              value = FALSE,
              fill = TRUE,
              inline = TRUE,
              status = "primary"
            )
          )
        ),
        # Wrapped with shinycssloaders
        withSpinner(
          girafeOutput("funnel_plot", height = "auto"),
          type = 8, color = "#003087", size = 0.7
        )
      )
    )
  )
)


server <- function(input, output, session) {
  tooltip_css <- "background-color:white;color:black;padding:8px 12px;border-radius:4px;font-family:Inter,sans-serif;font-size:12px;box-shadow:0 2px 8px rgba(0,0,0,0.15);border:1px solid #e9ecef;"

  target_month_cluster <- reactive({
    req(input$cluster_date)
    as.Date(input$cluster_date)
  })

  target_month_trust <- reactive({
    req(input$trust_date)
    as.Date(input$trust_date)
  })

  # 1. Time Series
  output$time_series_plot <- renderGirafe({
    req(input$ts_date_range)
    start_dt <- as.Date(input$ts_date_range[1])
    end_dt <- if (length(input$ts_date_range) > 1) {
      as.Date(input$ts_date_range[2])
    } else {
      start_dt
    }

    filtered_ts_data <- ae_impacts %>%
      filter(period >= start_dt & period <= end_dt)

    p <- time_series_plot(
      filtered_ts_data,
      region_plot,
      BASE_FONT_SIZE,
      PLOT_TITLE_WRAP
    )
    girafe(
      ggobj = p,
      options = list(
        opts_hover(css = "opacity:1.0; stroke-width:3px;"),
        opts_tooltip(css = tooltip_css),
        opts_hover_inv(css = "opacity:0.1;"),
        opts_toolbar(saveaspng = FALSE),
        opts_selection(type = "none"),
        opts_sizing(rescale = TRUE, width = 1)
      ),
      width_svg = 6.0,
      height_svg = 5.0
    )
  })

  # 2. Choropleth Map
  output$choropleth <- renderGirafe({
    filtered_map_data <- ae_impacts %>% filter(period == target_month_cluster())
    p <- choropleth_plot(
      filtered_map_data,
      cluster_shp,
      BASE_FONT_SIZE,
      PLOT_TITLE_WRAP
    )
    girafe(
      ggobj = p,
      options = list(
        opts_hover(css = "stroke-width:1.5px; stroke:white;"),
        opts_hover_inv(css = "opacity:0.2;"),
        opts_tooltip(css = tooltip_css),
        opts_toolbar(saveaspng = FALSE),
        opts_selection(type = "none"),
        opts_sizing(rescale = TRUE, width = 1)
      ),
      width_svg = 6.0,
      height_svg = 5.0
    )
  })

  # 3. Funnel Plot
  funnel_cache <- reactive({
    req(target_month_trust())

    filtered_funnel_data <- ae_impacts %>%
      filter(period == target_month_trust())

    p_linear <- funnel_plot(
      filtered_funnel_data,
      BASE_FONT_SIZE,
      PLOT_TITLE_WRAP,
      log_x = FALSE
    )
    p_log <- funnel_plot(
      filtered_funnel_data,
      BASE_FONT_SIZE,
      PLOT_TITLE_WRAP,
      log_x = TRUE
    )

    list(linear = p_linear, log = p_log)
  })

  output$funnel_plot <- renderGirafe({
    req(funnel_cache())

    selected_plot <- if (input$log_x) {
      funnel_cache()$log
    } else {
      funnel_cache()$linear
    }

    girafe(
      ggobj = selected_plot,
      options = list(
        opts_hover(css = "stroke-width:1.5px; stroke:white;"),
        opts_hover_inv(css = "opacity:0.2;"),
        opts_tooltip(css = tooltip_css),
        opts_toolbar(saveaspng = FALSE),
        opts_selection(type = "none"),
        opts_sizing(rescale = TRUE, width = 1)
      ),
      width_svg = 6.0,
      height_svg = 5.0
    )
  })
}

shinyApp(ui, server)