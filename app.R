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
library(shinycssloaders)
library(shinyWidgets)
require(rmapshaper)
require(ggrepel)

# UI params
PLOT_TITLE_WRAP <- 65
BASE_FONT_SIZE <- 10
SPINNER_TYPE <- 8

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
over_dispersion <- 3
sigmas_global <- seq(0.5, 3.0, by = 0.5)
x_grid_max <- max(ae_impacts$tot_ae_adm, na.rm = TRUE) * 1.5

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
      /* Main Canvas Area Spacing */
      .container-fluid { padding: 0.5rem 1rem !important; }
      .navbar { padding-top: 0.5rem !important; padding-bottom: 0.5rem !important; border-bottom: 1px solid #e9ecef !important; margin-bottom: 0.5rem !important; }
      
      /* Dashboard Component Cards */
      .column-card {
        border: 1px solid #e9ecef !important;
        border-radius: 8px !important;
        background-color: #ffffff !important;
        padding: 16px !important;
        height: 100% !important;
        box-shadow: 0 1px 3px rgba(0,0,0,0.03) !important;
        display: flex !important;
        flex-direction: column !important;
        overflow: visible !important;
      }

      /* Relaxed Control Block Wrapper - Clears space for labels naturally */
      .card-header-control-block {
        min-height: 75px !important;
        display: flex !important;
        align-items: flex-end !important;
        margin-bottom: 15px !important;
        width: 100% !important;
        overflow: visible !important;
      }

      /* Form Elements Global Styling Overrides */
      .shiny-input-container { margin-bottom: 0 !important; width: 100% !important; }
      .shiny-input-container label { font-weight: 600; margin-bottom: 0.4rem; font-size: 11px; color: #666; text-transform: uppercase; letter-spacing: 0.5px; }
      .shiny-spinner-output-container { flex-grow: 1 !important; display: flex !important; flex-direction: column !important; width: 100% !important; }
      
      .girafe_container_std { width: 100% !important; max-width: 100% !important; }
      .datepicker--addon { display: none !important; }

      .vscomp-toggle-button, 
      input.airDatepickerInput {
        border: 1px solid #ced4da !important;
        border-radius: 4px !important;
        height: 38px !important;
      }

      .vscomp-wrapper:focus .vscomp-toggle-button,
      input.airDatepickerInput:focus {
        border-color: #003087 !important;
        box-shadow: 0 0 0 0.2rem rgba(0, 48, 135, 0.25) !important;
      }

      /* Dropdown text wrapping guard */
      .vscomp-wrapper { max-width: 100% !important; }
      .vscomp-value-tag {
        max-width: 110px !important;
        overflow: hidden !important;
        text-overflow: ellipsis !important;
        white-space: nowrap !important;
      }

      /* Custom Form Switch Overrides */
      .funnel-switch-container {
        display: flex !important;
        align-items: center !important;
        height: 38px;
        padding-left: 0px !important;
        flex-shrink: 0;
      }
      .funnel-switch-container .form-check {
        padding-left: 2.3em !important;
        margin: 0 !important;
        display: flex !important;
        align-items: center !important;
      }
      .funnel-switch-container .form-check-input {
        height: 1.3em !important;
        width: 2.3em !important;
        margin-left: -2.3em !important;
        cursor: pointer !important;
        background-color: #e9ecef;
        border-color: #ced4da;
        background-image: url(\"data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' viewBox='-4 -4 8 8'%3e%3ccircle r='3' fill='%236c757d'/%3e%3c/svg%3e\") !important;
      }
      .funnel-switch-container .form-check-input:checked {
        background-color: #003087 !important;
        border-color: #003087 !important;
        background-image: url(\"data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' viewBox='-4 -4 8 8'%3e%3ccircle r='3' fill='%23fff'/%3e%3c/svg%3e\") !important;
      }
      .funnel-switch-container .form-check-label {
        font-weight: 600 !important;
        font-size: 11px !important;
        color: #666 !important;
        text-transform: uppercase !important;
        letter-spacing: 0.5px !important;
        margin-left: 6px !important;
        cursor: pointer !important;
        white-space: nowrap !important;
      }
      "
    ))
  ),

  div(
    class = "container-fluid",

    # THREE COLUMN GRID SYSTEM
    div(
      class = "row gx-4 mt-2",
      
      # COLUMN 1: Regional Time Series
      div(
        class = "col-md-4",
        div(
          class = "column-card",
          div(
            class = "card-header-control-block",
            sliderInput(
              inputId = "ts_date_slider",
              label = "Select time-series window:",
              min = min_date,
              max = max_date,
              value = c(max_date - months(6), max_date),
              timeFormat = "%Y-%m",
              step = 30.5,
              width = "100%"
            )
          ),
          withSpinner(
            girafeOutput("time_series_plot", height = "auto"),
            type = SPINNER_TYPE, color = "#003087", size = 0.7
          )
        )
      ),

      # COLUMN 2: ICB Cluster Choropleth Map
      div(
        class = "col-md-4",
        div(
          class = "column-card",
          div(
            class = "card-header-control-block",
            airDatepickerInput(
              inputId = "cluster_date",
              label = "Select target month:",
              value = max_date,
              minDate = min_date,
              maxDate = max_date,
              view = "months",
              minView = "months",
              dateFormat = "yyyy MMMM",
              monthsField = "months",
              addon = "none",
              width = "100%"
            )
          ),
          withSpinner(
            girafeOutput("choropleth", height = "auto"),
            type = SPINNER_TYPE, color = "#003087", size = 0.7
          )
        )
      ),

      # COLUMN 3: Funnel Plot per Trust
      div(
        class = "col-md-4",
        div(
          class = "column-card",
          div(
            class = "card-header-control-block",
            style = "gap: 8px;",
            
            # Allocation A: Month Picker
            div(
              style = "flex: 0 1 32%; min-width: 0;",
              airDatepickerInput(
                inputId = "trust_date",
                label = "Select target month:",
                value = max_date,
                minDate = min_date,
                maxDate = max_date,
                view = "months",
                minView = "months",
                dateFormat = "yyyy MMMM",
                monthsField = "months",
                addon = "none",
                width = "100%"
              )
            ),
            
            # Allocation B: Virtual Search Input Field
            div(
              style = "flex: 1 1 44%; min-width: 0;",
              shinyWidgets::virtualSelectInput(
                inputId = "highlighted_trusts",
                label = "Highlight Trust(s):",
                choices = NULL,
                multiple = TRUE,
                search = TRUE,
                placeholder = "Search...",
                width = "100%"
              )
            ),
            
            # Allocation C: Axis Toggle Switch
            div(
              class = "funnel-switch-container",
              style = "flex: 0 0 auto;",
              div(
                class = "form-check form-switch",
                tags$input(class = "form-check-input", type = "checkbox", id = "log_x"),
                tags$label(class = "form-check-label", `for` = "log_x", "Log X")
              )
            )
          ),
          withSpinner(
            girafeOutput("funnel_plot", height = "auto"),
            type = SPINNER_TYPE, color = "#003087", size = 0.7
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  tooltip_css <- "background-color:white;color:black;padding:8px 12px;border-radius:4px;font-family:Inter,sans-serif;font-size:1rem;box-shadow:0 2px 8px rgba(0,0,0,0.15);border:1px solid #e9ecef;"

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
    req(input$ts_date_slider)
    start_dt <- floor_date(as.Date(input$ts_date_slider[1]), "month")
    end_dt <- floor_date(as.Date(input$ts_date_slider[2]), "month")

    filtered_ts_data <- ae_impacts %>% filter(period >= start_dt & period <= end_dt)

    p <- time_series_plot(filtered_ts_data, region_plot, BASE_FONT_SIZE, PLOT_TITLE_WRAP)
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
        opts_selection(type = "none"),
        opts_sizing(rescale = TRUE, width = 1)
      ),
      width_svg = 6.0, height_svg = 5.0
    )
  })

  # 3. Funnel Plot Dropdown Dynamic Syncing
  observe({
    req(target_month_trust())
    available_trusts <- ae_impacts %>%
      filter(period == target_month_trust(), ae_type == "Type 1 (Major)", org != "Total") %>%
      filter(!is.na(excess_mort), !is.na(tot_ae_adm), !is.na(org)) %>%
      filter(tot_ae_adm > 0, excess_mort <= tot_ae_adm) %>%
      pull(org) %>% unique() %>% sort()

    updateVirtualSelect("highlighted_trusts", choices = available_trusts, session = session)
  })

  funnel_cache <- reactive({
    req(target_month_trust())
    filtered_funnel_data <- ae_impacts %>% filter(period == target_month_trust())

    p_linear <- funnel_plot(filtered_funnel_data, BASE_FONT_SIZE, PLOT_TITLE_WRAP, log_x = FALSE, selected_trusts = input$highlighted_trusts)
    p_log <- funnel_plot(filtered_funnel_data, BASE_FONT_SIZE, PLOT_TITLE_WRAP, log_x = TRUE, selected_trusts = input$highlighted_trusts)

    list(linear = p_linear, log = p_log)
  })

  output$funnel_plot <- renderGirafe({
    req(funnel_cache())
    is_log <- !is.null(input$log_x) && isTRUE(input$log_x)
    selected_plot <- if (is_log) funnel_cache()$log else funnel_cache()$linear

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
      width_svg = 6.0, height_svg = 5.0
    )
  })
}

shinyApp(ui, server)