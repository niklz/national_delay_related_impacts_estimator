dashboardUI <- function(id, min_date, max_date, SPINNER_TYPE, title) {
  ns <- NS(id)

  nav_panel(
    title = title,
    icon = icon("pie-chart"),

    card(
      fill = FALSE,
      style = "background-color: #F4F6F9;
         /*border: 1px solid #e2e8f0;*/
         box-shadow: none;",

      card_body(
        fill = FALSE,
        style = "padding: 0.75rem 1rem;", # Snug padding (Top/Bottom, Left/Right)

        tags$p(
          style = "margin: 0; font-size: 16px; color: #334155; line-height: 1.5;",
          "This section displays the ",
          tags$strong("Delay-Related Death"),
          tags$strong(
            " rate per 1,000 emergency admissions via type-1 A&E departments(*)"
          ),
          " at 3 operational levels: a ",
          tags$strong("Regional"),
          " time-series, a geospatial distribution at ",
          tags$strong("ICB / Cluster level"),
          ", and a funnel plot at ",
          tags$strong("Trust level"),
          ". Chart views can be modified using the selectors at the bottom of the page.",
        )
      )
    ),

    layout_columns(
      col_widths = c(4, 4, 4),

      # CARD 1: TIME SERIES
      card(
        full_screen = TRUE,
        card_body(
          class = "d-flex flex-column align-items-stretch content-card-body",
          style = "overflow: hidden !important; padding: 1rem; min-height: 0 !important;",
          div(
            style = "flex: 0 1 auto; width: 100%; aspect-ratio: 6 / 5; overflow: hidden;",
            # DEPRECATED chart title as div element 
          #   h4(
          #     style = "text-align: center !important; font-weight: 600;",
          #     "DRD* rate† per Region"
          #   ),
            withSpinner(
              girafeOutput(
                ns("time_series_plot"),
                width = "100%",
                height = "100%"
              ),
              type = SPINNER_TYPE,
              color = "#003087",
              size = 0.7
            )
          ),
          #   # CAPTION BLOCK
          #   div(
          #     class = "content-caption",
          #     tags$strong("Figure 1: "),
          #     "Regional time-series of delay-related deaths per 1000 admissions."
          #   )
          div(
            class = "slider-breathing-room",
            sliderInput(
              inputId = ns("ts_date_slider"),
              label = "Select time-series window:",
              min = min_date,
              max = max_date,
              value = c(max_date - months(6), max_date),
              timeFormat = "%Y-%m",
              step = 30.5,
              width = "100%"
            )
          )
        )
      ),

      # CARD 2: CHOROPLETH MAP
      card(
        full_screen = TRUE,
        card_body(
          class = "d-flex flex-column align-items-stretch content-card-body",
          style = "overflow: hidden !important; padding: 1rem; min-height: 0 !important;",
          div(
            style = "flex: 0 1 auto; width: 100%; aspect-ratio: 6 / 5; overflow: hidden;",
            withSpinner(
              girafeOutput(ns("choropleth"), width = "100%", height = "100%"),
              type = SPINNER_TYPE,
              color = "#003087",
              size = 0.7
            )
          ),
          div(
            class = "choropleth-control-header",
            airDatepickerInput(
              inputId = ns("cluster_date"),
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
          )
          # # CAPTION BLOCK
          # div(
          #   class = "content-caption",
          #   tags$strong("Figure 2: "),
          #   "Geographic distribution of delay-related deaths per thousand admissions."
          # )
        )
      ),

      # CARD 3: FUNNEL PLOT
      card(
        full_screen = TRUE,
        card_body(
          class = "d-flex flex-column align-items-stretch content-card-body",
          style = "overflow: hidden !important; padding: 1rem; min-height: 0 !important;",
          div(
            style = "flex: 0 1 auto; width: 100%; aspect-ratio: 6 / 5; overflow: hidden;",
            withSpinner(
              girafeOutput(ns("funnel_plot"), width = "100%", height = "100%"),
              type = SPINNER_TYPE,
              color = "#003087",
              size = 0.7
            )
          ),
          div(
            class = "funnel-control-header",
            style = "flex-wrap: wrap;",

            div(
              style = "flex: 1 1 140px; min-width: 0;",
              airDatepickerInput(
                inputId = ns("trust_date"),
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
            div(
              style = "flex: 2 1 180px; min-width: 0;",
              shinyWidgets::virtualSelectInput(
                inputId = ns("highlighted_trusts"),
                label = "Highlight Trust(s):",
                choices = NULL,
                multiple = TRUE,
                search = TRUE,
                placeholder = "Type to search...",
                width = "100%"
              )
            ),
            div(
              class = "funnel-switch-container",
              div(
                class = "form-check form-switch",
                tags$input(
                  class = "form-check-input",
                  type = "checkbox",
                  id = ns("log_x")
                ),
                tags$label(
                  class = "form-check-label",
                  `for` = ns("log_x"),
                  "Log X"
                )
              )
            )
          )
          # # CAPTION BLOCK
          # div(
          #   class = "content-caption",
          #   tags$strong("Figure 3: "),
          #   "Funnel plot of delay-related deaths versus montly admissions at Trust-level."
          # )
        )
      )
    )
  )
}

# 2. Server Component
dashboardServer <- function(id, max_date) {
  moduleServer(id, function(input, output, session) {
    # pre load charts once landing page is rendered
    # Run this locally when the main app finishes flushing its initial UI
    session$onFlushed(
      function() {
        print("Triggered background load successfully")
        outputOptions(output, "time_series_plot", suspendWhenHidden = FALSE)
        outputOptions(output, "choropleth", suspendWhenHidden = FALSE)
        outputOptions(output, "funnel_plot", suspendWhenHidden = FALSE)
      },
      once = TRUE
    )

    tooltip_css <- "background-color:white;color:black;padding:8px 12px;border-radius:4px;font-family:Inter,sans-serif;font-size:1rem;box-shadow:0 2px 8px rgba(0,0,0,0.15);border:1px solid #e9ecef;"

    target_month_cluster <- reactive({
      if (!is.null(input$cluster_date)) {
        as.Date(input$cluster_date)
      } else {
        as.Date(max_date) # Fallback before tab is clicked
      }
    })

    target_month_trust <- reactive({
      if (!is.null(input$trust_date)) {
        as.Date(input$trust_date)
      } else {
        as.Date(max_date) # Fallback before tab is clicked
      }
    })

    # 1. Time Series
    output$time_series_plot <- renderGirafe({
      print("time_series_plot is actively rendering")
      slider_val <- if (!is.null(input$ts_date_slider)) {
        input$ts_date_slider
      } else {
        # Fallback to the exact default matching your UI
        c(max_date - months(6), max_date)
      }

      start_dt <- lubridate::floor_date(as.Date(slider_val[1]), "month")
      end_dt <- lubridate::floor_date(as.Date(slider_val[2]), "month")

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
          opts_toolbar(
            hidden = c(
              'lasso_select',
              'lasso_deselect',
              'zoom_onoff',
              'zoom_rect',
              'zoom_reset',
              'fullscreen'
            )
          ),
          opts_selection(type = "none"),
          opts_sizing(rescale = TRUE, width = 1)
        ),
        width_svg = 6.0,
        height_svg = 5.0
      )
    })

    # 2. Choropleth Map
    output$choropleth <- renderGirafe({
      print("choropleth is actively rendering")
      filtered_map_data <- ae_impacts %>%
        filter(period == target_month_cluster())
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
          opts_toolbar(
            hidden = c(
              'lasso_select',
              'lasso_deselect',
              'zoom_onoff',
              'zoom_rect',
              'zoom_reset',
              'fullscreen'
            )
          ),
          opts_selection(type = "none"),
          opts_sizing(rescale = TRUE, width = 1)
        ),
        width_svg = 6.0,
        height_svg = 5.0
      )
    })

    # 3. Funnel Plot Dropdown Dynamic Syncing
observe({
  # We only *strictly* require a valid date to populate the choices
  req(target_month_trust())

  # 1. Capture the current selection without triggering a reactive loop
  # If it's NULL (initial load), default to an empty character vector
  current_selection <- isolate(input$highlighted_trusts)
  if (is.null(current_selection)) current_selection <- character(0)

  available_trusts <- ae_impacts %>%
    filter(
      period == target_month_trust(),
      ae_type == "Type 1 (Major)",
      org != "Total"
    ) %>%
    dplyr::filter(!is.na(excess_mort), !is.na(tot_ae_adm), !is.na(org)) %>%
    dplyr::filter(tot_ae_adm > 0, excess_mort <= tot_ae_adm) %>%
    pull(org) %>%
    unique() %>%
    sort()

  if (is.null(available_trusts)) {
    available_trusts <- character(0)
  }

  # 2. Keep only the previously selected trusts that exist in the new date's data
  valid_selection <- intersect(current_selection, available_trusts)
  
  # If the intersection is empty, pass character(0) instead of an empty list/NULL
  if (length(valid_selection) == 0) {
    valid_selection <- character(0)
  }
  
  # Update widget safely
  shinyWidgets::updateVirtualSelect(
    inputId = "highlighted_trusts", # Note: Do NOT wrap in ns() inside server update functions
    choices = available_trusts,
    selected = valid_selection,
    session = session
  )
})

    # Reactive calculation pipeline for both coordinate variations
    funnel_cache <- reactive({
      req(target_month_trust())

      filtered_funnel_data <- ae_impacts %>%
        filter(period == target_month_trust())

      p_linear <- funnel_plot(
        filtered_funnel_data,
        precomputed_lines = global_funnel_lines, # Grabbed from global scope
        precomputed_ribbons = global_funnel_ribbons, # Grabbed from global scope
        BASE_FONT_SIZE,
        PLOT_TITLE_WRAP,
        log_x = FALSE,
        selected_trusts = input$highlighted_trusts
      )
      p_log <- funnel_plot(
        filtered_funnel_data,
        precomputed_lines = global_funnel_lines, # Grabbed from global scope
        precomputed_ribbons = global_funnel_ribbons, # Grabbed from global scope
        BASE_FONT_SIZE,
        PLOT_TITLE_WRAP,
        log_x = TRUE,
        selected_trusts = input$highlighted_trusts
      )

      list(linear = p_linear, log = p_log)
    })

    output$funnel_plot <- renderGirafe({
      print("time_series_plot is actively rendering")
      req(funnel_cache())

      is_log <- !is.null(input$log_x) && isTRUE(input$log_x)

      selected_plot <- if (is_log) {
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
          opts_toolbar(
            hidden = c(
              'lasso_select',
              'lasso_deselect',
              'zoom_onoff',
              'zoom_rect',
              'zoom_reset',
              'fullscreen'
            )
          ),
          opts_selection(type = "none"),
          opts_sizing(rescale = TRUE, width = 1)
        ),
        width_svg = 6.0,
        height_svg = 5.0
      )
    })
  })
}