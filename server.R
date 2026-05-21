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

    start_dt <- lubridate::floor_date(as.Date(input$ts_date_slider[1]), "month")
    end_dt <- lubridate::floor_date(as.Date(input$ts_date_slider[2]), "month")

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

  # 3. Funnel Plot Dropdown Dynamic Syncing
  observe({
    req(target_month_trust())

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

    shinyWidgets::updateVirtualSelect(
      "highlighted_trusts",
      choices = available_trusts,
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
      BASE_FONT_SIZE,
      PLOT_TITLE_WRAP,
      log_x = FALSE,
      selected_trusts = input$highlighted_trusts
    )
    p_log <- funnel_plot(
      filtered_funnel_data,
      BASE_FONT_SIZE,
      PLOT_TITLE_WRAP,
      log_x = TRUE,
      selected_trusts = input$highlighted_trusts
    )

    list(linear = p_linear, log = p_log)
  })

  output$funnel_plot <- renderGirafe({
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
        opts_toolbar(saveaspng = FALSE),
        opts_selection(type = "none"),
        opts_sizing(rescale = TRUE, width = 1)
      ),
      width_svg = 6.0,
      height_svg = 5.0
    )
  })
}