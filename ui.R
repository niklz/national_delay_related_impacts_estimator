ui <- page_navbar(
  title = tags$p(
    style = "margin: 0; padding-top: 0.25rem; font-size: 24px; color: #000000; max-width: 900px; line-height: 1.4;",
    "NHS National A&E Delay-Related Impacts Dashboard"
  ),
  
  header = tags$p(
    style = "margin: 0; padding-top: 0.25rem; font-size: 18px; color: #555; max-width: 1080px; line-height: 1.4;",
    "This dashboard displays estimated excess deaths...",
    tags$a(href = "https://doi.org/10.1136/emermed-2025-214983", target = "_blank", "Howlett et al.", style = "color: #003087; text-decoration: underline;")
  ),

  theme = bs_theme(
    version = 5,
    bg = "#ffffff",
    fg = "#333333",
    primary = "#003087"
  ),

  # Link directly to your CSS file inside the www directory
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),

  div(
    class = "container-fluid",

    # ROW 1: Controls Only
    div(
      class = "row gx-4 control-row",
      div(
        class = "col-md-4",
        div(
          class = "slider-breathing-room",
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
        )
      ),
      div(
        class = "col-md-4",
        div(
          class = "choropleth-control-header",
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
        )
      ),
      div(
        class = "col-md-4",
        div(
          class = "funnel-control-header",
          style = "display: flex !important; align-items: flex-end !important; gap: 10px; width: 80% !important; min-width: 220px !important; margin-left: auto !important; margin-right: auto !important;",

          div(
            style = "flex: 1 1 30%; min-width: 0;", 
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

          div(
            style = "flex: 1 1 50%; min-width: 0;", 
            shinyWidgets::virtualSelectInput(
              inputId = "highlighted_trusts",
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
            style = "flex: 0 0 auto;", 
            div(
              class = "form-check form-switch",
              tags$input(
                class = "form-check-input",
                type = "checkbox",
                id = "log_x"
              ),
              tags$label(
                class = "form-check-label",
                `for` = "log_x",
                "Log X"
              )
            )
          )
        )
      )
    ),

    # ROW 2: Charts Only
    div(
      class = "row gx-4 chart-row",
      div(
        class = "col-md-4 custom-plot-container",
        withSpinner(
          girafeOutput("time_series_plot", height = "auto"),
          type = SPINNER_TYPE,
          color = "#003087",
          size = 0.7
        )
      ),
      div(
        class = "col-md-4 custom-plot-container",
        withSpinner(
          girafeOutput("choropleth", height = "auto"),
          type = SPINNER_TYPE,
          color = "#003087",
          size = 0.7
        )
      ),
      div(
        class = "col-md-4 custom-plot-container",
        withSpinner(
          girafeOutput("funnel_plot", height = "auto"), 
          type = SPINNER_TYPE,
          color = "#003087",
          size = 0.7
        )
      )
    )
  )
)