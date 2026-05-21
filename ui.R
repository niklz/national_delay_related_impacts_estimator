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

  # Wrap main content in a nav_panel to fit the page_navbar structure
  nav_panel(
    title = "Dashboard",
    
    layout_columns(
      col_widths = c(4, 4, 4), # Creates the 3-column layout automatically
      
      # ==========================================
      # CARD 1: TIME SERIES
      # ==========================================
      card(
        full_screen = TRUE,
        card_header(
          class = "bg-light",
          tags$strong("Time Series Impact")
        ),
        card_body(
          class = "d-flex flex-column align-items-center justify-content-center",
          style = "overflow: hidden !important;",
          
          # Control Wrapper (Protected from squishing)
          div(
            style = "width: 100%; margin-bottom: 10px; flex-shrink: 0;",
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
          
          # Output Wrapper (Flexible, bounds the spinner and chart)
          div(
            style = "flex-grow: 1; width: 100%; height: 100%; min-height: 0; overflow: hidden;",
            withSpinner(
              girafeOutput("time_series_plot", width = "100%", height = "100%"),
              type = SPINNER_TYPE,
              color = "#003087",
              size = 0.7
            )
          )
        )
      ),

      # ==========================================
      # CARD 2: CHOROPLETH MAP
      # ==========================================
      card(
        full_screen = TRUE,
        card_header(
          class = "bg-light",
          tags$strong("Regional Distribution")
        ),
        card_body(
          class = "d-flex flex-column align-items-center justify-content-center",
          style = "overflow: hidden !important;",
          
          # Control Wrapper
          div(
            style = "width: 100%; margin-bottom: 10px; flex-shrink: 0;",
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
          
          # Output Wrapper
          div(
            style = "flex-grow: 1; width: 100%; height: 100%; min-height: 0; overflow: hidden;",
            withSpinner(
              girafeOutput("choropleth", width = "100%", height = "100%"),
              type = SPINNER_TYPE,
              color = "#003087",
              size = 0.7
            )
          )
        )
      ),

      # ==========================================
      # CARD 3: FUNNEL PLOT
      # ==========================================
      card(
        full_screen = TRUE,
        card_header(
          class = "bg-light",
          tags$strong("Trust-Level Funnel Plot")
        ),
        card_body(
          class = "d-flex flex-column align-items-center justify-content-center",
          style = "overflow: hidden !important;",
          
          # Controls grouped in a flex container (Protected from squishing)
          div(
            style = "display: flex; flex-wrap: wrap; gap: 10px; align-items: flex-end; margin-bottom: 15px; width: 100%; flex-shrink: 0;",
            
            div(
              style = "flex: 1 1 140px; min-width: 0;", 
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
              style = "flex: 2 1 180px; min-width: 0;", 
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
                tags$input(class = "form-check-input", type = "checkbox", id = "log_x"),
                tags$label(class = "form-check-label", `for` = "log_x", "Log X")
              )
            )
          ),
          
          # Output Wrapper
          div(
            style = "flex-grow: 1; width: 100%; height: 100%; min-height: 0; overflow: hidden;",
            withSpinner(
              girafeOutput("funnel_plot", width = "100%", height = "100%"), 
              type = SPINNER_TYPE,
              color = "#003087",
              size = 0.7
            )
          )
        )
      )
    )
  )
)