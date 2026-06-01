# modules/mod_tables.R

glanceUI <- function(id, min_date, max_date, SPINNER_TYPE) {
  ns <- NS(id)

  nav_panel(
    title = "At a glance", #

    layout_columns(
      col_widths = c(4, 4, 4), # Keeps the consistent 3-column architecture

      # ==========================================
      # CARD 1: TRUST PERFORMANCE TABLE
      # ==========================================
      card(
        full_screen = TRUE,
        card_header("Overview"),
        card_body(
          style = "padding: 1rem;",

          # Scrollable container for formattable widget
          div(
            style = "overflow-x: auto; overflow-y: auto; max-height: 100%;",
            withSpinner(
              formattableOutput(ns("perf_table")),
              type = SPINNER_TYPE,
              color = "#003087",
              size = 0.7
            )
          )
        )
      ),

      # ==========================================
      # CARD 2: REGIONAL DELAY SUMMARY
      # ==========================================
      card(
        full_screen = TRUE,
        card_header("Regional Delay Statistics"),
        card_body(
          style = "padding: 1rem;",

          div(
            class = "table-control-header",
            style = "margin-bottom: 1rem;",
            airDatepickerInput(
              inputId = ns("region_date"),
              label = "Select regional month:",
              value = max_date,
              minDate = min_date,
              maxDate = max_date,
              view = "months",
              minView = "months",
              dateFormat = "yyyy MMMM",
              addon = "none",
              width = "100%"
            )
          ),

          div(
            style = "overflow-x: auto; overflow-y: auto; max-height: 100%;",
            withSpinner(
              formattableOutput(ns("region_table")),
              type = SPINNER_TYPE,
              color = "#003087",
              size = 0.7
            )
          )
        )
      ),

      # ==========================================
      # CARD 3: RISK ASSESSMENT BENCHMARKS
      # ==========================================
      card(
        full_screen = TRUE,
        card_header("Risk Assessment Benchmarks"),
        card_body(
          style = "padding: 1rem;",

          div(
            class = "table-control-header",
            style = "margin-bottom: 1rem;",
            shinyWidgets::virtualSelectInput(
              inputId = ns("filter_tier"),
              label = "Filter Risk Tier:",
              choices = c("All Tiers", "High Risk", "Medium Risk", "Low Risk"),
              selected = "All Tiers",
              width = "100%"
            )
          ),

          div(
            style = "overflow-x: auto; overflow-y: auto; max-height: 100%;",
            withSpinner(
              formattableOutput(ns("risk_table")),
              type = SPINNER_TYPE,
              color = "#003087",
              size = 0.7
            )
          )
        )
      )
    )
  )
}


glanceServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # 1. Dummy Data Generator for Performance Table
    output$perf_table <- renderFormattable({
      formattable(
        table_data,
        align = c("l", "c", "c", "r", "c"), # Added 'c' for Trend column
        list(
          # Bold entire "Total" row cells for text columns
          Trust = formatter(
            "span",
            style = x ~ style(
              font.weight = ifelse(x == "Total", "bold", "normal")
            )
          ),
          # Region = formatter("span", style = function(x) {
          #   style(
          #     font.weight = ifelse(table_data$Trust == "Total", "bold", "normal"),
          #     color = ifelse(table_data$Trust == "Total", "#475569", "#64748b") # Dim the region text slightly for hierarchy
          #   )
          # }),

          # Clean Progress Bars with internal centering
          `Total admissions` = custom_bar_formatter("#cbd5e1"),
          `Number of DTA > 4 hours` = custom_bar_formatter("#cbd5e1"),

          # Fully styled Deaths column (Total row bolded)
          `Estimated delay related deaths` = formatter(
            "span",
            style = function(x) {
              style(
                color = c("#0f172a", ifelse(x[-1] > 0, "#991b1b", "#166534")),
                font.weight = "bold"
              )
            },
            x ~ comma(x, digits = 0)
          ),

          # Color-coded Trends for immediate scannability
          Trend = formatter(
            "span",
            style = x ~ style(
              color = case_when(
                grepl("Decline", x) ~ "#166534", # Green for declining deaths
                grepl("Growth|Increase", x) ~ "#991b1b", # Red for rising issues
                TRUE ~ "#475569" # Neutral grey for stable
              ),
              font.weight = ifelse(table_data$Trust == "Total", "bold", "normal")
            )
          )
        )
      )
    })

    # 2. Dummy Regional Summary Table
    output$region_table <- renderFormattable({
      req(input$region_date)

      dummy_region <- data.frame(
        Region = c("North East", "Midlands", "London", "South West"),
        Excess_Deaths_Per_1k = c(1.2, 2.5, 3.1, 0.9),
        Trend = c("Rising", "Stable", "Rising", "Falling")
      )

      formattable(
        dummy_region,
        list(
          Excess_Deaths_Per_1k = color_tile("white", "orange")
        )
      )
    })

    # 3. Dummy Risk Assessment Table
    output$risk_table <- renderFormattable({
      req(input$filter_tier)

      dummy_risk <- data.frame(
        Indicator = c("Wait > 4 hrs", "Wait > 12 hrs", "Bed Occupancy"),
        National_Median = c("28.4%", "5.2%", "94.1%"),
        Status = c("High Risk", "Low Risk", "Medium Risk")
      )

      if (input$filter_tier != "All Tiers") {
        dummy_risk <- dummy_risk[dummy_risk$Status == input$filter_tier, ]
      }

      formattable(dummy_risk)
    })
  })
}