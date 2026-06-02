# modules/mod_tables.R

glanceUI <- function(id, min_date, max_date, SPINNER_TYPE) {
  ns <- NS(id)

  nav_panel(
    title = "At a glance", #

    layout_columns(
      col_widths = c(6, 3, 3), # Keeps the consistent 3-column architecture

      # ==========================================
      # CARD 1: Overview table
      # ==========================================
      card(
        full_screen = TRUE,
        card_header("Overview"),
        card_body(
          style = "padding: 1rem;",

          # Scrollable container for formattable widget
          div(
            style = "overflow-y: auto; max-height: 100%;",
            withSpinner(
              DTOutput(ns("overview_table")), 
              type = SPINNER_TYPE,
              color = "#003087",
              size = 0.7
            )
          )
        )
      ),

      # ==========================================
      # CARD 2: Top growers
      # ==========================================
      card(
        full_screen = TRUE,
        card_header("Top growers"),
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
      # CARD 3:Top shrinkers
      # ==========================================
      card(
        full_screen = TRUE,
        card_header("Top shrinkers"),
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
    output$overview_table <- renderDT({
      req(table_data)

      # 1. Keep your gorgeous, original formattable object exactly as it was
      f_table <- formattable(
        table_data,
        align = c("l", "c", "c", "r", "c"),
        list(
          Trust = formatter(
            "span",
            style = x ~ style(
              font.weight = ifelse(x == "Total", "bold", "normal")
            )
          ),
          `Total admissions` = custom_bar_formatter("#cbd5e1", has_total_row = TRUE),
          `Number of DTA > 4 hours` = custom_bar_formatter("#cbd5e1", has_total_row = TRUE),
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
          Trend = formatter(
            "span",
            style = x ~ style(
              color = case_when(
                grepl("Decline", x) ~ "#166534",
                grepl("Growth|Increase", x) ~ "#991b1b",
                TRUE ~ "#475569"
              ),
              font.weight = ifelse(
                table_data$Trust == "Total",
                "bold",
                "normal"
              )
            )
          )
        )
      )


      as.datatable(
        f_table,
        filter = "top",
        rownames = FALSE,
        options = list(
          dom = 'tf',
          pageLength = -1,
          autoWidth = FALSE,
          columnDefs = list(
            list(width = '34%', targets = 0), # Trust 
            list(width = '19%', targets = 1), # Admissions 
            list(width = '19%', targets = 2), # DTA 
            list(width = '15%', targets = 3), # Deaths 
            list(width = '13%', targets = 4) # Trend 
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