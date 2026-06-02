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
            style = "overflow-x: auto; overflow-y: auto; max-height: 100%;",
            withSpinner(
              DTOutput(ns("top_growers")),
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
            style = "overflow-x: auto; overflow-y: auto; max-height: 100%;",
            withSpinner(
              DTOutput(ns("top_shrinkers")),
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

      f_table <- formattable(
        select(table_data, -`Percent change`),
        align = c("l", "c", "c", "r", "c"),
        list(
          Trust = formatter(
            "span",
            style = x ~ style(
              font.weight = ifelse(x == "Total", "bold", "normal")
            )
          ),
          `Total admissions` = custom_bar_formatter(
            "#cbd5e1",
            has_total_row = TRUE
          ),
          `Number of DTA > 4 hours` = custom_bar_formatter(
            "#cbd5e1",
            has_total_row = TRUE
          ),
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


    output$top_growers <- renderDT({
      f_table <- formattable(
        top_growers, # 1. First argument must be the data frame explicitly
        align = rep("l", ncol(top_growers)), # Dynamically matches alignment to your column count
        list(
          # Bold entire "Total" row cells for text columns
          Trust = formatter(
            "span",
            style = x ~ style(
              font.weight = ifelse(x == "Total", "bold", "normal")
            )
          ),

          # Clean Progress Bars with internal centering
          `Percent change` = custom_bar_formatter("#cbd5e1")
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
            list(width = '70%', targets = 0), # Trust
            list(width = '30%', targets = 1) # Percent
          )
        )
      )
    })

    # 3. Dummy Risk Assessment Table
    output$top_shrinkers <- renderDT({
      f_table <- formattable(
        top_shrinkers, # 1. First argument must be the data frame explicitly
        align = rep("l", ncol(top_shrinkers)), # Dynamically matches alignment to your column count
        list(
          # Bold entire "Total" row cells for text columns
          Trust = formatter(
            "span",
            style = x ~ style(
              font.weight = ifelse(x == "Total", "bold", "normal")
            )
          ),

          # Clean Progress Bars with internal centering
          `Percent change` = custom_bar_formatter("#cbd5e1")
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
            list(width = '70%', targets = 0), # Trust
            list(width = '30%', targets = 1) # Percent
          )
        )
      )
    })
  })
}