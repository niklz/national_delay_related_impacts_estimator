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
        card_header("Top growers (past 3 months)"),
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
        card_header("Top shrinkers (past 3 months)"),
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

      global_align <- c("l", "c", "c", "c", "c")

      f_table <- formattable(
        select(table_data, -`Percent change`),
        align = global_align,
        list(
          Trust = formatter(
            "span",
            style = x ~ style(
              font.weight = ifelse(x == "Total", "bold", "normal")
            )
          ),
          `Total admissions` = custom_bar_formatter(
            "#cbd5e1",
            has_total_row = TRUE,
            dynamic_color = FALSE
          ),
          `Number of DTA > 4 hours` = custom_bar_formatter(
            "#cbd5e1",
            has_total_row = TRUE,
            dynamic_color = FALSE
          ),
          `Estimated DRD` = formatter(
            "span",
            style = function(x) {
              style(
                color = ifelse(table_data$Trust == "Total", "#0f172a", 
                               ifelse(x > 0, "#991b1b", "#166534")),
                font.weight = "bold",
                # --- FIX A: FORCE CSS ALIGNMENT WITHIN THE SPAN ---
                display = "block",
                text_align = "center"
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
            list(width = '8%', targets = 3), # Deaths
            list(width = '20%', targets = 4), # Trend
            # --- FIX B: FORCE DT CELL CLASSES TO CENTER TARGET 3 ---
            list(className = 'dt-center', targets = c(1, 2, 3, 4)),
            list(
          targets = c(2, 3, 4), # Target all numeric formatted columns
          render = JS(
            "function(data, type, row, meta) {",
            "  if (type === 'sort' || type === 'type' || type === 'filter') {",
            "    // Strip HTML tags and remove commas/symbols to extract pure numbers",
            "    var clean = data.replace(/<[^>]*>/g, '').replace(/[^0-9.-]/g, '');",
            "    return parseFloat(clean) || 0;",
            "  }",
            "  return data;",
            "}"
          )
          )
        )
        )
      )
    })


    output$top_growers <- renderDT({
      global_align <- rep("l", ncol(top_growers))
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
          `Percent change` = custom_bar_formatter("#cbd5e1", suffix = "%", dynamic_color = TRUE)
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
      global_align <- rep("l", ncol(top_shrinkers))
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
          `Percent change` = custom_bar_formatter("#cbd5e1", suffix = "%", dynamic_color = TRUE)
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