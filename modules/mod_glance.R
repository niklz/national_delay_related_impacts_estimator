# modules/mod_glance.R

glanceUI <- function(id, min_date, max_date, SPINNER_TYPE) {
  ns <- NS(id)

  nav_panel(
    title = "At a glance",

    layout_columns(
      col_widths = c(6, 3, 3),

      # ==========================================
      # CARD 1: Overview table (REACTABLE)
      # ==========================================
      card(
        full_screen = TRUE,
        card_header(stringr::str_c("Overview, ", format(report_date, "%B %y"))),
        card_body(
          style = "padding: 1rem;",
          div(
            style = "overflow-y: auto; max-height: 100%;",
            withSpinner(
              reactableOutput(ns("overview_table")),
              type = SPINNER_TYPE,
              color = "#003087",
              size = 0.7
            )
          )
        )
      ),

      # ==========================================
      # CARD 2: Top worseners (REACTABLE)
      # ==========================================
      card(
        full_screen = TRUE,
        card_header("Top worsening (past 3 months)"),
        card_body(
          style = "padding: 1rem;",
          div(
            style = "overflow-y: auto; max-height: 100%;",
            withSpinner(
              reactableOutput(ns("top_worsening")),
              type = SPINNER_TYPE,
              color = "#003087",
              size = 0.7
            )
          )
        )
      ),

      # ==========================================
      # CARD 3: Top improvers (REACTABLE)
      # ==========================================
      card(
        full_screen = TRUE,
        card_header("Top improving (past 3 months)"),
        card_body(
          style = "padding: 1rem;",
          div(
            style = "overflow-y: auto; max-height: 100%;",
            withSpinner(
              reactableOutput(ns("top_improving")),
              type = SPINNER_TYPE,
              color = "#003087",
              size = 0.7
            )
          )
        )
      )
    ) #,
    # # ==========================================================================
    #   # PERFORMANCE TRIGGER: Signal when the browser finishes drawing these tables
    #   # ==========================================================================
    #   tags$script(HTML(sprintf("
    #     $(document).one('shiny:idle', function(event) {
    #       Shiny.setInputValue('%s', true);
    #     });
    #   ", ns("tables_rendered"))))
  )
}


glanceServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # ==========================================================================
    # OVERVIEW TABLE (Reactable Implementation)
    # ==========================================================================
    output$overview_table <- renderReactable({
      req(table_data)

      display_df <- select(
        table_data,
        `Trust`,
        `Total admissions`,
        `Proportion DTA > 4 hours`,
        `Estimated DRD`,
        `Trend`
      )

      valid_rows <- display_df %>% filter(Trust != "Total")
      max_admissions <- max(valid_rows$`Total admissions`, na.rm = TRUE)

      clean_pcts <- valid_rows$`Proportion DTA > 4 hours`
      clean_pcts <- clean_pcts[!is.na(clean_pcts) & !is.nan(clean_pcts)]
      max_pct <- max(abs(clean_pcts), na.rm = TRUE)

      reactable(
        display_df,
        pagination = FALSE,
        filterable = TRUE,
        highlight = TRUE,
        defaultColDef = colDef(align = "center"),
        fullWidth = TRUE,
        style = list(width = "100%", overflowX = "hidden"),

        rowStyle = function(index) {
          if (display_df$Trust[index] == "Total") {
            return(list(fontWeight = "bold", background = "#e2e8f0"))
          }
          if (index %% 2 == 0) {
            return(list(background = "#f8fafc"))
          }
          return(list(background = "#ffffff"))
        },

        columns = list(
          Trust = colDef(name = "Trust", align = "left", minWidth = 240),

          `Total admissions` = colDef(
            name = "Total admissions",
            minWidth = 130,
            cell = reactable_bar_formatter(max_admissions, df = display_df)
          ),

          `Proportion DTA > 4 hours` = colDef(
            name = "Proportion DTA > 4 hours",
            minWidth = 110,
            headerStyle = list(
              whiteSpace = "normal",
              wordBreak = "normal",
              lineHeight = "1.2",
              paddingBottom = "4px"
            ),
            # FIX: Passed display_df context here
            cell = reactable_percent_bar_formatter(max_pct, df = display_df, bar_color = "#cbd5e1")
          ),

          `Estimated DRD` = colDef(
            name = "Estimated DRD",
            minWidth = 120,
            headerStyle = list(
              whiteSpace = "normal",
              wordBreak = "normal",
              lineHeight = "1.2",
              paddingBottom = "4px"
            ),
            cell = reactable_text_formatter()
          ),

          Trend = colDef(
            name = "Trend",
            minWidth = 100,
            headerStyle = list(
              whiteSpace = "normal",
              wordBreak = "normal",
              lineHeight = "1.2"
            ),
            cell = function(value, index) {
              is_total <- display_df$Trust[index] == "Total"

              if (is.na(value)) return("-")

              text_color <- case_when(
                grepl("Decline", value) ~ "#166534",
                grepl("Growth|Increase", value) ~ "#991b1b",
                TRUE ~ "#475569"
              )
              tags$span(
                style = list(
                  color = text_color,
                  fontWeight = if (is_total) "bold" else "normal"
                ),
                value
              )
            }
          )
        )
      )
    })

    # ==========================================================================
    # TOP GROWERS (Reactable Implementation)
    # ==========================================================================
    output$top_worsening <- renderReactable({
      req(top_worsening)

      max_pct <- max(abs(top_worsening$`Percent change (DRD)`), na.rm = TRUE)

      reactable(
        top_worsening,
        pagination = FALSE,
        filterable = TRUE,
        highlight = TRUE,
        defaultColDef = colDef(align = "center"),
        fullWidth = TRUE,
        style = list(width = "100%", overflowX = "hidden"),

        rowStyle = function(index) {
          if (top_worsening$Trust[index] == "Total") {
            return(list(fontWeight = "bold", background = "#e2e8f0"))
          }
          if (index %% 2 == 0) {
            return(list(background = "#f8fafc"))
          }
          return(list(background = "#ffffff"))
        },

        columns = list(
          Trust = colDef(name = "Trust", align = "left", minWidth = 160),
          `Percent change (DRD)` = colDef(
            name = "Percent change",
            minWidth = 110,
            headerStyle = list(
              whiteSpace = "normal",
              wordBreak = "normal",
              lineHeight = "1.2",
              paddingBottom = "4px"
            ),
            # FIX: Passed top_worsening context here
            cell = reactable_percent_bar_formatter(max_pct, df = top_worsening)
          )
        )
      )
    })

    # ==========================================================================
    # TOP SHRINKERS (Reactable Implementation)
    # ==========================================================================
    output$top_improving <- renderReactable({
      req(top_improving)

      max_pct <- max(abs(top_improving$`Percent change (DRD)`), na.rm = TRUE)

      reactable(
        top_improving,
        pagination = FALSE,
        filterable = TRUE,
        highlight = TRUE,
        defaultColDef = colDef(align = "center"),
        fullWidth = TRUE,
        style = list(width = "100%", overflowX = "hidden"),

        rowStyle = function(index) {
          if (top_improving$Trust[index] == "Total") {
            return(list(fontWeight = "bold", background = "#e2e8f0"))
          }
          if (index %% 2 == 0) {
            return(list(background = "#f8fafc"))
          }
          return(list(background = "#ffffff"))
        },

        columns = list(
          Trust = colDef(name = "Trust", align = "left", minWidth = 160),
          `Percent change (DRD)` = colDef(
            name = "Percent change",
            minWidth = 110,
            headerStyle = list(
              whiteSpace = "normal",
              wordBreak = "normal",
              lineHeight = "1.2",
              paddingBottom = "4px"
            ),
            # FIX: Passed top_improving context here
            cell = reactable_percent_bar_formatter(max_pct, df = top_improving)
          )
        )
      )
    })
  })
}