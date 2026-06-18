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
        card_body(
          # Force the body to be a full-height flex column with no internal scrolling
          style = "padding: 1rem; display: flex; flex-direction: column; overflow: hidden; height: 100%;",
          
          # This container now takes up all available space and handles the scrollbar
          div(
            style = "flex: 1; min-height: 0; overflow-y: auto;",
            withSpinner(
              reactableOutput(ns("overview_table")),
              type = SPINNER_TYPE,
              color = "#003087",
              size = 0.7
            )
          ),
          
          # CAPTION BLOCK: Fixed at the bottom, never pushed out
          div(
            style = "font-size: 1rem; color: #475569; font-style: italic; margin-top: 10px; border-top: 1px solid #e2e8f0; padding-top: 6px; flex-shrink: 0;text-align: center;",
            tags$strong("Table 1: "), 
            stringr::str_c("Key performance indicators and activity overview by Trust, ", format(report_date, "%B %Y"), ". DRD values represent estimated levels rounded to the nearest integer.")
          )
        )
      ),

      # ==========================================
      # CARD 2: Top worseners (REACTABLE)
      # ==========================================
      card(
        full_screen = TRUE,
        card_body(
          style = "padding: 1rem; display: flex; flex-direction: column; overflow: hidden; height: 100%;",
          
          div(
            style = "flex: 1; min-height: 0; overflow-y: auto;",
            withSpinner(
              reactableOutput(ns("top_worsening")),
              type = SPINNER_TYPE,
              color = "#003087",
              size = 0.7
            )
          ),
          
          # CAPTION BLOCK
          div(
            style = "font-size: 1rem; color: #475569; font-style: italic; margin-top: 10px; border-top: 1px solid #e2e8f0; padding-top: 6px; flex-shrink: 0;text-align: center;",
            tags$strong("Table 2: "), 
            "Top regional outliers by percentage increase in DRD over the preceding rolling 3-month period."
          )
        )
      ),

      # ==========================================
      # CARD 3: Top improvers (REACTABLE)
      # ==========================================
      card(
        full_screen = TRUE,
        card_body(
          style = "padding: 1rem; display: flex; flex-direction: column; overflow: hidden; height: 100%;",
          
          div(
            style = "flex: 1; min-height: 0; overflow-y: auto;",
            withSpinner(
              reactableOutput(ns("top_improving")),
              type = SPINNER_TYPE,
              color = "#003087",
              size = 0.7
            )
          ),
          
          # CAPTION BLOCK
          div(
            style = "font-size: 1rem; color: #475569; font-style: italic; margin-top: 10px; border-top: 1px solid #e2e8f0; padding-top: 6px; flex-shrink: 0;text-align: center;",
            tags$strong("Table 3: "), 
            "Top performing regional highlights by greatest percentage reduction in DRD over the past 3 months."
          )
        )
      )
    )
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
        `Estimated deaths per thousand admissions`,
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
            cell = reactable_percent_bar_formatter(max_pct, df = display_df, bar_color = "#cbd5e1")
          ),

          `Estimated DRD` = colDef(show = FALSE),

          `Estimated deaths per thousand admissions` = colDef(
            name = "Estimated DRD: Rate per thousand admissions (Total)",
            minWidth = 150, 
            headerStyle = list(
              whiteSpace = "normal",
              wordBreak = "normal",
              lineHeight = "1.2",
              paddingBottom = "4px"
            ),
            cell = function(value, index) {
              drd_value <- display_df$`Estimated DRD`[index]
              is_total <- display_df$Trust[index] == "Total"
              
              if (is.na(value)) {
                display_text <- "-"
              } else if (is.na(drd_value)) {
                display_text <- as.character(value)
              } else {
                display_text <- paste0(round(value, 1), " (", round(drd_value), ")")
              }
              
              tags$span(
                style = list(
                  fontWeight = if (is_total) "bold" else "normal"
                ),
                display_text
              )
            }
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
            cell = reactable_percent_bar_formatter(max_pct, df = top_improving)
          )
        )
      )
    })
  })
}