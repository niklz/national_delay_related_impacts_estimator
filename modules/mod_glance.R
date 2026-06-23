# modules/mod_glance.R

glanceUI <- function(id, min_date, max_date, SPINNER_TYPE) {
  ns <- NS(id)

  nav_panel(
    title = "At a glance",

    header = tags$p(
      style = "margin: 0; padding-top: 0rem; font-size: 14px; color: #555; line-height: 1.4;",
      str_c("Latest data from: ", format(report_date, "%B %Y"))
    ),

    layout_columns(
      col_widths = c(7, 5), # Split the main area 50/50 between the two cards

      # ==========================================
      # CARD 1: Overview table (REACTABLE)
      # ==========================================
      card(
        full_screen = TRUE,
        card_body(
          class = "content-card-body",

          div(
            class = "content-table-scroll",
            withSpinner(
              reactableOutput(ns("overview_table")),
              type = SPINNER_TYPE,
              color = "#003087",
              size = 0.7
            )
          ),

          # CAPTION BLOCK
          div(
            class = "content-caption",
            tags$strong("Table 1: "),
            HTML(stringr::str_c(
              "Type-1 A&E admissions and wait times by trust, ranked by the proportion of patients delayed over 4 hours. For each trust, the estimated excess deaths per thousand admissions (reflecting the impact of these delays) are shown alongside the total DRD¹ in brackets.<br>",
              "¹ DRD: Delay-related deaths"
            ))
          )
        )
      ),

      # ==========================================
      # CARD 2: Combined Top Worseners & Improvers
      # ==========================================
      card(
        full_screen = TRUE,
        card_body(
          class = "content-card-body",

          # Nested column layout so Table 2 and Table 3 sit side-by-side
          layout_columns(
            col_widths = c(6, 6),

            div(
              class = "content-table-scroll",
              withSpinner(
                reactableOutput(ns("top_improving")),
                type = SPINNER_TYPE,
                color = "#003087",
                size = 0.7
              )
            ),
            div(
              class = "content-table-scroll",
              withSpinner(
                reactableOutput(ns("top_worsening")),
                type = SPINNER_TYPE,
                color = "#003087",
                size = 0.7
              )
            ),
          ),

          # SHARED CAPTION BLOCK
          div(
            class = "content-caption",
            tags$strong("Table 2 & 3: "),
            "Top improving and worsening trust, where performance is ranked by percentage change in delay-related deaths per 1,000 admissions over the preceding 3-month period (seasonally adjusted). Table 2 shows the largest increases, and Table 3 shows the largest decreases."
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
            minWidth = 110,
            cell = reactable_bar_formatter(max_admissions, df = display_df)
          ),

          `Proportion DTA > 4 hours` = colDef(
            name = "% Waiting > 4 hrs for admission",
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
            name = "Estimated DRD¹: Rate per thousand admissions (Total)",
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
            name = "Relative increase of DRD over 3 months",
            minWidth = 120,
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
            name = "Relative decrease of DRD over 3 months",
            minWidth = 120,
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