# modules/mod_glance.R

# modules/mod_glance.R

glanceUI <- function(id, min_date, max_date, SPINNER_TYPE, title) {
  ns <- NS(id)

  nav_panel(
    title = title,
    icon = icon("table"),
    card(
      fill = FALSE, 
      style = "background-color: #F4F6F9;
         /*border: 1px solid #e2e8f0;*/
         box-shadow: none;",

      card_body(
        fill = FALSE, 
        style = "padding: 0.75rem 1rem;", # Snug padding (Top/Bottom, Left/Right)

        tags$p(
          style = "margin: 0; font-size: 16px; color: #334155; line-height: 1.5;",
          "This section displays the total monthly acute emergency admissions via type-1 A&E deparments per Trust for the latest reporting period (",
          tags$strong(format(report_date, "%B %Y")),
          "), alongside the proportion of patients who waited over four hours for admission. It estimates the impact of these delays on the admitted cohort using ",
          tags$strong("Delay-Related Deaths"),
          ", expressed both as a ",
          tags$strong("rate per 1,000 admissions (*)"),
          " and as a total monthly count. Additionally, the ",
          tags$strong("3-month trend(†) of the Delay-Related Death rate*"),
          " is modeled using regression on seasonally adjusted data. Finally, the top improving and worsening trusts are ranked based on this trend (decreasing = improving, increasing = worsening)."
        )
      )
    ),


    # Main layout containing the two cards
    layout_columns(
      col_widths = c(7, 5),

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
          ) #,

          # # CAPTION BLOCK (Unchanged)
          # div(
          #   class = "content-caption",
          #   tags$strong("Table 1: "),
          #   HTML(
          #     "Type-1 A&E admissions and wait times by Trust, ranked by the number of DRD¹ per thousand admissions. For each Trust, the estimated excess deaths per thousand admissions (reflecting the impact of these delays) are shown alongside the raw DRD¹ in brackets."
          #   )
          # )
        )
      ),

      # ==========================================
      # CARD 2: 3 month performance table
      # ==========================================
      card(
        full_screen = TRUE,
        card_body(
          class = "content-card-body",

            div(
              class = "content-table-scroll",
              withSpinner(
                reactableOutput(ns("perf_3month")),
                type = SPINNER_TYPE,
                color = "#003087",
                size = 0.7
              )
            )
        )
      )
    ) #,

    # # ==========================================
    # # GLOBAL FOOTNOTE SECTION
    # # ==========================================
    # div(
    #   class = "global-footnotes",
    #   tags$hr(),
    #   tags$p(
    #     tags$strong("¹ DRD:"),
    #     " Delay-related deaths"
    #   )
    # )
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
        `Estimated Delay-Related Deaths`,
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
        # defaultColDef = colDef(align = "center"),
        fullWidth = TRUE,
        style = list(width = "100%", overflowX = "hidden"),

        # Keep total row in filter no matter what
        defaultColDef = colDef(
          align = "center",
          filterMethod = JS(
            "function(rows, columnId, filterValue) {
        return rows.filter(function(row) {
          // Always keep the Total row visible
          if (row.values['Trust'] === 'Total') {
            return true;
          }
          // Perform standard text filtering for all other rows
          const val = row.values[columnId];
          if (val === null || val === undefined) {
            return false;
          }
          return String(val).toLowerCase().includes(String(filterValue).toLowerCase());
        });
      }"
          )
        ),

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
            cell = reactable_percent_bar_formatter(
              max_pct,
              df = display_df,
              bar_color = "#cbd5e1"
            )
          ),

          `Estimated Delay-Related Deaths` = colDef(show = FALSE),

          `Estimated deaths per thousand admissions` = colDef(
            name = "Estimated Delay-Related Deaths: Rate* (Total)",
            minWidth = 150,
            headerStyle = list(
              whiteSpace = "normal",
              wordBreak = "normal",
              lineHeight = "1.2",
              paddingBottom = "4px"
            ),
            cell = function(value, index) {
              drd_value <- display_df$`Estimated Delay-Related Deaths`[index]
              is_total <- display_df$Trust[index] == "Total"

              if (is.na(value)) {
                display_text <- "-"
              } else if (is.na(drd_value)) {
                display_text <- as.character(value)
              } else {
                display_text <- paste0(
                  round(value, 1),
                  " (",
                  scales::comma(round(drd_value)),
                  ")"
                )
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
            name = "Trend†",
            minWidth = 100,
            headerStyle = list(
              whiteSpace = "normal",
              wordBreak = "normal",
              lineHeight = "1.2"
            ),
            cell = function(value, index) {
              is_total <- display_df$Trust[index] == "Total"

              if (is.na(value)) {
                return("-")
              }

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
    # 3 month performance
    # ==========================================================================
    output$perf_3month <- renderReactable({
      req(table_data_3mo)
      # Use z = 3 for a standard 3-sigma "Control Limit" to account for overdispersion
      z <- 3 
      
      # Safe division for n=0 cases
      n <- pmax(1, table_data_3mo$`Total admissions`) 
      p <- table_data_3mo$`Proportion DTA > 4 hours`
      
      denominator <- 1 + (z^2 / n)
      center <- (p + (z^2 / (2 * n))) / denominator
      # Using pmax(0) inside sqrt as an extra safety net against float imprecision
      spread <- (z * sqrt(pmax(0, (p * (1 - p) / n) + (z^2 / (4 * n^2))))) / denominator
      
      table_data_3mo$Lower <- pmax(0, center - spread)
      table_data_3mo$Upper <- pmin(1, center + spread)
      
      # Calculate "Delays Prevented" (Net Impact vs National)
      global_rate <- table_data_3mo$`Number of DTA > 4 hours`[table_data_3mo$Trust == "Total"] / pmax(1, table_data_3mo$`Total admissions`[table_data_3mo$Trust == "Total"])
      table_data_3mo$Expected <- table_data_3mo$`Total admissions` * global_rate
      
      # Positive = Fewer delays than expected (Good). Negative = More delays (Bad).
      table_data_3mo$Net_Timely <- table_data_3mo$Expected - table_data_3mo$`Number of DTA > 4 hours`
      
      # Sort: 'Total' at TOP, sort the rest by Net Timely Admissions (Descending)
      is_not_total <- table_data_3mo$Trust != "Total"
      table_data_3mo <- table_data_3mo[order(is_not_total, -table_data_3mo$Net_Timely), ]
      
      table_data_3mo$Visual <- NA 
      
      # Render table
      reactable(
        table_data_3mo,
        pagination = FALSE,
        filterable = TRUE,
        highlight = TRUE,
        # defaultColDef = colDef(align = "center"),
        fullWidth = TRUE,
        style = list(width = "100%", overflowX = "hidden"),
        
        
        
        theme = reactableTheme(
          cellPadding = "16px 12px"
        ),
        
        rowStyle = function(index) {
          if (table_data_3mo$Trust[index] == "Total") {
            return(list(fontWeight = "bold", background = "#e2e8f0"))
          }
          if (index %% 2 == 0) {
            return(list(background = "#f1f5f9"))
          }
          return(list(background = "#ffffff"))
        },
        
        columns = list(
          Trust = colDef(name = "Trust", align = "left", minWidth = 160),
          `Number of DTA > 4 hours` = colDef(name = "Delayed Admissions (>4h)", width = 120),
          `Total admissions` = colDef(name = "Total Admissions", width = 120),
          
          `Proportion DTA > 4 hours` = colDef(show = FALSE),
          `Estimated Delay-Related Deaths` = colDef(show = FALSE),
          `Estimated deaths per thousand admissions` = colDef(show = FALSE),
          Lower = colDef(show = FALSE),
          Upper = colDef(show = FALSE), 
          Expected = colDef(show = FALSE),
          
          # Updated Metric Column for Delays
          Net_Timely = colDef(
            name = "Additional Timely Admissions (vs National Rate)", 
            width = 160,
            headerStyle = list(
              whiteSpace = "normal", wordBreak = "normal", lineHeight = "1.2", paddingBottom = "4px"
            ),
            cell = function(value, index) {
              if (table_data_3mo$Trust[index] == "Total") return("—") 
              
              formatted_val <- sprintf("%+.1f", value)
              
              # Color code: Green is good (+ prevented), Red is bad (- excess)
              color <- if (value > 0) "#16a34a" else if (value < 0) "#dc2626" else "#64748b"
              
              div(style = list(color = color, fontWeight = "bold"), formatted_val)
            }
          ),
          
          Visual = colDef(
            name = "Performance vs National Average",
            minWidth = 320, 
            
            headerStyle = list(
              whiteSpace = "normal",
              wordBreak = "normal",
              lineHeight = "1.2",
              paddingBottom = "4px"
            ),
            
            cell = function(value, index) {
              is_total <- table_data_3mo$Trust[index] == "Total"
              
              global_pct <- global_rate * 100
              est_pct   <- table_data_3mo$`Proportion DTA > 4 hours`[index] * 100
              
              lower_pct <- table_data_3mo$Lower[index] * 100
              upper_pct <- table_data_3mo$Upper[index] * 100
              width_pct <- upper_pct - lower_pct
              
              # Determine if CI should be suppressed (suppress if total range is < 2.5%)
              show_ci <- !is_total && (width_pct >= 2.5)
              
              # Create standard layout wrapper
              div(style = "padding: 0 24px; width: 100%;",
                  div(
                    # Height increased to 48px to cleanly separate labels above and below
                    style = "position: relative; width: 100%; height: 48px; display: flex; align-items: center; margin-top: 12px; margin-bottom: 12px;",
                    
                    # A baseline axis line spanning 0 to 100%
                    div(style = "position: absolute; left: 0; right: 0; height: 1px; background-color: #cbd5e1; z-index: 1;"),
                    
                    # Conditional CI (Only drawn if wide enough to prevent clutter)
                    if (show_ci) {
                      tagList(
                        # Light blue-gray CI background bar
                        div(style = sprintf(
                          "position: absolute; left: %s%%; width: %s%%; height: 4px; background-color: #cbd5e1; border-radius: 2px; z-index: 2; opacity: 0.6;",
                          lower_pct, width_pct
                        )),
                        # CI left cap
                        div(style = sprintf("position: absolute; left: %s%%; width: 1px; height: 10px; background-color: #94a3b8; transform: translateX(-50%%); z-index: 2;", lower_pct)),
                        # CI right cap
                        div(style = sprintf("position: absolute; left: %s%%; width: 1px; height: 10px; background-color: #94a3b8; transform: translateX(-50%%); z-index: 2;", upper_pct))
                      )
                    } else {
                      NULL
                    },
                    
                    # The Gap Bar (Not displayed for the Total/National row itself)
                    if (!is_total) {
                      gap_left <- min(global_pct, est_pct)
                      gap_width <- abs(global_pct - est_pct)
                      # Green if below national (fewer delays = good), Red if above (more delays = bad)
                      gap_color <- if (est_pct < global_pct) "#22c55e" else "#ef4444"
                      
                      div(style = sprintf(
                        "position: absolute; left: %s%%; width: %s%%; height: 6px; background-color: %s; border-radius: 3px; z-index: 3;",
                        gap_left, gap_width, gap_color
                      ))
                    } else {
                      NULL
                    },
                    
                    # Trust Dot (Blue marker)
                    div(style = sprintf(
                      "position: absolute; left: %s%%; width: 12px; height: 12px; background-color: #2563eb; border-radius: 50%%; transform: translateX(-50%%); z-index: 5; box-shadow: 0 0 0 2px #fff;",
                      est_pct
                    )),
                    
                    # Trust Value Label (Floats above the dot)
                    div(
                      style = sprintf(
                        "position: absolute; left: %s%%; top: -16px; transform: translateX(-50%%); font-size: 13px; font-weight: 700; color: #1e3a8a; z-index: 5; white-space: nowrap;",
                        est_pct
                      ), 
                      if (is_total) "National Average" else sprintf("%.1f%%", est_pct)
                    ),
                    
                    # National Reference Line (Slate vertical tick mark, not needed on the Total row)
                    if (!is_total) {
                      div(style = sprintf(
                        "position: absolute; left: %s%%; width: 2px; height: 16px; background-color: #475569; transform: translateX(-50%%); z-index: 4;",
                        global_pct
                      ))
                    } else {
                      NULL
                    },
                    
                    # National Reference Label (Floats below the tick mark)
                    if (!is_total) {
                      div(
                        style = sprintf(
                          "position: absolute; left: %s%%; bottom: -14px; transform: translateX(-50%%); font-size: 11px; font-weight: 600; color: #475569; z-index: 4; white-space: nowrap;",
                          global_pct
                        ),
                        sprintf("National (%.1f%%)", global_pct)
                      )
                    } else {
                      NULL
                    }
                  )
              )
            }
          )
        )
      )

      
    })

  })
}