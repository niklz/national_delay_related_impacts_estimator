library(shiny)
library(bslib)
library(reactable)
library(htmltools)
library(dplyr)
library(lubridate)
library(scales)

# ==========================================
# UI FUNCTION
# ==========================================
glanceUI <- function(id, min_date, max_date, SPINNER_TYPE, title) {
  ns <- NS(id)
  
  nav_panel(
    title = title,
    icon = icon("table"),
    card(
      fill = FALSE, 
      style = "background-color: #F4F6F9; box-shadow: none;",
      
      card_body(
        fill = FALSE, 
        style = "padding: 0.75rem 1rem;", # Snug padding
        
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
    
    # ==========================================
    # SINGLE CARD LAYOUT WITH TABS ON TOP
    # ==========================================
    navset_card_tab(
      full_screen = TRUE,
      
      # TAB 1: Latest Reporting Period Overview
      nav_panel(
        title = "Latest Monthly A&E Admission Data",
        # icon = icon("calendar-day"),
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
          )
        )
      ),
      
      # TAB 2: 3-Month Performance vs National
      nav_panel(
        title = "3-Month Admission Time Performance vs National",
        # icon = icon("chart-line"),
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
    )
  )
}

# ==========================================
# SERVER FUNCTION
# ==========================================
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
        fullWidth = TRUE,
        style = list(width = "100%", overflowX = "hidden"),
        
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
          Trust = colDef(name = "Trust", align = "left", minWidth = 200),
          
          `Total admissions` = colDef(
            name = "Total admissions",
            minWidth = 100,
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
            minWidth = 140,
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
    # 3 MONTH PERFORMANCE TABLE (With National-vs-Trust Visual Gaps)
    # ==========================================================================
    output$perf_3month <- renderReactable({
      req(table_data_3mo)
      
      # 1. Statistical Calculations (z = 3 for 3-sigma control limits)
      z <- 3 
      
      # Safe division for n=0 cases
      n <- pmax(1, table_data_3mo$`Total admissions`) 
      p <- table_data_3mo$`Proportion DTA > 4 hours`
      
      denominator <- 1 + (z^2 / n)
      center <- (p + (z^2 / (2 * n))) / denominator
      spread <- (z * sqrt(pmax(0, (p * (1 - p) / n) + (z^2 / (4 * n^2))))) / denominator
      
      table_data_3mo$Lower <- pmax(0, center - spread)
      table_data_3mo$Upper <- pmin(1, center + spread)
      
      # Calculate "Delays Prevented" (Net Impact vs National Rate)
      global_rate <- table_data_3mo$`Number of DTA > 4 hours`[table_data_3mo$Trust == "Total"] / pmax(1, table_data_3mo$`Total admissions`[table_data_3mo$Trust == "Total"])
      table_data_3mo$Expected <- table_data_3mo$`Total admissions` * global_rate
      
      # Positive = Fewer delays than expected (Good). Negative = More delays (Bad).
      table_data_3mo$Net_Timely <- table_data_3mo$Expected - table_data_3mo$`Number of DTA > 4 hours`
      
      # Sort: 'Total' at TOP, sort the rest by Net Timely Admissions (Descending)
      is_not_total <- table_data_3mo$Trust != "Total"
      table_data_3mo <- table_data_3mo[order(is_not_total, -table_data_3mo$Net_Timely), ]
      
      table_data_3mo$Visual <- NA 
      
      # 2. Render Table
      reactable(
        table_data_3mo,
        pagination = FALSE,
        filterable = TRUE,
        highlight = TRUE,
        fullWidth = TRUE,
        style = list(width = "100%", fontSize = "16px"),
        
        theme = reactableTheme(
          cellPadding = "16px 12px"
        ),
        
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
          if (table_data_3mo$Trust[index] == "Total") {
            return(list(fontWeight = "bold", background = "#e2e8f0"))
          }
          if (index %% 2 == 0) {
            return(list(background = "#f1f5f9")) # Clean gray striping
          }
          return(list(background = "#ffffff"))
        },
        
        columns = list(
          Trust = colDef(name = "Trust", align = "left", minWidth = 180),
          
          # Hiding this column as it's represented visually and via Net_Timely
          `Number of DTA > 4 hours` = colDef(show = FALSE),
          
          # Whole-number display with thousands separators, keeping native numeric sort intact
          `Total admissions` = colDef(
            name = "Total Admissions", 
            minWidth = 100,
            format = colFormat(separators = TRUE, digits = 0)
          ),
          
          # Hide analytical support variables
          `Proportion DTA > 4 hours` = colDef(show = FALSE),
          `Estimated Delay-Related Deaths` = colDef(show = FALSE),
          `Estimated deaths per thousand admissions` = colDef(show = FALSE),
          Lower = colDef(show = FALSE),
          Upper = colDef(show = FALSE), 
          Expected = colDef(show = FALSE),
          
          # Delays Prevented metric (styled + colorized whole numbers)
          Net_Timely = colDef(
            name = "Additional Timely-Admissions (vs National Performance)", 
            minWidth = 200,
            headerStyle = list(
              whiteSpace = "normal", wordBreak = "normal", lineHeight = "1.2", paddingBottom = "4px"
            ),
            cell = function(value, index) {
              if (table_data_3mo$Trust[index] == "Total") return("—") 
              if (is.na(value)) return("-")
              
              rounded_val <- round(value)
              sign_char <- if (rounded_val > 0) "+" else ""
              
              # Using accuracy = 1 safely to avoid division-by-zero crashes
              formatted_val <- paste0(sign_char, scales::comma(rounded_val, accuracy = 1))
              
              color <- if (rounded_val > 0) POS_CLR_LGT2 else if (rounded_val < 0) NEG_CLR_LGT2 else "#64748b"
              
              div(style = list(color = color, fontWeight = "bold"), formatted_val)
            }
          ),
          
          # The custom "Trust vs National" Gap & Confidence Visual Column
          Visual = colDef(
            name = ">4hr Admission Breach Performance vs National Average",
            minWidth = 200, 
            headerStyle = list(
              whiteSpace = "normal", wordBreak = "normal", lineHeight = "1.2", paddingBottom = "4px"
            ),
            
            cell = function(value, index) {
              is_total <- table_data_3mo$Trust[index] == "Total"
              
              global_pct <- global_rate * 100
              est_pct   <- table_data_3mo$`Proportion DTA > 4 hours`[index] * 100
              
              lower_pct <- table_data_3mo$Lower[index] * 100
              upper_pct <- table_data_3mo$Upper[index] * 100
              width_pct <- upper_pct - lower_pct
              
              # Smart CI Suppression: hide if the total range is narrower than 2.5%
              show_ci <- !is_total && (width_pct >= 2.5)
              
              div(style = "padding: 0 24px; width: 100%;",
                  div(
                    style = "position: relative; width: 100%; height: 20px; display: flex; align-items: center; margin-top: 12px; margin-bottom: 12px;",
                    
                    # Axis reference line
                    div(style = "position: absolute; left: 0; right: 0; height: 1px; background-color: #cbd5e1; z-index: 1;"),
                    
                    # 1. Error interval (CI)
                    if (show_ci) {
                      tagList(
                        div(style = sprintf(
                          "position: absolute; left: %s%%; width: %s%%; height: 4px; background-color: #cbd5e1; border-radius: 2px; z-index: 2; opacity: 0.6;",
                          lower_pct, width_pct
                        )),
                        div(style = sprintf("position: absolute; left: %s%%; width: 1px; height: 10px; background-color: #94a3b8; transform: translateX(-50%%); z-index: 2;", lower_pct)),
                        div(style = sprintf("position: absolute; left: %s%%; width: 1px; height: 10px; background-color: #94a3b8; transform: translateX(-50%%); z-index: 2;", upper_pct))
                      )
                    } else {
                      NULL
                    },
                    
                    # 2. Performance Gap Bar
                    if (!is_total) {
                      gap_left <- min(global_pct, est_pct)
                      gap_width <- abs(global_pct - est_pct)
                      # Green = below average (fewer delays), Red = above average (more delays)
                      gap_color <- if (est_pct < global_pct) POS_CLR_LGT else NEG_CLR_LGT
                      
                      div(style = sprintf(
                        "position: absolute; left: %s%%; width: %s%%; height: 6px; background-color: %s; border-radius: 3px; z-index: 3;",
                        gap_left, gap_width, gap_color
                      ))
                    } else {
                      NULL
                    },
                    
                    # 3. Trust Rate Dot (Blue marker)
                    div(style = sprintf(
                      "position: absolute; left: %s%%; width: 12px; height: 12px; background-color: #2563eb; border-radius: 50%%; transform: translateX(-50%%); z-index: 5; box-shadow: 0 0 0 2px #fff;",
                      est_pct
                    )),
                    
                    # 4. Trust Value Label (Above dot, formatted to 0 decimals)
                    div(
                      style = sprintf(
                        "position: absolute; left: %s%%; top: -16px; transform: translateX(-50%%); font-size: 13px; font-weight: 700; color: #1e3a8a; z-index: 5; white-space: nowrap;",
                        est_pct
                      ), 
                      if (is_total) "National Average" else sprintf("%.0f%%", est_pct)
                    ),
                    
                    # 5. National Tick Mark (Reference Line)
                    if (!is_total) {
                      div(style = sprintf(
                        "position: absolute; left: %s%%; width: 2px; height: 16px; background-color: #475569; transform: translateX(-50%%); z-index: 4;",
                        global_pct
                      ))
                    } else {
                      NULL
                    },
                    
                    # 6. National Label (Below line, formatted to 0 decimals)
                    if (!is_total) {
                      div(
                        style = sprintf(
                          "position: absolute; left: %s%%; bottom: -14px; transform: translateX(-50%%); font-size: 11px; font-weight: 600; color: #475569; z-index: 4; white-space: nowrap;",
                          global_pct
                        ),
                        sprintf("National (%.0f%%)", global_pct)
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