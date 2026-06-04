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
        card_header("Overview"),
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
      # CARD 2: Top growers (REACTABLE)
      # ==========================================
      card(
        full_screen = TRUE,
        card_header("Top worsening (past 3 months)"),
        card_body(
          style = "padding: 1rem;",
          div(
            style = "overflow-y: auto; max-height: 100%;",
            withSpinner(
              reactableOutput(ns("top_growers")), 
              type = SPINNER_TYPE,
              color = "#003087",
              size = 0.7
            )
          )
        )
      ),

      # ==========================================
      # CARD 3: Top shrinkers (REACTABLE)
      # ==========================================
      card(
        full_screen = TRUE,
        card_header("Top improving (past 3 months)"),
        card_body(
          style = "padding: 1rem;",
          div(
            style = "overflow-y: auto; max-height: 100%;",
            withSpinner(
              reactableOutput(ns("top_shrinkers")), 
              type = SPINNER_TYPE,
              color = "#003087",
              size = 0.7
            )
          )
        )
      )
    )#,
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
      
      display_df <- select(table_data, -`Percent change (DRD)`)
      
      valid_rows <- display_df %>% filter(Trust != "Total")
      max_admissions <- max(valid_rows$`Total admissions`, na.rm = TRUE)
      max_dta        <- max(valid_rows$`Number of DTA > 4 hours`, na.rm = TRUE)
      
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
          
          `Total admissions` = colDef(name = "Total admissions", minWidth = 130, cell = reactable_bar_formatter(max_admissions)),
          `Number of DTA > 4 hours` = colDef(name = "Number of DTA > 4 hours", minWidth = 140, cell = reactable_bar_formatter(max_dta)),
          
          # FIX: Explicit line-height control and normal wrapping rules forces 
          # phrases to stack vertically rather than clip words.
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
            headerStyle = list(whiteSpace = "normal", wordBreak = "normal", lineHeight = "1.2"),
            cell = function(value, index) {
              is_total <- display_df$Trust[index] == "Total"
              text_color <- case_when(
                grepl("Decline", value) ~ "#166534",
                grepl("Growth|Increase", value) ~ "#991b1b",
                TRUE ~ "#475569"
              )
              tags$span(style = list(color = text_color, fontWeight = if (is_total) "bold" else "normal"), value)
            }
          )
        )
      )
    })

    # ==========================================================================
    # TOP GROWERS (Reactable Implementation)
    # ==========================================================================
    output$top_growers <- renderReactable({
      req(top_growers)
      
      # FIX: Jittering logic completely dropped. Using pristine upstream data.
      max_pct <- max(abs(top_growers$`Percent change (DRD)`), na.rm = TRUE)
      
      reactable(
        top_growers,
        pagination = FALSE,
        filterable = TRUE,
        highlight = TRUE,
        defaultColDef = colDef(align = "center"),
        fullWidth = TRUE,
        style = list(width = "100%", overflowX = "hidden"),
        
        rowStyle = function(index) {
          if (top_growers$Trust[index] == "Total") return(list(fontWeight = "bold", background = "#e2e8f0"))
          if (index %% 2 == 0) return(list(background = "#f8fafc"))
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
            cell = reactable_percent_bar_formatter(max_pct)
          )
        )
      )
    })

    # ==========================================================================
    # TOP SHRINKERS (Reactable Implementation)
    # ==========================================================================
    output$top_shrinkers <- renderReactable({
      req(top_shrinkers)
      
      # FIX: Jittering logic completely dropped. Using pristine upstream data.
      max_pct <- max(abs(top_shrinkers$`Percent change (DRD)`), na.rm = TRUE)
      
      reactable(
        top_shrinkers,
        pagination = FALSE,
        filterable = TRUE,
        highlight = TRUE,
        defaultColDef = colDef(align = "center"),
        fullWidth = TRUE,
        style = list(width = "100%", overflowX = "hidden"),
        
        rowStyle = function(index) {
          if (top_shrinkers$Trust[index] == "Total") return(list(fontWeight = "bold", background = "#e2e8f0"))
          if (index %% 2 == 0) return(list(background = "#f8fafc"))
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
            cell = reactable_percent_bar_formatter(max_pct)
          )
        )
      )
    })
  # # ==========================================================================
  #   # Catch the browser signal and flip the global ready state
  #   # ==========================================================================
  #   observeEvent(input$tables_rendered, {
  #     session$userData$landing_page_ready <- TRUE
  #   }, once = TRUE) # Executes exactly once on startup
    
  })
}