# modules/mod_tables.R

glanceUI <- function(id, min_date, max_date, SPINNER_TYPE) {
  ns <- NS(id)

  nav_panel(
    title = "At a glance",

    layout_columns(
      col_widths = c(6, 3, 3), # Keeps the consistent 3-column architecture

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
      # CARD 2: Top growers (DT)
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
      # CARD 3: Top shrinkers (DT)
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
    
# ==========================================================================
    # OVERVIEW TABLE (Reactable Implementation with Container Fix)
    # ==========================================================================
    output$overview_table <- renderReactable({
      req(table_data)
      
      display_df <- select(table_data, -`Percent change`)
      
      # Calculate bar scaling limits dynamically once per table evaluation
      valid_rows <- display_df %>% filter(Trust != "Total")
      max_admissions <- max(valid_rows$`Total admissions`, na.rm = TRUE)
      max_dta        <- max(valid_rows$`Number of DTA > 4 hours`, na.rm = TRUE)
      
      reactable(
        display_df,
        pagination = FALSE,
        filterable = TRUE,
        highlight = TRUE,
        defaultColDef = colDef(align = "center"),

        # 1. FORCE THE ENGINE TO BE FLUID
        fullWidth = TRUE,

        # 2. THE CONTAINER FIX: Prevent the wrapper from generating scrollbars
        # and force the internal HTML table to respect the card's width boundaries.
        elementId = "overview-reactable",
        style = list(
          width = "100%",
          overflowX = "hidden"
        ),

        rowStyle = function(index) {
          # 1. Keep your distinct formatting for the Total summary row
          if (display_df$Trust[index] == "Total") {
            return(list(fontWeight = "bold", background = "#e2e8f0")) # Slightly darker slate for high contrast
          }

          # 2. Zebra stripe alternating data rows (Even rows get a soft tint)
          if (index %% 2 == 0) {
            return(list(background = "#f8fafc")) # Soft off-white/slate tint
          }

          # Odd rows stay naturally white
          return(list(background = "#ffffff"))
        },

        # 3. CONVERT TO A 100-POINT BASE RATIO
        # By dropping the zeros (e.g., 340 becomes 34), these act as fluid percentage
        # weights rather than hard pixel minimums.
        columns = list(
          # Trust column (34% weight)
          Trust = colDef(
            name = "Trust",
            align = "left",
            minWidth = 34
          ),

          # Total admissions (19% weight)
          `Total admissions` = colDef(
            name = "Total admissions",
            minWidth = 19,
            cell = reactable_bar_formatter(max_admissions)
          ),

          # Number of DTA > 4 hours (19% weight)
          `Number of DTA > 4 hours` = colDef(
            name = "Number of DTA > 4 hours",
            minWidth = 19,
            cell = reactable_bar_formatter(max_dta)
          ),

          # Estimated DRD (10% weight)
          `Estimated DRD` = colDef(
            name = "Estimated DRD",
            minWidth = 10,
            cell = reactable_text_formatter()
          ),

          # Trend (18% weight)
          Trend = colDef(
            name = "Trend",
            minWidth = 18,
            cell = function(value, index) {
              is_total <- display_df$Trust[index] == "Total"
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
    # TOP GROWERS (DT implementation retained safely)
    # ==========================================================================
    output$top_growers <- renderDT({
      req(top_growers) 
      f_table <- formattable(
        top_growers,
        align = rep("l", ncol(top_growers)),
        list(
          Trust = formatter(
            "span",
            style = x ~ style(font.weight = ifelse(x == "Total", "bold", "normal"))
          ),
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
            list(width = '70%', targets = 0),
            list(width = '30%', targets = 1)
          )
        )
      )
    })

    # ==========================================================================
    # TOP SHRINKERS (DT implementation retained safely)
    # ==========================================================================
    output$top_shrinkers <- renderDT({
      req(top_shrinkers) 
      f_table <- formattable(
        top_shrinkers,
        align = rep("l", ncol(top_shrinkers)),
        list(
          Trust = formatter(
            "span",
            style = x ~ style(font.weight = ifelse(x == "Total", "bold", "normal"))
          ),
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
            list(width = '70%', targets = 0),
            list(width = '30%', targets = 1)
          )
        )
      )
    })
    
  })
}