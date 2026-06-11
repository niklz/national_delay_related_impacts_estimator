# modules/mod_deepdive.R

deepDiveUI <- function(id, SPINNER_TYPE) {
ns <- NS(id)

  nav_panel(
    title = "Historical Trends",

    layout_columns(
      col_widths = c(3, 9),

      # ==========================================
      # CARD 1: Controls & Selectors
      # ==========================================
      card(
        card_header("Filter Options"),
        card_body(
          selectInput(
            inputId = ns("geo_level"),
            label = "Select Grouping Level:",
            choices = c("Region" = "region", "ICB" = "icb", "Trust" = "trust"),
            selected = "trust"
          ),
          
          tags$hr(),
          
          selectizeInput(
            inputId = ns("selected_entities"),
            label = "Select up to 5 options:",
            choices = NULL, 
            multiple = TRUE,
            options = list(
              maxItems = 5,
              placeholder = 'Type to search...',
              plugins = list('remove_button')
            )
          )
        )
      ),

      # ==========================================
      # CARD 2: Timeseries Bar Chart (GGIRAPH)
      # ==========================================
      card(
        full_screen = TRUE,
        card_header("Estimated DRD (Past 6 Months)"),
        card_body(
          style = "padding: 1rem;",
          withSpinner(
            # Using girafeOutput instead of plotlyOutput
            girafeOutput(ns("drd_barcode_plot"), height = "100%"),
            type = SPINNER_TYPE,
            color = "#003087",
            size = 0.7
          )
        )
      )
    )
  )
}


deepDiveServer <- function(id, ts_data) {
  moduleServer(id, function(input, output, session) {
   ns <- session$ns

    # --------------------------------------------------------------------------
    # 1. DYNAMIC SELECTIZE CHOICES
    # --------------------------------------------------------------------------
    observeEvent(input$geo_level, {
      req(ts_data())
      
      filtered_choices <- ts_data() %>%
        filter(Level == input$geo_level) %>%
        pull(Group_Name) %>%
        unique() %>%
        sort()
      
      updateSelectizeInput(
        session = session,
        inputId = "selected_entities",
        choices = filtered_choices,
        server = TRUE
      )
    })

    # --------------------------------------------------------------------------
    # 2. TIMESERIES PLOT RENDERING (GGIRAPH)
    # --------------------------------------------------------------------------
    output$drd_barcode_plot <- renderGirafe({
      req(ts_data(), input$geo_level, input$selected_entities)
      
      # Filter and clean up data structures
      plot_df <- ts_data() %>%
        filter(
      Level == input$geo_level,
      Group_Name %in% input$selected_entities
    ) %>%
      filter(Month_Date >= (max(Month_Date, na.rm = TRUE) %m-% months(5))) %>%
        # Format month as an ordered string factor so ggplot handles x-axis layout correctly
      mutate(Month_Label = format(Month_Date, "%b %y")) %>%
        mutate(Month_Label = fct_reorder(Month_Label, Month_Date))
      
      validate(
        need(nrow(plot_df) > 0, "No historical data found for the selections.")
      )
      
      browser()
      # Build standard static ggplot object using interactive geoms
      gg <- ggplot(
        data = plot_df, 
        aes(
          x = Month_Label, 
          y = `Estimated DRD`, 
          fill = Group_Name,
          # Custom tooltip construction using standard HTML line breaks
          tooltip = paste0(
            "<div style='font-family: sans-serif; padding: 5px;'>",
            "<strong>", Group_Name, "</strong><br/>",
            "Month: ", format(Month_Date, "%B %Y"), "<br/>",
            "Estimated DRD: ", scales::comma(`Estimated DRD`),
            "</div>"
          ),
          # data_id triggers beautiful group elements highlighting together on hover
          data_id = Group_Name
        )
      ) +
        # Dynamic interactive bar plot grouped together per month
        geom_bar_interactive(
          stat = "identity", 
          position = position_dodge(width = 0.8),
          width = 0.7,
          color = "white",
          linewidth = 0.2
        ) +
        scale_fill_brewer(palette = "Set2") +
        theme_minimal(base_size = 13) +
        theme(
          plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
          axis.title.x = element_blank(),
          axis.title.y = element_text(color = "#475569", size = 11, face = "bold"),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(color = "#f1f5f9"),
          panel.grid.minor = element_blank(),
          legend.position = "bottom",
          legend.title = element_blank()
        ) +
        labs(y = "Estimated DRD")

      # Generate the final interactive HTML widget output
      girafe(
        ggobj = gg,
        width_svg = 7,
        height_svg = 4,
        options = list(
          opts_tooltip(
            css = "background-color: #1e293b; color: #ffffff; border-radius: 6px; padding: 4px;",
            opacity = 0.95
          ),
          opts_hover(
            css = "opacity: 1; stroke: #003087; stroke-width: 1.5px;"
          ),
          opts_hover_inv(
            css = "opacity: 0.35;" # Dims other entities smoothly when focused on one
          ),
          opts_toolbar(saveaspng = FALSE) # Keeps dashboard visual space crisp
        )
      )
    })
    
  })
}