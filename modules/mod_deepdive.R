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
            choices = c("Region" = "region", "ICB / cluster" = "cluster", "Trust" = "trust"),
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


deepDiveServer <- function(id, ts_data, choices_list) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --------------------------------------------------------------------------
    # 1. INSTANT SELECTIZE CHOICES
    # --------------------------------------------------------------------------
    observeEvent(input$geo_level, {
      current_choices <- choices_list[[input$geo_level]]
      
      updateSelectizeInput(
        session = session,
        inputId = "selected_entities",
        choices = current_choices,
        server = TRUE
      )
    })

    # --------------------------------------------------------------------------
    # 2. TIMESERIES PLOT RENDERING (With Constant Font Scaling)
    # --------------------------------------------------------------------------
    output$drd_barcode_plot <- renderGirafe({
      req(ts_data(), input$geo_level, input$selected_entities)
      
      plot_df <- ts_data() %>%
        filter(
          Level == input$geo_level,
          Group_Name %in% input$selected_entities
        ) %>%
        filter(Month_Date >= (max(Month_Date, na.rm = TRUE) %m-% months(5)))

      validate(
        need(nrow(plot_df) > 0, "No historical data found for the selections.")
      )

      # --- Configuration ---
      axis_shade <- "grey40"
      col_width  <- 10
      num_selected <- length(input$selected_entities)
      
      # 1. DYNAMIC SVG HEIGHT SETUP
      # Base canvas size scales lineally with selections to maintain layout spacing
      calculated_height <- 1.5 + (num_selected * 1.5)

      # 2. THE SECRET SAUCE: CONSTANT TEXT SIZE CALCULATION
      # As the SVG canvas gets taller, ggplot shrinks elements to fit.
      # We introduce a scalar multiplier to keep the text exactly uniform.
      font_scalar <- num_selected ^ 0.45 
      
      # Base sizes matching your aesthetic
      b_s        <- 11 * font_scalar
      label_pos  <- -0.4 
      
      max_y <- max(plot_df$`Estimated DRD`, na.rm = TRUE) * 1.35
      if(max_y == 0) max_y <- 10

      # --- Clean Theme Layout using rel() mappings ---
      shared_theme <- theme_minimal(base_size = b_s) +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.y      = element_blank(),
          axis.text.x      = element_text(size = rel(1.1)), # Stabilized via scalar
          axis.ticks.y     = element_blank(),
          axis.title       = element_blank(),
          axis.line.x      = element_line(color = axis_shade, linewidth = 1.5),
          strip.background = element_blank(),
          strip.text       = element_text(size = rel(1), face = "bold", hjust = 0.5),
          strip.placement  = "outside",
          panel.spacing.y  = unit(2 / font_scalar, "lines") # Keeps vertical gap clean
        )

      # --- Constructing the Plot ---
      gg <- ggplot(plot_df, aes(x = Month_Date, y = `Estimated DRD`)) +
        geom_col_interactive(
          aes(
            tooltip = paste0("<strong>", Group_Name, "</strong><br/>",
                             "Month: ", format(Month_Date, "%B %Y"), "<br/>",
                             "DRD: ", scales::comma(`Estimated DRD`)),
            data_id = paste0(Group_Name, "_", Month_Date)
          ),
          width = col_width, 
          fill = "#B4CEB3"
        ) +
        # Applying the font scalar directly to geom_text
        geom_text_interactive(
          aes(
            label = round(`Estimated DRD`),
            data_id = paste0(Group_Name, "_", Month_Date)
          ), 
          size = (0.7 * 11) * (font_scalar / 3), # Normalizes the ggplot pt-to-mm mapping
          vjust = label_pos
        ) +
        scale_y_continuous(limits = c(0, max_y), expand = c(0, 0)) +
        scale_x_date(
          breaks = unique(plot_df$Month_Date),
          labels = \(x) format(x, "%b %y")
        ) +
        labs(x = NULL, y = NULL) +
        facet_wrap(~Group_Name, ncol = 1, axes = "all_x", strip.position = "bottom") +
        shared_theme

      # --- Render HTML Widget ---
      girafe(
        ggobj = gg,
        width_svg = 8,
        height_svg = calculated_height, 
        options = list(
          opts_tooltip(
            css = "background-color: #1e293b; color: #ffffff; border-radius: 6px; padding: 6px; font-family: sans-serif;",
            opacity = 0.95
          ),
          opts_hover(css = "fill: #93c5fd; cursor: pointer;"),
          opts_toolbar(saveaspng = FALSE),
          # Force ggiraph to fill the card container responsively without breaking aspect structural integrity
          opts_sizing(rescale = TRUE, width = 1) 
        )
      )
    })
    
  })
}