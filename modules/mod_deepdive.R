# modules/mod_deepdive.R

deepDiveUI <- function(id, SPINNER_TYPE) {
ns <- NS(id)

  nav_panel(
    title = "Historical Trends",

    layout_columns(
      col_widths = c(2, 10),

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
            selected = "region"
          ),
          
          tags$hr(),
          
          shinyWidgets::virtualSelectInput(
            inputId = ns("selected_entities"),
            label = "Select up to 5 options:",
            choices = NULL, 
            multiple = TRUE,
            maxValues = 5,
            placeholder = 'Type to search...',
            selected = "Total"
          )
        )
      ),

      # ==========================================
      # CARD 2: Timeseries Bar Chart (GGIRAPH)
      # ==========================================
      card(
        full_screen = TRUE,
        card_header("Estimated DRD (Past 12 Months)"),
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
      
      shinyWidgets::updateVirtualSelect(
        session = session,
        inputId = "selected_entities",
        choices = current_choices
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
        filter(Month_Date >= (max(Month_Date, na.rm = TRUE) %m-% months(11)))

      validate(
        need(nrow(plot_df) > 0, "No historical data found for the selections.")
      )

      # --- Configuration ---
      axis_shade <- "grey40"
      col_width  <- 20
      num_selected <- length(input$selected_entities)
     

      pal <- paletteer_d("lisa::ClaudeMonet_1")
      
      # 1. DYNAMIC SVG HEIGHT SETUP
      # Base canvas size scales lineally with selections to maintain layout spacing
      calculated_height <- 1.5 + (num_selected * 1.5)

      # 2. CONSTANT TEXT SIZE CALCULATION
      # As the SVG canvas gets taller, ggplot shrinks elements to fit.
      # We introduce a scalar multiplier to keep the text exactly uniform.
      font_scalar <- 1
       geom_text_size <- 3.8 * font_scalar
      
      # Base sizes matching your aesthetic
      b_s        <- 11 * font_scalar
      label_pos  <- -0.4 
      
      max_y <- max(plot_df$`Estimated DRD`, na.rm = TRUE) * 1.35
      if(max_y == 0) max_y <- 10

      # --- Clean Theme Layout using rel() mappings ---

      title_styling <- theme(
        plot.title = element_text(
          face = "bold",
          size = rel(1.2), # Make it larger
          hjust = 0.5, # Center align
          margin = margin(b = 5) # Add slight padding below
        ),
        plot.subtitle = element_text(
          color = "grey40", # Make it subtle
          size = rel(0.8),
          hjust = 0.5, # Center align
          margin = margin(b = 15) # Add padding before the chart starts
        )
      )

      shared_theme <- theme_minimal(base_size = b_s) +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.y      = element_blank(),
          axis.text.x      = element_text(size = rel(1.1)), # Stabilized via scalar
          axis.ticks.y     = element_blank(),
          axis.title       = element_blank(),
          axis.line.x      = element_line(color = axis_shade, linewidth = 1),
          strip.background = element_blank(),
          strip.text       = element_text(size = rel(0.8), face = "bold", hjust = 0.5),
          strip.placement  = "outside",
          panel.spacing.y  = unit(2 / font_scalar, "lines"), # Keeps vertical gap clean
          legend.position = "none"
        ) +
          title_styling
      # --- Constructing the Plot ---
      p_mort <- ggplot(
        plot_df,
        aes(x = Month_Date, y = `Estimated DRD`, fill = Group_Name)
      ) +
        geom_col_interactive(
          aes(
            tooltip = paste0(
              "<strong>",
              Group_Name,
              "</strong><br/>",
              "Month: ",
              format(Month_Date, "%B %Y"),
              "<br/>",
              "DRD: ",
              scales::comma(`Estimated DRD`)
            ),
            data_id = paste0(Group_Name, "_", Month_Date)
          ),
          width = col_width
        ) +
        # Applying the font scalar directly to geom_text
        geom_text_interactive(
          aes(
            label = round(`Estimated DRD`),
            data_id = paste0(Group_Name, "_", Month_Date)
          ),
          size = geom_text_size, # <-- Unified size applied here
          vjust = label_pos
        ) +
        scale_fill_manual(values = pal) +
        scale_y_continuous(limits = c(0, max_y), expand = c(0, 0)) +
        scale_x_date(
          breaks = unique(plot_df$Month_Date),
          labels = function(x) {
            ifelse(
              lubridate::month(x) == 1,
              format(x, "%b\n%Y"),
              format(x, "%b")
            )
          }
        ) +
        labs(
          title = "Estimated monthly delay-related deaths",
          subtitle = "(number of deaths that wouldn't have occurred if zero admission delays)",
          x = NULL,
          y = NULL
        ) +
        facet_wrap2(
          ~Group_Name,
          ncol = 1,
          axes = "x",
          strip.position = "bottom",
          strip = strip_themed(
            text_x = elem_list_text(color = pal)
          )
        ) +
        shared_theme 
  

# 1. Define a generous offset based on your data range 
max_val <- max(plot_df$`Estimated excess bed utilisation`)
x_offset <- max_val * 0.09   
max_x_bed <- max_val * 1.25  

p_bed <- plot_df %>%
  summarise(
    `Estimated excess bed utilisation` = mean(
      `Estimated excess bed utilisation`
    ),
    .by = Group_Name
  ) %>%
  ggplot(aes(
    y = fct_rev(Group_Name)
  )) +
  geom_col(
    aes(
      x = `Estimated excess bed utilisation`,
      fill = Group_Name,
      color = Group_Name
    ),
    width = 0.3
  ) +

# 2. Value Label
  geom_text(
    aes(
      x = `Estimated excess bed utilisation` + x_offset,
      label = round(`Estimated excess bed utilisation`, 0)
    ),
    vjust = 1.3,
    hjust = 0.5,
    size = geom_text_size, # <-- Unified size applied here
    show.legend = FALSE
  ) +

  # 3. Bed Icon Label
  geom_text(
    aes(x = `Estimated excess bed utilisation` + x_offset),
    label = fontawesome("fa-bed"),
    family = "fontawesome-webfont",
    vjust = -0.3,
    hjust = 0.5, 
    size = geom_text_size, # <-- Unified size applied here
    show.legend = FALSE
  ) +
    labs(
          title = "Estimated avoidable acute bed utilisation",
          subtitle = "(average number of acute beds in use at any time attributable solely to admission delays)"
    ) +

  scale_x_continuous(limits = c(0, max_x_bed), expand = c(0, 0)) +
  scale_fill_manual(values = pal) +
  scale_colour_manual(values = pal) +
  theme_minimal(base_size = b_s) +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.line.y = element_line(color = axis_shade, linewidth = 1.5),
    legend.position = "none"
  ) +
  title_styling

      
      gg <- (p_mort | p_bed) + plot_layout(widths = c(3, 2))

      # --- Render HTML Widget ---
      girafe(
        ggobj = gg,
        width_svg = 14,
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