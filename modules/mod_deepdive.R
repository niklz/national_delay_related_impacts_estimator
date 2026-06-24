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
            choices = c(
              "Region" = "region",
              "ICB / cluster" = "cluster",
              "Trust" = "trust"
            ),
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
            updateOn = "close"
          )
        )
      ),

      # ==========================================
      # CARD 2: Timeseries Bar Chart (GGIRAPH)
      # ==========================================
      card(
        full_screen = TRUE,
        card_body(
          class = "d-flex flex-column align-items-stretch content-card-body",
          style = "overflow: hidden !important; padding: 1rem; min-height: 0 !important;",

          # --- ADDED: Local CSS overrides for the wide plot ---
          tags$style(HTML(sprintf(
            "
            /* Break the global 6:5 ratio for this 10-col widescreen plot */
            .%1$s .html-widget.girafe svg {
              aspect-ratio: 21 / 9 !important; 
              max-height: 100%% !important;
            }
            /* Force the plot to align flush with the top instead of centering */
            .%1$s .html-widget.girafe > div {
              align-items: flex-start !important;
            }
            /* Replicate the negative margin pull-up used by Page 1's slider */
            .%1$s {
              margin-top: -10px !important;
            }
          ",
            "barcode-plot-wrapper"
          ))),

          # Wrapped spinner in an auto-flexing container box with the new override class
          div(
            class = "barcode-plot-wrapper", # <-- Applied custom class here
            style = "flex: 1 1 auto; width: 100%; min-height: 0; overflow: hidden;",
            withSpinner(
              girafeOutput(
                ns("drd_barcode_plot"),
                width = "100%",
                height = "100%"
              ),
              type = SPINNER_TYPE,
              color = "#003087",
              size = 0.7
            )
          ),

          # Pinned standard caption block
          div(
            class = "content-caption",
            tags$strong("Figure 4: "),
            HTML(
              "Monthly delay related excess mortality and excess bed utilisation."
            )
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

      # Determine if "Total" exists in this group layer; if not, grab the first element
      default_selection <- if ("Total" %in% current_choices) {
        "Total"
      } else {
        current_choices[1]
      }

      shinyWidgets::updateVirtualSelect(
        session = session,
        inputId = "selected_entities",
        choices = current_choices,
        selected = default_selection # <-- FIX 2: Explicitly handles selection during server updates
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
      col_width <- 20
      num_selected <- length(input$selected_entities)

      pal <- paletteer_d("lisa::ClaudeMonet_1")

      # REMOVED: Dynamic calculated_height canvas blocks to stop fighting the CSS.
      font_scalar <- 1
      geom_text_size <- 3.8 * font_scalar
      b_s <- 11 * font_scalar
      label_pos <- -0.4

      max_y <- max(plot_df$`Estimated DRD`, na.rm = TRUE) * 1.35
      if (max_y == 0) {
        max_y <- 10
      }

      # --- Clean Theme Layout using rel() mappings ---
      title_styling <- theme(
        plot.title = element_text(
          face = "bold",
          size = rel(1.2),
          hjust = 0.5,
          margin = margin(b = 5)
        ),
        plot.subtitle = element_text(
          color = "grey40",
          size = rel(0.8),
          hjust = 0.5,
          margin = margin(b = 15)
        )
      )

      shared_theme <- theme_minimal(base_size = b_s) +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = rel(1.1)),
          axis.ticks.y = element_blank(),
          axis.title = element_blank(),
          axis.line.x = element_line(color = axis_shade, linewidth = 0.8),
          strip.background = element_blank(),
          strip.text = element_text(
            size = rel(0.8),
            face = "bold",
            hjust = 0.5
          ),
          strip.placement = "outside",
          panel.spacing.y = unit(2 / font_scalar, "lines"),
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
        geom_text_interactive(
          aes(
            label = round(`Estimated DRD`),
            data_id = paste0(Group_Name, "_", Month_Date)
          ),
          size = geom_text_size,
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
          strip = strip_themed(text_x = elem_list_text(color = pal))
        ) +
        shared_theme

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
        ggplot(aes(y = 1)) +
        geom_col(
          aes(
            x = `Estimated excess bed utilisation`,
            fill = Group_Name,
            color = Group_Name
          ),
          width = 0.3,
          orientation = "y"
        ) +
        geom_text(
          aes(
            x = `Estimated excess bed utilisation` + x_offset,
            y = 1,
            label = round(`Estimated excess bed utilisation`, 0)
          ),
          vjust = 1.3,
          hjust = 0.5,
          size = geom_text_size,
          show.legend = FALSE
        ) +
        geom_text(
          aes(x = `Estimated excess bed utilisation` + x_offset, y = 1),
          label = fontawesome("fa-bed"),
          family = "fontawesome-webfont",
          vjust = -0.3,
          hjust = 0.5,
          size = geom_text_size,
          show.legend = FALSE
        ) +
        labs(
          title = "Estimated avoidable acute bed utilisation",
          subtitle = "(average number of acute beds in use at any time attributable solely to admission delays)"
        ) +
        scale_x_continuous(limits = c(0, max_x_bed), expand = c(0, 0)) +
        scale_y_continuous(limits = c(0.5, 1.5), expand = c(0, 0)) +
        scale_fill_manual(values = pal) +
        scale_colour_manual(values = pal) +
        facet_wrap2(~Group_Name, ncol = 1, strip.position = "bottom") +
        theme_minimal(base_size = b_s) +
        theme(
          panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.line.y = element_line(color = axis_shade, linewidth = 0.8),
          legend.position = "none",
          panel.spacing.y = unit(2 / font_scalar, "lines"),
          strip.text = element_text(
            size = rel(0.8),
            face = "bold",
            color = "transparent"
          ),
          strip.background = element_blank()
        ) +
        title_styling

      gg <- (p_mort | p_bed) + plot_layout(widths = c(3, 2))

      browser()
      # --- Render HTML Widget ---
      girafe(
        ggobj = gg,
        # MATCHED LOGIC: Set fixed bounds that exactly match your global CSS 6:5 aspect ratio rule
        width_svg = 12.0,
        height_svg = 10.0,
        options = list(
          opts_tooltip(
            css = "background-color: #1e293b; color: #ffffff; border-radius: 6px; padding: 6px; font-family: sans-serif;",
            opacity = 0.95
          ),
          opts_hover(css = "fill: #93c5fd; cursor: pointer;"),
          opts_toolbar(saveaspng = FALSE),
          opts_sizing(rescale = TRUE, width = 1)
        )
      )
    })
  })
}