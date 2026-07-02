# modules/mod_syscomp

sysCompUI <- function(id, SPINNER_TYPE, title) {
  ns <- NS(id)

  nav_panel(
    title = title,
    icon = icon("chart-line"),

    layout_columns(
      col_widths = c(2, 5, 5),

      # ==========================================
      # CARD 1: Controls & Selectors
      # ==========================================
      card(
        card_header("Select Region / ICB / Trust to compare:"),
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
            search = TRUE,
            maxValues = 5,
            placeholder = 'Type to search...',
            updateOn = "close"
          )
        )
      ),

      # ==========================================
      # CARD 2: Timeseries Analysis Stack
      # ==========================================
      card(
        # full_screen = TRUE,
        card_header(
          style = "display: flex; justify-content: center; align-items: center; text-align: center; min-height: auto; flex: 0 0 auto;",
          span(
            "Estimated monthly Delay-Related deaths per 1,000 admissions", # (Or appropriate text for Card 3)
            style = "font-weight: bold; font-size: 0.8vw; margin: 0;"
          )
        ),
        # REMOVED overflow: hidden !important from card_body to allow scrollbars to display naturally
        card_body(
          style = "padding: 0.5rem; display: flex; flex-direction: column; gap: 0; height: 100%;",

          tags$style(HTML(
            "
            /* Pinned Total view constraints */
            .total-container .html-widget.girafe, 
            .total-container .html-widget.girafe svg { 
              height: 100% !important; 
              max-height: 100% !important;
              width: 100% !important;
            }
            .total-container .html-widget.girafe > div { 
              align-items: center !important; 
              height: 100% !important;
            }
            
            /* Comparison Container: Let it overflow vertically and show scrollbars */
            .comparison-container {
              overflow-y: auto !important;
              overflow-x: hidden !important;
              flex-grow: 1;
              max-height: 66%;
            }
            .comparison-container .html-widget.girafe {
              height: auto !important;
              width: 100% !important;
            }
            .comparison-container .html-widget.girafe svg { 
              height: auto !important;
              max-height: none !important;
              aspect-ratio: auto !important;
            }
            .comparison-container .html-widget.girafe > div { 
              align-items: flex-start !important; 
              height: auto !important;
            }
            
            /* Bed Utilisation Container */
            .bed-container {
              flex: 1 1 auto !important;
              width: 100% !important;
              overflow-y: auto !important;
              overflow-x: hidden !important;
            }
            .bed-container .html-widget.girafe {
              height: auto !important;
              width: 100% !important;
            }
          "
          )),

          # Pinned Top Row: Allocated exactly 33% of card height (Never Scrolls)
          div(
            class = "total-container",
            style = "flex: 0 0 100%; height: 100%; max-height: 100%; width: 100%; padding-bottom: 0.25rem; overflow: hidden;",
            withSpinner(
              girafeOutput(
                ns("drd_plot"),
                width = "100%",
                height = "100%"
              ),
              type = SPINNER_TYPE,
              color = "#003087",
              size = 0.7
            )
          )
        ),
        card_footer(
          div(
            class = "content-caption",
            tags$strong("Figure 4: "),
            HTML(
              "Latest 12 months Delay-Related Deaths per 1,000 admissions. Top plot is national baseline for comparisson."
            )
          )
        )
      ),

      # ==========================================
      # CARD 3: Avoidable Bed Utilisation Chart
      # ==========================================
      card(
        # full_screen = TRUE,
        card_header(
          style = "display: flex; justify-content: center; align-items: center; text-align: center; min-height: auto; flex: 0 0 auto;",
          span(
            "Estimated avoidable acute bed utilisation", # (Or appropriate text for Card 3)
            style = "font-weight: bold; font-size: 0.8vw; margin: 0;"
          )
        ),
        card_body(
          style = "padding: 0.5rem; display: flex; flex-direction: column;",
          div(
            class = "bed-container",
            # Simplified flex styling to fill the card
            style = "flex: 1; min-height: 0; width: 100%;",
            withSpinner(
              # Changed height from "auto" to "100%"
              girafeOutput(ns("bed_plot"), width = "100%", height = "100%"),
              type = SPINNER_TYPE,
              color = "#003087",
              size = 0.7
            )
          )
        ),
        card_footer(
          div(
            class = "content-caption",
            tags$strong("Figure 5: "),
            HTML(
              "Delays in A&E are shown to increase acute length of stay. This bar chart displays the average number of acute beds in use at any time over the last 12-months attributable solely to admission delays."
            )
          )
        )
      )
    )
  )
}

sysCompServer <- function(id, ts_data, choices_list) {
  moduleServer(id, function(input, output, session) {
    session$onFlushed(
      function() {
        outputOptions(output, "drd_plot", suspendWhenHidden = FALSE)
      },
      once = TRUE
    )

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

    # Shared graphic configurations
    axis_shade <- "grey40"
    col_width <- 20
    pal <- paletteer_d("lisa::ClaudeMonet_1")

    # BUMPED UP FONT SIZES
    geom_text_size <- 5.0 # Increased from 3.8
    b_s <- 14 # Increased from 11
    label_pos <- -0.4
    aligned_margin <- margin(t = 10, r = 25, b = 10, l = 25, unit = "pt")

    shared_theme <- theme_minimal(base_size = b_s) +
      theme(
        plot.margin = aligned_margin,
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = rel(1.15)),
        axis.ticks.y = element_blank(),
        axis.title = element_blank(),
        axis.line.x = element_line(color = axis_shade, linewidth = 0.8),
        strip.background = element_blank(),
        strip.text = element_text(size = rel(1.2), face = "bold", hjust = 0.5),
        strip.placement = "outside",
        panel.spacing.y = unit(1.5, "lines"),
        legend.position = "none"
      )

    # --------------------------------------------------------------------------
    # 2. TOTAL PLOT RENDERING (Pinned, Flattened Anchor)
    # --------------------------------------------------------------------------
    output$drd_plot <- renderGirafe({
      req(ts_data())

      max_date <- max(ts_data()$Month_Date, na.rm = TRUE)
      raw_start <- max_date %m-% months(11)
      date_limits <- c(
        raw_start - lubridate::days(15),
        max_date + lubridate::days(15)
      )

      plot_df <- ts_data() %>%
        filter(
          Level == input$geo_level,
          Group_Name %in% input$selected_entities
        ) %>%
        filter(Month_Date >= raw_start) %>%
        bind_rows(
          ts_data() %>%
          filter(Level == "region", Group_Name == "Total", Month_Date >= raw_start) %>%
          mutate(Group_Name = "National/Baseline")
        ) %>%
        mutate(rate = (`Estimated DRD` / `Total Admissions`))

      validate(need(
        nrow(plot_df) > 0,
        "Select up to 5 elements to generate comparison panels."
      ))

      # --- DYNAMIC RANGE CALCULATIONS (Crucial for Shiny) ---
      min_date <- min(plot_df$Month_Date)
      max_date <- max(plot_df$Month_Date)
      date_span <- as.numeric(max_date - min_date)

      # Dynamically calculate the perfect horizontal jump (5% of the total timeline width)
      dynamic_nudge <- 0.05 * date_span

      label_data <- plot_df %>% filter(Month_Date == max_date)

      pal <- paletteer_d("MetBrewer::Juarez") %>%
        as.character() %>%
        set_names(label_data$Group_Name) %>%
        `[[<-`("National/Baseline", "cornsilk4")

      # --- THEME ---
      shared_theme <- theme_minimal(base_size = b_s) +
        theme(
          axis.title = element_blank(),
          axis.text = element_text(color = "grey40"),
          axis.text.x = element_text(size = 20, margin = margin(t = 5)),
          axis.text.y = element_text(size = 17, margin = margin(r = 5)),
          axis.ticks = element_line(color = "grey91", size = .5),
          axis.ticks.length.x = unit(1.3, "lines"),
          axis.ticks.length.y = unit(.7, "lines"),

          plot.margin = margin(10, 10, 10, 10),

          # plot.background = element_rect(fill = "grey98", color = "grey98"),
          # panel.background = element_rect(fill = "grey98", color = "grey98"),

          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(color = "grey91", size = 0.5),

          plot.title = element_text(
            color = "grey10",
            size = 28,
            face = "bold",
            margin = margin(t = 15)
          ),
          plot.subtitle = element_markdown(
            color = "grey30",
            size = 16,
            lineheight = 1.35,
            margin = margin(t = 15, b = 40)
          ),
          plot.title.position = "plot",
          plot.caption.position = "plot",
          plot.caption = element_text(
            color = "grey30",
            size = 13,
            lineheight = 1.2,
            hjust = 0,
            margin = margin(t = 40)
          ),
          legend.position = "none"
        )

      # --- PLOT ---
      drd_plot <- ggplot(
        plot_df,
        aes(x = Month_Date, y = round(1000 * rate, 1), color = Group_Name)
      ) +
        geom_segment(
          data = data.frame(y = 3:7),
          aes(x = min_date, xend = max_date, y = y, yend = y),
          color = "grey91",
          size = 0.5,
          inherit.aes = FALSE
        ) +
        geom_line_interactive(
          aes(
            group = Group_Name,
            data_id = Group_Name
          ),
          linewidth = 1.2
        ) +
        geom_point_interactive(
          aes(
            group = Group_Name,
            data_id = Group_Name,
            tooltip = paste0(
              "<strong>",
              Group_Name,
              "</strong><br/>",
              "Month: ",
              format(Month_Date, "%B %Y"),
              "<br/>",
              "Rate: ",
              round(1000 * rate, 1)
            ),
            size = 2
          )
        ) +
        ggrepel::geom_text_repel(
          data = label_data,
          aes(color = Group_Name, label = str_wrap(Group_Name, 25)),
          fontface = "bold",
          size = 7,
          direction = "y",
          lineheight = 0.9,
          hjust = 0, # Left-aligns the text box
          segment.size = .5,
          segment.alpha = .6,
          segment.linetype = "dotted",

          box.padding = 0.7,
          # force = 4,
          # max.overlaps = Inf,

          # FIX: Explicitly forces all labels to shift right by a proportion of the timeline
          nudge_x = dynamic_nudge
        ) +
        scale_colour_manual(values = pal) +
        scale_x_date(
          breaks = unique(plot_df$Month_Date),
          labels = function(x) {
            ifelse(
              lubridate::month(x) == 1,
              format(x, "%b\n%Y"),
              format(x, "%b")
            )
          },
          # Expands the gray panel canvas by 45% on the right to perfectly accommodate the text
          expand = expansion(mult = c(0.05, 0.45))
        ) +
        coord_cartesian(clip = "off") +
        labs(title = NULL, subtitle = NULL, x = NULL, y = NULL) +
        shared_theme

      girafe(
        ggobj = drd_plot,
        width_svg = 12.0,
        height_svg = 10.0,
        options = list(
          opts_tooltip(
            css = "background-color: #1e293b; color: #ffffff; padding: 6px; font-family: sans-serif;",
            opacity = 0.95
          ),
          opts_hover(css = "opacity:1.0; stroke-width:3px;"),
          opts_hover_inv(css = "opacity:0.1;"),
          opts_toolbar(
            hidden = c(
              'lasso_select',
              'lasso_deselect',
              'zoom_onoff',
              'zoom_rect',
              'zoom_reset',
              'fullscreen'
            )
          ),
          opts_sizing(rescale = TRUE, width = 1)
        )
      )
    })

    # --------------------------------------------------------------------------
    # 4. BED UTILISATION PLOT
    # --------------------------------------------------------------------------
    output$bed_plot <- renderGirafe({
      req(ts_data(), input$geo_level, input$selected_entities)

      plot_df <- ts_data() %>%
        filter(
          Level == input$geo_level,
          Group_Name %in% input$selected_entities
        )

      validate(need(
        nrow(plot_df) > 0,
        "Select entities to display bed metrics."
      ))

      summary_df <- plot_df %>%
        summarise(
          `Estimated excess bed utilisation` = mean(
            `Estimated excess bed utilisation`,
            na.rm = TRUE
          ),
          .by = Group_Name
        ) %>%
        mutate(Group_Name = stringr::str_wrap(Group_Name, 20))

      max_val <- max(
        summary_df$`Estimated excess bed utilisation`,
        na.rm = TRUE
      )
      max_y_bed <- max_val * 1.25
      if (max_y_bed == 0) {
        max_y_bed <- 10
      }

      p_bed <- ggplot(
        summary_df,
        aes(
          y = `Estimated excess bed utilisation`,
          x = Group_Name,
          fill = Group_Name
        )
      ) +
        geom_col_interactive(
          aes(
            tooltip = paste0(
              "<strong>",
              Group_Name,
              "</strong><br>Estimated excess beds utilised: ",
              round(`Estimated excess bed utilisation`, 0)
            )
          ),
          width = 0.4,
          color = NA
        ) +
        geom_text(
          aes(
            y = `Estimated excess bed utilisation`,
            label = scales::comma(round(`Estimated excess bed utilisation`, 0))
          ),
          vjust = -2,
          hjust = 0.5,
          size = geom_text_size,
          show.legend = FALSE
        ) +
        geom_text(
          aes(y = `Estimated excess bed utilisation`, x = Group_Name),
          label = fontawesome("fa-bed"),
          family = "fontawesome-webfont",
          vjust = -0.5,
          hjust = 0.5,
          size = geom_text_size,
          show.legend = FALSE
        ) +
        scale_fill_manual(values = pal) +
        scale_y_continuous(limits = c(0, max_y_bed), expand = c(0, 0)) +
        labs(
          # title = "Estimated avoidable acute bed utilisation",
          x = NULL,
          y = NULL
        ) +
        theme_minimal(base_size = b_s) +
        theme(
          plot.margin = aligned_margin,
          panel.grid = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(face = "bold", size = rel(1.15)),
          axis.title = element_blank(),
          axis.line.x = element_line(color = axis_shade, linewidth = 0.8),
          legend.position = "none",
          plot.title = element_text(
            face = "bold",
            size = rel(1.3),
            hjust = 0.5,
            margin = margin(b = 4)
          ),
          plot.subtitle = element_text(
            color = "grey40",
            size = rel(1.0),
            hjust = 0.5,
            margin = margin(b = 8)
          )
        )

      girafe(
        ggobj = p_bed,
        width_svg = 12.0,
        height_svg = 10, # Fixed height ratio instead of dynamic
        options = list(
          opts_tooltip(
            css = "background-color: #1e293b; color: #ffffff; padding: 6px; font-family: sans-serif;",
            opacity = 0.95
          ),
          opts_hover(css = "fill: #93c5fd; cursor: pointer;"),
          opts_toolbar(
            hidden = c(
              'lasso_select',
              'lasso_deselect',
              'zoom_onoff',
              'zoom_rect',
              'zoom_reset',
              'fullscreen'
            )
          ),
          opts_sizing(rescale = TRUE, width = 1)
        )
      )
    })
  })
}