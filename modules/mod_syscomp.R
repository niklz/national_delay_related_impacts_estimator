# modules/mod_syscomp

sysCompUI <- function(id, SPINNER_TYPE, title) {
  ns <- NS(id)

  nav_panel(
    title = title,
    icon = icon("chart-line"),
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
          "This section displays the estimated",
          tags$strong("Delay-Related Death rate per 1,000 emergency admissions via type-1 A&E departments(*)"
          ),
          " as a time-series chart. This chart can be used to compare any Region/ICB/Trust to the National baseline level via the selectors on the left.",
          "Delays in A&E are shown to increase ",
          tags$strong("acute length of stay"),
           ". A second chart displays the average number of ",
            tags$strong("acute beds"),
          " in use at any time over the last 12-months attributable solely to admission delays.",
        )
      )
    ),

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
        card_body(
          style = "padding: 0.5rem; display: flex; flex-direction: column; height: 100%; overflow: hidden;",

          tags$style(HTML(
            "
      /* Constrain girafe widget to fill flex space without overflowing */
      .total-container {
        flex: 1 1 auto;
        min-height: 0; /* Critical for flex child shrink/grow calculations */
        width: 100%;
        overflow: hidden;
      }
      .total-container .html-widget.girafe, 
      .total-container .html-widget.girafe svg { 
        height: 100% !important; 
        max-height: 100% !important;
        width: 100% !important;
      }
      /* Compact radio group styling */
      .toggle-container {
        flex: 0 0 auto;
        padding-top: 0.25rem;
        margin-top: auto;
      }
      .toggle-container .awesome-radio {
        margin-bottom: 0 !important;
      }
    "
          )),

          # Pinned Top Row: Takes up remaining flex height
          div(
            class = "total-container",
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
          ),

          div(
            class = "toggle-container",
            awesomeRadio(
              inputId = ns("resid"),
              label = "Display raw or residual (to national baseline) Delay-Related Death Rate*:",
              choices = c("Raw", "Residual"),
              selected = "Raw",
              inline = TRUE,
              status = "warning",
              width = "auto"
            )
          )
        )
      ),

      # ==========================================
      # CARD 3: Avoidable Bed Utilisation Chart
      # ==========================================
      card(
        # full_screen = TRUE,
        # card_header(
        #   style = "display: flex; justify-content: center; align-items: center; text-align: center; min-height: auto; flex: 0 0 auto;",
        #   span(
        #     "Estimated avoidable acute bed utilisation", # (Or appropriate text for Card 3)
        #     style = "font-weight: bold; font-size: 0.8vw; margin: 0;"
        #   )
        # ),
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

    # 1. Base palette definitions
    base_palette <- c(
      "#A82203FF",
      "#208CC0FF",
      "#F1AF3AFF",
      "#946795",
      "#637B31FF",
      "#003967FF"
    )
    baseline_color <- "cornsilk4"

    # 2. Store assigned colors in a reactiveValues container
    color_assignments <- reactiveValues(mapping = character())

    # 3. Dynamic color manager observer
    observeEvent(input$selected_entities, ignoreNULL = FALSE, {
      selected <- input$selected_entities %||% character(0)

      # Clean entity names (mirroring processing logic)
      selected_clean <- stringr::str_trim(
        stringr::str_remove_all(
          selected,
          "NHS England|NHS Foundation Trust|NHS Trust"
        )
      )

      current_mapping <- color_assignments$mapping

      # Retain colors for entities that are still selected
      updated_mapping <- current_mapping[
        names(current_mapping) %in% selected_clean
      ]

      # Assign palette colors to newly selected entities
      unassigned <- setdiff(selected_clean, names(updated_mapping))

      if (length(unassigned) > 0) {
        # Find unused palette colors
        used_colors <- unname(updated_mapping)
        available_colors <- setdiff(base_palette, used_colors)

        # Fallback if selected entities exceed base palette size
        if (length(available_colors) < length(unassigned)) {
          available_colors <- c(
            available_colors,
            rep_len(base_palette, length(unassigned))
          )
        }

        new_assignments <- setNames(
          available_colors[seq_along(unassigned)],
          unassigned
        )
        updated_mapping <- c(updated_mapping, new_assignments)
      }

      # Commit back to reactiveValues
      color_assignments$mapping <- updated_mapping
    })

    # 4. Helper reactive to fetch active palette + National Baseline
    active_palette <- reactive({
      pal_vec <- color_assignments$mapping
      pal_vec["National Baseline"] <- baseline_color
      pal_vec
    })


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

    # Shared graphic configurations (Your exact theme variables)
    axis_shade <- "grey40"
    col_width <- 20
    geom_text_size <- 5.0
    b_s <- 14
    label_pos <- -0.4
    aligned_margin <- margin(t = 10, r = 25, b = 10, l = 25, unit = "pt")

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
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(color = "grey91", size = 0.5),
        plot.title = element_text(
          color = "grey10",
          size = 18,
          face = "bold",
          margin = margin(t = 15),
          hjust = 0.5,
          vjust = 5
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

    # --------------------------------------------------------------------------
    # NEW: REACTIVE DATA LAYER (Baseline always renders by default)
    # --------------------------------------------------------------------------
    processed_plot_data <- reactive({
      req(ts_data())

      max_date <- max(ts_data()$Month_Date, na.rm = TRUE)
      raw_start <- max_date %m-% months(11)

      # Always extract national baseline
      baseline_df <- ts_data() %>%
        filter(
          Level == "region",
          Group_Name == "Total",
          Month_Date >= raw_start
        ) %>%
        mutate(Group_Name = "National Baseline")

      # Extract user choices
      selected_df <- ts_data() %>%
        filter(
          Level == input$geo_level,
          Group_Name %in% input$selected_entities,
          Month_Date >= raw_start
        )

      # Union them together and compute raw rates
      bind_rows(selected_df, baseline_df) %>%
        mutate(rate = (`Estimated DRD` / `Total Admissions`)) %>%
        mutate(rate = ifelse(!is.finite(rate), 0, rate)) %>%
        mutate(
          Group_Name = str_trim(str_remove_all(
            Group_Name,
            "NHS England|NHS Foundation Trust|NHS Trust"
          ))
        )
    })

    output$drd_plot <- renderGirafe({
      plot_df <- processed_plot_data()
      req(plot_df, !is.null(input$resid))

      validate(need(
        nrow(plot_df) > 0,
        "Select up to 5 elements to generate comparison panels."
      ))

      min_date <- min(plot_df$Month_Date)
      max_date <- max(plot_df$Month_Date)
      label_size <- 7

      # --- CONVERT TO RESIDUAL VALUES IF TOGGLED ---
      if (input$resid == "Residual") {
        baseline_rates <- plot_df %>%
          filter(Group_Name == "National Baseline") %>%
          select(Month_Date, baseline_rate = rate)

        plot_df <- plot_df %>%
          left_join(baseline_rates, by = "Month_Date") %>%
          mutate(display_rate = rate - baseline_rate) %>%
          filter(Group_Name != "National Baseline")
      } else {
        plot_df <- plot_df %>%
          mutate(display_rate = rate)
      }

      # Scale rate to per 1,000
      plot_df <- plot_df %>%
        mutate(y_val = round(1000 * display_rate, 1))

      # Latest data points for repelled labels
      label_data <- plot_df %>%
        filter(Month_Date == max(Month_Date, na.rm = TRUE), .by = Group_Name)

      # ---------------------------------------------------------------------------
      # DYNAMIC GRID LINE CALCULATION
      # Calculate human-friendly axis breaks matching what ggplot would pick
      # ---------------------------------------------------------------------------
      y_breaks <- pretty(plot_df$y_val, n = 5)
      grid_df <- data.frame(y = y_breaks)

      # Anchor points for repelled labels
      label_x_anchor <- max_date + lubridate::days(20)
      label_x_max <- max_date + lubridate::days(100)

      # --- GENERATE PLOT OBJ ---
      drd_plot <- ggplot(
        plot_df,
        aes(
          x = Month_Date,
          y = y_val,
          color = Group_Name
        )
      ) +
        # 1. Dynamic horizontal gridlines strictly bounded between min_date and max_date
        geom_segment(
          data = grid_df,
          aes(x = min_date, xend = max_date, y = y, yend = y),
          color = "grey91",
          linewidth = 0.5,
          inherit.aes = FALSE
        ) +

        # 2. Reference Line for Residual state
        {
          if (input$resid == "Residual") {
            geom_hline(
              yintercept = 0,
              color = "cornsilk4",
              linetype = "dashed",
              linewidth = 1
            )
          }
        } +
        {
          if (input$resid == "Residual") {
            annotate(
              geom = "text",
              label = "National Baseline",
              x = min_date,
              y = 0,
              colour = "cornsilk4",
              fontface = "bold",
              size = label_size,
              hjust = 0,
              vjust = -0.6
            )
          }
        } +

        # 3. Interactive Lines & Points
        geom_line_interactive(
          aes(group = Group_Name, data_id = Group_Name),
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
              ifelse(input$resid == "Residual", "Difference: ", "Rate: "),
              y_val
            )
          ),
          size = 3.5
        ) +

        # 4. Vertical Stacked Repelled Labels
        geom_text_repel_interactive(
          data = label_data,
          aes(
            x = Month_Date,
            y = y_val,
            color = Group_Name,
            data_id = Group_Name,
            label = str_wrap(Group_Name, 18)
          ),
          fontface = "bold",
          size = label_size,
          direction = "y",
          hjust = 0,
          xlim = c(label_x_anchor, label_x_max),
          force = 3,
          force_pull = 0.2,
          max.overlaps = Inf,
          lineheight = 0.9,
          segment.size = 0.8,
          segment.alpha = 0.6,
          segment.linetype = "dotted",
          box.padding = 0.5,
          point.padding = 0.3
        ) +
        scale_colour_manual(values = active_palette()) +
        scale_y_continuous(
          # breaks = y_breaks # Ensure tick marks line up with grid segments
          expand = expansion(mult = c(0.1, 0.1))
        ) +
        scale_x_date(
          breaks = unique(plot_df$Month_Date),
          labels = function(x) {
            ifelse(
              lubridate::month(x) == 1,
              format(x, "%b\n%Y"),
              format(x, "%b")
            )
          },
          expand = expansion(mult = c(0.03, 0.35))
        ) +
        coord_cartesian(clip = "off") +
        labs(
          title = if (input$resid == "Residual") {
            "Monthly Delay-Related Death Rate* residual to national baseline over latest 12-month period"
          } else {
            "Monthly Delay-Related Death Rate* over latest 12-month period"
          },
          x = NULL,
          y = NULL
        ) +
        shared_theme +
        theme(
          # Keep theme grid empty so custom bounded geom_segment controls the Y lines
          panel.grid.major.y = element_blank()
        )

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
      req(ts_data(), input$geo_level)

      if (
        is.null(input$selected_entities) || length(input$selected_entities) == 0
      ) {
        p_empty <- ggplot() +
          annotate(
            geom = "text",
            x = 1,
            y = 1,
            label = "Please select groups from the selector to produce chart",
            size = 1.5 * geom_text_size,
            fontface = "italic",
            color = "grey50",
            hjust = 0.5,
            vjust = 0.5
          ) +
          theme_void() +
          theme(plot.margin = aligned_margin)

        return(
          girafe(
            ggobj = p_empty,
            width_svg = 12.0,
            height_svg = 10,
            options = list(
              opts_sizing(rescale = TRUE, width = 1)
            )
          )
        )
      }

      plot_df <- ts_data() %>%
        filter(
          Level == input$geo_level,
          Group_Name %in% input$selected_entities
        ) %>%
        mutate(
          Group_Name = str_trim(str_remove_all(
            Group_Name,
            "NHS England|NHS Foundation Trust|NHS Trust"
          ))
        )

      validate(need(
        nrow(plot_df) > 0,
        "Please select groups from the selector to produce chart"
      ))

      # Wrap Group_Name string
      summary_df <- plot_df %>%
        summarise(
          `Estimated excess bed utilisation` = mean(
            `Estimated excess bed utilisation`,
            na.rm = TRUE
          ),
          .by = Group_Name
        ) %>%
        mutate(Group_Name = stringr::str_wrap(Group_Name, 15))

      max_val <- max(
        summary_df$`Estimated excess bed utilisation`,
        na.rm = TRUE
      )
      max_y_bed <- max_val * 1.25
      if (max_y_bed == 0) {
        max_y_bed <- 10
      }

      # FIX 1: Adapt palette vector names to match wrapped string names in summary_df
      bed_palette <- active_palette()
      names(bed_palette) <- stringr::str_wrap(names(bed_palette), 15)

      p_bed <- ggplot(
        summary_df,
        aes(
          y = `Estimated excess bed utilisation`,
          x = Group_Name, # FIX 2: Removed rogue ', 15,' here
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
            label = scales::comma(round(`Estimated excess bed utilisation`, 0)),
            col = Group_Name
          ),
          vjust = -2,
          hjust = 0.5,
          size = 1.5 * geom_text_size,
          show.legend = FALSE
        ) +
        geom_text(
          aes(
            y = `Estimated excess bed utilisation`,
            x = Group_Name,
            col = Group_Name
          ),
          label = fontawesome("fa-bed"),
          family = "fontawesome-webfont",
          vjust = -0.5,
          hjust = 0.5,
          size = 1.5 * geom_text_size,
          show.legend = FALSE
        ) +
        scale_fill_manual(values = bed_palette) +
        scale_colour_manual(values = bed_palette) +
        scale_y_continuous(limits = c(0, max_y_bed), expand = c(0, 0)) +
        labs(
          title = "Estimated avoidable acute-bed utilisation",
          x = NULL,
          y = NULL
        ) +
        theme_minimal(base_family = "open_sans", base_size = b_s) +
        theme(
          plot.margin = aligned_margin,
          plot.title = element_text(
            color = "grey10",
            size = 18,
            margin = margin(t = 15),
            hjust = 0.5,
            vjust = 5
          ),
          axis.text.y = element_blank(),
          axis.text.x = element_text(face = "bold", size = rel(1.15)),
          axis.title = element_blank(),
          axis.line.x = element_line(color = axis_shade, linewidth = 0.8),
          panel.grid = element_blank(),
          legend.position = "none"
        )

      girafe(
        ggobj = p_bed,
        width_svg = 12.0,
        height_svg = 10,
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