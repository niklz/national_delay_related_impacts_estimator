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
          tags$strong("delay-related death (*)"),
          tags$strong(
            " rate per 1,000 emergency admissions via type-1 A&E departments(†)"
          ),
          " as a time-series chart. This chart can be used to compare any Region/ICB/Trust to the National baseline level via the selectors on the left.",
          "Delays in A&E are shown to increase acute length of stay. A second chart displays the average number of acute beds in use at any time over the last 12-months attributable solely to admission delays.",
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

    tags$style(HTML("
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
    ")),

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
        label = "Display raw or residual (to national baseline) DRD* rate†:",
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
    pal <-
      structure(
        c(
          "#A82203FF",
          "#208CC0FF",
          "#F1AF3AFF",
          "#946795",
          "#637B31FF",
          "#003967FF"
        ),
        class = "colors"
      )
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
        plot.title = element_text(color = "grey10", size = 18, face = "bold", margin = margin(t = 15), hjust = 0.5, vjust = 5),
        plot.subtitle = element_markdown(color = "grey30", size = 16, lineheight = 1.35, margin = margin(t = 15, b = 40)),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.caption = element_text(color = "grey30", size = 13, lineheight = 1.2, hjust = 0, margin = margin(t = 40)),
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
        filter(Level == "region", Group_Name == "Total", Month_Date >= raw_start) %>%
        mutate(Group_Name = "National/Baseline")
      
      # Extract user choices
      selected_df <- ts_data() %>%
        filter(
          Level == input$geo_level,
          Group_Name %in% input$selected_entities,
          Month_Date >= raw_start
        )
      
      # Union them together and compute raw rates
      bind_rows(selected_df, baseline_df) %>%
        mutate(rate = (`Estimated DRD` / `Total Admissions`))
    })

    # --------------------------------------------------------------------------
    # 2. DRD plot 
    # --------------------------------------------------------------------------
    output$drd_plot <- renderGirafe({
      # Require the reactive data to be ready and the switch to be initialized
      plot_df <- processed_plot_data()
      req(plot_df, !is.null(input$resid))

      validate(need(
        nrow(plot_df) > 0,
        "Select up to 5 elements to generate comparison panels."
      ))

      min_date <- min(plot_df$Month_Date)
      max_date <- max(plot_df$Month_Date)
      label_size <- 7
      date_span <- as.numeric(max_date - min_date)
      dynamic_nudge <- 0.05 * date_span

      # --- CONVERT TO RESIDUAL VALUES IF TOGGLED ---
      if (input$resid == "Residual") {
        baseline_rates <- plot_df %>%
          filter(Group_Name == "National/Baseline") %>%
          select(Month_Date, baseline_rate = rate)

        plot_df <- plot_df %>%
          left_join(baseline_rates, by = "Month_Date") %>%
          mutate(display_rate = rate - baseline_rate) %>%
          filter(Group_Name != "National/Baseline")
      } else {
        plot_df <- plot_df %>% 
          mutate(display_rate = rate)
      }

      label_data <- plot_df %>% filter(Month_Date == max_date)

      pal <- pal %>%
        as.character() %>%
        set_names(label_data$Group_Name) %>%
        `[[<-`("National/Baseline", "cornsilk4")

      # --- GENERATE PLOT OBJ ---
      drd_plot <- ggplot(
        plot_df,
        aes(x = Month_Date, y = round(1000 * display_rate, 1), color = Group_Name)
      ) +
        # Dynamic grid lines mapping based on toggle state
        { if(input$resid == "Residual") geom_hline(yintercept = 0, color = "cornsilk4", linetype = "dashed", size = 1) } +
        { if(input$resid == "Residual") annotate(
            geom = "text", 
            label = "National / Baseline", 
            x = min_date, 
            y = 0, 
            colour = "cornsilk4", 
            fontface = "bold",
            size = label_size, 
            hjust = 0, 
            vjust = -0.6
          ) 
        } +
        { if(input$resid == "Raw") geom_segment(
            data = data.frame(y = 3:7),
            aes(x = min_date, xend = max_date, y = y, yend = y),
            color = "grey91", size = 0.5, inherit.aes = FALSE
          ) 
        } +
        ggh4x::geom_pointpath(
          aes(group = Group_Name),
            size = 1.5,
            linewidth = 1
        ) +
        geom_point_interactive(
          aes(
            group = Group_Name,
            data_id = Group_Name,
            tooltip = paste0(
              "<strong>", Group_Name, "</strong><br/>",
              "Month: ", format(Month_Date, "%B %Y"), "<br/>",
              ifelse(input$resid  == "Residual", "Difference: ", "Rate: "), round(1000 * display_rate, 1)
            )
          ),
          size = 4
        ) +
        ggrepel::geom_text_repel(
          data = label_data,
          aes(color = Group_Name, label = str_wrap(Group_Name, 25)),
          fontface = "bold", size = label_size, direction = "y", lineheight = 0.9,
          hjust = 0, segment.size = 1, segment.alpha = .6, segment.linetype = "dotted",
          box.padding = 0.7, nudge_x = dynamic_nudge
        ) +
        scale_colour_manual(values = pal) +
        scale_x_date(
          breaks = unique(plot_df$Month_Date),
          labels = function(x) ifelse(lubridate::month(x) == 1, format(x, "%b\n%Y"), format(x, "%b")),
          expand = expansion(mult = c(0.05, 0.45))
        ) +
        coord_cartesian(clip = "off") +
        labs(title = "Monthly DRD* rate† over latest 12-month period", subtitle = NULL, x = NULL, y = NULL) +
        {if(input$resid == "Residual") labs(title = "Montlyh DRD* rate† residual to national baseline over latest 12-month period")} +
        shared_theme

      girafe(
        ggobj = drd_plot,
        width_svg = 12.0, height_svg = 10.0,
        options = list(
          opts_tooltip(css = "background-color: #1e293b; color: #ffffff; padding: 6px; font-family: sans-serif;", opacity = 0.95),
          opts_hover(css = "opacity:1.0; stroke-width:3px;"),
          opts_hover_inv(css = "opacity:0.1;"),
          opts_toolbar(hidden = c('lasso_select', 'lasso_deselect', 'zoom_onoff', 'zoom_rect', 'zoom_reset', 'fullscreen')),
          opts_sizing(rescale = TRUE, width = 1)
        )
      )
    })

# --------------------------------------------------------------------------
    # 4. BED UTILISATION PLOT
    # --------------------------------------------------------------------------
    output$bed_plot <- renderGirafe({
      # 1. Let execution continue when selections are empty
      req(ts_data(), input$geo_level)

      # 2. Catch the empty state immediately and return the placeholder plot safely
      if (
        is.null(input$selected_entities) || length(input$selected_entities) == 0
      ) {
        p_empty <- ggplot() +
          annotate(
            geom = "text",
            x = 1,
            y = 1,
            label = "Please select groups from the selector to produce chart",
            size = 1.5*geom_text_size,
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
              # opts_toolbar(position = "none"), # Completely and safely hides toolbar
              opts_sizing(rescale = TRUE, width = 1)
            )
          )
        )
      }

      # 3. Normal data processing (only runs if choices exist)
      plot_df <- ts_data() %>%
        filter(
          Level == input$geo_level,
          Group_Name %in% input$selected_entities
        )

      validate(need(
        nrow(plot_df) > 0,
        "Please select groups from the selector to produce chart"
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
            label = scales::comma(round(`Estimated excess bed utilisation`, 0)),
            col = Group_Name
          ),
          vjust = -2,
          hjust = 0.5,
          size = 1.5*geom_text_size,
          show.legend = FALSE
        ) +
        geom_text(
          aes(y = `Estimated excess bed utilisation`, x = Group_Name, col = Group_Name),
          label = fontawesome("fa-bed"),
          family = "fontawesome-webfont",
          vjust = -0.5,
          hjust = 0.5,
          size = 1.5*geom_text_size,
          show.legend = FALSE
        ) +
        scale_fill_manual(values = pal) +
        scale_colour_manual(values = pal) +
        scale_y_continuous(limits = c(0, max_y_bed), expand = c(0, 0)) +
        labs(title = "Estimated avoidable actue-bed utilisation", x = NULL, y = NULL) +
        theme_minimal(base_family = "open_sans", base_size = b_s) +
        # shared_theme +
        theme(
          plot.margin = aligned_margin,
          plot.title = element_text(color = "grey10", size = 18,  margin = margin(t = 15), hjust = 0.5, vjust = 5),
          axis.text.y = element_blank(),
          axis.text.x = element_text(face = "bold", size = rel(1.15)),
          axis.title = element_blank(),
          axis.line.x = element_line(color = axis_shade, linewidth = 0.8),
          panel.grid = element_blank(),
          legend.position = "none"
        )

      # 4. Render actual chart
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