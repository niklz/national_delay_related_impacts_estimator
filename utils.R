library(ggplot2)
library(dplyr)
library(stringr)
library(purrr)
library(ggiraph)
library(ggrepel)
library(patchwork)


# ==========================================
# HELPERS
# ==========================================
rate_labeller <- function(x) {
  ifelse(x < 1e-10, "0", paste0("1 in ", round(1 / x)))
}

per_k_labeller <- function(x) {
  ifelse(
    x < 1e-10,
    "0",
    paste0(round(1000 * x), " per thousand\n", "(1 in ", round(1 / x), ")")
  )
}

round_denom <- function(val, round = 25) {
  if (is.na(val) || val == 0) {
    return("0")
  }
  denom <- 1 / val
  rounded_denom <- round(denom / round) * round
  return(rounded_denom)
}

# ==========================================
# PLOT FUNCTIONS
# ==========================================
funnel_plot <- function(
  data,
  base = 11,
  wrap = 40,
  log_x = FALSE,
  zebra = TRUE,
  over_dispersion = 3,
  sigmas = seq(0.5, 3, by = 0.5)
) {

  plot_data <- data %>%
    filter(ae_type == "Type 1 (Major)", org != "Total") #%>%
    # dplyr::filter(!is.na(excess_mort), !is.na(tot_ae_adm), !is.na(org)) %>%
    # dplyr::filter(tot_ae_adm > 0, excess_mort <= tot_ae_adm)

  if (nrow(plot_data) == 0) {
    return(ggplot() + theme_minimal())
  }

  sum_excess <- sum(plot_data$excess_mort)
  sum_adm <- sum(plot_data$tot_ae_adm)
  mu <- sum_excess / sum_adm

  plot_data <- plot_data %>%
    mutate(
      rate = excess_mort / tot_ae_adm,
      z_score = (rate - mu) / sqrt(mu * (1 - mu) / tot_ae_adm),
      precise_denom = round(1 / rate),
      tooltip = paste0(
        org,
        ", ",
        format(period, "%Y %B"),
        "\n",
        scales::comma(round(excess_mort)),
        " delay-related deaths\n",
        "Rate: 1 in ",
        precise_denom,
        " admissions"
      )
    )

  x_min <- min(plot_data$tot_ae_adm, na.rm = TRUE)
  x_max <- max(plot_data$tot_ae_adm, na.rm = TRUE)
  y_limit <- max(max(plot_data$rate, na.rm = TRUE) * 1.2, 0.02)
  x_limit_extended <- x_max * 1.02

  x_seq <- seq(x_min * 0.4, x_max * 1.05, length.out = 250)
  sorted_sigmas <- sort(unique(sigmas))

  logit_mu <- log(mu / (1 - mu))

  funnel_lines <- tidyr::crossing(
    tot_ae_adm = x_seq,
    z_val = sorted_sigmas
  ) %>%
    mutate(
      logit_se = sqrt(over_dispersion) * sqrt(1 / (tot_ae_adm * mu * (1 - mu))),
      upper = 1 / (1 + exp(-(logit_mu + z_val * logit_se))),
      sigma = factor(z_val)
    ) %>%
    arrange(sigma, tot_ae_adm)

  funnel_ribbons <- tibble()
  if (zebra && length(sorted_sigmas) >= 2) {
    stripe_indices <- seq(1, length(sorted_sigmas) - 1, by = 2)

    logit_se_seq <- sqrt(over_dispersion) * sqrt(1 / (x_seq * mu * (1 - mu)))

    funnel_ribbons <- lapply(stripe_indices, function(i) {
      z_lower <- sorted_sigmas[i]
      z_upper <- sorted_sigmas[i + 1]

      tibble(
        tot_ae_adm = x_seq,
        ymin = pmin(
          1 / (1 + exp(-(logit_mu + z_lower * logit_se_seq))),
          y_limit
        ),
        ymax = pmin(
          1 / (1 + exp(-(logit_mu + z_upper * logit_se_seq))),
          y_limit
        ),
        group_id = factor(paste0(z_lower, "-", z_upper))
      )
    }) %>%
      dplyr::bind_rows()
  }

  base_colors <- paletteer::paletteer_d("beyonce::X41", direction = -1)

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = tot_ae_adm, y = rate))

  if (zebra && nrow(funnel_ribbons) > 0) {
    p <- p +
      ggplot2::geom_ribbon(
        data = funnel_ribbons,
        ggplot2::aes(
          x = tot_ae_adm,
          ymin = ymin,
          ymax = ymax,
          group = group_id
        ),
        inherit.aes = FALSE,
        fill = "grey40",
        alpha = 0.05
      )
  }

  p <- p +
    ggplot2::geom_line(
      data = funnel_lines,
      ggplot2::aes(x = tot_ae_adm, y = upper, group = sigma),
      alpha = 0.6,
      color = "grey50",
      linetype = "dashed",
      inherit.aes = FALSE
    ) +
    ggplot2::scale_alpha_continuous(range = c(0.6, 0.15), guide = "none") +
    ggplot2::geom_hline(yintercept = mu, color = "steelblue", alpha = 0.5) +

    ggplot2::annotate(
      "text",
      x = x_max,
      y = mu,
      colour = "steelblue",
      label = paste0("National average\n(", rate_labeller(mu), ")"),
      hjust = 1.05,
      vjust = 0.5,
      size = base * 0.8 / 2.83464,
      fontface = "italic"
    ) +
    ggplot2::annotate(
      "text",
      x = Inf,
      y = Inf,
      colour = "grey60",
      label = str_wrap(
        "Dashed lines represent control limits, which define the range of expected variation with hospital volume.",
        wrap * 0.6
      ),
      hjust = 1.05,
      vjust = 1.5,
      size = base * 0.8 / 2.83464,
      fontface = "italic"
    ) +
    ggiraph::geom_point_interactive(
      aes(tooltip = tooltip, col = rate),
      size = 2.5,
      alpha = 0.6
    ) +
    ggplot2::labs(
      title = str_wrap("Delay-related deaths per trust", wrap),
      x = "Total type-1 A&E admissions",
      y = NULL,
      colour = str_wrap("Mortality risk rate (e.g., 1 in 100 admissions)", 60)
    ) +
    scale_y_continuous(limits = c(0, y_limit), labels = \(x) {
      str_c(1000 * x, " ‰")
    }) +
    scale_colour_stepsn(
      n.breaks = 5,
      colors = as.character(base_colors),
      labels = per_k_labeller,
      guide = guide_colorsteps(
        title.position = "top",
        even.steps = TRUE,      # MATCHED WITH MAP
        show.limits = FALSE,    # MATCHED WITH MAP
        barheight = unit(0.04, 'npc'),
        barwidth = unit(0.9, 'npc')
      )
    ) +
    ggplot2::theme_minimal(base_size = base) +
    ggplot2::theme(
      plot.title = element_text(hjust = 0.5),
      plot.margin = margin(5, 5, 5, 5),
      axis.title.y = element_text(vjust = 2.5, margin = margin(r = 10)),
      legend.position = "bottom",
      legend.title = element_text(hjust = 0.5, size = base * 0.9),
      legend.text = element_text(size = base * 0.8)
    )

  if (log_x) {
    p <- p +
      scale_x_log10(labels = scales::comma) +
      ggplot2::coord_cartesian(
        xlim = c(max(10, x_min * 0.5), x_limit_extended),
        ylim = c(0, y_limit),
        clip = "on"
      )
  } else {
    p <- p +
      scale_x_continuous(labels = scales::comma) +
      ggplot2::coord_cartesian(
        xlim = c(x_min, x_limit_extended),
        ylim = c(0, y_limit),
        clip = "on"
      )
  }

  return(p)
}

time_series_plot <- function(data, plot_region, base = 11, wrap = 40) {
  
  # Fast Vectorized Pipeline
  plot_data <- data %>%
    filter(parent_org != "Total") %>%
    mutate(
      parent_org = str_wrap(
        str_to_title(str_trim(str_remove_all(parent_org, "NHS England"))),
        width = 15
      )
    ) %>%
    summarise(
      across(c(excess_mort, tot_ae_adm), sum),
      .by = c(period, parent_org)
    ) %>%
    mutate(
      rate = excess_mort / tot_ae_adm,
      # FIX: Purely vectorized instead of slow sapply loop
      denom = ifelse(is.na(rate) | rate == 0, 0, round((1 / rate) / 10) * 10),
      precise_denom = round(1 / rate),
      tooltip_text = paste0(
        parent_org, ", ", format(period, "%Y %B"), "\n",
        scales::comma(round(excess_mort)), " delay-related deaths\n",
        "Rate: 1 in ", precise_denom, " admissions"
      )
    )

  label_data <- plot_data %>% filter(period == max(period))

  ts_plot <- ggplot(
    plot_data,
    aes(x = period, y = rate * 1000, col = parent_org, group = parent_org, data_id = parent_org)
  ) +
    geom_line(linewidth = 2.5, col = "white") +
    geom_line_interactive(linewidth = 1.2) +
    geom_point_interactive(aes(tooltip = tooltip_text), size = 2.5, hover_nearest = TRUE) +
    
    # FIX: Native text offset layer is infinitely faster than heavy ggrepel simulation
    geom_text_interactive(
      data = label_data,
      aes(label = parent_org, data_id = parent_org),
      hjust = 0,
      nudge_x = 8, 
      size = base * 0.85 / 2.83464,
      fontface = "bold"
    ) +
    scale_x_date(
      breaks = scales::breaks_pretty(n = 6),
      expand = expansion(mult = c(0.02, 0.25)),
      labels = function(x) ifelse(lubridate::month(x) == 1, format(x, "%b\n%Y"), format(x, "%b"))
    ) +
    scale_y_continuous(labels = \(x) str_c(x, " ‰")) +
    paletteer::scale_color_paletteer_d("MetBrewer::Hokusai1") +
    labs(title = str_wrap("Delay-related deaths per region", wrap), x = NULL, y = NULL) +
    theme_minimal(base_size = base) +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5),
      plot.margin = margin(5, 5, 5, 5), 
      panel.grid.minor.x = element_line(color = "#e9ecef", linewidth = 0.5),
      panel.grid.major.x = element_line(color = "#ced4da", linewidth = 0.5)
    )

  ts_plot +
    inset_element(plot_region, on_top = FALSE, left = -0.05, bottom = 0, right = 0.9, top = 1)
}


funnel_plot <- function(
  data,
  precomputed_lines = NULL,    # <-- NEW: Pass from global.R
  precomputed_ribbons = NULL,  # <-- NEW: Pass from global.R
  base = 11,
  wrap = 40,
  log_x = FALSE,
  zebra = FALSE,
  selected_trusts = NULL 
) {

  # 1. High-speed Vectorized Data Preparation
  plot_data <- data %>%
    dplyr::filter(
      ae_type == "Type 1 (Major)", 
      org != "Total",
      !is.na(tot_ae_adm),
      tot_ae_adm > 0,              
      !is.na(excess_mort)
    )

  if (nrow(plot_data) == 0) {
    return(ggplot2::ggplot() + ggplot2::theme_minimal())
  }

  sum_excess <- sum(plot_data$excess_mort)
  sum_adm <- sum(plot_data$tot_ae_adm)
  mu <- sum_excess / sum_adm

  # Flag selected rows and map explicit aesthetic overrides
  plot_data <- plot_data %>%
    dplyr::mutate(
      rate = excess_mort / tot_ae_adm,
      z_score = (rate - mu) / sqrt(mu * (1 - mu) / tot_ae_adm),
      precise_denom = round(1 / rate),
      is_highlighted = org %in% selected_trusts, 
      
      point_stroke = ifelse(is_highlighted, 1.8, 0.2),
      point_size   = ifelse(is_highlighted, 3.5, 2.5),
      point_alpha  = ifelse(is_highlighted, 1.0, 0.6),
      
      tooltip = paste0(
        org, ", ", format(period, "%Y %B"), "\n",
        scales::comma(round(excess_mort)), " delay-related deaths\n",
        "Rate: 1 in ", precise_denom, " admissions"
      )
    )

  # 2. Derive Coordinate Anchors
  x_min <- max(1, min(plot_data$tot_ae_adm, na.rm = TRUE)) 
  x_max <- max(plot_data$tot_ae_adm, na.rm = TRUE)
  y_limit <- max(max(plot_data$rate, na.rm = TRUE) * 1.2, 0.02)
  x_limit_extended <- x_max * 1.02

  # ============================================================================
  # REMOVED: tidyr::crossing(), x_seq generation, and mathematical loop equations!
  # ============================================================================

  # 3. Canvas Assembly Pipeline
  base_colors <- paletteer::paletteer_d("beyonce::X41", direction = -1)

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = tot_ae_adm, y = rate))

  # Use the pre-computed ribbons directly from global.R
  if (zebra && !is.null(precomputed_ribbons) && nrow(precomputed_ribbons) > 0) {
    p <- p +
      ggplot2::geom_ribbon(
        data = precomputed_ribbons,
        ggplot2::aes(x = tot_ae_adm, ymin = ymin, ymax = ymax, group = group_id),
        inherit.aes = FALSE, fill = "grey40", alpha = 0.05
      )
  }

  # 4. Background lines (Half sigmas) using pre-computed global data
  if (!is.null(precomputed_lines)) {
    p <- p +
      ggplot2::geom_line(
        data = dplyr::filter(precomputed_lines, z_val %% 1 != 0), 
        ggplot2::aes(x = tot_ae_adm, y = upper, group = sigma),
        color = "grey50", linetype = "dashed", alpha = 0.4, inherit.aes = FALSE
      )
  }
  
  p <- p + ggplot2::geom_hline(yintercept = mu, color = "steelblue", alpha = 0.5)

  # 5. Textpath Lines (Full sigmas) using pre-computed global data
  if (requireNamespace("geomtextpath", quietly = TRUE) && !is.null(precomputed_lines)) {
    p <- p + 
      geomtextpath::geom_textline(
        data = dplyr::filter(precomputed_lines, z_val %% 1 == 0), 
        ggplot2::aes(x = tot_ae_adm, y = upper, group = sigma, label = paste0(z_val, "σ")),
        color = "grey50", linetype = "dashed", alpha = 0.4, textcolour = "black",
        size = base * 0.75 / 2.83464, linewidth = 0.5, 
        vjust = 0.5, hjust = 0.92, inherit.aes = FALSE
      )
  }
  
  p <- p + 
    ggplot2::annotate(
      "text", x = x_max, y = mu, colour = "steelblue",
      label = paste0("National average\n(", rate_labeller(mu), ")"),
      hjust = 1.05, vjust = 0.5, size = base * 0.8 / 2.83464, fontface = "italic"
    ) +
    ggplot2::annotate(
      "text", x = Inf, y = Inf, colour = "grey60",
      label = str_wrap("Dashed lines represent control limits, which define the range of expected variation with hospital volume.", wrap * 0.6),
      hjust = 1.05, vjust = 1.5, size = base * 0.8 / 2.83464, fontface = "italic"
    ) +
    
    ggiraph::geom_point_interactive(
      ggplot2::aes(
        tooltip = tooltip, fill = rate, colour = is_highlighted,      
        size = point_size, stroke = point_stroke, alpha = point_alpha
      ),
      shape = 21                      
    ) +
    
    ggplot2::scale_colour_manual(values = c("FALSE" = "transparent", "TRUE" = "#111111"), guide = "none") +
    ggplot2::scale_size_identity(guide = "none") +
    ggplot2::scale_alpha_identity(guide = "none") +
    
    ggplot2::labs(
      title = str_wrap("Delay-related deaths per trust", wrap),
      x = "Total type-1 A&E admissions",
      y = NULL,
      fill = str_wrap("Mortality risk rate (e.g., 1 in 100 admissions)", 60) 
    ) +
    
    ggplot2::scale_fill_stepsn(
      n.breaks = 5,
      colors = as.character(base_colors),
      labels = per_k_labeller,
      limits = c(0, y_limit), 
      guide = ggplot2::guide_colorsteps(
        title.position = "top", even.steps = TRUE, show.limits = FALSE,
        barheight = unit(0.04, 'npc'), barwidth = unit(0.9, 'npc')
      )
    ) +
    ggplot2::theme_minimal(base_size = base) +
    ggplot2::theme(
      plot.title = element_text(hjust = 0.5),
      plot.margin = margin(5, 5, 5, 5),
      axis.title.y = element_text(vjust = 2.5, margin = margin(r = 10)),
      legend.position = "bottom",
      legend.title = element_text(hjust = 0.5, size = base * 0.9),
      legend.text = element_text(size = base * 0.8)
    )

  # 6. Axis Configurations
  if (log_x) {
    p <- p + 
      ggplot2::scale_x_log10(labels = scales::comma, limits = c(x_min * 0.85, x_limit_extended), expand = c(0, 0)) +
      ggplot2::scale_y_continuous(limits = c(0, y_limit), labels = \(x) str_c(1000 * x, " ‰"), expand = c(0, 0))
  } else {
    p <- p + 
      ggplot2::scale_x_continuous(labels = scales::comma, limits = c(x_min * 0.95, x_limit_extended), expand = c(0, 0)) +
      ggplot2::scale_y_continuous(limits = c(0, y_limit), labels = \(x) str_c(1000 * x, " ‰"), expand = c(0, 0))
  }

  return(p)
}

choropleth_plot <- function(data, shp, base = 11, wrap = 40) {
  cluster_impacts <- data %>%
    filter(
      period == max(period, na.rm = TRUE),
      org != "Total",
      icb_name != ""
    ) %>%
    summarise(across(c(excess_mort, tot_ae_adm), sum), .by = c(period, cluster))

  plot_data <- shp %>%
    left_join(cluster_impacts, by = join_by(cluster == cluster)) %>%
    mutate(
      rate = excess_mort / tot_ae_adm,
      denom = sapply(rate, round_denom, round = 10),
      rate_bin = str_c("1 in ", denom),
      bin_numeric = 1 / as.numeric(str_extract(rate_bin, "\\d+")),
      precise_denom = round(1 / rate),
      tooltip_text = paste0(
        cluster,
        ", ",
        format(period, "%Y %B"),
        "\n",
        scales::comma(round(excess_mort)),
        " delay-related deaths",
        "\nRate: 1 in ",
        precise_denom,
        " admissions"
      )
    )

  unique_bins <- plot_data %>% arrange(denom) %>% pull(rate_bin) %>% unique()
  breaks <- plot_data %>% arrange(denom) %>% pull(denom) %>% unique()

  # base_colors <- paletteer::paletteer_d("beyonce::X41")
  # pal_func <- colorRampPalette(as.character(base_colors))
  # pal <- pal_func(length(unique_bins))

  base_colors <- paletteer::paletteer_d("beyonce::X41", direction = -1)
  rate_breaks <- c(1 / 400, 1 / 200, 1 / 150, 1 / 100, 1 / 75, 1 / 50)

  p <- ggplot(plot_data, aes(fill = 1 / denom)) +
    geom_sf_interactive(
      aes(tooltip = tooltip_text, data_id = cluster),
      col = "white",
      linewidth = 0.3
    ) +
    scale_fill_stepsn(
      n.breaks = 5,
      colors = as.character(base_colors),
      # breaks = rate_breaks,
      # values = scales::rescale(rate_breaks),
      labels = per_k_labeller,
      # limits = range(rate_breaks),
      guide = guide_colorsteps(
        even.steps = FALSE,
        show.limits = FALSE,
        title.position = "top",
        barheight = unit(0.04, 'npc'),
        barwidth = unit(0.9, 'npc')
      )
    ) +
    labs(
      title = str_wrap("Delay-related deaths, per ICB cluster", wrap),
      fill = str_wrap("Mortality risk rate (e.g., 1 in 100 admissions)", 80)
    ) +
    theme_void(base_size = base) +
    theme(
      plot.margin = margin(5, 5, 5, 5), # Removed unneeded padding bounding the maps
      plot.title = element_text(hjust = 0.5),
      legend.position = "bottom",
      legend.title = element_text(
        hjust = 0.5,
        # face = "bold",
        size = base * 0.9
      ),
      legend.text = element_text(size = base * 0.8)
    )
  p
}


#' Create a reusable percent bar formatter for reactable side cards
#' Now safely handles environment context and disables background bars for Total rows.
#' 
#' @param max_val The maximum value used to scale the width of the background bar.
#' @param df The dataframe context being evaluated (helps resolve Shiny module scoping issues).
#' @param column_name The column to check for identifying row roles. Defaults to "Trust".
#' @param bar_color Optional override for the visual bar background.
reactable_percent_bar_formatter <- function(max_val, df, column_name = "Trust", bar_color = NULL, pos_clr = POS_CLR_LGT, neg_clr = NEG_CLR_LGT) {
  function(value, index) {
    
    # Handle NA and NaN cleanly
    if (is.na(value) || is.nan(value)) {
      return("-") 
    }
    
    # Safely evaluate if this is the Total row based on the passed dataframe context
    is_total_row <- df[[column_name]][index] == "Total"
    
    if (is_total_row) {
      # If the value is a string "Total", return it as is. Otherwise format numeric total text.
      display_text <- if (is.character(value) || value == "Total") value else paste0(sprintf("%.1f", value), "%")
      
      # Return a bold element with NO background bar gradient
      return(
        tags$div(
          style = list(
            color = "#1e293b",
            fontWeight = "bold",
            padding = "2px 4px",
            width = "100%",
            textAlign = "center"
          ),
          display_text
        )
      )
    }
    
    # --- Standard Row Rendering (With Progress Bar) ---
    pct <- min(round((abs(value) / max_val) * 100, 1), 100)
    
    # Color fallback logic
    final_bar_color <- if (!is.null(bar_color)) {
      bar_color
    } else if (value > 0) {
      pos_clr # Soft orange
    } else {
      neg_clr# Soft green
    }
    
    tags$div(
      style = list(
        background = sprintf("linear-gradient(90deg, %s %f%%, #f1f5f9 %f%%)", final_bar_color, pct, pct),
        color = "#1e293b",
        fontWeight = 500,
        borderRadius = "4px",
        padding = "2px 4px",
        width = "100%",
        textAlign = "center"
      ),
      paste0(sprintf("%.1f", value), "%")
    )
  }
}

#' Create a reusable standard numeric bar formatter for reactable cards
#' Migrated to use safe dataframe contextual matching for robust rendering inside modules.
#' 
#' @param max_val The maximum value used to scale the width of the background bar.
#' @param df The dataframe context being evaluated (resolves Shiny module scoping issues).
#' @param column_name The column to check for identifying row roles. Defaults to "Trust".
#' @param color The hex code for the progress bar color.
#' @param track_color The hex code for the empty background track color.
reactable_bar_formatter <- function(max_val, df, column_name = "Trust", color = "#cbd5e1", track_color = "#f1f5f9") {
  function(value, index) {
    
    # Handle NA and NaN cleanly
    if (is.na(value) || is.nan(value)) {
      return("-") 
    }
    
    # FIX: Dynamically evaluate row lookups based on the passed df context instead of global table_data
    is_total_row <- df[[column_name]][index] == "Total"
    
    if (is_total_row) {
      return(
        tags$div(
          style = list(
            color = "#1e293b",
            fontWeight = "bold",
            padding = "2px 4px",
            width = "100%",
            textAlign = "center"
          ),
          scales::comma(value, accuracy = 1)
        )
      )
    }
    
    # --- Standard Row Rendering (With Progress Bar) ---
    pct <- min(round((abs(value) / max_val) * 100), 100)
    
    tags$div(
      style = list(
        background = sprintf("linear-gradient(90deg, %s %d%%, %s %d%%)", color, pct, track_color, pct),
        color = "#1e293b",
        fontWeight = 500,
        borderRadius = "4px",
        padding = "2px 4px",
        width = "100%",
        textAlign = "center"
      ),
      scales::comma(value, accuracy = 1)
    )
  }
}

reactable_text_formatter <- function(positive_color = "#991b1b", negative_color = "#166534", total_color = "#0f172a") {
  function(value, index, name) {
    is_total <- table_data$Trust[index] == "Total"
    text_color <- if (is_total) total_color else if (value > 0) positive_color else negative_color
    
    tags$span(
      style = list(
        color = text_color, 
        fontWeight = "bold",
        display = "block",
        textAlign = "center"
      ),
      comma(value, accuracy = 1) # FIX: Swap digits = 0 for accuracy = 1
    )
  }
}


prep_historical_data <- function(df) {
  
  # --- 1. REGION LEVEL AGGREGATION ---
  df_region <- df %>%
    filter(!is.na(parent_org)) %>%
    group_by(Month_Date = period, Group_Name = parent_org) %>%
    summarise(
      across(c(tot_ae_adm, dta_gt4, excess_mort), sum),
      .groups = "drop"
    ) %>%
    mutate(
      Level = "region",
      # Replace this with your actual formula for Estimated DRD:
      `Estimated DRD` = round(excess_mort, 0) 
    )

  # --- 2. CLUSTER LEVEL AGGREGATION ---
  df_cluster <- df %>%
    filter(!is.na(cluster)) %>%
    group_by(Month_Date = period, Group_Name = cluster) %>%
    summarise(
      across(c(tot_ae_adm, dta_gt4, excess_mort), sum),
      .groups = "drop"
    ) %>%
    mutate(
      Level = "cluster",
      `Estimated DRD` = round(excess_mort, 0) 
    )

  # --- 3. TRUST (ORG) LEVEL AGGREGATION ---
  df_trust <- df %>%
    filter(!is.na(org)) %>%
    group_by(Month_Date = period, Group_Name = org) %>%
    summarise(
      across(c(tot_ae_adm, dta_gt4, excess_mort), sum),
      .groups = "drop"
    ) %>%
    mutate(
      Level = "trust",
      `Estimated DRD` = round(excess_mort, 0) 
    )

  # --- 4. BIND TOGETHER INTO LONG-FORMAT ---
  processed_ts_data <- bind_rows(df_region, df_cluster, df_trust) %>%
    select(Level, Group_Name, Month_Date, `Estimated DRD`, `Total Admissions` = tot_ae_adm)

  return(processed_ts_data)
}
