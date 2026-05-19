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
    paste0(round(1000 * x), " per mille\n", "(1 in ", round(1 / x), ")")
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

funnel_plot <- function(data, base = 11, wrap = 40, log_x = FALSE, zebra = TRUE, sigmas = seq(0.5, 5.0, by = 0.5)) {
  over_dispersion <- 3 

  plot_data <- data %>%
    filter(period == max(period)) %>%
    filter(ae_type == "Type 1 (Major)") %>%
    filter(org != "Total") %>%
    dplyr::filter(!is.na(excess_mort), !is.na(tot_ae_adm), !is.na(org)) %>%
    dplyr::filter(tot_ae_adm > 0, excess_mort <= tot_ae_adm) %>%
    mutate(mu = sum(excess_mort) / sum(tot_ae_adm)) %>%
    mutate(rate = excess_mort / tot_ae_adm) %>%
    mutate(z_score = (rate - mu) / (sqrt(mu * (1 - mu) / tot_ae_adm))) %>%
    mutate(
      denom = sapply(rate, round_denom, round = 10),
      rate_bin = str_c("1 in ", denom),
      bin_numeric = 1 / as.numeric(str_extract(rate_bin, "\\d+")),
      precise_denom = round(1 / rate),
      tooltip = paste0(org, ", ", format(period, "%Y %B"), "\n", scales::comma(round(excess_mort)), " delay-related deaths", "\nRate: 1 in ", precise_denom, " admissions")
    )

  mu <- sum(plot_data$excess_mort) / sum(plot_data$tot_ae_adm)
  current_rate <- plot_data$excess_mort / plot_data$tot_ae_adm

  x_min <- min(plot_data$tot_ae_adm)
  x_max <- max(plot_data$tot_ae_adm)
  
  y_limit <- max(max(current_rate) * 1.2, 0.02)
  x_limit_extended <- x_max * 1.02

  # FIX: Set the sequence to start slightly below your minimum data point 
  # instead of 1, preventing the standard error from exploding to infinity.
  x_seq <- seq(x_min * 0.4, x_max * 1.05, length.out = 500)

  sorted_sigmas <- sort(unique(sigmas))
  
  funnel_base <- tibble(tot_ae_adm = x_seq) %>%
    mutate(
      logit_mu = log(mu / (1 - mu)),
      logit_se = sqrt(over_dispersion) * sqrt(1 / (tot_ae_adm * mu * (1 - mu)))
    )

  funnel_lines <- purrr::map_df(sorted_sigmas, function(z) {
    tibble(
      tot_ae_adm = x_seq,
      upper = 1 / (1 + exp(-(funnel_base$logit_mu + z * funnel_base$logit_se))),
      sigma = as.character(z),
      z_val = z
    )
  })

  funnel_ribbons <- tibble()
  if (zebra && length(sorted_sigmas) >= 2) {
    stripe_indices <- seq(1, length(sorted_sigmas) - 1, by = 2)
    
    funnel_ribbons <- purrr::map_df(stripe_indices, function(i) {
      z_lower <- sorted_sigmas[i]
      z_upper <- sorted_sigmas[i + 1]
      
      # FIX: Use pmin() to cap the ribbon ceiling at the chart's y_limit. 
      # This stops the ribbon from creating an artificial vertical pillar.
      tibble(
        tot_ae_adm = x_seq,
        ymin = pmin(1 / (1 + exp(-(funnel_base$logit_mu + z_lower * funnel_base$logit_se))), y_limit),
        ymax = pmin(1 / (1 + exp(-(funnel_base$logit_mu + z_upper * funnel_base$logit_se))), y_limit),
        group_id = paste0(z_lower, "-", z_upper)
      )
    })
  }

  base_colors <- paletteer::paletteer_d("beyonce::X41", direction = -1)

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = tot_ae_adm, y = rate)
  )
  
  if (zebra && nrow(funnel_ribbons) > 0) {
    p <- p + ggplot2::geom_ribbon(
      data = funnel_ribbons,
      ggplot2::aes(x = tot_ae_adm, ymin = ymin, ymax = ymax, group = group_id),
      inherit.aes = FALSE,
      fill = "grey40",
      alpha = 0.05
    )
  }

  p <- p + 
    ggplot2::geom_line(
      data = funnel_lines,
      ggplot2::aes(x = tot_ae_adm, y = upper, group = sigma, alpha = z_val),
      color = "grey50",
      linetype = "dashed"
    ) +
    ggplot2::scale_alpha_continuous(
      range = c(0.6, 0.15), 
      guide = "none"
    ) +
    
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
      colour = str_wrap("Mortality risk rate (e.g., 1 in 100 admissions)", 80)
    ) +
    scale_y_continuous(limits = c(0, y_limit), labels = \(x) {
      str_c(1000 * x, " ‰")
    }) +
    scale_x_continuous(labels = scales::comma) +
    scale_colour_stepsn(
      n.breaks = 5,
      colors = as.character(base_colors),
      labels = per_k_labeller,
      guide = guide_colorsteps(
        title.position = "top",
        even.steps = TRUE,
        barheight = unit(0.04, 'npc'),
        barwidth = unit(0.9, 'npc')
      )
    ) +
    ggplot2::coord_cartesian(
      xlim = c(x_min, x_limit_extended),
      ylim = c(0, y_limit),
      clip = "on"
    ) +
    ggplot2::theme_minimal(base_size = base) +
    ggplot2::theme(
      plot.margin = margin(5, 5, 5, 5),
      axis.title.y = element_text(
        vjust = 2.5,
        margin = margin(r = 10)
      ),
      legend.position = "bottom",
      legend.title = element_text(
        hjust = 0.5,
        size = base * 0.9
      ),
      legend.text = element_text(size = base * 0.8)
    )

    if(log_x){
      p <- p + scale_x_log10(labels = scales::comma)
    }
  
  return(p)
}

funnel_plot_optimized <- function(data, base = 11, wrap = 40, log_x = FALSE, zebra = TRUE, lines_df, ribbons_df, mu_val) {
  
  plot_data <- data %>%
    filter(org != "Total") %>%
    dplyr::filter(!is.na(excess_mort), !is.na(tot_ae_adm), !is.na(org)) %>%
    dplyr::filter(tot_ae_adm > 0, excess_mort <= tot_ae_adm) %>%
    mutate(
      rate = excess_mort / tot_ae_adm,
      precise_denom = round(1 / rate),
      tooltip = paste0(org, ", ", format(period, "%Y %B"), "\n", scales::comma(round(excess_mort)), " delay-related deaths", "\nRate: 1 in ", precise_denom, " admissions")
    )

  current_rate <- plot_data$excess_mort / plot_data$tot_ae_adm
  x_min <- min(plot_data$tot_ae_adm)
  x_max <- max(plot_data$tot_ae_adm)
  y_limit <- max(max(current_rate) * 1.2, 0.02)
  x_limit_extended <- x_max * 1.05

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = tot_ae_adm, y = rate))

  # Render Zebra Ribbons cleanly with support for log scales
  if (zebra && nrow(ribbons_df) > 0) {
    # If using a log scale, clip values below 10 to keep the geometry clean
    display_ribbons <- if(log_x) filter(ribbons_df, tot_ae_adm >= 10) else ribbons_df
    
    p <- p + ggplot2::geom_ribbon(
      data = display_ribbons,
      ggplot2::aes(x = tot_ae_adm, ymin = pmin(ymin, y_limit), ymax = pmin(ymax, y_limit), group = group_id),
      inherit.aes = FALSE, fill = "grey40", alpha = 0.05
    )
  }

  # Render Control Lines
  display_lines <- if(log_x) filter(lines_df, tot_ae_adm >= 10) else lines_df
  p <- p + 
    ggplot2::geom_line(
      data = display_lines,
      ggplot2::aes(x = tot_ae_adm, y = upper, group = sigma, alpha = z_val),
      color = "grey50", linetype = "dashed"
    ) +
    ggplot2::scale_alpha_continuous(range = c(0.5, 0.15), guide = "none") +
    ggplot2::geom_hline(yintercept = mu_val, color = "steelblue", alpha = 0.5) +
    
    # Original Annotations
    ggplot2::annotate(
      "text", x = x_max, y = mu_val, colour = "steelblue",
      label = paste0("National average\n(", rate_labeller(mu_val), ")"),
      hjust = 1.05, vjust = -0.5, size = base * 0.8 / 2.83464, fontface = "italic"
    ) +
    ggplot2::annotate(
      "text", x = Inf, y = Inf, colour = "grey60",
      label = str_wrap("Dashed lines represent control limits, which define the range of expected variation with hospital volume.", wrap * 0.6),
      hjust = 1.05, vjust = 1.5, size = base * 0.8 / 2.83464, fontface = "italic"
    ) +
    ggiraph::geom_point_interactive(
      aes(tooltip = tooltip, col = rate), size = 2.5, alpha = 0.6
    ) +
    ggplot2::labs(
      title = str_wrap("Delay-related deaths per trust", wrap),
      x = "Total type-1 A&E admissions", y = NULL,
      colour = str_wrap("Mortality risk rate (e.g., 1 in 100 admissions)", 80)
    ) +
    scale_y_continuous(limits = c(0, y_limit), labels = \(x) str_c(1000 * x, " ‰"))

  # DYNAMIC AXIS HANDLER: Toggle between linear and log scales
  if (log_x) {
    p <- p + scale_x_log10(labels = scales::comma, limits = c(max(10, x_min * 0.5), x_limit_extended))
  } else {
    p <- p + scale_x_continuous(labels = scales::comma) +
      ggplot2::coord_cartesian(xlim = c(x_min, x_limit_extended), ylim = c(0, y_limit), clip = "on")
  }

  base_colors <- paletteer::paletteer_d("beyonce::X41", direction = -1)

  p <- p + 
    scale_colour_stepsn(
      n.breaks = 5, colors = as.character(base_colors), labels = per_k_labeller,
      guide = guide_colorsteps(
        title.position = "top", even.steps = TRUE,
        barheight = unit(0.04, 'npc'), barwidth = unit(0.9, 'npc')
      )
    ) +
    ggplot2::theme_minimal(base_size = base) +
    ggplot2::theme(
      plot.margin = margin(5, 5, 5, 5),
      axis.title.y = element_text(vjust = 2.5, margin = margin(r = 10)),
      legend.position = "bottom",
      legend.title = element_text(hjust = 0.5, size = base * 0.9),
      legend.text = element_text(size = base * 0.8)
    )
  
  return(p)
}


time_series_plot <- function(data, plot_region, base = 11, wrap = 40) {
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
      denom = sapply(rate, round_denom, round = 10),
      rate_bin = str_c("1 in ", denom),
      bin_numeric = 1 / as.numeric(str_extract(rate_bin, "\\d+")),
      precise_denom = round(1 / rate),
      .by = c(period, parent_org)
    ) %>%
    mutate(tooltip_text = paste0(parent_org, ", ", format(period, "%Y %B"), "\n", scales::comma(round(excess_mort)), " delay-related deaths", "\nRate: 1 in ", precise_denom, " admissions"))

  label_data <- plot_data %>% filter(period == max(period))

  ts_plot <- ggplot(
    plot_data,
    aes(
      x = period,
      y = rate * 1000,
      col = parent_org,
      group = parent_org,
      data_id = parent_org
    )
  ) +
    geom_line(linewidth = 2.5, col = "white") +
    geom_line_interactive(linewidth = 1.2) +
    geom_point_interactive(
      aes(tooltip = tooltip_text),
      size = 2.5,
      hover_nearest = TRUE
    ) +
    geom_text_repel_interactive(
      data = label_data,
      aes(label = parent_org, data_id = parent_org),
      hjust = 0,
      nudge_x = 7.5,
      direction = "y", # Reduced horizontal nudge
      segment.color = NA,
      size = base * 0.85 / 2.83464,
      fontface = "bold"
    ) +
    scale_x_date(expand = expansion(mult = c(0.02, 0.25))) +
    scale_y_continuous(labels = \(x) str_c(x, " ‰")) +
    paletteer::scale_color_paletteer_d("MetBrewer::Hokusai1") +
    labs(
      title = str_wrap(
        "Delay-related deaths per region",
        wrap
      ),
      x = NULL,
      y = NULL
    ) +
    theme_minimal(base_size = base) +
    theme(
      legend.position = "none",
      plot.margin = margin(5, 5, 5, 5), # Zeroed/minimized extra margins
      axis.title = element_text(angle = 0, vjust = 1, hjust = 0, face = "bold")
    )

  ts_plot +
    inset_element(
      plot_region,
      on_top = FALSE,
      left = -0.05,
      bottom = 0,
      right = 0.9,
      top = 1
    )
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
      tooltip_text = paste0(cluster, ", ", format(period, "%Y %B"), "\n", scales::comma(round(excess_mort)), " delay-related deaths", "\nRate: 1 in ", precise_denom, " admissions")
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