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
funnel_plot <- function(
  data,
  base = 11,
  wrap = 40,
  log_x = FALSE,
  zebra = TRUE,
  over_dispersion = 3,
  sigmas = seq(0.5, 5.0, by = 0.5)
) {
  # 1. High-speed Vectorized Data Preparation
  plot_data <- data %>%
    filter(ae_type == "Type 1 (Major)", org != "Total") %>%
    dplyr::filter(!is.na(excess_mort), !is.na(tot_ae_adm), !is.na(org)) %>%
    dplyr::filter(tot_ae_adm > 0, excess_mort <= tot_ae_adm)

  if (nrow(plot_data) == 0) {
    return(ggplot() + theme_minimal())
  }

  # Calculate constants up front to avoid repeated sum iterations
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

  # 2. Derive Coordinate Anchors
  x_min <- min(plot_data$tot_ae_adm)
  x_max <- max(plot_data$tot_ae_adm)
  y_limit <- max(max(plot_data$rate) * 1.2, 0.02)
  x_limit_extended <- x_max * 1.02

  # 3. Vectorized Mathematical Grid Generation
  x_seq <- seq(x_min * 0.4, x_max * 1.05, length.out = 250)
  sorted_sigmas <- sort(unique(sigmas))

  logit_mu <- log(mu / (1 - mu))

  # FIX: Clean, foolproof combination matrix using tidyr::crossing.
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

  # Fast calculation of ribbons
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

  # 4. Canvas Assembly Pipeline
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
      ggplot2::aes(x = tot_ae_adm, y = upper, group = sigma, alpha = z_val),
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
    mutate(
      tooltip_text = paste0(
        parent_org,
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
      direction = "y", 
      segment.color = NA,
      size = base * 0.85 / 2.83464,
      fontface = "bold"
    ) +
    scale_x_date(
      breaks = scales::breaks_pretty(n = 6),
      minor_breaks = "1 month",
      expand = expansion(mult = c(0.02, 0.25)),
      labels = function(x) {
        ifelse(
          lubridate::month(x) == 1,
          format(x, "%Y"),
          format(x, "%b")
        )
      }
    ) +
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
      plot.title = element_text(hjust = 0.5),
      plot.margin = margin(5, 5, 5, 5), 
      axis.title = element_text(angle = 0, vjust = 1, hjust = 0, face = "bold"),
      axis.text.x = element_text(size = base - 1, color = "#555555"),
      panel.grid.minor.x = element_line(color = "#e9ecef", linewidth = 0.5),
      panel.grid.major.x = element_line(color = "#ced4da", linewidth = 0.5)
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
      labels = per_k_labeller,
      guide = guide_colorsteps(
        even.steps = TRUE,      # CHANGED TO TRUE TO MATCH FUNNEL
        show.limits = FALSE,    # MATCHED WITH FUNNEL
        title.position = "top",
        barheight = unit(0.04, 'npc'),
        barwidth = unit(0.9, 'npc')
      )
    ) +
    labs(
      title = str_wrap("Delay-related deaths, per ICB cluster", wrap),
      fill = str_wrap("Mortality risk rate (e.g., 1 in 100 admissions)", 60) # REDUCED WRAP TO MATCH FUNNEL
    ) +

    theme_minimal(base_size = base) + # Switch to minimal to inherit structural spacing
    theme(
      # Hide all panel grid lines and background features
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title.y = element_blank(),
      
      # Match the funnel plot margin layout
      plot.margin = margin(5, 5, 5, 5), 
      plot.title = element_text(hjust = 0.5),
      
      # Force an empty x-axis title that matches the funnel plot's vertical height
      axis.title.x = element_text(
        color = "transparent", 
        margin = margin(t = 10) # Matches default breathing room of funnel axis title
      ),
      
      # Legend specs remain perfectly unified
      legend.position = "bottom",
      legend.title = element_text(hjust = 0.5, size = base * 0.9),
      legend.text = element_text(size = base * 0.8)
    )
  p
}