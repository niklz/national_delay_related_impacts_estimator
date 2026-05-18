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

funnel_plot <- function(data, base = 11, wrap = 40) {

  over_dispertion <- 3
  line_breaks <- c("95%" = 1.96, "99.7%" = 3)

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
      tooltip = paste0(org, "\nRate: 1 in ", precise_denom)
    )

  mu <- sum(plot_data$excess_mort) / sum(plot_data$tot_ae_adm)
  current_rate <- plot_data$excess_mort / plot_data$tot_ae_adm

  x_min <- min(plot_data$tot_ae_adm)
  x_max <- max(plot_data$tot_ae_adm)

  funnel_lines <- purrr::map_df(names(line_breaks), function(label) {
    z <- line_breaks[label]
    tibble(
      tot_ae_adm = seq(x_min, x_max, length.out = 500),
      logit_mu = log(mu / (1 - mu)),
      logit_se = sqrt(over_dispertion) * sqrt(1 / (tot_ae_adm * mu * (1 - mu))),
      upper = 1 / (1 + exp(-(logit_mu + z * logit_se))),
      lower = 1 / (1 + exp(-(logit_mu - z * logit_se))),
      label = label
    )
  })

  unique_bins <- plot_data %>% arrange(denom) %>% pull(rate_bin) %>% unique()
  breaks <- plot_data %>% arrange(denom) %>% pull(denom) %>% unique()

  base_colors <- paletteer::paletteer_d("beyonce::X41", direction = -1)
  rate_breaks <- c(1 / 400, 1 / 200, 1 / 100, 1 / 75, 1 / 50)

  y_limit <- max(max(current_rate) * 1.2, 0.02)
  # Reduced from 1.1 to 1.02 to tighten horizontal space right of the data
  x_limit_extended <- x_max * 1.02

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = tot_ae_adm, y = rate, col = rate)
  ) +
    ggplot2::geom_line(
      data = funnel_lines,
      ggplot2::aes(y = upper, group = label),
      color = "black",
      linetype = "dashed",
      alpha = 0.4
    ) +
    ggplot2::geom_hline(yintercept = mu, color = "steelblue", alpha = 0.5) +
    ggplot2::annotate(
      "text",
      x = x_max,
      y = mu,
      colour = "steelblue",
      label = paste0("National average\n(", rate_labeller(mu), ")"),
      hjust = 1.05, # Flipped to inside the plot canvas so it doesn't require clip expansion
      vjust = 0.5,
      size = base * 0.8 / 2.83464, # Matches ggplot text sizing down to baseline scale
      fontface = "italic"
    ) +
    ggiraph::geom_point_interactive(
      aes(tooltip = tooltip),
      size = 2.5,
      alpha = 0.6
    ) +
ggplot2::labs(
      title = str_wrap("Delay-related deaths per trust", wrap),
      # Combine subtitle and caption text using a newline (\n)
      subtitle = str_wrap("Dashed lines represent control limits, which define the range of expected variation with hospital volume.",  wrap*1.25),
      x = "Total type-1 A&E Admissions",
      y = NULL, #"Expected delay-related deaths per 1000 admission",
      colour = str_wrap("Mortality risk rate (e.g., 1 in 100 admissions)", 80)
    ) +
    scale_y_continuous(limits = c(0, y_limit), labels = \(x) {
      str_c(1000 * x, " ‰")
    }) +
    scale_x_continuous(labels = scales::comma) +
    scale_colour_stepsn(
      colors = as.character(base_colors),
      # breaks = rate_breaks,
      # values = scales::rescale(rate_breaks),
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
      
      # Aligns titles to the entire plot width rather than the inner panel grid
      # plot.title.position = "plot", 
      
      # Style the multi-line subtitle block
      plot.subtitle = element_text(
        color = "gray30", 
        size = base * 0.85, 
        lineheight = 1.2 # Adds clean vertical breathing room between the lines
      ),

      axis.title.y = element_text(
        vjust = 2.5,             # Pushes text outward away from the numbers
        margin = margin(r = 10)  # Alternately guarantees a 10pt buffer on the right
      ),
      
      legend.position = "bottom",
      legend.title = element_text(
        hjust = 0.5,
        size = base * 0.9
      ),
      legend.text = element_text(size = base * 0.8)
    )
  p
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
    mutate(rate = excess_mort / tot_ae_adm, .by = c(period, parent_org))

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
      aes(tooltip = scales::comma(round(rate * 1000))),
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
      tooltip_text = paste0(cluster, "\nRate: 1 in ", precise_denom)
    )

  unique_bins <- plot_data %>% arrange(denom) %>% pull(rate_bin) %>% unique()
  breaks <- plot_data %>% arrange(denom) %>% pull(denom) %>% unique()

  # base_colors <- paletteer::paletteer_d("beyonce::X41")
  # pal_func <- colorRampPalette(as.character(base_colors))
  # pal <- pal_func(length(unique_bins))

  base_colors <- paletteer::paletteer_d("beyonce::X41", direction = -1)
  rate_breaks <- c(1 / 400, 1 / 200, 1/150, 1 / 100, 1 / 75, 1 / 50)

  p <- ggplot(plot_data, aes(fill = 1 / denom)) +
    geom_sf_interactive(
      aes(tooltip = tooltip_text, data_id = cluster),
      col = "white",
      linewidth = 0.3
    ) +
    scale_fill_stepsn(
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