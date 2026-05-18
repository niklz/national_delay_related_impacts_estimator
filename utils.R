funnel_plot <- function(data) {

  over_dispertion <- 3
  rate_breaks <- c(1 / 400, 1 / 200, 1 / 100, 1 / 50)
  # add labels to these if you want to display labels on chart
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
  z_scores <- (current_rate - mu) / sqrt(mu * (1 - mu) / plot_data$tot_ae_adm)

  # Generate Dynamic Funnel Lines
  x_min <- min(plot_data$tot_ae_adm)
  x_max <- max(plot_data$tot_ae_adm)

  # Create a sequence for the lines
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

  unique_bins <- plot_data %>%
    arrange(denom) %>%
    pull(rate_bin) %>%
    unique()

  breaks <- plot_data %>%
    arrange(denom) %>%
    pull(denom) %>%
    unique()

  base_colors <- paletteer::paletteer_d("beyonce::X41")
  pal_func <- colorRampPalette(as.character(base_colors))
  pal <- pal_func(length(unique_bins))

  rate_breaks <- c(1 / 400, 1 / 200, 1 / 100, 1 / 75, 1 / 50)

  y_limit <- max(max(current_rate) * 1.2, 0.02)
  x_limit_extended <- x_max * 1.1

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = tot_ae_adm, y = rate, col = rate)
  ) +
    # 2. Control Limits
    ggplot2::geom_line(
      data = funnel_lines,
      ggplot2::aes(y = upper, group = label),
      color = "black",
      linetype = "dashed",
      alpha = 0.4
    ) +

    # 3. Clean Baseline & Label
    ggplot2::geom_hline(yintercept = mu, color = "black", alpha = 0.5) +
    ggplot2::annotate(
      "text",
      x = x_max,
      y = mu,
      label = paste0("National average\n(", rate_labeller(mu), ")"),
      hjust = -0.1, # Shift slightly right of the last data point
      vjust = 1.25,
      size = 3.5,
      fontface = "italic"
    ) +
    ggiraph::geom_point_interactive(
      aes(tooltip = tooltip),
      size = 2.5,
      alpha = 0.6
    ) +
    ggplot2::labs(
      title = "Delay-related mortality funnel plot",
      subtitle = "Each dot represents a major (type-1) A&E department",
      caption = "Dashed lines represent control limits, which define the range of expected statistical variation based on hospital volume.",
      x = "Total type-1 A&E Admissions",
      y = "Risk rate of delay-related deaths per 1000 admission"
    ) +
    scale_y_continuous(limits = c(0, y_limit), labels = \(x) {
      str_c(1000 * x, " ‰")
    }) +
    scale_x_continuous(labels = scales::comma) +
    scale_colour_stepsn(
      colors = as.character(base_colors),
      breaks = rate_breaks, # Use your 1/400, 1/200, etc.
      values = scales::rescale(rate_breaks), # Ensures colors align with actual values
      labels = rate_labeller,
      guide = guide_colorsteps(
        even.steps = TRUE, # Makes the legend blocks equal width for readability
        barwidth = unit(0.8, 'npc'),
        title = "Mortality Risk (e.g., 1 in 100 admissions)"
      )
    ) +
    ggplot2::coord_cartesian(
      xlim = c(x_min, x_limit_extended),
      ylim = c(0, y_limit),
      clip = "off"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.margin = margin(5, 50, 5, 5),
      panel.grid.minor = element_blank(),
      plot.caption = element_text(hjust = 0, color = "gray30", size = 9)
    )
  p
}


rate_labeller <- function(x) {
  # Small epsilon check for zero
  ifelse(x < 1e-10, "0", paste0("1 in ", round(1 / x)))
}

per_k_labeller <- function(x) {
  # Small epsilon check for zero
  ifelse(x < 1e-10, "0", paste0(round(1000*x), " per mille\n", "(1 in ", round(1 / x), ")"))
}

# Helper to bin the data into "1 in X" categories
round_denom <- function(val, round = 25) {
  if (is.na(val) || val == 0) return("0")
  
  # Calculate denominator and round to nearest round
  denom <- 1 / val
  rounded_denom <- round(denom / round) * round
  
  return(rounded_denom)
}

time_series_plot <- function(data, plot_region) {

 region_shp <- st_read("data/shapefiles/region/region.shp")

plot_data <- data %>%
  filter(period > max(period)-dmonths(6), parent_org != "Total") %>%
  mutate(parent_org = str_wrap(str_to_title(str_trim(str_remove_all(parent_org, "NHS England"))), width = 15)) %>%
  summarise(across(c(excess_mort, tot_ae_adm), sum), .by = c(period, parent_org)) %>%
  mutate(rate = excess_mort/tot_ae_adm, .by = c(period, parent_org))




label_data <- plot_data %>% 
  filter(period == max(period))


# 1. Map: Extreme simplification + static lines
# region_plot <- region_shp %>%
#   ms_simplify(keep = 0.005, keep_shapes = TRUE) %>% 
#   ggplot(aes(fill = parent_org, data_id = parent_org)) +
#   geom_sf_interactive(colour = "white", size = 0.5, alpha = 0.15) + 
#   coord_sf(datum = NA) +
#   theme_void() +
#   paletteer::scale_fill_paletteer_d("MetBrewer::Hokusai1") +
#   theme(legend.position = "none")

# 2. Time Series: Mix static and interactive layers
ts_plot <- ggplot(plot_data, aes(x = period, y = rate*1000, 
                                 col = parent_org, group = parent_org,
                                 data_id = parent_org)) +
  # Keep the halo STATIC (standard geom_line) to save memory
  geom_line(linewidth = 2.5, col = "white") + 
  # Only the colored line is interactive
  geom_line_interactive(linewidth = 1.2) +
  geom_point_interactive(
    aes(tooltip = scales::comma(round(rate*1000))),
    size = 2.5, 
    hover_nearest = TRUE
  ) +
  geom_text_repel_interactive(
    data = label_data, aes(label = parent_org, data_id = parent_org), 
    hjust = 0, nudge_x = 10, direction = "y", 
    segment.color = NA, size = 3.5, fontface = "bold"
  ) +
  scale_x_date(expand = expansion(mult = c(0.05, 0.3))) + 
  scale_y_continuous(labels = scales::comma) +
  paletteer::scale_color_paletteer_d("MetBrewer::Hokusai1") +
  labs(title = str_wrap("Estimated monthly excess deaths, per 1000 type-1 A&E admissions per region", 50), x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title = element_text(angle = 0, vjust = 1, hjust = 0, face = "bold")
  );
  ts_plot + inset_element(plot_region, on_top = FALSE, left = -0.15, bottom = 0, right = 0.9, top = 1)
}

choropleth_plot <- function(data, shp) {


  cluster_impacts <- data %>%
  filter(period == max(period, na.rm = TRUE), org != "Total", icb_name != "") %>%
  summarise(across(c(excess_mort, tot_ae_adm), sum), .by = c(period, cluster))


# Process the data
plot_data <- shp %>%
  left_join(cluster_impacts, by = join_by(cluster == cluster)) %>%
  mutate(
    rate = excess_mort/tot_ae_adm,
    denom = sapply(rate, round_denom, round = 10),
    rate_bin = str_c("1 in ", denom),
    # Create a numeric version of the bin for sorting purposes
    bin_numeric = 1 / as.numeric(str_extract(rate_bin, "\\d+")),
    precise_denom = round(1 / rate),
    # Tooltip string with the specific '1 in X' value
    tooltip_text = paste0(cluster, "\nRate: 1 in ", precise_denom)
  )

# Create a color mapping for ONLY the bins present in the data
unique_bins <- plot_data %>% 
  arrange(denom) %>% 
  pull(rate_bin) %>% 
  unique()

breaks <- plot_data %>% 
  arrange(denom) %>% 
  pull(denom) %>% 
  unique()

base_colors <- paletteer::paletteer_d("beyonce::X41")
pal_func <- colorRampPalette(as.character(base_colors))
pal <- pal_func(length(unique_bins))


ggplot(plot_data, aes(fill = 1/denom)) + # Use numeric fill for stepsn
  geom_sf(col = "white", linewidth = 0.3) +
  scale_fill_stepsn(
    # Use the hex codes from paletteer here
    colors = as.character(pal), 
    breaks = 1/breaks,
    values = scales::rescale(1/breaks),
    labels = rate_labeller,
    limits = range(1/breaks),
    guide = guide_colorsteps(
      even.steps = FALSE, 
      show.limits = FALSE,
      title.position = "top",
      barheight = unit(0.05, 'npc'),
      barwidth = unit(1, 'npc') 
    )
  ) +
  theme_void() +
  labs(
    fill = "Risk rate (excess expected deaths)",
    caption = "Rates binned to nearest 1/25 resolution"
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(hjust = 0.5, face = "bold"),
    legend.text = element_text(size = 8)
  )



p <- ggplot(plot_data, aes(fill = 1/denom)) + 
  geom_sf_interactive(
    aes(
      tooltip = tooltip_text, 
      data_id = cluster
    ),
    col = "white", 
    linewidth = 0.3
  ) +
  scale_fill_stepsn(
    colors = as.character(pal), 
    breaks = 1/breaks,
    values = scales::rescale(1/breaks), 
    labels = rate_labeller,
    limits = range(1/breaks),
    guide = guide_colorsteps(
      even.steps = FALSE, 
      show.limits = FALSE,
      title.position = "top",
      barheight = unit(0.05, 'npc'),
      barwidth = unit(0.9, 'npc') 
    )
  ) +
  labs(
    fill = "Risk rate (excess expected deaths, per type-1 A&E admission)",
    # caption = "Rates binned to nearest 1/25 resolution"
  ) +
    theme_void() + 
  theme(
    legend.position = "bottom",
    legend.title = element_text(hjust = 0.5, face = "bold"),
    legend.text = element_text(size = 8)
  );p

  
}
