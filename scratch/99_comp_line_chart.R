# --- DYNAMIC RANGE CALCULATIONS (Crucial for Shiny) ---
min_date   <- min(plot_df$Month_Date)
max_date   <- max(plot_df$Month_Date)
date_span  <- as.numeric(max_date - min_date)


# Dynamically calculate the perfect horizontal jump (5% of the total timeline width)
dynamic_nudge <- 0.05 * date_span 

label_data <- plot_df %>% filter(Month_Date == max_date)

pal <- paletteer_d("lisa::ClaudeMonet_1") %>%
  as.character() %>%
  set_names(label_data$Group_Name)  %>%
  `[[<-`("Total", "cornsilk4")

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
    
    plot.margin = margin(20, 10, 20, 40), 
    
    plot.background = element_rect(fill = "grey98", color = "grey98"),
    panel.background = element_rect(fill = "grey98", color = "grey98"),
    
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey91", size = 0.5),
    
    plot.title = element_text(color = "grey10", size = 28, face = "bold", margin = margin(t = 15)),
    plot.subtitle = element_markdown(color = "grey30", size = 16, lineheight = 1.35, margin = margin(t = 15, b = 40)),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.caption = element_text(color = "grey30", size = 13, lineheight = 1.2, hjust = 0, margin = margin(t = 40)),
    legend.position = "none"
  )

# --- PLOT ---
ggplot(
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
      data_id = Group_Name,
      tooltip = paste0(
        "<strong>", Group_Name, "</strong><br/>",
        "Month: ", format(Month_Date, "%B %Y"), "<br/>",
        "Rate: ", round(1000 * rate, 1)
      )
    )
  ) +
  ggrepel::geom_text_repel(
    data = label_data, 
    aes(color = Group_Name, label = str_wrap(Group_Name, 20)),
    fontface = "bold",
    size = 7,
    direction = "y",
    lineheight = 0.9,
    hjust = 0,              # Left-aligns the text box
    segment.size = .5,
    segment.alpha = .6,
    segment.linetype = "dotted",
    box.padding = .25,
    
    # FIX: Explicitly forces all labels to shift right by a proportion of the timeline
    nudge_x = dynamic_nudge 
  ) +
  scale_colour_manual(values = pal) +
  scale_x_date(
    breaks = unique(plot_df$Month_Date), 
    labels = function(x) {
      ifelse(lubridate::month(x) == 1, format(x, "%b\n%Y"), format(x, "%b"))
    },
    # Expands the gray panel canvas by 45% on the right to perfectly accommodate the text
    expand = expansion(mult = c(0.05, 0.45)) 
  ) +
  coord_cartesian(clip = "off") +
  labs(title = NULL, subtitle = NULL, x = NULL, y = NULL) +
  shared_theme