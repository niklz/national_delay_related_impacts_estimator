library(shiny)
library(bslib)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(magrittr)
library(scales)
library(stringr)
library(lubridate)
library(ggiraph)
library(sf)
library(shinycssloaders)
library(shinyWidgets)
require(rmapshaper)
require(ggrepel)
library(geomtextpath)
library(reactable)
library(htmltools)


# UI params
PLOT_TITLE_WRAP <- 65
BASE_FONT_SIZE <- 11
SPINNER_TYPE <- 8
NEG_CLR <- "#991b1b"
POS_CLR <- "#166534"
NEG_CLR_LGT <- monochromeR::generate_palette(NEG_CLR, modification = "go_lighter", 2)[2]
POS_CLR_LGT <- monochromeR::generate_palette(POS_CLR, modification = "go_lighter", 2)[2]


# Utils
source("utils.R")
# Source modules
source("modules/mod_main.R")
source("modules/mod_glance.R")
source("modules/mod_deepdive.R")

# Read data
ae_impacts <- read_csv(
  "https://raw.githubusercontent.com/niklz/excess_impacts_national/refs/heads/main/data/ae_impacts.csv"
)

# long data for time series
ae_impacts_long <- prep_historical_data(ae_impacts)
# Pre-compile the selection lists upfront (Global environment)
geo_choices <- list(
  region  = sort(unique(ae_impacts_long$Group_Name[ae_impacts_long$Level == "region"])),
  cluster = sort(unique(ae_impacts_long$Group_Name[ae_impacts_long$Level == "cluster"])),
  trust   = sort(unique(ae_impacts_long$Group_Name[ae_impacts_long$Level == "trust"]))
)

report_date <- ae_impacts$period %>% max()

region_plot <- readRDS("data/region_plot.RDS")
cluster_shp <- readRDS("data/cluster_shp_simple.RDS")

ae_impacts$period <- as.Date(ae_impacts$period)
available_dates <- sort(unique(ae_impacts$period))
min_date <- min(available_dates, na.rm = TRUE)
max_date <- max(available_dates, na.rm = TRUE)

# ==============================================================================
# PERF FIX: PRE-COMPUTE MATHEMATICAL GRID MATRIX ONCE
# ==============================================================================
over_dispersion <- 3
sigmas_global <- seq(0.5, 3.0, by = 0.5)
x_grid_max <- max(ae_impacts$tot_ae_adm, na.rm = TRUE) * 1.5

mu_global <- sum(ae_impacts$excess_mort, na.rm = TRUE) /
  sum(ae_impacts$tot_ae_adm, na.rm = TRUE)

funnel_base_grid <- tibble(
  tot_ae_adm = seq(10, x_grid_max, length.out = 600)
) %>%
  mutate(
    logit_mu = log(mu_global / (1 - mu_global)),
    logit_se = sqrt(over_dispersion) *
      sqrt(1 / (tot_ae_adm * mu_global * (1 - mu_global)))
  )

global_funnel_lines <- purrr::map_df(sigmas_global, function(z) {
  tibble(
    tot_ae_adm = funnel_base_grid$tot_ae_adm,
    upper = 1 /
      (1 + exp(-(funnel_base_grid$logit_mu + z * funnel_base_grid$logit_se))),
    sigma = as.character(z),
    z_val = z
  )
})


global_funnel_ribbons <- tibble()
if (length(sigmas_global) >= 2) {
  stripe_indices <- seq(1, length(sigmas_global) - 1, by = 2)
  global_funnel_ribbons <- purrr::map_df(stripe_indices, function(i) {
    z_lower <- sigmas_global[i]
    z_upper <- sigmas_global[i + 1]
    tibble(
      tot_ae_adm = funnel_base_grid$tot_ae_adm,
      ymin = 1 /
        (1 +
          exp(
            -(funnel_base_grid$logit_mu + z_lower * funnel_base_grid$logit_se)
          )),
      ymax = 1 /
        (1 +
          exp(
            -(funnel_base_grid$logit_mu + z_upper * funnel_base_grid$logit_se)
          )),
      group_id = paste0(z_lower, "-", z_upper)
    )
  })
}

# ==============================================================================
# AT A GLANCE TABLE DATA
# ==============================================================================

table_data <- local({
  processed_data <- ae_impacts %>%
    filter(period == max(period)) %>%
    select(
      Trust = org,
      # Region = parent_org,
      `Total admissions` = tot_ae_adm,
      `Number of DTA > 4 hours` = dta_gt4,
      `Estimated DRD` = excess_mort,
      `Percent change (DRD)` = trend_velocity_pct,
      Trend = status_arrow
    ) %>%
    mutate(`Proportion DTA > 4 hours` = 100*(`Number of DTA > 4 hours`/`Total admissions`)) %>%
    mutate(`Estimated deaths per thousand admissions` = case_when(
      (`Estimated DRD` != 0 & `Total admissions` != 0) ~ 1000*(`Estimated DRD`/`Total admissions`),
      (`Estimated DRD` == 0 | `Total admissions` ==0) ~ 0,
      .default = 0)
    )%>%
    # FIX: Round the big volume metrics to whole numbers, but keep precision for the percentage!
    mutate(
      across(
        c(`Total admissions`, `Number of DTA > 4 hours`, `Estimated DRD`),
        ~ round(.x, 0)
      ),
      across(
      c(`Percent change (DRD)`, `Proportion DTA > 4 hours`),  ~ round(.x, 2)
    ))

  total_row <- processed_data %>% filter(Trust == "Total")
  main_data <- processed_data %>%
    filter(Trust != "Total") %>%
    arrange(desc(`Estimated deaths per thousand admissions`))

  bind_rows(total_row, main_data)
})

top_worsening <- table_data %>%
  filter(Trend == "▲ Growth") %>%
  select(
    -`Total admissions`,
    -`Number of DTA > 4 hours`,
    -`Estimated DRD`,
    -`Proportion DTA > 4 hours`,
    -Trend
  ) %>%
  arrange(desc(`Percent change (DRD)`))

top_improving <- table_data %>%
  filter(Trend == "▼ Decline") %>%
  select(
    -`Total admissions`,
    -`Number of DTA > 4 hours`,
    -`Estimated DRD`,
    -`Proportion DTA > 4 hours`,
    -Trend
  ) %>%
  arrange(`Percent change (DRD)`)