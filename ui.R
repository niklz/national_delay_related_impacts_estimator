# UI.R

ui <- page_navbar(
  title = tags$p(
    style = "margin: 0; padding: 0; font-size: 20px; font-weight: 600; color: #000000; max-width: 900px; line-height: 1.2;",
    "Estimated impacts from delayed admission from A&E"
  ),

  header = tags$p(
    style = "margin: 0; padding-top: 0rem; font-size: 18px; color: #555; line-height: 1.4;",
    "This dashboard displays estimated excess deaths related with prolonged waits for A&E admission (delay-related deaths - DRD), ",
    "applying the risk associations established in ",
    tags$a(
      href = "https://doi.org/10.1136/emermed-2025-214983",
      target = "_blank",
      "Howlett et al.",
      style = "color: #003087; text-decoration: underline;"
    ),
    ". Waiting times and admission volumes data are sourced directly from ",
    tags$a(
      href = "https://www.england.nhs.uk/statistics/statistical-work-areas/ae-waiting-times-and-activity/",
      target = "_blank",
      "NHS England Statistics",
      style = "color: #003087; text-decoration: underline;"
    ),
    ". All metrics are expressed as rates per 1,000 Type-1 A&E admissions."
  ),

  theme = bs_theme(
    version = 5,
    bg = "#ffffff",
    fg = "#333333",
    primary = "#003087"
  ),

  # Link directly to your CSS file inside the www directory
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),

  glanceUI(
    id = "at_a_glance",
    min_date = min_date,
    max_date = max_date,
    SPINNER_TYPE = SPINNER_TYPE
  ),

  dashboardUI(
    id = "main",
    min_date = min_date,
    max_date = max_date,
    SPINNER_TYPE = SPINNER_TYPE
  ),

  deepDiveUI(
    id = "deep_dive",
    SPINNER_TYPE = SPINNER_TYPE
  )
)