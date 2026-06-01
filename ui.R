# UI.R

ui <- page_navbar(
  title = tags$p(
    style = "margin: 0; padding-top: 0.25rem; font-size: 24px; color: #000000; max-width: 900px; line-height: 1.4;",
    "NHS National A&E Delay-Related Impacts Dashboard"
  ),

  header = tags$p(
    style = "margin: 0; padding-top: 0.25rem; font-size: 18px; color: #555; max-width: 1080px; line-height: 1.4;",
    "This dashboard displays estimated excess deaths associated with prolonged waits for A&E admission, ",
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

  # CRITICAL FIX: Use your module's UI function here!
  # This automatically matches the dashboardServer("main") in server.R
  dashboardUI(
    id = "main", 
    min_date = min_date, 
    max_date = max_date, 
    SPINNER_TYPE = SPINNER_TYPE
  )
)