# UI.R

ui <- page_navbar(
  title = 
    tags$strong(style = "color: #393939", "Estimated impacts due to delayed admissions from A&E"),
  # title = tags$p(
  #   style = "margin: 0; padding: 0; font-size: 20px; font-weight: 600; color: #000000; max-width: 900px; line-height: 1.2;",
  #   "Estimated impacts from delayed admission from A&E"
  # ),

  theme = bs_theme(
    version = 5,
    bg = "#ffffff",
    fg = "#333333"
    # primary = "#0#03087"
  ),

  # Link directly to your CSS file inside the www directory
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),

  glanceUI(
    id = "at_a_glance",
    min_date = min_date,
    max_date = max_date,
    SPINNER_TYPE = SPINNER_TYPE,
    title = PANEL_TITLE_1
  ),

  dashboardUI(
    id = "main",
    min_date = min_date,
    max_date = max_date,
    SPINNER_TYPE = SPINNER_TYPE,
    title = PANEL_TITLE_2
  ),

  deepDiveUI(
    id = "deep_dive",
    SPINNER_TYPE = SPINNER_TYPE,
    title = PANEL_TITLE_3
  ),
  
  aboutUI(id = "about_page")
)