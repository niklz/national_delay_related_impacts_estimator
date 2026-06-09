server <- function(input, output, session) {
  glanceServer("at_a_glance")
  dashboardServer("main", max_date = max_date)
}