server <- function(input, output, session) {

  historical_data_r <- reactive({
  # This should be your historical long data frame spanning months
  ae_impacts_long 
})

  glanceServer("at_a_glance")
  dashboardServer("main", max_date = max_date)
  deepDiveServer(id = "deep_dive", ts_data = historical_data_r, choices_list = geo_choices)
}