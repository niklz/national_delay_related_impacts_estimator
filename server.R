server <- function(input, output, session) {
  load_landing <- reactiveVal(FALSE)
  glanceServer("at_a_glance")

  dashboardServer("main", load_landing = load_landing)

  # Once landing page renders, change load_landing
  session$onFlushed(
    function() {
      load_landing(TRUE)
    },
    once = TRUE
  )
}