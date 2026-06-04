server <- function(input, output, session) {
  # # 1. Create a proper reactive value for the flag
  # landing_page_ready <- reactiveVal(FALSE)
  
  # # Share it via session$userData so your glance module can see it
  # session$userData$landing_page_ready <- landing_page_ready

  # 2. Invoke the landing page module immediately
  glanceServer("at_a_glance")
  dashboardServer("main")

  # # 3. Structural Fix: Execute ONCE when the reactive value flips to TRUE
  # observeEvent(landing_page_ready(), {
  #   req(landing_page_ready() == TRUE)
    
  #   # This initializes the main dashboard exactly once in the background
  #   dashboardServer("main")
    
  # }, once = TRUE) # <-- This stops the infinite loop!
}