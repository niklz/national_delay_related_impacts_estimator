#' @export
aboutUI <- function(id) {
  ns <- NS(id)
  
  nav_panel(
    "About",
    tags$div(
      style = "max-width: 900px; margin-top: 20px;",
      tags$hr(style = "border-top: 1px solid #ccc;"),
      tags$p(
        style = "margin-top: 15px; font-size: 18px; color: #555; line-height: 1.4;",
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
      )
    )
  )
}

#' @export
aboutServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Server logic goes here if the About page becomes interactive
  })
}