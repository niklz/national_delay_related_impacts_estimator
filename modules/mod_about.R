#' @export
aboutUI <- function(id) {
  ns <- NS(id)

  nav_panel(
    title = "About",
    icon = icon("info-circle"),
    div(
      style = "max-width: 900px; margin: 0 auto; padding: 40px 20px;",

      # Section 1: Methodology
      tags$h3(
        "About the dashboard",
        style = "font-weight: bold; margin-bottom: 15px;"
      ),
      tags$p(
        style = "font-size: 1.1rem; line-height: 1.6; margin-bottom: 20px;",
        "Studies have indicated that crowding within A&E departments is associated with patient harm. Boarding is the practice of holding patients in the A&E department while they wait for an impatient bed and repressents a clinically unproductive delay.",
      ),
      tags$p(
        style = "font-size: 1.1rem; line-height: 1.6; margin-bottom: 20px;",
        "This dashboard estimates the excess clinical impacts associated with prolonged boarding times, hightlighting the 'hidden' costs of admission bottlenecks",
        "Estimates of these impacts are produced by applying the risk associations established by ",
        tags$a(
          href = "https://doi.org/10.1136/emermed-2025-214983",
          target = "_blank",
          "Howlett et al.",
          style = "color: #003087; text-decoration: underline;"
        ),
        ", utilising robust statistical modeling that controls for patient demographics, arrival times, and other sources of delays to emergency admissions. This study found that each additional 4 hours of boarding time was associated with an extra 8.6 hours of inpatient length of stay and an 8.4% increase in the odds of 30-day mortality."
      ),

      tags$hr(style = "margin: 30px 0;"),

      # Section 2: Data & Metrics
      tags$h3(
        "Data sources & metrics",
        style = "font-weight: bold; margin-bottom: 15px;"
      ),
      tags$p(
        style = "font-size: 1.1rem; line-height: 1.6; margin-bottom: 20px;",
        "The impact metrics are computed based on waiting times and admission volumes which are sourced directly from ",
        tags$a(
          href = "https://www.england.nhs.uk/statistics/statistical-work-areas/ae-waiting-times-and-activity/",
          target = "_blank",
          "NHS England Statistics",
          style = "color: #003087; text-decoration: underline;"
        ),
        ". We report two distinct measures of delay-related harm:"
      ),

      # Bulleted metrics section to utilize extra vertical space beautifully
      tags$ul(
        style = "font-size: 1.1rem; line-height: 1.7; padding-left: 20px;",
        tags$li(
          style = "margin-bottom: 12px;",
          tags$strong("Delay-Related Deaths (DRD): "),
          "The estimated increase in mortality statistically linked to admission delays, contextualised as a rate per 1,000 Type-1 A&E admissions to normalise hospital size differences."
        ),
        tags$li(
          tags$strong("Excess Length of Stay (LOS): "),
          "The additional in-hospital stay due to initial A&E bottlenecks, accumlated over all admissions and scaled to represent the number of excess acute beds in continuous use over the selected period."
        )
      ),
      tags$p(
        style = "font-size: 1.1rem; line-height: 1.6; margin-bottom: 20px;",
        "All the data-processing and calculation steps for these metrics can be found on ",
        tags$a(
          href = "https://github.com/niklz/excess_impacts_national",
          target = "_blank",
          "github",
          style = "color: #003087; text-decoration: underline;"
          ),
        "."
        ),
      tags$hr(style = "margin: 30px 0;"),

      tags$h3(
        "Table of contents",
        style = "font-weight: bold; margin-bottom: 15px;"
      ),
      tags$h5(
        "At a glance",
        style = "font-weight: bold; margin-bottom: 15px;"
      ),
      tags$p(
        style = "font-size: 1.1rem; line-height: 1.6; margin-bottom: 20px;",
        "This tabular overview provides a master record of the raw A&E admissions and wait-times across England. Also included are cateorgies denoting the recent (latest 3 months) trend of specific trusts and tables ranking those trusts which are improving or deteriorating. This trend is computed from a STL (Seasonal and Trend decomposition using LOESS) model.",
      ),
            tags$h5(
        "Graphical overview & outlier analysis",
        style = "font-weight: bold; margin-bottom: 15px;"
      ),
      tags$p(
        style = "font-size: 1.1rem; line-height: 1.6; margin-bottom: 20px;",
        "This panel hosts 3 figures visualising DRD:",
        tags$ul(
        style = "font-size: 1.1rem; line-height: 1.7; padding-left: 20px;",
        tags$li(
          style = "margin-bottom: 12px;",
          "A regional time-series, facilitating trend analysis and system seasonal pressures."
        ),
        tags$li(
          "A spatial heatmap aiding in the identification of regional hotspots and giving ICB / cluster perfomance at a glance."
        ),
          tags$li(
          "A funnel chart allowing comparission on admission delays between trusts of different sizes. The inclusion of control limits help users avoid misinterpretting noise in the data."
        )
      )
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