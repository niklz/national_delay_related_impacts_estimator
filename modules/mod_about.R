#' @export
aboutUI <- function(id, title_1, title_2, title_3) {
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
        "Studies have indicated that crowding and delays within A&E departments is associated with patient harm. Boarding is the practice of holding patients in the A&E department while they wait for an impatient bed and represents a clinically unproductive delay."
      ),
      tags$p(
        style = "font-size: 1.1rem; line-height: 1.6; margin-bottom: 20px;",
        "This dashboard estimates the excess clinical impacts associated with prolonged boarding times, highlighting the 'hidden' costs of admission bottlenecks. ",
        "Estimates of these impacts are produced by applying the risk associations established by ",
        tags$a(
          href = "https://doi.org/10.1136/emermed-2025-214983",
          target = "_blank",
          "Howlett et al.",
          style = "color: #003087; text-decoration: underline;"
        ),
        ", utilising robust statistical modeling that controls for patient demographics, arrival times, and other sources of delays to emergency admissions. This study found that ",
        tags$strong("each additional 4 hours of boarding time was associated with an extra 8.6 hours of inpatient length of stay and an 8.4% increase in the odds of 30-day mortality.")
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
          tags$strong("NHS England Statistics"),
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
          "The estimated increase in mortality statistically linked to admission delays, both in raw count and normalised as a rate per 1,000 Type-1 A&E admissions to adjust for hospital size differences."
        ),
        tags$li(
          tags$strong("Excess Length of Stay (LOS): "),
          "The additional in-hospital stay due to initial A&E bottlenecks, accumulated over all admissions and scaled to represent the number of excess acute beds in continuous use over the selected period."
        )
      ),
      tags$p(
        style = "font-size: 1.1rem; line-height: 1.6; margin-bottom: 20px;",
        tags$strong("All the data-processing and calculation steps for these metrics can be found on ",
                    tags$a(
                      href = "https://github.com/niklz/excess_impacts_national",
                      target = "_blank",
                      "GitHub",
                      style = "color: #003087; text-decoration: underline;"
                    ),
                    ".")
      ),
      
      tags$hr(style = "margin: 30px 0;"),
      
      # Section 3: Dashboard Architecture
      tags$h3(
        "Contents",
        style = "font-weight: bold; margin-bottom: 15px;"
      ),
      tags$h5(
        title_1, # Replace with actual variable/text if needed
        style = "font-weight: bold; margin-bottom: 15px;"
      ),
      tags$p(
        style = "font-size: 1.1rem; line-height: 1.6; margin-bottom: 20px;",
        "A tabular overview of the raw A&E admissions and wait-times data across England for the latest month. Includes an ", tags$strong("STL (Seasonal and Trend decomposition using LOESS)"), " model tracking 3-month trajectories to isolate improving or deteriorating Acute Trusts."
      ),
      tags$h5(
        title_2, # Replace with actual variable/text if needed
        style = "font-weight: bold; margin-bottom: 15px;"
      ),
      tags$p(
        style = "font-size: 1.1rem; line-height: 1.6; margin-bottom: 20px;",
        "Visualises DRD via adjustable regional time-series for seasonal analysis, spatial heatmaps for ICB/Cluster hotspot detection, and a Trust-level funnel plot. The funnel utilises an ", tags$strong("overdispersed binomial distribution (inflation factor of 3)"), " to separate true performance outliers from expected statistical noise."
      ),
      tags$h5(
        title_3, # Replace with actual variable/text if needed
        style = "font-weight: bold; margin-bottom: 15px;"
      ),
      tags$p(
        style = "font-size: 1.1rem; line-height: 1.6; margin-bottom: 20px;",
        "Allows users to isolate and side-by-side compare up to 5 Regions, ICBs, or Trusts over a longitudinal 12-month window. This view compares both the raw DRD and the estimated number of acute hospital beds lost to A&E boarding delays, quantifying the operational impact and failure-demand arising from issues in A&E."
      ),
      
      tags$hr(style = "margin: 30px 0;"),
      
      # Section 4: Author & Contact
      tags$div(
        style = "display: flex; flex-direction: row; flex-wrap: wrap; gap: 40px; margin-top: 20px;",
        
        # Author Sub-section
        tags$div(
          style = "flex: 1; min-width: 300px;",
          tags$h3(
            "Author",
            style = "font-weight: bold; margin-bottom: 15px;"
          ),
          tags$div(
            style = "display: flex; align-items: center; gap: 20px;",
            # Optional: Add an image here. If no image, you can delete the img tag.
            tags$img(
              src = "headshot.jpg", # Replace with your image path (e.g., "headshot.png" if in www folder)
              style = "border-radius: 50%; width: 100px; height: 100px; object-fit: cover; border: 2px solid #003087;"
            ),
            tags$div(
              tags$p(
                style = "font-size: 1.1rem; font-weight: bold; margin-bottom: 5px;",
                "Nick Howlett"
              ),
              tags$p(
                style = "font-size: 1rem; color: #555; margin-bottom: 10px;",
                "Senior Data Scientist / BNSSG ICB"
              ),
              tags$a(
                href = "https://www.linkedin.com/in/nick-howlett-879179a3/", 
                target = "_blank", 
                icon("linkedin"), 
                style = "font-size: 1.2rem; color: #0077b5; margin-right: 10px; text-decoration: none;"
              ),
              tags$a(
                href = "https://github.com/niklz", 
                target = "_blank", 
                icon("github"), 
                style = "font-size: 1.2rem; color: #333; text-decoration: none;"
              )
            )
          )
        ),
        
        # Contact Sub-section
        tags$div(
          style = "flex: 1; min-width: 300px;",
          tags$h3(
            "Contact & Support",
            style = "font-weight: bold; margin-bottom: 15px;"
          ),
          tags$p(
            style = "font-size: 1.1rem; line-height: 1.6; margin-bottom: 15px;",
            "If you have any questions regarding the methodology, data sources, or would like to report an issue with the dashboard, please reach out."
          ),
          tags$p(
            style = "font-size: 1.1rem; margin-bottom: 5px;",
            icon("envelope", style = "margin-right: 10px; color: #003087;"),
            tags$a(
              href = "mailto:nick.howlett5@nhs.net",
              "nick.howlett5@nhs.net",
              style = "color: #003087; text-decoration: underline;"
            )
          ),
          tags$p(
            style = "font-size: 1.1rem;",
            icon("bug", style = "margin-right: 10px; color: #003087;"),
            tags$a(
              href = "https://github.com/niklz/excess_impacts_national/issues",
              target = "_blank",
              "Report an issue on GitHub",
              style = "color: #003087; text-decoration: underline;"
            )
          )
        )
      )
    )
  )
}