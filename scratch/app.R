library(shiny)
library(bslib)
library(ggplot2)
library(ggiraph)

ui <- page_navbar(
  title = "Interactive Plots (Capped Native Scaling)",
  theme = bs_theme(version = 5),
  
  # CRITICAL STRUCTURAL OVERRIDE:
  # Intercepts ggiraph's automatic layout rules, eliminates the vertical padding loop, 
  # and locks the entire graphic inside a rigid aspect-ratio box that respects card heights.
  tags$head(
    tags$style(HTML("
      /* 1. Neutralize ggiraph's width-based padding container */
      .html-widget.girafe > div {
        padding-top: 0 !important;
        height: 100% !important;
        width: 100% !important;
        display: flex !important;
        align-items: center !important;
        justify-content: center !important;
      }
      
      /* 2. Bind the SVG to a hard aspect ratio and set layout caps */
      .html-widget.girafe svg {
        max-width: 100% !important;
        max-height: 100% !important;
        width: auto !important;
        height: auto !important;
        aspect-ratio: 7 / 4.5 !important;
        object-fit: contain !important;
      }
    "))
  ),
  
  nav_panel(
    "Dashboard",
    layout_columns(
      col_widths = c(4, 4, 4),
      
      # Card 1
      card(
        full_screen = TRUE,
        card_header("Plot 1: Scatter Plot"),
        card_body(
          fill = TRUE,
          class = "d-flex flex-column align-items-center justify-content-center",
          style = "overflow: hidden !important;", 
          
          div(
            style = "width: 100%; margin-bottom: 10px; flex-shrink: 0;",
            selectInput("dataset1", "Dataset:", 
                        choices = c("mtcars", "iris", "faithful"),
                        selected = "mtcars"),
            selectInput("color1", "Color:", 
                        choices = c("blue", "red", "green", "purple"),
                        selected = "blue")
          ),
          
          # Simplified, clean container layout block
          div(
            style = "flex-grow: 1; width: 100%; height: 100%; min-height: 0; overflow: hidden;",
            girafeOutput("plot1", width = "100%", height = "100%")
          )
        )
      ),
      
      # Card 2
      card(
        full_screen = TRUE,
        card_header("Plot 2: Bar Chart"),
        card_body(
          fill = TRUE,
          class = "d-flex flex-column align-items-center justify-content-center",
          style = "overflow: hidden !important;",
          
          div(
            style = "width: 100%; margin-bottom: 10px; flex-shrink: 0;",
            selectInput("dataset2", "Dataset:", 
                        choices = c("mtcars", "iris"),
                        selected = "iris"),
            sliderInput("bins2", "Number of bins:", 
                        min = 5, max = 50, value = 20)
          ),
          
          div(
            style = "flex-grow: 1; width: 100%; height: 100%; min-height: 0; overflow: hidden;",
            girafeOutput("plot2", width = "100%", height = "100%")
          )
        )
      ),
      
      # Card 3
      card(
        full_screen = TRUE,
        card_header("Plot 3: Density Plot"),
        card_body(
          fill = TRUE,
          class = "d-flex flex-column align-items-center justify-content-center",
          style = "overflow: hidden !important;",
          
          div(
            style = "width: 100%; margin-bottom: 10px; flex-shrink: 0;",
            selectInput("dataset3", "Dataset:", 
                        choices = c("mtcars", "iris"),
                        selected = "mtcars"),
            selectInput("fill3", "Fill color:", 
                        choices = c("steelblue", "coral", "forestgreen", "gold"),
                        selected = "steelblue")
          ),
          
          div(
            style = "flex-grow: 1; width: 100%; height: 100%; min-height: 0; overflow: hidden;",
            girafeOutput("plot3", width = "100%", height = "100%")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {

  # Plot 1: Scatter plot
  output$plot1 <- renderGirafe({
    data <- switch(input$dataset1,
                   "mtcars" = mtcars,
                   "iris" = iris,
                   "faithful" = faithful)
    
    if (input$dataset1 == "mtcars") {
      p <- ggplot(data, aes(x = wt, y = mpg, tooltip = rownames(mtcars))) +
        geom_point_interactive(color = input$color1, size = 3, alpha = 0.7) +
        labs(title = "Weight vs MPG", x = "Weight", y = "Miles per Gallon") +
        theme_minimal()
    } else if (input$dataset1 == "iris") {
      p <- ggplot(data, aes(x = Sepal.Length, y = Sepal.Width, tooltip = Species)) +
        geom_point_interactive(color = input$color1, size = 3, alpha = 0.7) +
        labs(title = "Sepal Length vs Width", x = "Sepal Length", y = "Sepal Width") +
        theme_minimal()
    } else {
      p <- ggplot(data, aes(x = eruptions, y = waiting, tooltip = paste("Eruption:", eruptions))) +
        geom_point_interactive(color = input$color1, size = 3, alpha = 0.7) +
        labs(title = "Old Faithful Eruptions", x = "Eruption time (min)", y = "Waiting time (min)") +
        theme_minimal()
    }
    
    girafe(
      ggobj = p, 
      width_svg = 7,     
      height_svg = 4.5,   
      options = list(
        opts_sizing(rescale = TRUE, width = 1) 
      )
    )
  })
  
  # Plot 2: Histogram
  output$plot2 <- renderGirafe({
    data <- switch(input$dataset2,
                   "mtcars" = mtcars$mpg,
                   "iris" = iris$Sepal.Length)
    
    df <- data.frame(value = data)
    
    p <- ggplot(df, aes(x = value)) +
      geom_histogram_interactive(bins = input$bins2, fill = "steelblue", 
                                 color = "white", alpha = 0.7,
                                 aes(tooltip = after_stat(count))) +
      labs(title = paste("Distribution of", 
                        ifelse(input$dataset2 == "mtcars", "MPG", "Sepal Length")),
           x = "Value", y = "Frequency") +
      theme_minimal()
    
    girafe(
      ggobj = p, 
      width_svg = 7, 
      height_svg = 4.5,
      options = list(
        opts_sizing(rescale = TRUE, width = 1)
      )
    )
  })
  
  # Plot 3: Density plot
  output$plot3 <- renderGirafe({
    data <- switch(input$dataset3,
                   "mtcars" = mtcars$mpg,
                   "iris" = iris$Petal.Length)
    
    df <- data.frame(value = data)
    
    p <- ggplot(df, aes(x = value)) +
      geom_density_interactive(fill = input$fill3, alpha = 0.5,
                               aes(tooltip = after_stat(density))) +
      labs(title = paste("Density of", 
                        ifelse(input$dataset3 == "mtcars", "MPG", "Petal Length")),
           x = "Value", y = "Density") +
      theme_minimal()
    
    girafe(
      ggobj = p, 
      width_svg = 7, 
      height_svg = 4.5,
      options = list(
        opts_sizing(rescale = TRUE, width = 1)
      )
    )
  })
}

shinyApp(ui, server)