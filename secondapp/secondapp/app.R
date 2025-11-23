# Packages ----
library(shiny)  # Required to run any Shiny app
library(ggplot2)  # For creating pretty plots
library(dplyr)  # For filtering and manipulating data
library(bslib)

# Define UI for random distribution app ----
# Sidebar layout with input and output definitions ----
ui <- page_sidebar(
  title = "Tabsets",
  sidebar = sidebar(
    radioButtons(
      "dist",
      "Distribution type:",
      c(
        "Normal" = "norm",
        "Uniform" = "unif",
        "Log-normal" = "lnorm",
        "Exponential" = "exp"
      )
    ),
    br(),
    selectInput(
      inputId = "color",
      label = "Choose your color",
      choices = c("snow", "lightblue", "darkgreen", "pink", "black", "darkred"), selected = "darkred"
    ),
    hr(),
    sliderInput(
      "n",
      "Number of observations:",
      value = 500,
      min = 1, 
      max = 1000
    )
  ),
  # Main panel for displaying outputs ----
  # Output: A tabset that combines three panels ----
  navset_card_underline(
    nav_panel("Plot", plotOutput("plot")),
    nav_panel("Summary", verbatimTextOutput("summary")),
    nav_panel("Table", tableOutput("table"))
  )
  
)

# Define server logic for random distribution app ----

server <- function(input, output) {
  d <- reactive({
    dist <- switch(
      input$dist,
      norm = rnorm,
      unif = runif,
      lnorm = rlnorm,
      exp = rexp,
      rnorm
    )
    dist(input$n)
  })
  
  output$plot <- renderPlot({
    dist <- input$dist
    n <- input$n
    
    hist(
      d(),
      main = paste("r", dist, "(", n, ")", sep = ""),
      col = input$color,
      border = "white"
    )
  })
  
  output$summary <- renderPrint({
    summary(d())
  })
  
  output$table <- renderTable({
    d()
  })
  
}


# Create Shiny app ----
shinyApp(ui, server) 
