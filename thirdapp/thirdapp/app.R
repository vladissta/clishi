library(shiny)
library(ggplot2)  # For creating pretty plots
library(dplyr)  # For filtering and manipulating data
library(bslib)

# Define UI ----
ui <- page_navbar(
  title = "training example",
  layout_sidebar(
    sidebar = sidebar("sidebar"),
    card(
      "Card content"
    ),
    "Main contents"
  )
)

# Define server logic ----
server <- function(input, output) {

}

# Run the application ----
shinyApp(ui = ui, server = server)
