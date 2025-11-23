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
      card_header("Header"),
      card_image("https://i.pinimg.com/236x/d9/42/66/d94266793cf8cb2220810979e0ae926f.jpg"),
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
