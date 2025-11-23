library(shiny)
library(ggplot2)  # For creating pretty plots
library(dplyr)  # For filtering and manipulating data
library(bslib)

# Define UI ----
ui <- page_navbar(
  title = "training example",
  layout_sidebar(
    sidebar = sidebar("sidebar"),
    value_box(
      title = "Value box",
      value = "100% of your attention is here",
      showcase = bsicons::bs_icon("bar-chart"),
      theme = "red"
    ),
    card(
      card_header("Header"),
      card_image("https://i.pinimg.com/236x/d9/42/66/d94266793cf8cb2220810979e0ae926f.jpg"),
      "Card content"
    ),
    card(
      card_header("Another card"),
      card_image("https://pbs.twimg.com/media/Edi5vQHXoAArFjf.png"),
      "Hello"
    ),
    "Main contents"
  )
)

# Define server logic ----
server <- function(input, output) {

}

# Run the application ----
shinyApp(ui = ui, server = server)
