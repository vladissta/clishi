library(shiny)
library(ggplot2)  # For creating pretty plots
library(dplyr)  # For filtering and manipulating data
library(bslib)
library(shinythemes)

# Define UI ----
ui <- page_sidebar(
  title = "My Shiny App and basic widgets",
  actionButton("action", label = "action"),
  shinythemes::themeSelector(),
  sidebar = sidebar("Shiny is available on CRAN",
                    code("install.packages('shiny')"),
                    actionButton("action", label = "Action")),
  card(
    card_header("Introducing shiny"),
    "Shiny is a package from Posit",
    card_image("https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcQaf96u1wrCUDNus5ywN4qNGzjV94vO3puQtL-orlMHkkiwQ7o6Nc-lzqa8Ss0R_Iv0HyY&usqp=CAU", height = "300px", width = "300px"),
    card_footer("Shiny is a product of Posit")
    ),
  card(
      card_header("Buttons"), 
      actionButton("action", label = "Action"),
      submitButton("Submit")
    ),
  card(
    card_header("Single checkbox"),
    checkboxInput("checkbox", "Choice A", value = TRUE)
  ),
  card(
    card_header("Checkbox group"),
    checkboxGroupInput(
      "checkGroup",
      "Select all that apply",
      choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),
      selected = 1
    )
  ),
  card(
    card_header("Date input"),
    dateInput("date", "Select date", value = "2014-01-01")
  ),
  card(
    card_header("Date range input"),
    dateRangeInput("dates", "Select dates")
  )
)
  



# Define server logic ----
server <- function(input, output) {

}

# Run the application ----
shinyApp(ui = ui, server = server)
