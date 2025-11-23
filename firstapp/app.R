# Packages ----
library(shiny)  # Required to run any Shiny app
library(ggplot2)  # For creating pretty plots
library(dplyr)  # For filtering and manipulating data
library(agridat)  # The package where the data comes from
library(bslib)

# Loading data ----
Barley <- as.data.frame(beaven.barley)

# ui.R ----
ui <- fluidPage(
  titlePanel("Barley Yield"),
  sidebarLayout(
    sidebarPanel(
      h3("Inputs for histogram"),
      position = "right",
      selectInput(inputId = "gen",
                  label = "1. Select genotype",
                  choices = c("A" = "a","B" = "b","C" = "c","D" = "d","E" = "e","F" = "f","G" = "g","H" = "h"), selected = "a"),
      hr(),
      selectInput(inputId = "colour",
                  label = "2. Select histogram colour",
                  choices = c("blue","green","red","purple","grey"), selected = "grey"),
      hr(),
      sliderInput(inputId = "bin",
                  label = "3. Select number of histogram bins",
                  min = 1,
                  max = 25,
                  value = c(10)),
      hr(),
      textInput(inputId = "text",
                label = "4. Enter some text to be displayed", ""),
      a(href = "https://ourcodingclub.github.io/tutorials/shiny/", "Source")
    ),
    mainPanel(
      plotOutput("plot"),
      tableOutput("mytable"),
      textOutput("mytext"),
      tags$div(style="color:red",
               tags$p("Visit us at:"),
               tags$a(href = "https://ourcodingclub.github.io", "Coding Club")
      )
    )
  )
)
#___________________________second app__________________________________________
ui1 <- page_sidebar(
  # App title ----
  title = "Hello World!",
  # Sidebar panel for inputs ----
  sidebar = sidebar( #убираемая боковая панель!!!!!
    # Input: Slider for the number of bins ----
    sliderInput(inputId = "bins",
                label = "Number of bins",
                min = 5,
                max = 50,
                value = 30)
  ),
  plotOutput(outputId = "distPlot")
)




# server.R ----
server <- function(input, output) {
  output$plot  <- renderPlot(ggplot(Barley, aes(x = yield)) +
                               geom_histogram(bins = input$bin, fill = input$colour, data = Barley[Barley$gen == input$gen,], colour = "black")
                             )
  
  output$mytext <- renderText(input$text)
  
  output$mytable <- renderTable(Barley %>%
                                  filter(gen == input$gen) %>%
                                  summarise("Mean" = mean(yield), 
                                            "Median" = median(yield),
                                            "STDEV" = sd(yield), 
                                            "Min" = min(yield),
                                            "Max" = max(yield)))
}

#___________________________second app__________________________________________
server1 <- function(input, output) {
  output$distPlot <- renderPlot({
    x <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins+1)
    
    hist(x, breaks = bins, col = "skyblue", border = "orange", 
         xlab = "Waiting time to next eruprion (in mins)",
         main = "Histogram of waiting times")
  })
}


# Run the app ----
#first app
shinyApp(ui = ui, server = server)
#second app
# shinyApp(ui = ui1, server = server1)
