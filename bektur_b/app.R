# Packages ----
library(shiny)  # Required to run any Shiny app
library(ggplot2)  # For creating pretty plots
library(dplyr)  # For filtering and manipulating data
library(agridat)  # The package where the data comes from

# Loading data ----
Barley <- as.data.frame(beaven.barley)

# ui.R ----
ui <- fluidPage(
  titlePanel("Barley Yield"),  # указывает, что нам нужна отдельная панель в верхней части страницы, на которой мы можем разместить заголовок
  sidebarLayout(  # означает, что мы хотим, чтобы наше приложение Shiny имело макет боковой панели, один из множества макетов
    position = "right",
    sidebarPanel(h3("Inputs for histogram"),  # Внутри sidebarLayoutмы имеем:sidebarPanel()указывает на то, что мы хотим включить в наше приложение боковую панель. Боковые панели часто содержат виджеты ввода, такие как ползунки, поля ввода текста, переключатели и т. д.
    selectInput("gen", "1. Select genotype", choices = c("A" = "a","B" = "b","C" = "c","D" = "d","E" = "e","F" = "f","G" = "g","H" = "h"), selected = "a"),
    br(),
    selectInput("col", "2. Select histogram colour", choices = c("blue","green","red","purple","grey"), selected = "grey"),
    br(),
    sliderInput("bin", "3. Select number of histogram bins", min=1, max=25, value= c(10)),
    br(),
    textInput("text", "4. Enter some text to be displayed", "")),
    
    mainPanel( # Inside the sidebarLayout, add a mainPanel mainPanel(). Это означает, что нам нужна большая главная панель. Главные панели часто содержат выходные данные приложения, будь то таблица, карта, график или что-то ещё.
      plotOutput("myhist"),
      tableOutput("mytable"),
      textOutput("mytext")  
    )  
  )
)


# server.R ----
server <- function(input, output) {
  output$myhist <- renderPlot(ggplot(Barley, aes(x = yield)) + 
                                geom_histogram(bins = input$bin, fill = input$col, group=input$gen, 
                                               data=Barley[Barley$gen == input$gen,],
                                               colour = "black"))
  
  output$mytext <- renderText(input$text)
  
  output$mytable <- renderTable(Barley %>%
                                  filter(gen == input$gen) %>%
                                  summarise("Mean" = mean(yield), 
                                            "Median" = median(yield),
                                            "STDEV" = sd(yield), 
                                            "Min" = min(yield),
                                            "Max" = max(yield)))
}

# Run the app ----
shinyApp(ui = ui, server = server)





