library(shiny)
library(bslib)
library(ggplot2)
library(tidyverse)
library(shinythemes)

source("distribution_tab/ui_render.R")
source("distribution_tab/utils.R")

####### UI #######
ui <- navbarPage(
  title = "Статистика наглядно",
  theme = shinytheme("flatly"),
  
  
  ####### FORM TAB PANEL #######
  tabPanel(
    "Настройки графика",
    fluidRow(
      column(10,
             radioButtons(
               "distribution",
               "Выберете тип распределения",
               choices = list("нормальное", "экспоненциальное", "равномерное"),
               inline = TRUE
             )
      )
    ),
    fluidRow(
      column(4,
             uiOutput("dist_settings"),
             actionButton(
               "generate_graph",
               "Сгенерировать распределение"
             )
      ),
      column(8,
             plotOutput("distribution_plot")
      )
    )
  )
)


server <- function(input, output){
  observeEvent(
    input$distribution,
    output$dist_settings <- render_distribution_panel(input$distribution)
  )
  
  observeEvent(
    input$generate_graph,
    {
      params <- get_distribution_params(input$distribution, input)
      sample <- generate_samples(input$distribution, params)
      output$distribution_plot <- renderPlot(plot_distribution(sample, params))
    }
  )
  
 
  output$dist_settings <- render_distribution_panel("нормальное")
}


####### START APP #######
shinyApp(ui=ui, server=server)
