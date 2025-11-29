library(shiny)
library(bslib)
library(ggplot2)
library(tidyverse)
library(shinythemes)
library(shinyBS)

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
               "Выберете тип распределения признака",
               choiceNames = list(
                 tags$span(id = "normal_button", "нормальное"),
                 tags$span(id = "exponential_button",  "экспоненциальное"),
                 tags$span(id = "uniform_button", "равномерное")
               ),
               choiceValues = c("нормальное", "экспоненциальное", "равномерное"),
               inline = TRUE
             ),
             bsPopover(
               id = "normal_button", 
               title = "нормальное распределение признака",
               content = '<img src="normal.png" width="250">', 
               placement = "right",
               trigger = "hover"
             ),
             bsPopover(
               id = "exponential_button",  
               title = "экспоненциальное распределение признака", 
               content = '<img src="exponential.png" width="250">',
               placement = "right", 
               trigger = "hover"
             ),
             bsPopover(
               id = "uniform_button",
               title = "равномерное распределение признака",
               content = '<img src="uniform.png" width="250">', 
               placement = "right", 
               trigger = "hover"
            ) 
      )
    ),
    fluidRow(
      column(4,
             uiOutput("dist_settings"),
             actionButton(
               "generate_graph",
               HTML("Сгенерировать распределение <br>средних в каждой выборке")
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
      samples_df <- generate_samples(input$distribution, params)
      samples_mean_df <- calculate_samples_mean_df(samples_df, params)
      output$distribution_plot <- renderPlot(plot_distribution(samples_mean_df))
    }
  )
  
 
  output$dist_settings <- render_distribution_panel("нормальное")
}


####### START APP #######
shinyApp(ui=ui, server=server)
