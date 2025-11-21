library(shiny)
library(bslib)
library(ggplot2)
library(tidyverse)
library(shinythemes)

SAVE_FILE_NAME <- "Internet_survey.csv"

ui <- navbarPage(
  title = "Internet Survey",
  theme = shinytheme("flatly"),
  tabPanel("Survey Form",
           fluidRow(
             column(6, 
                    numericInput( 
                      "age", 
                      "Your age", 
                      min = 1, 
                      max = 100,
                      value = 25,
                      step = 1
                    )
             ),
             column(6, 
                    numericInput( 
                      "app_score", 
                      "Rate the app (1-10)", 
                      min = 1, 
                      max = 10,
                      value = 5,
                      step = 1
                    )
             )
           ),
           fluidRow(
             column(6, 
                    radioButtons(
                      inputId = "sex",
                      label = "Choose your sex",
                      choices = list("Female" = 0, "Male" = 1)
                    )
             ),
             column(6,
                    textAreaInput( 
                      "opinon", 
                      "Your opinion about the app", 
                      value = "", 
                      rows = 5
                    )
             )
           ),
           fluidRow(
             column(6, 
                    selectInput(
                      "selection_education",
                      "Your education",
                      list(
                        "General education (school)" = 0, 
                        "Secondary vocational education (College)" = 1, 
                        "Bachelor's degree" = 2, 
                        "Master's degree" = 3, 
                        "Post-master’s degree" = 4
                      ),
                      multiple = FALSE
                    )
             )
           ),
           
           fluidRow(
             column(12,
                    br(),
                    actionButton(
                      "add_btn",
                      "Add information",
                      class = "btn-primary btn-lg"
                    )
             )
           )
  ),
  tabPanel("Graphs",
           h3("Survey results"),
           plotOutput("result_plot"))
)

server <- function(input, output, session) {
  if (file.exists(SAVE_FILE_NAME)) {
    data <- read.csv(SAVE_FILE_NAME)
  } else {
    data <- tibble(
      age = numeric(0), 
      educ = numeric(0)
    )
  }
  
  survey_table <- reactiveVal(data)
  
  observeEvent(
    input$add_btn, 
    {
      age_num <- as.numeric(input$age)
      score_num <- as.numeric(input$app_score)
      
      if (age_num <= 0 | age_num > 100 | is.na(age_num)) {
        showModal(
          modalDialog(
            title = "Error",
            "You put incorrect age",
            easyClose = TRUE,
            footer = modalButton("Close")
          )
        )
        return()
      }
      
      if (score_num  < 0 | score_num  >= 10 | is.na(score_num)) {
        showModal(
          modalDialog(
            title = "Error",
            "You put incorrect score",
            easyClose = TRUE,
            footer = modalButton("Close")
          )
        )
        return()
      }

      new_row <- tibble(age = input$age, educ = input$selection_education, score = input$app_score, sex = input$sex, opinion = input$opinon) 
      survey_table(rbind(survey_table(), new_row))
        
      write.csv(survey_table(), file = SAVE_FILE_NAME, row.names = FALSE)
      showModal(
        modalDialog(
          title = "Succes!",
          "Your data has been saved",
          easyClose = TRUE,
          footer = modalButton("Close")
        )
      )
    }
  )
  output$result_plot <- renderPlot(
    data <- survey_table(),
    
    if(nrow(data) == 0){
      return()
    }
    
    ggplot(data, aes())
  )
  
  
}

shinyApp(ui = ui, server = server)