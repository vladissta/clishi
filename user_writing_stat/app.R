library(shiny)
library(bslib)
library(ggplot2)
library(tidyverse)
library(shinythemes)



####### CONFIG VARIABLES #######
SURVEY_CSV <- "Internet_survey.csv"


####### INIT DATA #######
if (file.exists(SURVEY_CSV)) {
  data <- read.csv(SURVEY_CSV)
} else {
  data <- tibble(
    age = numeric(0), 
    educ = numeric(0)
  )
}
survey_table <- reactiveVal(data)





get_survey_graph <- function(data) {
  if(nrow(data) == 0){
    return()
  }
  graph <- ggplot(data, aes(x = factor(educ), y = score, fill = factor(sex))) +
    geom_boxplot(position = position_dodge(width = 0.8)) + 
    geom_jitter(aes(colour = factor(sex)), position = position_jitterdodge(dodge.width = 0.8, jitter.width = 0.5), alpha = 0.7, size = 3,  show.legend = FALSE) +
    scale_fill_manual(
      name = "Gender",
      values = c("0" = "firebrick", "1" = "darkblue"),
      labels = c("0" = "Female", "1" = "Male")
    ) +
    scale_color_manual(
      name = "Gender",
      values = c("0" = "lightcoral", "1" = "steelblue"),
      labels = c("0" = "Female", "1" = "Male")
    ) +
    scale_x_discrete(
      labels = c(
        "0" = "General education (school)", 
        "1" = "Secondary vocational education (College)",
        "2" = "Bachelor's degree",
        "3" = "Master's degree", 
        "4" ="Post-master’s degree" 
      )
    ) +
    labs(
      title = "User satisfaction across education levels and gender",
      x = "Education",
      y = "App rating",
      caption = "The boxplots display the median, IQR, and whisker range"
    ) +
    scale_y_continuous(
      breaks = seq(0, 10, 2),
      limits = c(0, 10)
    ) +
    theme(
      plot.title = element_text(size = 20, margin = margin(b = 5, unit = "pt"), hjust = 0.5),
      plot.caption = element_text(size = 12),
      axis.title.x = element_text(size = 20, margin = margin(t = 5, unit = "pt")),
      axis.text.x = element_text(size = 15, angle = 15, margin = margin(t = 25, unit = "pt")),
      axis.title.y = element_text(size = 20, angle = 90, margin = margin(r = 15, unit = "pt")),
      axis.text.y = element_text(size = 15),
      panel.background = element_rect(fill = "white"),
      panel.grid = element_line(linewidth = 0.3, color = "grey"),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_text(size = 16),   
      legend.text  = element_text(size = 14)
      
    )
    
  return(graph)
}

  
####### UI #######
ui <- navbarPage(
  title = "User satisfaction survey for the App",
  theme = shinytheme("flatly"),
  
####### FORM TAB PANEL #######
  tabPanel(
    "Survey form",
    fluidRow(
      column(6,          # Column Size
        numericInput(
          "age", 
          "Your age", 
          min = 1, 
          max = 100,
          value = 25,    # Default value
          step = 1
        )
      ),
      column(6, 
        numericInput( 
          "app_score", 
          "Rate the app (1-10)", 
          min = 1, 
          max = 10,
          value = 10,
          step = 1
        )
      )
    ),
    fluidRow(
      column(6, 
        radioButtons(
          inputId = "sex",
          label = "Choose your gender",
          choices = list("Female" = 0, "Male" = 1)
        )
      ),
      column(6,
        textAreaInput(
          "opinon", 
           "Please, write your opinion about the app",
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
             "Secondary education (College)" = 1,
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
           "add_survey_data",
           "Add information",
           class = "btn-primary btn-lg"
         )
      )
    )
  ),


####### OUTPUT TAB PANEL #######
  tabPanel(
    "Graph",
    h3("Survey results"),
    plotOutput("result_plot", width = "800px", height = "800px")
  )
)


###### SERVER FUNCTION #######
server <- function(input, output, session) {
  
    observeEvent(
    input$add_survey_data, 
    {
      age_num <- as.numeric(input$age)
      score_num <- as.numeric(input$app_score)
      
      if (is.na(age_num) | age_num <= 0 | age_num > 100) {
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
      
      if (score_num  < 0 | score_num  > 10 | is.na(score_num)) {
        showModal(
          modalDialog(
            title = "Error",
            "You have put incorrect score",
            easyClose = TRUE,
            footer = modalButton("Close")
          )
        )
        return()
      }

      new_row <- tibble(age = input$age, educ = input$selection_education, score = input$app_score, sex = input$sex, opinion = input$opinon) 
      survey_table(rbind(survey_table(), new_row))
        
      write.csv(survey_table(), file = SURVEY_CSV, row.names = FALSE)
      showModal(
        modalDialog(
          title = "Success!",
          "Your data has been saved",
          easyClose = TRUE,
          footer = modalButton("Close")
        )
      )
      output$result_plot <- renderPlot(
        get_survey_graph(survey_table())
      )
    }
  )
  
  # Render box plot
  output$result_plot <- renderPlot(
    get_survey_graph(survey_table())
  )
}


####### START APP #######
shinyApp(ui = ui, server = server)