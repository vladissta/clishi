library(shiny)
library(TrialSize)

ui <- fluidPage(
  titlePanel("Расчет объема выборки"),
  
  sidebarLayout(
    sidebarPanel(
      # выбор типа данных
      selectInput(
        "data_type",
        "Тип данных с которыми работаем:",
        choices = c(
          "Количественные (среднее)" = "mean",
          "Качественные (пропорция, доля)" = "proportion"
        ),
        selected = "mean"
      ),
      # выбор гипотезы
      selectInput(
        "hypothesis",
        "Гипотеза:",
        choices = c("Превосходство" = "superiority", 
                    "Не меньшая эффективность" = "noninferiority"),
        selected = "noninferiority"
      ),
      # общие параметры для обоих типов данных
      numericInput("alpha", "Alpha:", value = 0.05, min = 0, max = 1, step = 0.01),
      numericInput("beta", "Beta:", value = 0.2, min = 0, max = 1, step = 0.01),
      sliderInput("k", "k (соотношение терапия:контроль):", value = 1, min = 1, max = 10, step = 1),
      
      # Динамические элементы в зависимости от типа данных
      uiOutput("dynamic_inputs"),
      
      actionButton("calculate", "Рассчитать")
    ),
    
    mainPanel(
      h3("Результаты расчета"),
      br(),
      verbatimTextOutput("result"),
      br(),
      h4("Инструкция по использованию:"),
      tags$ol(
        tags$li("Выберите тип данных (количественные или качественные)"),
        tags$li("Выберите тип исследования (превосходство или не меньшая эффективность)"),
        tags$li("Задайте параметры alpha и beta (обычно 0.05 и 0.2)"),
        tags$li("Задайте параметр k - соотношение между группой терапии и контроля"),
        tags$li("Заполните специфичные параметры для выбранного типа данных"),
        tags$li("Нажмите кнопку 'Рассчитать объем выборки'")
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Динамические элементы ввода
  output$dynamic_inputs <- renderUI({
    if (input$data_type == "proportion") {
      # Параметры для пропорций
      tagList(
        numericInput(
          "p1",
          "Пропорция в группе терапии p1:",
          value = 0.2,
          min = 0.001,
          max = 0.999,
          step = 0.01
        ),
        
        numericInput(
          "p2",
          "Пропорция в группе контроля p2:",
          value = 0.2,
          min = 0.001,
          max = 0.999,
          step = 0.01
        ),
        
        numericInput(
          "margin",
          HTML("Граница превосходства/не меньшей эффективности (margin):"),
          value = ifelse(input$hypothesis == "superiority", 0.1, -0.1),
          step = 0.01
        ),
        
        helpText(HTML("Ho: p1−p2≤margin Ha: p1-p2 > margin"))
      )
    } else {
      # Параметры для количественных
      tagList(
        numericInput(
          "mu1",
          "Среднее в группе терапии (μ1):",
          value = 10.0,
          step = 0.1
        ),
        
        numericInput(
          "mu2",
          "Среднее в группе контроля (μ2):",
          value = 10.0,
          step = 0.1
        ),
        
        numericInput(
          "sigma",
          "Объединенное стандартное отклонение (σ):",
          value = 2.0,
          min = 0.01,
          step = 0.1
        ),
        
        numericInput(
          "delta1",
          HTML("Граница превосходства/не меньшей эффективности (delta):"),
          value = 2.0,
          step = 0.1
        ),
        
        helpText(HTML("Ho: margin≤delta Ha: margin > delta"))
      )
    }
  })
  
  observeEvent(input$calculate, {
    
    result <- tryCatch({
      if (input$data_type == "proportion") {
        delta <- input$p1 - input$p2
        res <- TwoSampleProportion.NIS(
          alpha = input$alpha,
          beta = input$beta,
          p1 = input$p1,
          p2 = input$p2,
          k = input$k,
          delta = delta,
          margin = input$margin
        )
        
        n1 <- ceiling(res[1])
        n2 <- ceiling(res[1]/input$k)
        total_end <- ceiling(n1 + n2) 
        total_study <- ceiling(ceiling(n1 / 0.8) / 0.9) + ceiling(ceiling(n2 / 0.8) / 0.9)
        
        list(
          n1 = n1, 
          n2 = n2, 
          difference = delta, 
          diff_name = "delta = p1 - p2",
          total_end = total_end, 
          total_study = total_study
        )
        
      } else if (input$data_type == "mean") {
        margin1 <- input$mu1 - input$mu2
        res <- TwoSampleMean.NIS(
          alpha = input$alpha,
          beta = input$beta,
          sigma = input$sigma,
          k = input$k,
          delta = input$delta1,
          margin = margin1
        )
        
        n1 <- ceiling(res[1])
        n2 <- ceiling(res[1]/input$k)
        total_end <- ceiling(n1 + n2) 
        total_study <- ceiling(ceiling(n1 / 0.8) / 0.9) + ceiling(ceiling(n2 / 0.8) / 0.9)
        
        list(
          n1 = n1, 
          n2 = n2, 
          difference = margin1, 
          diff_name = "margin = μ1 - μ2",
          total_end = total_end, 
          total_study = total_study
        )
      }
    }, error = function(e) {
      return(list(error = e$message))
    })
    
    if (!is.null(result$error)) {
      output$result <- renderText({
        paste("Ошибка:", result$error)
      })
      return()
    }
    
    output$result <- renderText({
      paste(
        result$diff_name, " = ", round(result$difference, 3), "\n",
        "Группа терапии (n1): ", result$n1, "\n",
        "Группа контроля (n2): ", result$n2, "\n",
        "Общий объем выборки: ", result$total_end, "\n",
        "Объем выборки с учетом выбывших во время исследования (20%) и с учетом выбывших во время скрининга (10%): ", result$total_study
      )
    })
  })
}

shinyApp(ui = ui, server = server)