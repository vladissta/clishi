source('scripts/ui_components.R')

create_block3_tabs <- function() {
  
}

sidebar_block3_sample_size_calc_input <- function() {
  tagList(
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
    
  )
}

sidebar_block3_dynamic_inputs <- function(data_type, hypothesis) {
  tagList(
    if (data_type == "proportion") {
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
          value = ifelse(hypothesis == "superiority", 0.1, -0.1),
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
  )
}

create_block3_content <- function() {
  tagList(
  h3("Результаты расчета выборки"),
  br(),
  verbatimTextOutput("result_sample_size_calc"),
  br(),
  h4("Инструкция по использованию:"),
  tags$ol(
    tags$li("Выберите тип данных (количественные или качественные)"),
    tags$li("Выберите тип исследования (превосходство или не меньшая эффективность)"),
    tags$li("Задайте параметры alpha и beta (обычно 0.05 и 0.2)"),
    tags$li("Задайте параметр k - соотношение между группой терапии и контроля"),
    tags$li("Заполните специфичные параметры для выбранного типа данных"),
    tags$li("Нажмите кнопку 'Рассчитать объем выборки'")
  ))
}