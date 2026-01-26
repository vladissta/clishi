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