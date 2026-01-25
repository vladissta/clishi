create_block2_tabs <- function() {
  
}

sidebar_block2_tests_inputs <- function() {
  tagList(
    h4("Выбор теста"),
    selectInput(
      "test_type", "Статистический тест",
      choices = c(
        "Одновыборочный t-тест" = "t_one_sample",
        "Двухвыборочный t-тест (Welch)" = "t_two_sample",
        "Тесты на таблицах сопряжённости" = "contingency_tables",
        "Mann-Whitney U тест" = "mann_whitney",
        "Brunner-Munzel тест" = "brunner_munzel"
      )
    ),
    
    conditionalPanel(
      "input.test_type == 't_one_sample'",
      selectInput(
        "dist_type", "Тип распределения",
        choices = c(
          "Нормальное N(μ, σ²)" = "norm",
          # "Равномерное U(a, b)" = "unif",
          "Экспоненциальное Exp(λ)" = "exp"
        )
      ),
      fluidRow(
        column(4, numericInput("mu_0", create_tooltip("μ_0", "Истинное среднее"), 0)),
        column(4, numericInput("mu", create_tooltip("μ", "Cреднее выборок"), 0)),
        column(4, numericInput("sigma", create_tooltip("σ", "Стандартное отклонение выборок"), 1, min = 0.0001))
      )
    ),
    conditionalPanel(
      "input.test_type == 't_two_sample'",
      selectInput(
        "dist_type", "Тип распределения",
        choices = c(
          "Нормальное N(μ, σ²)" = "norm",
          # "Равномерное U(a, b)" = "unif",
          "Экспоненциальное Exp(λ)" = "exp"
        )
      ),
      h5("Выборка 1", style = "margin-top: 5px; margin-bottom: 0px; margin-left: 15px;"),
      fluidRow(
        column(4, numericInput("mu1", create_tooltip("μ_1", "Cреднее выборки 1"), 0)),
        column(4, numericInput("sigma1", create_tooltip("σ_1", "Стандартное отклонение выборки 1"), 1, min = 0.0001))
      ),
      h5("Выборка 2", style = "margin-top: 5px; margin-bottom: 0px; margin-left: 15px;"),
      fluidRow(
        column(4, numericInput("mu2", create_tooltip("μ_2", "Cреднее выборки 2"), 0)),
        column(4, numericInput("sigma2", create_tooltip("σ_2", "Стандартное отклонение выборки 2"), 1, min = 0.0001))
      )
    ),
    
    # conditionalPanel(
    #   "input.test_type == 'contingency_tables'",
    #   fluidRow(
    #     column(4, numericInput("mu_1", create_tooltip("μ_1", "Cреднее выборки 1"), 0)),
    #     column(4, numericInput("sigma", create_tooltip("σ_1", "Стандартное отклонение выборки 1"), 1, min = 0.0001))
    #   ),
    #   fluidRow(
    #     column(4, numericInput("mu_1", create_tooltip("μ_2", "Cреднее выборки 2"), 0)),
    #     column(4, numericInput("sigma", create_tooltip("σ_2", "Стандартное отклонение выборки 2"), 1, min = 0.0001))
    #   )
    # ),
    
    conditionalPanel(
      "input.test_type == 'mann_whitney'",
      selectInput(
        "dist_type", "Тип распределения",
        choices = c(
          "Нормальное N(μ, σ²)" = "normal",
          # "Равномерное U(a, b)" = "unif",
          "Экспоненциальное Exp(λ)" = "exponential"
        )
      ),
      fluidRow(
        column(4, numericInput("mu1", create_tooltip("μ_1", "Cреднее выборки 1"), 0)),
        column(4, numericInput("sigma1", create_tooltip("σ_1", "Стандартное отклонение выборки 1"), 1, min = 0.0001))
      ),
      fluidRow(
        column(4, numericInput("mu2", create_tooltip("μ_2", "Cреднее выборки 2"), 0)),
        column(4, numericInput("sigma2", create_tooltip("σ_2", "Стандартное отклонение выборки 2"), 1, min = 0.0001))
      )
    ),
    conditionalPanel(
      "input.test_type == 'brunner_munzel'",
      selectInput(
        "dist_type", "Тип распределения",
        choices = c(
          "Нормальное N(μ, σ²)" = "normal",
          # "Равномерное U(a, b)" = "unif",
          "Экспоненциальное Exp(λ)" = "exponential"
        )
      ),
      fluidRow(
        column(4, numericInput("mu1", create_tooltip("μ_1", "Cреднее выборки 1"), 0)),
        column(4, numericInput("sigma1", create_tooltip("σ_1", "Стандартное отклонение выборки 1"), 1, min = 0.0001))
      ),
      fluidRow(
        column(4, numericInput("mu2", create_tooltip("μ_2", "Cреднее выборки 2"), 0)),
        column(4, numericInput("sigma2", create_tooltip("σ_2", "Стандартное отклонение выборки 2"), 1, min = 0.0001))
      )
    ),
    
    fluidRow(
      column(4, numericInput("sample_size", create_tooltip("N", "Размер выборки"), 0)),
      column(4, numericInput("n_sim", create_tooltip("#", "Количество выборок"), 0)),
      column(4, numericInput("alpha", create_tooltip("α", "Уровень значимости (фикс. Ошибка I рода)"), 1, min = 0.0001))
    )
    
  )
}