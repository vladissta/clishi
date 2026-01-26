library(future)

create_block2_tabs <- function() {
  
}

sidebar_block2_tests_inputs <- function() {
  
  tagList(
    numericInput("cores", "Число ядер для симуляции", 
                 value = 4, 
                 min = 1, 
                 max = availableCores(), 
                 step = 1),
    
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
    
    # ============================================
    # T-ONE-SAMPLE
    # ============================================
    conditionalPanel(
      condition = "input.test_type == 't_one_sample'",
      selectInput(
        "dist_type_sim", "Тип распределения",
        choices = c(
          "Нормальное N(μ, σ²)" = "normal",
          "Экспоненциальное Exp(λ)" = "exponential"
        )
      ),
      fluidRow(
        column(4, numericInput("mu_0", "μ₀", value = 0)),
        column(4, numericInput("mu", "μ", value = 0)),
        column(4, numericInput("sigma", "σ", value = 1, min = 0.0001))
      )
    ),
    
    # ============================================
    # T-TWO-SAMPLE
    # ============================================
    conditionalPanel(
      condition = "input.test_type == 't_two_sample'",
      selectInput(
        "dist_type_sim", "Тип распределения",
        choices = c(
          "Нормальное N(μ, σ²)" = "normal",
          "Экспоненциальное Exp(λ)" = "exponential"
        )
      ),
      h5("Выборка 1", style = "margin-top: 5px; margin-bottom: 0px; margin-left: 15px;"),
      fluidRow(
        column(6, numericInput("mu1", "μ₁", value = 0)),
        column(6, numericInput("sigma1", "σ₁", value = 1, min = 0.0001))
      ),
      h5("Выборка 2", style = "margin-top: 5px; margin-bottom: 0px; margin-left: 15px;"),
      fluidRow(
        column(6, numericInput("mu2", "μ₂", value = 0)),
        column(6, numericInput("sigma2", "σ₂", value = 1, min = 0.0001))
      )
    ),
    
    # ============================================
    # CONTINGENCY TABLES
    # ============================================
    conditionalPanel(
      condition = "input.test_type == 'contingency_tables'",
      selectInput('test_method', 'Метод теста',
                  choices = c('Хи-квадрат Пирсона' = 'chi',
                              'Точный тест Фишера' = 'fisher')
      ),
      
      conditionalPanel(
        condition = "input.test_method == 'chi'",
        checkboxInput('chi_correction', 
                      "Поправка Йейтса на непрерывность",
                      value = FALSE)
      ),
      
      selectInput('trial_type', 'Тип исследования',
                  choices = c('Когортное исследование' = 'cohort',
                              'Исследование случаев и контролей' = 'case_control',
                              'Перекрёстное исследование' = 'cross_sectional',
                              'Фишер' = 'fisher')
      ),
      
      # Cohort study parameters
      conditionalPanel(
        condition = "input.trial_type == 'cohort'",
        numericInput("event_probability", "Вероятность события (p_event)", 
                     value = 0.5, min = 0, max = 1, step = 0.01),
        numericInput("exposure_proportion", "Доля экспонированных (prop_exposure)", 
                     value = 0.5, min = 0, max = 1, step = 0.01)
      ),
      
      # Case-control study parameters
      conditionalPanel(
        condition = "input.trial_type == 'case_control'",
        numericInput("exposure_probability", "Вероятность экспозиции (p_exposure)", 
                     value = 0.5, min = 0, max = 1, step = 0.01),
        numericInput("event_proportion", "Доля случаев (prop_event)", 
                     value = 0.5, min = 0, max = 1, step = 0.01)
      ),
      
      # Cross-sectional study parameters
      conditionalPanel(
        condition = "input.trial_type == 'cross_sectional'",
        numericInput("event_probability", "Вероятность события (p_event)", 
                     value = 0.5, min = 0, max = 1, step = 0.01),
        numericInput("exposure_probability", "Вероятность экспозиции (p_exposure)", 
                     value = 0.5, min = 0, max = 1, step = 0.01)
      ),
      
      # Fisher exact test parameters
      conditionalPanel(
        condition = "input.trial_type == 'fisher'",
        numericInput("event_proportion", "Доля случаев (prop_event)", 
                     value = 0.5, min = 0, max = 1, step = 0.01),
        numericInput("exposure_proportion", "Доля экспонированных (prop_exposure)",
                     value = 0.5, min = 0, max = 1, step = 0.01)
      )
    ),
    
    # ============================================
    # MANN-WHITNEY
    # ============================================
    conditionalPanel(
      condition = "input.test_type == 'mann_whitney'",
      selectInput(
        "dist_type_sim", "Тип распределения",
        choices = c(
          "Нормальное N(μ, σ²)" = "normal",
          "Экспоненциальное Exp(λ)" = "exponential"
        )
      ),
      h5("Выборка 1", style = "margin-top: 5px; margin-bottom: 0px; margin-left: 15px;"),
      fluidRow(
        column(6, numericInput("mu1", "μ₁", value = 20)),
        column(6, numericInput("sigma1", "σ₁", value = 2, min = 0.0001))
      ),
      h5("Выборка 2", style = "margin-top: 5px; margin-bottom: 0px; margin-left: 15px;"),
      fluidRow(
        column(6, numericInput("mu2", "μ₂", value = 20)),
        column(6, numericInput("sigma2", "σ₂", value = 2, min = 0.0001))
      )
    ),
    
    # ============================================
    # BRUNNER-MUNZEL
    # ============================================
    conditionalPanel(
      condition = "input.test_type == 'brunner_munzel'",
      selectInput(
        "dist_type_sim", "Тип распределения",
        choices = c(
          "Нормальное N(μ, σ²)" = "normal",
          "Экспоненциальное Exp(λ)" = "exponential"
        )
      ),
      h5("Выборка 1", style = "margin-top: 5px; margin-bottom: 0px; margin-left: 15px;"),
      fluidRow(
        column(6, numericInput("mu1", "μ₁", value = 0)),
        column(6, numericInput("sigma1", "σ₁", value = 1, min = 0.0001))
      ),
      h5("Выборка 2", style = "margin-top: 5px; margin-bottom: 0px; margin-left: 15px;"),
      fluidRow(
        column(6, numericInput("mu2", "μ₂", value = 0)),
        column(6, numericInput("sigma2", "σ₂", value = 1, min = 0.0001))
      )
    ),
    
    # ============================================
    # PARAMETER SELECTION (common for all tests)
    # ============================================
    h4("Параметры симуляции", style = "margin-top: 15px;"),
    
    selectInput(
      "parameter_name", "Изменяемый параметр",
      choices = c("Размер выборки N" = "sample_size")
    ),
    
    fluidRow(
      column(4, numericInput("parameter_from", "От", value = 10, min = 2)),
      column(4, numericInput("parameter_to", "До", value = 100, min = 2)),
      column(4, numericInput("parameter_by", "Шаг", value = 10, min = 1))
    ),
    
    fluidRow(
      column(6, numericInput("n_sim", "Число симуляций", value = 1000, min = 10)),
      column(6, numericInput("alpha", "α (уровень значимости)", value = 0.05, min = 0.001, max = 0.999))
    )
  )
}