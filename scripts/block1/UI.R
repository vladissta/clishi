source('scripts/ui_components.R')

sidebar_block1_dist_inputs <- function() {
  tagList(
    h4("Распределение в генеральной совокупности"),
    selectInput(
      "dist_type", "Тип распределения",
      choices = c(
        "Нормальное N(μ, σ²)" = "norm",
        "Равномерное U(a, b)" = "unif",
        "Экспоненциальное Exp(λ)" = "exp"
      )
    ),
    conditionalPanel(
      "input.dist_type == 'norm'",
      fluidRow(
        column(6, numericInput("norm_mean", create_tooltip("μ", "Истинное среднее"), 0)),
        column(6, numericInput("norm_sd", create_tooltip("σ", "Стандартное отклонение"), 1, min = 0.0001))
      )
    ),
    conditionalPanel(
      "input.dist_type == 'unif'",
      numericInput("unif_min", create_tooltip("a", "Нижняя граница"), 0),
      numericInput("unif_max", create_tooltip("b", "Верхняя граница"), 1)
    ),
    conditionalPanel(
      "input.dist_type == 'exp'",
      numericInput("exp_rate", create_tooltip("λ", "Параметр интенсивности"), 1, min = 0.0001)
    )
  )
}

sidebar_block1_sample_inputs <- function() {
  tagList(
    h4("Выборка и симуляции"),
    numericInput("n", 
                 create_tooltip("n", "Размер выборки"), 
                 value=30, min = 2, step = 1),
    numericInput("n_sim", 
                 create_tooltip("n<sub>sim</sub>", "Число повторений эксперимента"), 
                 value = 1000, min = 10, step = 10)
  )
}

sidebar_block1_hypothesis_inputs <- function() {
  tagList(
    tags$hr(),
    h4("Гипотеза о математическом ожидании"),
    fluidRow(
      column(6, checkboxInput("use_true_mu", create_tooltip("μ₀ = μ", "Если включено, устанавливаем μ₀ равным истинному μ (и фиксируем μ₀)"), TRUE)),
      column(6, numericInput("mu0", 
                             create_tooltip("μ₀", "Гипотетическое значение математического ожидания (H₀: μ = μ₀)"), 0))
    ),
    selectInput(
      "alt_type", create_tooltip("H₁", "Тип альтернативной гипотезы H₁"),
      choices = c(
        "двусторонняя (μ ≠ μ₀)" = "two.sided",
        "правосторонняя (μ > μ₀)" = "greater",
        "левосторонняя (μ < μ₀)" = "less"
      )
    )
  )
}

sidebar_block1_test_inputs <- function() {
  tagList(
    h4("Доверительный интервал и уровень значимости α"),
    sliderInput("conf_level", create_tooltip("CI", "Доверительный интервал"), 
                0.80, 0.99, 0.95, 0.01),
    numericInput("alpha", create_tooltip("α", "Уровень значимости"), 
                 0.05, 0.001, 0.2, 0.01)
  )
}

create_block1_tabs <- function() {
  tags$div(
    class = "clishi-top-tabs",
    tabsetPanel(
      id = "block1_subtab",
      type = "tabs",
      selected = "one_exp",
      tabPanel("1. Одна выборка", value = "one_exp"),
      tabPanel("2. Выборочные средние", value = "means"),
      tabPanel("3. Доверительные интервалы", value = "ci"),
      tabPanel("4. p-value", value = "pval"),
      tabPanel("5. Критические области t-распределения", value = "crit"),
      tabPanel("Help", value = "help")
    )
  )
}

create_block1_content <- function(){
  tagList(
    # 1) One experiment
    conditionalPanel(
      condition = "input.block1_subtab == 'one_exp'",
      fluidRow(
        box(
          width = 5,
          sliderInput(
            "exp_id",
            "Номер эксперимента (выборки) для детального просмотра",
            min = 1, max = 100, value = 1, step = 1
          )
        ),
        box(
          width = 7,
          uiOutput("smart_slider_ui")  # <-- один умный слайдер (I или II)
        )
      ),
      fluidRow(
        box(
          width = 12,
          br(),
          plotOutput("one_exp_plot", height = "300px"),
          br(),
          verbatimTextOutput("one_exp_text")
        )
      )
    ),
    
    # 2) Means
    conditionalPanel(
      condition = "input.block1_subtab == 'means'",
      uiOutput("hypothesis_text"),
      br(),
      plotOutput("means_hist", height = "350px"),
      br(),
      verbatimTextOutput("se_summary")
    ),
    
    # 3) CI
    conditionalPanel(
      condition = "input.block1_subtab == 'ci'",
      h4("Отображение доверительных интервалов"),
      sliderInput(
        "ci_range",
        "Диапазон экспериментов для графика ДИ:",
        min = 1, max = 100,
        value = c(1, 100), step = 1
      ),
      checkboxInput(
        "show_only_miss",
        "Показывать только ДИ, не содержащие истинное мат. ожидание",
        FALSE
      ),
      tags$hr(),
      plotOutput("ci_plot", height = "400px"),
      br(),
      DTOutput("ci_table")
    ),
    
    # 4) p-values
    conditionalPanel(
      condition = "input.block1_subtab == 'pval'",
      plotOutput("p_hist", height = "350px"),
      br(),
      verbatimTextOutput("p_summary")
    ),
    
    # 5) Critical regions
    conditionalPanel(
      condition = "input.block1_subtab == 'crit'",
      plotOutput("crit_plot", height = "350px"),
      br(),
      verbatimTextOutput("crit_text")
    ),
    
    # Help
    conditionalPanel(
      condition = "input.block1_subtab == 'help'",
      br(),
      tabsetPanel(
        tabPanel(
          "Summary",
          h3("Распределение случайной величины X в генеральной совокупности"),
          p(
            "В левой панели выбирается распределение случайной величины X в генеральной совокупности. ",
            "Ниже приведены типичные графики данных распределений. ",
            "Выбранное распределение выделено более толстой линией."
          ),
          fluidRow(
            column(4, h5("Нормальное распределение"), plotOutput("help_norm", height = "180px")),
            column(4, h5("Равномерное распределение"), plotOutput("help_unif", height = "180px")),
            column(4, h5("Экспоненциальное распределение"), plotOutput("help_exp", height = "180px"))
          ),
          tags$hr(),
          h4("Структура симуляции"),
          tags$ul(
            tags$li("задается распределение случайной величины X в генеральной совокупности с истинным математическим ожиданием и дисперсией;"),
            tags$li("многократно (n_sim раз) генерируются независимые выборки объема n из данного распределения;"),
            tags$li("в каждой выборке вычисляется выборочное среднее — точечная оценка истинного математического ожидания;"),
            tags$li("формируется эмпирическое распределение выборочных средних;"),
            tags$li("строятся доверительные интервалы для параметра μ и выполняется проверка гипотезы H₀: μ = μ₀ с использованием t-статистики.")
          )
        ),
        tabPanel("Термины", h3("Основные термины в симуляции"))
      )
    ))}