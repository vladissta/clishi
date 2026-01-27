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
          uiOutput("type1_slider_ui")  # <--  слайдер ошибок I рода
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