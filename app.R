# Packages ----
library(shiny)  # Required to run any Shiny app
library(ggplot2)  # For creating pretty plots
library(dplyr)  # For filtering and manipulating data
library(agridat)  # The package where the data comes from
library(DT)  # подключает пакет DT для интерактивных таблиц

source('scripts/calculations.R')
source('scripts/plots.R')
source('scripts/texts.R')
source('scripts/help_output.R')

ui <- fluidPage(
  withMathJax(), # подключает поддержку LaTeX-формул через MathJax
  
  tags$hr(),
  
  sidebarLayout(
    sidebarPanel(  
      width = 3, # боковая панель занимает 3/12, соответсвенно остальная часть панель мейн
      h4("Распределение в генеральной совокупности"),
      
      # 1. Выбор типа распределения
      selectInput(                            # виджет «выпадающий список»
        "dist_type", "Тип распределения",      # имя переменной в Shiny
        choices = c("Нормальное N(μ, σ²)"      = "norm",  #μ — математическое ожидание, σ² — дисперсия
                    "Равномерное U(a, b)"      = "unif",
                    "Экспоненциальное Exp(λ)"  = "exp")
      ),
      
      # Параметры в генеральной совокупности
      conditionalPanel(               # Панель отображается только когда мы выбираем "norm"
        "input.dist_type == 'norm'",
        numericInput("norm_mean", "Истинное среднее (мат. ожидание) μ", 0),   # По умолчанию μ = 0
        numericInput("norm_sd", "Cтандартное отклонение σ", 1, min = 0.0001)  # По умолчанию σ = 1
      ),
      conditionalPanel(               # Панель отображается только когда выбираем "unif"
        "input.dist_type == 'unif'",
        numericInput("unif_min", "Минимум a в генеральной совокупности", 0),
        numericInput("unif_max", "Максимум b в генеральной совокупности", 1)
      ),
      conditionalPanel(
        "input.dist_type == 'exp'", # Панель отображается только когда выбираем "exp"
        numericInput("exp_rate", "Параметр λ экспоненциального распределения", 1, min = 0.0001)
      ),
      
      tags$hr(),
      h4("Выборка и симуляции"),
      numericInput("n", "Объём одной выборки n", value = 30, min = 2, step = 1),
      numericInput("n_sim", "Число повторений эксперимента (число выборок)", value = 1000, min = 10, step = 10),
      numericInput("seed", "Фиксация состояния генератора случайных чисел (seed)", value = 1),
      
      tags$hr(),
      h4("Гипотеза о математическом ожидании"),
      
      checkboxInput("use_true_mu", "Установить μ₀ = μ (использовать истинное значение параметра)", TRUE),
      
      numericInput("mu0", "Гипотетическое значение математического ожидания μ₀ (H₀: μ = μ₀)", value = 0),
      
      selectInput("alt_type", "Тип альтернативной гипотезы H₁",         # Пока добавил просто, чтоб потом можно было разыне тестирования 
                  choices = c("двусторонняя (μ ≠ μ₀)" = "two.sided",
                              "правосторонняя (μ > μ₀)" = "greater",
                              "левосторонняя (μ < μ₀)" = "less")),
      
      tags$hr(),
      h4("Доверительный интервал и уровень значимости α"),
      sliderInput("conf_level", "Доверительный интервал для математического ожидания",
                  min = 0.80, max = 0.99, value = 0.95, step = 0.01),
      numericInput("alpha", "Уровень значимости α для проверки H₀: μ = μ₀", value = 0.05,  #  уровень значимости для проверки гипотезы
                   
                   min = 0.001, max = 0.2, step = 0.01),
      
      tags$hr(),
      actionButton("run", "Смоделировать выборки", class = "btn-primary"),
      
      tags$hr(),
      sliderInput("exp_id", "Номер эксперимента (выборки) для детального просмотра",
                  min = 1, max = 100, value = 1, step = 1),
      
      tags$hr(),
      h4("Отображение доверительных интервалов"),
      
      sliderInput(
        "ci_range", 
        "Диапазон экспериментов для графика ДИ:",
        min = 1, 
        max = 100,          # обновится после симуляции
        value = c(1, 100), 
        step = 1
      ),
      
      checkboxInput(
        "show_only_miss",
        "Показывать только ДИ, не содержащие истинное мат. ожидание",
        FALSE
      )
    ),
    mainPanel(         # СПРАВА: ОСНОВНАЯ ПАНЕЛЬ
      width = 9,
      tabsetPanel(
        # tabPanel("1. Один эксперимент (одна выборка)",
        #          br(),
        #          plotOutput("one_exp_plot", height = "300px"),
        #          br(),
        #          verbatimTextOutput("one_exp_text")
        # ),
        tabPanel("2. Распределение выборочных средних",
                 br(),
                 uiOutput("hypothesis_text"),
                 br(),
                 plotOutput("means_hist", height = "350px"),
                 br(),
                 verbatimTextOutput("se_summary")
        ),
        tabPanel("3. Доверительные интервалы для μ",
                 br(),
                 plotOutput("ci_plot", height = "400px"),
                 br(),
                 DTOutput("ci_table")
        ),
        tabPanel("4. Распределение p-значений (t-тест)",
                 br(),
                 plotOutput("p_hist", height = "350px"),
                 br(),
                 verbatimTextOutput("p_summary")
        ),
        tabPanel("5. Критические области t-распределения",
                 br(),
                 plotOutput("crit_plot", height = "350px"),
                 br(),
                 verbatimTextOutput("crit_text")
        ),
        
        tabPanel("Help",
                 br(),
                 tabsetPanel(
                   tabPanel("Summary",
                            h3("Распределение случайной величины X в генеральной совокупности"),
                            p("В левой панели выбирается распределение случайной величины X в генеральной совокупности. ",
                              "Ниже приведены типичные графики данных распределений. ",
                              "Выбранное распределение выделено более толстой линией."),
                            
                            fluidRow(
                              column(
                                4,
                                h5("Нормальное распределение"),
                                plotOutput("help_norm", height = "180px")
                              ),
                              column(
                                4,
                                h5("Равномерное распределение"),
                                plotOutput("help_unif", height = "180px")
                              ),
                              column(
                                4,
                                h5("Экспоненциальное распределение"),
                                plotOutput("help_exp", height = "180px")
                              )
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
                   
                   tabPanel("Термины",
                            h3("Основные термины в симуляции")
                            # текст с определениями далее вставлю....
                   )
                 )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # observeEvent(
  #   input$dist_type,
  #   output$dist_params <- dist_params(input$dist_type)
  # )
  
  params_list <- eventReactive(input$run,{
    list(
      norm_mean  = input$norm_mean,
      norm_sd    = input$norm_sd,
      unif_min   = input$unif_min,
      unif_max   = input$unif_max,
      exp_rate   = input$exp_rate
    )
  })

  # Истинное среднее (мат. ожидание) для генеральной совокупности
  true_mu <- reactive(true_mu_calc(input$dist_type, params_list()))
  
  # Истинное генеральное SD
  true_sd <- reactive(true_sd_calc(input$dist_type, params_list()))
  
  # Автообновление μ0 вокруг истинного среднего
  observeEvent(
    # list(
    #   input$use_true_mu,
    #   input$dist_type,
    #   input$norm_mean,
    #   input$unif_min,
    #   input$unif_max,
    #   input$exp_rate
    # ),
    
    params_list(),
    {if (input$use_true_mu) {
        updateNumericInput(session, "mu0", value = true_mu())
      }
    },
    # ignoreInit = FALSE
  )
  
  # Текст гипотезы
  output$hypothesis_text <- renderUI({
    hypothesis_text_func(mu0 = input$mu0, alt_type = input$alt_type)
  })
  
  # Основная симуляция
df_from_sim <- eventReactive(input$run, {
    # n     <- input$n
    # n_sim <- input$n_sim
    # alpha_test <- input$alpha
    # conf_level <- input$conf_level
    
    validate(
      need(input$n >= 2, "Объем выборки должен быть ≥ 2"),
      need(input$n_sim >= 10, "Число повторений эксперимента должно быть ≥ 10")
    )
    
    set.seed(input$seed)
    
    # Генерация выборок из генеральной совокупности
    x <- reactive({
      samples_values_calc(
        n = input$n,
        n_sim = input$n_sim,
        dist_type = input$dist_type,
        params_list = params_list()
      )
    })
    
    simulate_fun_calc_new(
      n = input$n,
      n_sim = input$n_sim,
      conf_level = input$conf_level,
      alpha_test = input$alpha,
      alt_type = input$alt_type,
      mu0 = input$mu0,
      true_mu = true_mu(),
      true_sd = true_sd(),
      x = x()
    )
    
    # simulate_fun(
    #   n = input$n,
    #   n_sim = input$n_sim,
    #   conf_level = input$conf_level,
    #   alpha_test = input$alpha,
    #   alt_type = input$alt_type,
    #   mu0 = input$mu0,
    #   true_mu = true_mu(),
    #   true_sd = true_sd(),
    #   x = x())
    
    
  })
  
  # Обновление слайдеров exp_id и ci_range каждый раз после симуляции
observeEvent(df_from_sim(), {
    # res <- sim_res()
    # n_sim <- nrow(res$df)
    
    updateSliderInput(session, "exp_id",
                      max = input$n_sim,
                      value = min(1, input$n_sim))
    
    updateSliderInput(session, "ci_range",
                      min = 1,
                      max = input$n_sim,
                      value = c(1, min(200, input$n_sim)))
  })
  
  # TODO: График и текст для одного эксперимента

  # output$one_exp_plot <- renderPlot({
  #   stripchart_one_sample_plot(
  #     df_from_sim = df_from_sim(),
  #     Xmat = sim_res()$Xmat,
  #     exp_id = input$exp_id
  #   )
  # })
  
  output$one_exp_text <- renderPrint({
    one_exp_text_func(
      df_from_sim = df_from_sim(),
      exp_id = input$exp_id,
      alpha_test = input$alpha
    )
  })
  
  # Распределение выборочных средних
  #Этот блок каждый раз перерисовывает график, когда меняется результат симуляции sim_res()
  output$means_hist <- renderPlot({
    means_hist_plot(
      n = input$n,
      df_from_sim = df_from_sim(),
      true_mu = isolate(true_mu()),
      true_sd = isolate(true_sd()),
      mu0 = input$mu0
    )
  })
  
  output$se_summary <- renderPrint({
    se_summary_text_func(
      df_from_sim = df_from_sim(),
      conf_level = input$conf_level,
      true_sd = true_sd(),
      n = input$n
    )
  })
  
  # 2. Доверительные интервалы
  output$ci_plot <- renderPlot({
    ci_func_plot(
      df_from_sim = df_from_sim(),
      ci_range = input$ci_range,
      show_only_miss = input$show_only_miss,
      mu0 = input$mu0,
      true_mu = true_mu(),
      conf_level = input$conf_level
    )
  })
  
  output$ci_table <- renderDT({
    ci_table_calc(
      df_from_sim = df_from_sim()
    )
  })
  
  # 3. Распределение p-значений
  output$p_hist <- renderPlot({
    p_values_hist_plot(
      df_from_sim = df_from_sim(),
      alpha_test = input$alpha)
  })
  
  output$p_summary <- renderPrint({
    p_values_summary_text_func(
      df_from_sim = df_from_sim(),
      alpha_test = input$alpha)
  })
  
  
# HELP: формы генеральных распределений с подсветкой выбранного типа
  output$help_norm <- renderPlot({
    norm_plot_help(input$dist_type)
  })
  
  output$help_unif <- renderPlot({
    unif_plot_help(input$dist_type)
  })
  
  output$help_exp <- renderPlot({
    exp_plot_help(input$dist_type)
  })
}

shinyApp(ui = ui, server = server)