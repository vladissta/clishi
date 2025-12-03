# Packages ----
library(shiny)  # Required to run any Shiny app
library(ggplot2)  # For creating pretty plots
library(dplyr)  # For filtering and manipulating data
library(agridat)  # The package where the data comes from
library(DT)  # подключает пакет DT для интерактивных таблиц

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
        tabPanel("1. Один эксперимент (одна выборка)",
                 br(),
                 plotOutput("one_exp_plot", height = "300px"),
                 br(),
                 verbatimTextOutput("one_exp_text")
        ),
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
  
  # Истинное среднее (мат. ожидание) для генеральной совокупности
  true_mu <- reactive({
    if (input$dist_type == "norm") {
      input$norm_mean
    } else if (input$dist_type == "unif") {
      (input$unif_min + input$unif_max) / 2
    } else {
      1 / input$exp_rate
    }
  })
  
  # Истинное генеральное SD
  true_sd <- reactive({
    if (input$dist_type == "norm") {
      input$norm_sd
    } else if (input$dist_type == "unif") {
      (input$unif_max - input$unif_min) / sqrt(12)
    } else {
      1 / input$exp_rate
    }
  })
  
  # Автообновление μ0 вокруг истинного среднего
  observeEvent(
    list(input$use_true_mu,
         input$dist_type,
         input$norm_mean,
         input$unif_min,
         input$unif_max,
         input$exp_rate),
    {
      if (isTRUE(input$use_true_mu)) {
        updateNumericInput(session, "mu0", value = true_mu())
      }
    },
    ignoreInit = FALSE
  )
  
  # Текст гипотезы
  output$hypothesis_text <- renderUI({
    req(input$mu0)
    alt_symbol <- switch(
      input$alt_type,
      "two.sided" = "\\neq",
      "greater"   = ">",
      "less"      = "<"
    )
    
    withMathJax(HTML(paste0(
      "<h4>Нулевая гипотеза:</h4>",
      "$$H_0: \\mu = ", input$mu0, ".$$",
      "<h4>Альтернативная гипотеза:</h4>",
      "$$H_1: \\mu ", alt_symbol, " ", input$mu0, ".$$"
    )))
  })
  
  # Основная симуляция
  sim_res <- eventReactive(input$run, {
    n     <- input$n
    n_sim <- input$n_sim
    alpha_test <- input$alpha
    conf_level <- input$conf_level
    
    validate(
      need(n >= 2, "Объем выборки должен быть ≥ 2"),
      need(n_sim >= 10, "Число повторений эксперимента должно быть ≥ 10")
    )
    
    set.seed(input$seed)
    
    # Генерация выборок из генеральной совокупности
    if (input$dist_type == "norm") {
      x <- rnorm(n * n_sim, mean = input$norm_mean, sd = input$norm_sd)
    } else if (input$dist_type == "unif") {
      validate(need(input$unif_max > input$unif_min,
                    "Для равномерного распределения необходимо, чтобы b > a"))
      x <- runif(n * n_sim, min = input$unif_min, max = input$unif_max)
    } else {
      x <- rexp(n * n_sim, rate = input$exp_rate)
    }
    
    # строка = отдельный эксперимент (выборка), столбец = наблюдение
    Xmat <- matrix(x, nrow = n_sim, ncol = n, byrow = TRUE)
    
    # Выборочные средние, выборочные SD, выборочные SE
    means <- rowMeans(Xmat)
    sds   <- apply(Xmat, 1, sd)
    se    <- sds / sqrt(n)
    
    # ДИ для математического ожидания по t-распределению
    alpha_ci <- 1 - conf_level
    t_crit   <- qt(1 - alpha_ci / 2, df = n - 1)
    
    ci_low  <- means - t_crit * se
    ci_high <- means + t_crit * se
    
    mu0   <- input$mu0
    t_stat <- (means - mu0) / se
    
    # p-value 
    p_val <- switch(
      input$alt_type,
      "two.sided" = 2 * pt(-abs(t_stat), df = n - 1),
      "greater"   = 1 - pt(t_stat, df = n - 1),
      "less"      = pt(t_stat, df = n - 1)
    )
    
    cover_mu0  <- (ci_low <= mu0) & (ci_high >= mu0)
    cover_true <- (ci_low <= true_mu()) & (ci_high >= true_mu())
    reject_H0  <- p_val < alpha_test
    
    df <- data.frame(
      experiment = 1:n_sim,
      mean = means,
      sd = sds,
      se = se,
      ci_low = ci_low,
      ci_high = ci_high,
      p_value = p_val,
      covers_mu0 = cover_mu0,
      covers_true_mu = cover_true,
      reject_H0 = reject_H0
    )
    
    list(
      df = df,
      Xmat = Xmat,
      true_mu = true_mu(),
      true_sd = true_sd(),
      n = n,
      alpha_test = alpha_test,
      conf_level = conf_level,
      mu0 = mu0
    )
  })
  
  # Обновление слайдеров exp_id и ci_range каждый разпосле симуляции
  observeEvent(sim_res(), {
    res <- sim_res()
    n_sim <- nrow(res$df)
    
    updateSliderInput(session, "exp_id",
                      max = n_sim,
                      value = min(1, n_sim))
    
    updateSliderInput(session, "ci_range",
                      min = 1,
                      max = n_sim,
                      value = c(1, min(200, n_sim)))
  })
  
  # 4. Один эксперимент (одна выборка)
  output$one_exp_plot <- renderPlot({
    res <- sim_res()
    df  <- res$df
    id  <- input$exp_id
    validate(
      need(id >= 1 && id <= nrow(df), "Неверный номер эксперимента")
    )
    x_i <- res$Xmat[id, ]
    
    stripchart(x_i, method = "jitter", pch = 16,
               main = paste("Наблюдения одной выборки (эксперимент №", id, ")"),
               xlab = "Наблюдения Xᵢ",
               ylab = "",
               yaxt = "n")
    abline(v = df$mean[id], col = "blue", lwd = 2)
  })
  
  output$one_exp_text <- renderPrint({
    res <- sim_res()
    df  <- res$df
    id  <- input$exp_id
    validate(
      need(id >= 1 && id <= nrow(df), "Неверный номер эксперимента")
    )
    row <- df[df$experiment == id, ]
    
    cat("Эксперимент (выборка) №", id, "\n\n")
    cat("Выборочное среднее \\bar{X}   =", round(row$mean, 4), "\n")
    cat("Выборочное SD                  =", round(row$sd, 4), "\n")
    cat("Стандартная ошибка SE          =", round(row$se, 4), "\n")
    cat("Доверительный интервал для μ   = [", round(row$ci_low, 4), ";", round(row$ci_high, 4), "]\n")
    cat("p-value t-теста для H₀: μ = μ₀ =", signif(row$p_value, 4), "\n\n")
    
    cat("Интерпретация для данной выборки:\n")
    
    cat("- Доверительный интервал для μ ",
        ifelse(row$covers_true_mu, "покрывает", "не покрывает"),
        " истинное математическое ожидание μ.\n", sep = "")
    
    cat("- Доверительный интервал ",
        ifelse(row$covers_mu0, "покрывает", "не покрывает"),
        " гипотетическое значение μ₀.\n", sep = "")
    
    cat("- На основе p-value = ", signif(row$p_value, 4),
        " нулевая гипотеза H₀: μ = μ₀ ",
        ifelse(row$reject_H0, "отвергается", "не отвергается"),
        " при уровне α = ", res$alpha_test, ".\n", sep = "")
  })
  
  # 1. Распределение выборочных средних
  output$means_hist <- renderPlot({          #Этот блок каждый раз перерисовывает график, когда меняется результат симуляции sim_res()
    res <- sim_res()                     # 
    df <- res$df
    
    hist(df$mean,                          # df$mean — вектор из n_sim выборочных средних
         breaks = 30,
         freq = FALSE,
         main = "Распределение выборочных средних (оценок математического ожидания)",
         xlab = "Выборочное среднее \\(\\bar{X}\\)")
    
    #  Считаем теоретические параметры распределения выборочного среднего
    theor_mu <- res$true_mu                    # истинное математическое ожидание μ
    theor_se <- res$true_sd / sqrt(res$n)     # теоретическая SE( \bar{X} )
    
    #  Диапазон оси X: учитываем и данные, и μ, и μ0
    x_min <- min(df$mean)
    x_max <- max(df$mean)
    x_lim <- range(c(x_min, x_max, theor_mu, res$mu0))
    
    hist(df$mean,
         breaks = 30,
         freq   = FALSE,
         xlim   = x_lim,   
         main   = "Распределение выборочных средних (оценок математического ожидания)",
         xlab   = "Выборочное среднее \\(\\bar{X}\\)")
    
    #  Теоретическая кривая распределения \bar{X}
    x_vals <- seq(x_lim[1], x_lim[2], length.out = 200)
    lines(x_vals,
          dnorm(x_vals, mean = theor_mu, sd = theor_se),
          lwd = 2)
    
    # Вертикальные линии: гипотетическое μ0 и истинное μ
    abline(v = res$mu0,  col = "red",       lwd = 2, lty = 2)  # гипотетическое значение μ0 (H0)
    abline(v = theor_mu, col = "darkgreen", lwd = 2, lty = 3)  # истинный параметр μ
    
    # Легенда
    legend("topright",
           legend = c("Теоретическое распределение \\(\\bar{X}\\) по ЦПТ",
                      "Гипотетическое μ₀",
                      "Истинное математическое ожидание μ"),
           col = c("black", "red", "darkgreen"),
           lwd = c(2, 2, 2),
           lty = c(1, 2, 3),
           bty = "n")
  })
  
  output$se_summary <- renderPrint({
    res <- sim_res()
    df <- res$df
    
    cat("Эмпирическое распределение выборочных средних:\n")
    cat("Среднее выборочных средних  E(\\bar{X})_emp =", round(mean(df$mean), 4), "\n")
    cat("Эмпирическое SD(\\bar{X}) =", round(sd(df$mean), 4),
        "   (теоретическая стандартная ошибка SE =",
        round(res$true_sd / sqrt(res$n), 4), ")\n\n")
    
    cat("Средняя выборочная оценка стандартной ошибки SE_i:\n")
    cat("mean(SE_i) =", round(mean(df$se), 4), "\n\n")
    
    cat("Доля доверительных интервалов, покрывающих гипотетическое значение μ₀:\n")
    cat("coverage(μ₀) =", round(mean(df$covers_mu0), 4), "\n\n")
    
    cat("Доля доверительных интервалов, покрывающих истинное математическое ожидание μ:\n")
    cat("coverage(μ_true) =", round(mean(df$covers_true_mu), 4),
        "   при уровне доверия =", res$conf_level, "\n\n")
    
    cat("Интерпретация:\n")
    cat("- Истинное математическое ожидание μ является фиксированным параметром и не является случайной величиной.\n")
    cat("- Каждый построенный доверительный интервал либо покрывает μ, либо не покрывает его.\n")
    cat("- Уровень доверия", res$conf_level,
        "означает, что при многократном повторении эксперимента\n")
    cat("примерно", res$conf_level * 100, "% построенных доверительных интервалов будут покрывать μ,\n")
    cat("если все допущения сделанные в процессе оценки данных интервалов, соблюдаются.\n")
  })
  
  # 2. Доверительные интервалы
  output$ci_plot <- renderPlot({
    res <- sim_res()
    df  <- res$df
    
    rng <- input$ci_range
    df  <- df[df$experiment >= rng[1] & df$experiment <= rng[2], ]
    
    if (isTRUE(input$show_only_miss)) {
      df <- df[!df$covers_true_mu, ]
    }
    
    n_sim_sub <- nrow(df)
    validate(need(n_sim_sub > 0, "В выбранном диапазоне нет доверительных интервалов для отображения"))
    
    plot(df$experiment, df$mean,
         ylim = range(c(df$ci_low, df$ci_high, res$mu0, res$true_mu)),
         xlab = "Номер эксперимента",
         ylab = "Выборочное среднее и ДИ для математического ожидания",
         pch = 16,
         main = paste0("Доверительные интервалы для μ (уровень доверия = ",
                       res$conf_level, ")\nПоказаны эксперименты ",
                       rng[1], "–", rng[2]))
    
    for (i in 1:n_sim_sub) {
      col_i <- if (df$covers_true_mu[i]) "gray50" else "red"
      segments(x0 = df$experiment[i], y0 = df$ci_low[i],
               x1 = df$experiment[i], y1 = df$ci_high[i],
               lwd = 2, col = col_i)
    }
    
    abline(h = res$true_mu, col = "darkgreen", lty = 3, lwd = 2)
    abline(h = res$mu0,    col = "red",       lty = 2, lwd = 2)
    
    legend("topright",
           legend = c(
             "Истинное математическое ожидание μ",
             "Гипотетическое значение μ₀ (H₀)",
             "Доверительный интервал покрывает μ",
             "Доверительный интервал не покрывает μ"
           ),
           lwd = c(2, 2, 2, 2),
           col = c("darkgreen", "red", "gray50", "red"),
           lty = c(3, 2, 1, 1),
           bty = "n")
  })
  
  output$ci_table <- renderDT({
    res <- sim_res()
    df <- res$df
    
    df$mean    <- round(df$mean, 4)
    df$se      <- round(df$se, 4)
    df$ci_low  <- round(df$ci_low, 4)
    df$ci_high <- round(df$ci_high, 4)
    df$p_value <- signif(df$p_value, 4)
    
    datatable(
      df,
      options = list(pageLength = 10),
      rownames = FALSE
    )
  })
  
  # 3. Распределение p-значений
  output$p_hist <- renderPlot({
    res <- sim_res()
    df <- res$df
    
    hist(df$p_value,
         breaks = 20,
         xlim = c(0, 1),
         main = "Распределение p-значений t-теста для H₀: μ = μ₀",
         xlab = "p-value")
    abline(v = res$alpha_test, lty = 2, lwd = 2, col = "red")
    
    legend("topright",
           legend = c("Граница уровня значимости α"),
           lwd = 2, lty = 2, col = "red", bty = "n")
  })
  
  output$p_summary <- renderPrint({
    res <- sim_res()
    df <- res$df
    
    cat("Доля экспериментов с p < α (отклоняем H₀: μ = μ₀):\n")
    cat("freq(reject H₀) =", round(mean(df$reject_H0), 4),
        " при α =", res$alpha_test, "\n\n")
    
    cat("Интерпретация:\n")
    cat("- если...")
    cat("- если...")
  })
  
  
  
  # HELP: формы генеральных распределений с подсветкой выбранного типа
  output$help_norm <- renderPlot({
    selected <- input$dist_type == "norm"
    col <- if (selected) "black" else "gray70"
    lwd <- if (selected) 3 else 1.5
    
    curve(dnorm(x, mean = 0, sd = 1),
          from = -3, to = 3,
          xlab = "x", ylab = "Плотность",
          main = if (selected) "Нормальное N(0,1) — выбрано" else "Нормальное N(0,1)",
          lwd = lwd, col = col)
  })
  
  output$help_unif <- renderPlot({
    selected <- input$dist_type == "unif"
    col <- if (selected) "black" else "gray70"
    lwd <- if (selected) 3 else 1.5
    
    curve(dunif(x, min = 0, max = 1),
          from = -0.2, to = 1.2,
          xlab = "x", ylab = "Плотность",
          main = if (selected) "Равномерное U(0,1) — выбрано" else "Равномерное U(0,1)",
          lwd = lwd, col = col)
    abline(v = c(0,1), lty = 2)
  })
  
  output$help_exp <- renderPlot({
    selected <- input$dist_type == "exp"
    col <- if (selected) "black" else "gray70"
    lwd <- if (selected) 3 else 1.5
    
    curve(dexp(x, rate = 1),
          from = 0, to = 5,
          xlab = "x", ylab = "Плотность",
          main = if (selected) "Экспоненциальное Exp(1) — выбрано" else "Экспоненциальное Exp(1)",
          lwd = lwd, col = col)
  })
}

shinyApp(ui = ui, server = server)