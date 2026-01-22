# app_blocks.R ----

# Packages ----
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)
library(shinyjs)

# Sources ----
source("scripts/calculations.R")
source("scripts/plots.R")
source("scripts/texts.R")
source("scripts/help_output.R")

# Helpers ----
default_seed <- 42

# UI ----
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    titleWidth = 320,
    title = tags$span(""),  
  
    
    # Верхняя панель: подпункты блока 1
    tags$li(
      class = "dropdown",
      style = "padding: 0; margin-left: 14px; margin-top: 6px;",
      conditionalPanel(
        condition = "input.top_block == 'block1'",
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
      )
    )
  ),
  
  dashboardSidebar(
    width = 320,
    
    # 
    conditionalPanel(
      condition = "!input.top_block || input.top_block == 'home'",
      tags$div(
        class = "clishi-sidebar-brand",
        tags$div(class = "clishi-sidebar-brand-title", "CliShi"),
        tags$div(
          class = "clishi-sidebar-brand-subtitle",
          "Interactive Clinical Research Simulator - Shiny Application"
        ),
        tags$div(class = "clishi-sidebar-brand-version", "v 1.0 (beta)")
      )
    ),
    
    # Динамическая часть (параметры)
    uiOutput("sidebar_inputs"),
    
    # Sticky footer: кнопки
    tags$div(
      class = "clishi-sticky-footer",
      
      conditionalPanel(
        condition = "input.top_block == 'block1'",
        actionButton("run", "Смоделировать выборки", class = "btn-primary btn-block"),
        tags$div(style = "height:8px;")
      ),
      
      conditionalPanel(
        condition = "input.top_block && input.top_block != 'home'",
        actionButton("go_home", "\u2190 Назад к выбору блоков", class = "btn-default btn-block")
      )
    )
  ),
  
  dashboardBody(
    shinyjs::useShinyjs(),
    withMathJax(),
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "clishi.css"),
      tags$script(src = "clishi.js")
    ),
    
    # Hidden controller for blocks
    tabsetPanel(
      id = "top_block",
      type = "tabs",
      selected = "home",
      tabPanel("home", value = "home"),
      tabPanel("block1", value = "block1"),
      tabPanel("block2", value = "block2"),
      tabPanel("block3", value = "block3"),
      tabPanel("block4", value = "block4")
    ),
    
    # Логотип показываем ТОЛЬКО внутри блоков (НЕ home)
    conditionalPanel(
      condition = "input.top_block && input.top_block != 'home'",
      tags$div(
        class = "clishi-fixed-logo",
        tags$img(src = "clishi_hex_transparent.png", alt = "CliShi")
      )
    ),
    
    # HOME (выбор блоков) — кликабельные карточки 
    conditionalPanel(
      condition = "input.top_block == 'home' || !input.top_block",
      fluidRow(
        box(
          width = 12,
          title = NULL,
          solidHeader = FALSE,
          
          tags$div(
            style = "text-align:center; margin-bottom:20px;",
            tags$img(src = "clishi_logo.png", height = "180px", alt = "CliShi logo")
          ),
          
          # BLOCK 1
          tags$div(
            class = "clishi-card clishi-card-click",
            onclick = "Shiny.setInputValue('go_block1', Math.random(), {priority: 'event'})",
            tags$h3("Моделирование и проверка гипотез"),
            tags$p("Симуляции, распределения выборочных средних, ДИ, p-value, критические области.")
          ),
          
          # BLOCK 2
          tags$div(
            class = "clishi-card clishi-card-click",
            onclick = "Shiny.setInputValue('go_block2', Math.random(), {priority: 'event'})",
            tags$h3("Оценка зависимости результатов теста от величины параметра"),
            tags$p("Сравнение тестов при изменении эффекта/параметров (планируется).")
          ),
          
          # BLOCK 3
          tags$div(
            class = "clishi-card clishi-card-click",
            onclick = "Shiny.setInputValue('go_block3', Math.random(), {priority: 'event'})",
            tags$h3("Расчет выборки классическими методами"),
            tags$p("Классические формулы для n, α, power, effect (планируется).")
          ),
          
          # BLOCK 4
          tags$div(
            class = "clishi-card clishi-card-click",
            onclick = "Shiny.setInputValue('go_block4', Math.random(), {priority: 'event'})",
            tags$h3("Расчет выборки методом имитационного моделирования"),
            tags$p("Симуляционный подбор n под заданную мощность (планируется).")
          )
        )
      )
    ),
    # -------------------------
    # BLOCK 1 (контент переключается по input$block1_subtab)
    # -------------------------
    conditionalPanel(
      condition = "input.top_block == 'block1'",
      
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
      )
    ),
    
    # -------------------------
    # BLOCK 2–4 placeholders
    # -------------------------
    conditionalPanel(
      condition = "input.top_block == 'block2'",
      h2("Блок 2...."),
      p("Здесь будут: one-sample t, two-sample t, Mann–Whitney, Brunner–Munzel, χ², Fisher.")
    ),
    
    conditionalPanel(
      condition = "input.top_block == 'block3'",
      h2("Блок 3... Расчет выборки классическими методами"),
      p("Сюда добавим классические формулы.")
    ),
    
    conditionalPanel(
      condition = "input.top_block == 'block4'",
      h2("Блок 4...."),
      p("Сюда добавим симуляционный подбор n под заданную мощность.")
    )
  )
)

# SERVER ----
server <- function(input, output, session) {
  
  # Re-init tooltips after every UI flush (важно для dynamic UI)
  session$onFlushed(function() {
    shinyjs::runjs("$('[data-toggle=\"tooltip\"]').tooltip({container:'body'});")
  }, once = FALSE)
  
  observeEvent(input$go_block1, {
    updateTabsetPanel(session, "top_block", selected = "block1")
    updateTabsetPanel(session, "block1_subtab", selected = "one_exp")
  })
  
  observeEvent(input$go_block2, {
    updateTabsetPanel(session, "top_block", selected = "block2")
  })
  
  observeEvent(input$go_block3, {
    updateTabsetPanel(session, "top_block", selected = "block3")
  })
  
  observeEvent(input$go_block4, {
    updateTabsetPanel(session, "top_block", selected = "block4")
  })
  
  # --- Назад на главную ---
  observeEvent(input$go_home, {
    updateTabsetPanel(session, "top_block", selected = "home")
  })
  
  
  # Sidebar
  output$sidebar_inputs <- renderUI({
    req(input$top_block)
    
    if (input$top_block != "block1") {
      return(NULL)
    }
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
          column(
            6,
            numericInput(
              "norm_mean",
              HTML("<span data-toggle='tooltip' title='Истинное среднее (математическое ожидание)'>μ</span>"),
              0
            )
          ),
          column(
            6,
            numericInput(
              "norm_sd",
              HTML("<span data-toggle='tooltip' title='Стандартное отклонение'>σ</span>"),
              1,
              min = 0.0001
            )
          )
        )
      ),
      
      conditionalPanel(
        "input.dist_type == 'unif'",
        numericInput(
          "unif_min",
          HTML("<span data-toggle='tooltip' title='Нижняя граница равномерного распределения (параметр a)'>a</span>"),
          0
        ),
        numericInput(
          "unif_max",
          HTML("<span data-toggle='tooltip' title='Верхняя граница равномерного распределения (параметр b)'>b</span>"),
          1
        )
      ),
      conditionalPanel(
        "input.dist_type == 'exp'",
        numericInput(
          "exp_rate",
          HTML("<span data-toggle='tooltip' title='Параметр интенсивности экспоненциального распределения (λ)'>λ</span>"),
          1,
          min = 0.0001
        )
      ),
      
      
      tags$hr(),
      h4("Выборка и симуляции"),
      
      # ---- tooltip для n ----
      numericInput(
        "n",
        HTML("<span data-toggle='tooltip' title='Размер выборки (объём одной выборки)'>n</span>"),
        value = 30, min = 2, step = 1
      ),
      # -------------------------------------------
      
      numericInput("n_sim", HTML("<span data-toggle='tooltip' title='Число повторений эксперимента (число выборок)'>n<sub>sim</sub></span>"),
                   value = 1000, min = 10, step = 10),
      
      tags$hr(),
      h4("Гипотеза о математическом ожидании"),
      
      fluidRow(
        column(
          6,
          checkboxInput(
            "use_true_mu",
            HTML("<span data-toggle='tooltip' title='Если включено, устанавливаем μ₀ равным истинному μ (и фиксируем поле μ₀)'>μ₀ = μ</span>"),
            TRUE
          )
        ),
        column(
          6,
          numericInput(
            "mu0",
            HTML("<span data-toggle='tooltip' title='Гипотетическое значение математического ожидания (H₀: μ = μ₀)'>μ₀</span>"),
            value = 0
          )
        )
      ),
      
      selectInput(
        "alt_type",
        HTML("<span data-toggle='tooltip' title='Тип альтернативной гипотезы H₁'>H₁</span>"),
        choices = c(
          "двусторонняя (μ ≠ μ₀)" = "two.sided",
          "правосторонняя (μ > μ₀)" = "greater",
          "левосторонняя (μ < μ₀)" = "less"
        ), selected = "two.sided"
      ),
      
      tags$hr(),
      h4("Доверительный интервал и уровень значимости α"),
      
      sliderInput(
        "conf_level",
        HTML("<span data-toggle='tooltip' title='Доверительный интервал'>CI</span>"),
        min = 0.80, max = 0.99, value = 0.95, step = 0.01
      ),
      numericInput(
        "alpha",
        HTML("<span data-toggle='tooltip' title='Уровень значимости α для проверки H₀'>α</span>"),
        value = 0.05, min = 0.001, max = 0.2, step = 0.01
      )
    )
  })
  
  # -----------------------------
  # Block 1 logic (твоя логика)
  # -----------------------------
  params_list <- reactive({
    list(
      norm_mean = input$norm_mean,
      norm_sd   = input$norm_sd,
      unif_min  = input$unif_min,
      unif_max  = input$unif_max,
      exp_rate  = input$exp_rate
    )
  })
  
  true_mu <- reactive(true_mu_calc(input$dist_type, params_list()))
  true_sd <- reactive(true_sd_calc(input$dist_type, params_list()))
  
  # μ0 фиксируем при включенной галочке, но НЕ обновляем при изменении μ.
  observeEvent(input$use_true_mu, {
    if (isTRUE(input$use_true_mu)) {
      updateNumericInput(session, "mu0", value = true_mu())
      shinyjs::disable("mu0")
    } else {
      shinyjs::enable("mu0")
    }
  }, ignoreInit = FALSE)
  
  output$hypothesis_text <- renderUI({
    hypothesis_text_func(mu0 = input$mu0, alt_type = input$alt_type)
  })
  
  samples_values_simulated <- eventReactive(input$run, {
    
    validate(
      need(input$n >= 2, "Объем выборки должен быть ≥ 2"),
      need(input$n_sim >= 10, "Число повторений эксперимента должно быть ≥ 10")
    )
    
    # Синхронизируем μ0 с текущим μ ТОЛЬКО в момент симуляции
    if (isTRUE(input$use_true_mu)) {
      updateNumericInput(session, "mu0", value = true_mu())
    }
    
    set.seed(default_seed)
    
    samples_values_simulated_calc(
      n = input$n,
      n_sim = input$n_sim,
      dist_type = input$dist_type,
      params_list = params_list()
    )
  })
  
  df_from_sim <- reactive({
    simulate_fun_calc_new(
      n = input$n,
      n_sim = input$n_sim,
      conf_level = input$conf_level,
      alpha_test = input$alpha,
      alt_type = input$alt_type,
      mu0 = input$mu0,
      true_mu = true_mu(),
      true_sd = true_sd(),
      simulated_values_df = samples_values_simulated()
    )
  })
  
  type1_ids <- reactive({
    df <- df_from_sim()
    alpha <- input$alpha
    which(df$p_value < alpha)
  })
  
  output$type1_slider_ui <- renderUI({
    req(input$top_block == "block1")
    req(input$block1_subtab == "one_exp")   # показывать на вкладке "Одна выборка"
    
    if (!isTRUE(input$use_true_mu)) {
      return(tags$div(style="color:#b8c7ce;", "Включи μ₀ = μ, чтобы отобразить ошибки I рода."))
    }
    
    ids <- type1_ids()
    
    if (length(ids) == 0) {
      return(tags$div(style="color:#b8c7ce;", "Ошибок I рода (p < α) не найдено при текущих параметрах."))
    }
    
    tagList(
      sliderInput(
        "type1_pick",
        "Эксперименты с ошибкой I рода (p < α при μ₀ = μ)",
        min = 1,
        max = length(ids),
        value = 1,
        step = 1
      ),
      tags$small(
        style="color:#b8c7ce; display:block; margin-top:6px;",
        paste0("Найдено: ", length(ids), " из ", input$n_sim,
               " (доля = ", round(length(ids)/input$n_sim, 3), ")")
      )
    )
  })
  
  observeEvent(input$type1_pick, {
    req(isTRUE(input$use_true_mu))
    ids <- type1_ids()
    req(length(ids) > 0)
    
    exp_selected <- ids[input$type1_pick]
    updateSliderInput(session, "exp_id", value = exp_selected)
  }, ignoreInit = TRUE)
  
  observeEvent(df_from_sim(), {
    updateSliderInput(
      session, "exp_id",
      max = input$n_sim,
      value = min(1, input$n_sim)
    )
    
    try(
      updateSliderInput(
        session, "ci_range",
        min = 1,
        max = input$n_sim,
        value = c(1, min(200, input$n_sim))
      ),
      silent = TRUE
    )
  })
  
  # Outputs
  output$one_exp_plot <- renderPlot({
    stripchart_one_sample_plot(
      simulated_values_df = samples_values_simulated(),
      exp_id = input$exp_id,
      mu0 = input$mu0,
      true_mu = true_mu(),
      conf_level = input$conf_level
    )
  })
  
 
  output$one_exp_text <- renderPrint({
    one_exp_text_func(
      df_from_sim = df_from_sim(),
      exp_id = input$exp_id,
      alpha_test = input$alpha,
      mu0 = input$mu0,
      true_mu = true_mu(),
      conf_level = input$conf_level,
      use_true_mu = input$use_true_mu
    )
  })
  
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
  
  output$ci_plot <- renderPlot({
    ci_rng <- input$ci_range
    if (is.null(ci_rng) || length(ci_rng) != 2) {
      ci_rng <- c(1, min(100, input$n_sim))
    }
    
    ci_func_plot(
      df_from_sim = df_from_sim(),
      ci_range = ci_rng,
      show_only_miss = isTRUE(input$show_only_miss),
      mu0 = input$mu0,
      true_mu = true_mu(),
      conf_level = input$conf_level
    )
  })
  
  output$ci_table <- renderDT({
    ci_table_calc(df_from_sim = df_from_sim())
  })
  
  output$p_hist <- renderPlot({
    p_values_hist_plot(
      df_from_sim = df_from_sim(),
      alpha_test = input$alpha
    )
  })
  
  output$p_summary <- renderPrint({
    p_values_summary_text_func(
      df_from_sim = df_from_sim(),
      alpha_test = input$alpha
    )
  })
  
  output$crit_plot <- renderPlot({
    df <- df_from_sim()
    id <- input$exp_id
    
    validate(need(id >= 1 && id <= nrow(df), "Неверный номер эксперимента"))
    
    if ("statistic" %in% names(df)) {
      t_obs <- df$statistic[id]
    } else if ("t_stat" %in% names(df)) {
      t_obs <- df$t_stat[id]
    } else if ("t" %in% names(df)) {
      t_obs <- df$t[id]
    } else {
      validate(need(FALSE, "В df_from_sim нет t-статистики (нужен столбец statistic / t_stat / t)"))
    }
    
    dfree <- input$n - 1
    alpha <- input$alpha
    alt <- input$alt_type
    
    x <- seq(-4, 4, length = 400)
    y <- dt(x, dfree)
    
    plot(
      x, y, type = "l", lwd = 2,
      main = "Проверка H₀ с использованием критических значений t-статистики",
      xlab = "t", ylab = "Плотность"
    )
    
    if (alt == "two.sided") {
      tcrit <- qt(1 - alpha / 2, dfree)
      
      polygon(c(x[x < -tcrit], -tcrit),
              c(y[x < -tcrit], 0),
              col = rgb(1, 0, 0, 0.3), border = NA)
      
      polygon(c(x[x > tcrit], tcrit),
              c(y[x > tcrit], 0),
              col = rgb(1, 0, 0, 0.3), border = NA)
      
    } else if (alt == "greater") {
      tcrit <- qt(1 - alpha, dfree)
      
      polygon(c(x[x > tcrit], tcrit),
              c(y[x > tcrit], 0),
              col = rgb(1, 0, 0, 0.3), border = NA)
      
    } else {
      tcrit <- qt(alpha, dfree)
      
      polygon(c(x[x < tcrit], tcrit),
              c(y[x < tcrit], 0),
              col = rgb(1, 0, 0, 0.3), border = NA)
    }
    
    abline(v = t_obs, col = "blue", lwd = 2)
    
    legend(
      "topright",
      legend = c("t-распределение", "t_obs", "Критическая область"),
      col = c("black", "blue", "red"),
      lwd = 2,
      bty = "n"
    )
  })
  
  output$crit_text <- renderPrint({
    
    req(input$n, input$alpha, input$alt_type)
    req(df_from_sim())
    
    alpha <- input$alpha
    alt <- input$alt_type
    dfree <- input$n - 1
    
    if (alt == "two.sided") {
      tcrit <- qt(1 - alpha / 2, dfree)
      cat("Критические значения (двусторонний тест): ±", round(tcrit, 4), "\n")
      cat("Отклоняем H₀, если |t_obs| >", round(tcrit, 4), "\n")
    } else if (alt == "greater") {
      tcrit <- qt(1 - alpha, dfree)
      cat("Критическое значение (правосторонний тест):", round(tcrit, 4), "\n")
      cat("Отклоняем H₀, если t_obs >", round(tcrit, 4), "\n")
    } else {
      tcrit <- qt(alpha, dfree)
      cat("Критическое значение (левосторонний тест):", round(tcrit, 4), "\n")
      cat("Отклоняем H₀, если t_obs <", round(tcrit, 4), "\n")
    }
    
    cat("\nПримечание:\n")
    cat("- Красная область на графике — критическая область уровня α.\n")
    cat("- Синяя линия — наблюдаемое значение t-статистики для выбранной выборки.\n")
  })
  
  output$help_norm <- renderPlot({
    req(input$dist_type)
    norm_plot_help(input$dist_type)
  })
  
  output$help_unif <- renderPlot({
    req(input$dist_type)
    unif_plot_help(input$dist_type)
  })
  
  output$help_exp <- renderPlot({
    req(input$dist_type)
    exp_plot_help(input$dist_type)
  })
  
}

shinyApp(ui = ui, server = server)