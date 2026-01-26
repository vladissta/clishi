# app_blocks.R ----

# Packages ----
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(shinyjs)
library(shinycssloaders)

library(furrr)
library(future)

# Sources ----
source("scripts/calculations.R")
source("scripts/plots.R")
source("scripts/texts.R")
source("scripts/ui_components.R")
source("scripts/ui_block1.R")
source("scripts/ui_block2.R")
source("scripts/ui_block3.R")
source("scripts/ui_block4.R")
source("scripts/help_output.R")

source('scripts/t_test_one_sample.R')
source('scripts/t_test_two_sample.R')
source('scripts/Mann-Whitney test.R')
source('scripts/Brunner-Munzel test.R')
source('scripts/contingency_table.R')

source('scripts/simulation_wrapper.R')

# Helpers ----
DEFAULT_SEED <- 42

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
        create_block1_tabs() # <---
      ),
      
      conditionalPanel(
        condition = "input.top_block == 'block2'",
        create_block2_tabs() # <---
      ),
      
      conditionalPanel(
        condition = "input.top_block == 'block3'",
        create_block3_tabs() # <---
      ),
      
      conditionalPanel(
        condition = "input.top_block == 'block4'",
        create_block4_tabs() # <---
      )
      
      
    )
  ),
  
dashboardSidebar(
    width = 320,
    
    conditionalPanel(
      condition = "!input.top_block || input.top_block == 'home'",
      create_sidebar_brand()), # <---
    
    # Динамическая часть (параметры)
    uiOutput("sidebar_inputs"),
    
    # Sticky footer: кнопки
    create_sidebar_footer_buttons() # <---
  ),
  
  dashboardBody(
    shinyjs::useShinyjs(),
    withMathJax(),
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "clishi.css"),
      tags$script(src = "clishi.js")
    ),
    
    # Hidden controller for blocks TODO!
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
      create_fixed_logo()), # <---
    
    # HOME (выбор блоков) — кликабельные карточки 
    conditionalPanel(
      condition = "input.top_block == 'home' || !input.top_block",
      create_home_page() # <---
    ),
    
    # -------------------------
    # BLOCK 1 (контент переключается по input$block1_subtab)
    # -------------------------
    
    conditionalPanel(
      condition = "input.top_block == 'block1'",
      create_block1_content() # <---
    ),
    
    # -------------------------
    # BLOCK 2–4 
    # -------------------------
    conditionalPanel(
      condition = "input.top_block == 'block2'",
      fluidRow(
        box(
          width = 12,
          withSpinner(
            plotOutput("parameters_grid_line_plot", height = "300px"),
            type = 6, 
            caption = "Симуляция в прогрессе.."
          )
        )
      )
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
    
    switch(input$top_block,
           "block1" = tagList(
             sidebar_block1_dist_inputs(),
             sidebar_block1_sample_inputs(),
             sidebar_block1_hypothesis_inputs(),
             sidebar_block1_test_inputs()
           ),
           "block2" = tagList(
             sidebar_block2_tests_inputs()
             # sidebar_block2_yyy()
           ),
           "block3" = tagList(
             # sidebar_block3_xxx()
           ),
           "block4" = tagList(
             # sidebar_block4_xxx()
           ),
           NULL  # default case (home or other)
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
  
  samples_values_simulated <- eventReactive(input$run_block1, {
    
    validate(
      need(input$n >= 2, "Объем выборки должен быть ≥ 2"),
      need(input$n_sim >= 10, "Число повторений эксперимента должно быть ≥ 10"))
    
    # Синхронизируем μ0 с текущим μ ТОЛЬКО в момент симуляции
    if (isTRUE(input$use_true_mu)) {
      updateNumericInput(session, "mu0", value = true_mu())
    }
    
    set.seed(DEFAULT_SEED)
    
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
    which(df_from_sim()$p_value < input$alpha)
  })
  
  output$type1_slider_ui <- renderUI({
    req(input$top_block == "block1", 
        input$block1_subtab == "one_exp")   # показывать на вкладке "Одна выборка"
    
    if (!isTRUE(input$use_true_mu)) {
      return(tags$div(style="color:#b8c7ce;",
                      "Включи μ₀ = μ, чтобы отобразить ошибки I рода."))
    }
    
    ids <- type1_ids()
    
    if (length(ids) == 0) {
      return(tags$div(style="color:#b8c7ce;",
                      "Ошибок I рода (p < α) не найдено."))}
    
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
        sprintf("Найдено: %d из %d (доля = %.3f)", 
                length(ids), input$n_sim, 
                length(ids) / input$n_sim)
      )
    )
  })
  
  observeEvent(input$type1_pick, {
    req(isTRUE(input$use_true_mu))
    req(length(type1_ids()) > 0)
    updateSliderInput(session, "exp_id", 
                      value = type1_ids()[input$type1_pick])
  }, ignoreInit = TRUE)
  
  observeEvent(df_from_sim(), {
    updateSliderInput(
      session, "exp_id",
      max = input$n_sim,
      # value = min(1, input$n_sim)
      value = 1
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
  
  # ---------------------
  # OUTPUTS
  # ---------------------
  
  # BLOCK 1
  
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
    ci_rng <- if (is.null(input$ci_range) || length(input$ci_range) != 2) {
      c(1, min(100, input$n_sim))
    } else {
      input$ci_range
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
    t_test_crit_plot(
      df_from_sim = df_from_sim(),
      exp_id = input$exp_id,
      n = input$n,
      alpha = input$alpha, 
      alt_type = input$alt_type)
  })
  
  output$crit_text <- renderPrint({
    req(input$n, input$alpha, input$alt_type)
    req(df_from_sim())
    
    t_test_crit_text_func(
      n = input$n, 
      alpha = input$alpha,
      alt_type = input$alt_type)
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
 
  # BLOCK 2
  
  # observe({
  #   print(input$test_type)
  #   print(length(input$test_type))
  # })

  sim_args <- reactive({
    req(input$test_type)
    
    switch(
      input$test_type,
      "t_one_sample" = list(
        fun = t_test_one_sample,
        distribution = input$dist_type_sim,
        mu = input$mu,
        sigma = input$sigma,
        mu_0 = input$mu_0
      ),
      
      "t_two_sample" = list(
        fun = t_test_two_sample,
        distribution = input$dist_type_sim,
        mu1 = input$mu1,
        sigma1 = input$sigma1,
        mu2 = input$mu2,
        sigma2 = input$sigma2
      ),
      
      "mann_whitney" = list(
        fun = mann_whitney,
        distribution = input$dist_type_sim,
        mu1 = input$mu1,
        sigma1 = input$sigma1,
        mu2 = input$mu2,
        sigma2 = input$sigma2
      ),
      
      "brunner_munzel" = list(
        fun = brunner_munzel,
        distribution = input$dist_type_sim,
        mu1 = input$mu1,
        sigma1 = input$sigma1,
        mu2 = input$mu2,
        sigma2 = input$sigma2
      ),
      
      "contingency_tables" = {
        req(input$trial_type, input$test_method)
        
        args <- list(
          fun = generation_binary_experiment,
          design = input$trial_type,
          method = input$test_method
        )
        
        if (input$trial_type == "cohort") {
          req(input$event_probability, input$exposure_proportion)
          args$event_probability <- input$event_probability
          args$exposure_proportion <- input$exposure_proportion
          
        } else if (input$trial_type == "case_control") {
          req(input$exposure_probability, input$event_proportion)
          args$exposure_probability <- input$exposure_probability
          args$event_proportion <- input$event_proportion
          
        } else if (input$trial_type == "cross_sectional") {
          req(input$event_probability, input$exposure_probability)
          args$event_probability <- input$event_probability
          args$exposure_probability <- input$exposure_probability
          
        } else if (input$trial_type == "fisher") {
          req(input$event_proportion, input$exposure_proportion)
          args$event_proportion <- input$event_proportion
          args$exposure_proportion <- input$exposure_proportion
        }
        
        args
      }
    )
  })
  
  
  grid_output_values <- eventReactive(input$run_block2, {
    # req(input$top_block == "block2")
    req(sim_args())
    
    do.call(
      simulation_wrapper,
      c(
        sim_args(),
        list(
          parameter = input$parameter_name,
          grid = seq(input$parameter_from,
                     input$parameter_to,
                     by = input$parameter_by),
          cores = input$cores,
          seed = DEFAULT_SEED,
          n_sim = input$n_sim,
          alpha = input$alpha
        )
      )
    )
  })

  output$parameters_grid_line_plot <- renderPlot({
    parameters_grid_line_plot(grid_output_values(), 
                              "BLANK", 
                              input$alpha)
  })
   
}

shinyApp(ui = ui, server = server)