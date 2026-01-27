create_tooltip <- function(label, tooltip_text) {
  HTML(sprintf("<span data-toggle='tooltip' title='%s'>%s</span>", 
               tooltip_text, label))
}

create_block_card <- function(block_id, title, description) {
  tags$div(
    class = "clishi-card clishi-card-click",
    onclick = sprintf("Shiny.setInputValue('go_%s', Math.random(), {priority: 'event'})", block_id),
    h3(title),
    p(description)
  )
}

create_sidebar_brand <- function() {
  tags$div(
    class = "clishi-sidebar-brand",
    tags$div(class = "clishi-sidebar-brand-title", "CliShi"),
    tags$div(
      class = "clishi-sidebar-brand-subtitle",
      "Interactive Clinical Research Simulator - Shiny Application"
    ),
    tags$div(class = "clishi-sidebar-brand-version", "v 1.0 (beta)")
  )
}

create_sidebar_footer_buttons <- function() {
  tags$div(
    class = "clishi-sticky-footer",
    
    conditionalPanel(
      condition = "input.top_block == 'block1'",
      actionButton("run_block1", "Смоделировать выборки", 
                   class = "btn-primary btn-block"),
      tags$div(style = "height:8px;")
    ),
    
    conditionalPanel(
      condition = "input.top_block == 'block2'",
      actionButton("run_block2", "Смоделировать", 
                   class = "btn-primary btn-block"),
      tags$div(style = "height:8px;")
    ),
    
    conditionalPanel(
      condition = "input.top_block == 'block3'",
      actionButton("run_block3", "Рассчитать выборку", 
                   class = "btn-primary btn-block"),
      tags$div(style = "height:8px;")
    ),
    
    conditionalPanel(
      condition = "input.top_block && input.top_block != 'home'",
      actionButton("go_home", "\u2190 Назад к выбору блоков", 
                   class = "btn-default btn-block")
    )
  )
}

create_fixed_logo <- function() {
  tags$div(
    class = "clishi-fixed-logo",
    tags$img(src = "clishi_hex_transparent.png", alt = "CliShi")
  )
}

create_home_page <- function() {
  fluidRow(
    box(
      width = 12,
      title = NULL,
      solidHeader = FALSE,
      
      tags$div(
        style = "text-align:center; margin-bottom:20px;",
        img(src = "clishi_logo.png", height = "180px", alt = "CliShi logo")
      ),
      
      create_block_card("block1", "Моделирование и проверка гипотез",
                        "Симуляции, распределения выборочных средних, ДИ, p-value, критические области."),
      
      create_block_card("block2", "Оценка зависимости результатов теста от величины параметра",
                        "Сравнение тестов при изменении эффекта/параметров (планируется)."),
      
      create_block_card("block3", "Расчет выборки классическими методами",
                        "Классические формулы для n, α, power, effect (планируется)."),
      
      create_block_card("block4", "Расчет выборки методом имитационного моделирования",
                        "Симуляционный подбор n под заданную мощность (планируется).")
    )
  )
}
