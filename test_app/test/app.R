library(shiny)
library(bslib)
library(ggplot2)

# helper
generate_data <- function(dist, params, n = 2000) {
  if (dist == "normal") {
    rnorm(n, mean = params$mean, sd = params$sd)
  } else if (dist == "exponential") {
    rexp(n, rate = params$rate)
  } else numeric(0)
}

# default params for preview
default_params <- list(
  normal = list(mean = 0, sd = 2, bins = 30),
  exponential = list(rate = 1, bins = 30)
)

ui <- page_sidebar(
  theme = bs_theme(version = 5),
  
  # SIDEBAR: все inputs всегда присутствуют, но показываем части через conditionalPanel
  sidebar = sidebar(
    h4("Parameters"),
    # Normal panel (client-side conditional)
    conditionalPanel(
      condition = "input.dist == 'normal'",
      p("Normal distribution parameters:"),
      sliderInput("mean", "Mean:", min = -10, max = 10, value = default_params$normal$mean, step = 0.1),
      sliderInput("sd", "SD:", min = 0.1, max = 10, value = default_params$normal$sd, step = 0.1),
      sliderInput("bins_n", "Bins:", min = 5, max = 200, value = default_params$normal$bins, step = 1)
    ),
    # Exponential panel
    conditionalPanel(
      condition = "input.dist == 'exponential'",
      p("Exponential distribution parameters:"),
      sliderInput("rate", "Rate (λ):", min = 0.05, max = 5, value = default_params$exponential$rate, step = 0.05),
      sliderInput("bins_e", "Bins:", min = 5, max = 200, value = default_params$exponential$bins, step = 1)
    )
  ),
  
  # MAIN content
  h2("Distribution Explorer"),
  
  # radioButtons под заголовком
  div(
    id = "dist_group",
    radioButtons(
      inputId = "dist",
      label = NULL,
      choices = c("Normal" = "normal", "Exponential" = "exponential"),
      selected = "normal",
      inline = TRUE
    )
  ),
  
  # CSS для hover preview box
  tags$head(
    tags$style(HTML("
      #dist_group { position: relative; display: inline-block; }
      #hover_preview_box {
        display: none;
        position: absolute;
        top: 36px;
        left: 0px;
        width: 360px;
        height: 240px;
        background: white;
        border: 1px solid #ddd;
        box-shadow: 0 6px 18px rgba(0,0,0,0.12);
        padding: 6px;
        z-index: 2000;
        border-radius: 6px;
      }
      #dist_group.show-preview #hover_preview_box { display: block; }
    "))
  ),
  
  # preview plot (в DOM всегда присутствует, рендерится только при hover)
  div(
    id = "hover_preview_box",
    plotOutput("hover_plot", height = "220px")
  ),
  
  br(), br(),
  plotOutput("main_plot", height = "480px")
)

server <- function(input, output, session) {
  
  # ---------- Hover preview plot (ggplot2) ----------
  # input$radio_hover устанавливается из JS (значение 'normal' или 'exponential' или NULL)
  output$hover_plot <- renderPlot({
    hv <- input$radio_hover
    if (is.null(hv) || hv == "") return(NULL)
    
    p <- if (hv == "normal") default_params$normal else default_params$exponential
    d <- generate_data(hv, p, n = 3000)
    df <- data.frame(x = d)
    
    # используем соответствующее число бинов
    bins <- p$bins
    
    ggplot(df, aes(x = x)) +
      geom_histogram(bins = bins, fill = "steelblue", color = "white") +
      theme_minimal() +
      labs(title = paste0("Preview (", hv, ")"))
  })
  
  # ---------- Main plot ----------
  output$main_plot <- renderPlot({
    req(input$dist)  # обязательно
    
    if (input$dist == "normal") {
      # inputs mean/sd/bins_n всегда существуют (мы их создали в sidebar), но когда UI еще не построен --
      # req() защитит, если они вдруг NULL
      req(input$mean, input$sd, input$bins_n)
      params <- list(mean = input$mean, sd = input$sd, bins = input$bins_n)
    } else {
      req(input$rate, input$bins_e)
      params <- list(rate = input$rate, bins = input$bins_e)
    }
    
    d <- generate_data(input$dist, params, n = 8000)
    df <- data.frame(x = d)
    
    ggplot(df, aes(x = x)) +
      geom_histogram(bins = params$bins, fill = "darkorange", color = "white") +
      theme_minimal() +
      labs(title = paste0("Main plot: ", input$dist))
  })
  
  # Инициализация hover-handlers на клиенте
  session$sendCustomMessage("initHover", list())
}

# JS: отслеживаем mouseenter/leave только внутри контейнера #dist_group
jsCode <- "
Shiny.addCustomMessageHandler('initHover', function() {
  var container = document.getElementById('dist_group');
  if (!container) return;

  var labels = container.querySelectorAll('label.btn');

  labels.forEach(function(lbl) {
    var input = lbl.querySelector('input[type=\"radio\"]');
    if (!input) return;

    lbl.addEventListener('mouseenter', function() {
      Shiny.setInputValue('radio_hover', input.value, {priority: 'event'});
      container.classList.add('show-preview');
    });

    lbl.addEventListener('mouseleave', function() {
      Shiny.setInputValue('radio_hover', null, {priority: 'event'});
      container.classList.remove('show-preview');
    });

    lbl.addEventListener('click', function() {
      Shiny.setInputValue('radio_hover', null, {priority: 'event'});
      container.classList.remove('show-preview');
    });
  });
});
"

shinyApp(
  ui = tagList(tags$script(HTML(jsCode)), ui),
  server = server
)