#libraries ----
library(shiny)
library(ggplot2)  # For creating pretty plots
library(dplyr)  # For filtering and manipulating data
library(bslib)
library(shinythemes)

#ui----
ui <- page_sidebar(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  sidebar = sidebar(
    h4("Parameters"),
    # вывод боковой панели по условию
    # Normal distr ----
    conditionalPanel(
      condition = "input.select == 'rnorm'",
      p("Normal distribution parameters:"),
      numericInput("n", label = "Number of patients", min = 1, value = 20, step = 1),
      numericInput("mean", label = "Mean", value = 0, step = 0.1),
      numericInput("sd", label = "SD", min = 0, max = 1000, value = 1, step = 0.1),
      numericInput("n_exp", label = "Number of experiments", min = 1, value = 1, step = 1),
      sliderInput("bins", label = "Bins", min = 5, max = 150, value = 30, step = 1)
    ),
    # Exponential distr ----
    conditionalPanel(
      condition = "input.select == 'rexp'",
      p("Exponential distribution parameters:"),
      numericInput("n", label = "Number of patients", min = 1, value = 20, step = 1),
      numericInput("rate", "Rate (λ)", min = 0.01, max = 1000, value = 1, step = 0.01),
      numericInput("n_exp", label = "Number of experiments", min = 1, value = 1, step = 1),
      sliderInput("bins", label = "Bins", min = 5, max = 150, value = 30, step = 1)
    ),
    # Uniform distr ----
    conditionalPanel(
      condition = "input.select == 'runif'",
      p("Uniform distribution parameters:"),
      numericInput("n", label = "Number of patients", min = 1, value = 20, step = 1),
      numericInput("min", label = "Min", value = 0, step = 0.1),
      numericInput("max", label = "Max", value = 1, step = 0.1),
      numericInput("n_exp", label = "Number of experiments", min = 1, value = 1, step = 1),
      sliderInput("bins", label = "Bins", min = 5, max = 150, value = 30, step = 1)
    )
  ),
  
  #main content----
  h2("Let's explore distributions"), 
  radioButtons("select",
               label = "Select your distribution:",
               choices = c("Normal" = "rnorm", "Exponential" = "rexp", "Uniform" = "runif"),
               selected = "rnorm",
               inline=T),
  
  
  conditionalPanel(
    condition = "input.n_exp == 1",
    plotOutput("one_sample_plot", height = "480px")
  ),
  
  conditionalPanel(
    condition = "input.n_exp > 1",
    plotOutput("main_plot", height = "480px")
  )

)

#server----
server <- function(input, output) {
  #one_sample_plot ----
  output$one_sample_plot <- renderPlot({
    if(input$select == "rnorm" & input$n_exp == 1) {
      req(input$sd >= 0, input$n >= 0, input$n_exp == 1)
      #params <- list(mean = input$mean, sd = input$sd, n = input$n, bins = input$bins, n_exp = input$n_exp)
      df <- data.frame(
        n_exp = rep(1:input$n_exp, each = input$n),
        ID = rep(1:input$n, input$n_exp),
        result = rnorm(input$n_exp*input$n, mean = input$mean, sd = input$sd))
    } else if(input$select == "rexp" & input$n_exp == 1) {
      req(input$n >= 0, input$n_exp == 1, input$rate > 0)
      df <- data.frame(
        n_exp = rep(1, input$n),
        ID = rep(1:input$n),
        result = rexp(input$n, rate = input$rate)
      )
    } else if(input$select == "runif" & input$n_exp == 1) {
      req(input$min < input$max)
      df <- data.frame(
        n_exp = rep(1, input$n),
        ID = rep(1:input$n),
        result = runif(input$n, min = input$min, max = input$max))
    }
    
    
    ggplot(df)+
      geom_histogram(aes(x = result), fill = "skyblue", color = "white", bins = input$bins)+
      labs(
        title = paste(input$select, "distribution for ONE SAMPLE"),
        x = "Result",
        y = "Number of patients"
      )+
      theme_minimal()
    
    # data <-  function(select, params){
    #   if(select == "rnorm" & params$n_exp == 1) {
    #     rnorm(params$n, params$mean, params$sd)
    #   }
    # }
    # df <- data.frame(data)
    
  })
  #main plot----
  output$main_plot <- renderPlot({
    if(input$select == "rnorm" & input$n_exp > 1) {
      df <- data.frame(
        n_exp = rep(1:input$n_exp, each = input$n),
        ID = rep(1:input$n, input$n_exp),
        result = rnorm(input$n_exp*input$n, mean = input$mean, sd = input$sd))
      df_mean <- df %>% 
        group_by(n_exp) %>% 
        summarise(mean = mean(result)) %>% 
        mutate(normalized_mean = (mean - input$mean)*sqrt(input$n)/input$sd)
    } else if(input$select == "rexp" & input$n_exp > 1) {
      df <- data.frame(
        n_exp = rep(1:input$n_exp, each = input$n),
        ID = rep(1:input$n, input$n_exp),
        result = rexp(input$n_exp*input$n, rate = input$rate))
      df_mean <- df %>% 
        group_by(n_exp) %>% 
        summarise(mean = mean(result)) %>% 
        mutate(normalized_mean = (mean - 1/input$rate)*sqrt(input$n)/sqrt(1/input$rate^2))
    } else if(input$select == "runif" & input$n_exp > 1) {
      df <- data.frame(
        n_exp = rep(1:input$n_exp, each = input$n),
        ID = rep(1:input$n, input$n_exp),
        result = runif(input$n_exp*input$n, min = input$min, max = input$max))
      df_mean <- df %>% 
        group_by(n_exp) %>% 
        summarise(mean = mean(result)) %>% 
        mutate(normalized_mean = (mean - (input$min + input$max)/2)*sqrt(input$n)/sqrt(((input$max - input$min)^2)/12))
    }
    
    ggplot(df_mean, aes(x = normalized_mean)) +
      geom_histogram(aes(y= after_stat(density)), fill = "skyblue", color = "black", bins = input$bins) +
      labs(
        title = paste("Sample mean distribution (normalized mean, error mean) for", input$select, "distribution"),
        x = "normalized mean"
      )+
      stat_function(fun = dnorm, args = list(mean = 0, sd = 1), color = "red")+
      theme_minimal()
  })
  
}

# Run the application----
shinyApp(ui = ui, server = server)
