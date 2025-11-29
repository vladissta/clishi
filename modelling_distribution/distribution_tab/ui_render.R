render_distribution_panel <- function(distribution_type) {
  #distribution_type: нормальное, экспоненциальное, равномерное 
  if(distribution_type == "нормальное") {
    return(renderUI({
      # normal distribution
        wellPanel(
          numericInput(
            "mean",
            "Среднее",
            value = 0
          ),
          numericInput(
            "sd",
            "Дисперсия",
            value = 1,
            min = 0
          ),
          numericInput(
            "sample_size",
            "Размер выборки",
            value = 10,
            min = 1
          ),
          numericInput(
            "generation_size",
            "Количество генераций",
            value = 20,
            min = 1
          )
        )
      })
    )
  } else if (distribution_type == "экспоненциальное") {
    return(renderUI({
        #экспоненциальное распределение
        wellPanel(
          numericInput(
            "rate",
            "Интенсивность",
            value = 1.5,
            min = 0
          ),
          numericInput(
            "sample_size",
            "Размер выборки",
            value = 20,
            min = 1
          ),
          numericInput(
            "generation_size",
            "Количество генераций",
            value = 300,
            min = 1
          )
        )
      })
    )
  } else {
    return(renderUI({
        #равномерное распределение
        wellPanel(
          numericInput(
            "min",
            "Минимальное значение",
            value = 0
          ),
          numericInput(
            "max",
            "Максимальное значение",
            value = 1
          ),
          numericInput(
            "sample_size",
            "Размер выборки",
            value = 10,
            min = 1
          ),
          numericInput(
            "generation_size",
            "Количество генераций",
            value = 20, 
            min = 1
          )
        )
      })
    )
  }
}


get_distribution_params <- function(distribution_type, input) {
  if (distribution_type == "нормальное") {
    return(c(
      mean = input$mean, 
      sd = input$sd, 
      sample_size = input$sample_size, 
      generation_size = input$generation_size
    ))
  } else if (distribution_type == "экспоненциальное") {
    return(c(
      rate = input$rate, 
      sample_size = input$sample_size, 
      generation_size = input$generation_size
    ))
  } else {
    return(c(
      min = input$min,
      max = input$max,
      sample_size = input$sample_size, 
      generation_size = input$generation_size
    ))
  }
}