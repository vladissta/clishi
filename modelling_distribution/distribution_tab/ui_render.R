# Место для защиты от дурака
# validate_integer_input <- function(number, min = 1, max = 10000000) {
#   number <- as.integer(number)
#   if (is.na(number) | number < min | number > max) {
#       showModal(
#         modalDialog(
#           title = "Ошибка",
#           "Вы ввели некорректный параметр",
#           easyClose = TRUE,
#           footer = modalButton("Заркыть")
#         )
#       )
#       return()
#   }
#   return(number)
# }
# 
# 
# validate_numeric_input <- function(number, min = 0, max = 10000000) {
#   number <- as.numeric(number)
#   if (is.na(number) | number <= min | number > max) {
#     showModal(
#       modalDialog(
#         title = "Ошибка",
#         "Вы ввели некорректный параметр",
#         easyClose = TRUE,
#         footer = modalButton("Заркыть")
#       )
#     )
#     return()
#   }
#   return(number)
# }


render_distribution_panel <- function(distribution_type) {
  #distribution_type: нормальное, экспоненциальное, равномерное 
  if(distribution_type == "нормальное") {
    return(renderUI({
      # нормальное распределение
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
            min = 1
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

# Место для защиты от дурака
# validate_inputs <- function(input) {
#   sample_size <- validate_integer_input(input$sample_size)
#   generation_size <- validate_integer_input(input$generation_size)
#   rate <- validate_numeric_input(input$rate)
#   sd <- validate_numeric_input(input$sd)
#   mean <- validate_numeric_input(input$mean, min = -10000000)
#   min <- validate_numeric_input(input$min, min = -10000000)
#   mix <- validate_numeric_input(input$max, min = -10000000)
#   if (is.null(sample_size) | is.null(generation_size) | is.null(rate) | is.null(sd) | is.null(mean) | is.null(min) | is.null(max)) return(FALSE)
#   else return(TRUE)
# }


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
  