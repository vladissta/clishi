sample_size_results_text <- function(result_sample_size_calc, input){
  
  if (input$data_type == "proportion") {
    if (input$hypothesis == "superiority") {
      paste0(
        "Граница превосходства \u03B4 = ", input$margin, "\n",
        "Вероятность ошибки I рода: ", input$alpha, ", мощность: ", (1-input$beta)*100, "%", "\n",
        "Таким образом, получено, что минимально необходимый объем", "\n",
        "выборки для проведения исследования равен ", result_sample_size_calc$total_end, "\n", 
        "(", result_sample_size_calc$n1, " в группу 1 (терапии), ", result_sample_size_calc$n2, " в группу 2 (контроль)", ")", "\n",
        "С учетом 20% выбываемости во время исследования и 10% выбываемости", "\n", 
        "во время скрининга, объем выборки составит ", result_sample_size_calc$total_study, " пациентов", "\n",
        "(", ceiling(ceiling(result_sample_size_calc$n1 / 0.8) / 0.9), " в группу 1, ", ceiling(ceiling(result_sample_size_calc$n2 / 0.8) / 0.9), " в группу 2)", "\n",
        "Расчет произведен с помощью приложения CliShi")
      

    } else {
      paste0(
        "Граница не меньшей эффективности \u03B4 = ", input$margin, "\n",
        "Вероятность ошибки I рода: ", input$alpha, ", мощность: ", (1-input$beta)*100, "%", "\n",
        "Таким образом, получено, что минимально необходимый объем", "\n",
        "выборки для проведения исследования равен ", result_sample_size_calc$total_end, "\n", 
        "(", result_sample_size_calc$n1, " в группу 1 (терапии), ", result_sample_size_calc$n2, " в группу 2 (контроль)", ")", "\n",
        "С учетом 20% выбываемости во время исследования и 10% выбываемости", "\n", 
        "во время скрининга, объем выборки составит ", result_sample_size_calc$total_study, " пациентов", "\n",
        "(", ceiling(ceiling(result_sample_size_calc$n1 / 0.8) / 0.9), " в группу 1, ", ceiling(ceiling(result_sample_size_calc$n2 / 0.8) / 0.9), " в группу 2)", "\n",
        "Расчет произведен с помощью приложения CliShi")
    }}
  else {
      if (input$hypothesis == "superiority") {
        paste0(
          "Граница превосходства \u03B4 = ", input$delta1, "\n",
          "Стандартное отклонение составляет приблизительно \u03C3 = ", input$sigma, "\n",
          "Таким образом, получено, что минимально необходимый объем", "\n",
          "выборки для проведения исследования равен ", result_sample_size_calc$total_end, "\n", 
          "(", result_sample_size_calc$n1, " в группу 1 (терапии), ", result_sample_size_calc$n2, " в группу 2 (контроль)", ")", "\n",
          "С учетом 20% выбываемости во время исследования и 10% выбываемости", "\n", 
          "во время скрининга, объем выборки составит ", result_sample_size_calc$total_study, " пациентов", "\n",
          "(", ceiling(ceiling(result_sample_size_calc$n1 / 0.8) / 0.9), " в группу 1, ", ceiling(ceiling(result_sample_size_calc$n2 / 0.8) / 0.9), " в группу 2)", "\n",
          "Расчет произведен с помощью приложения CliShi")

        
      } else {
        paste0(
          "Граница не меньшей эффективности \u03B4 = ", input$delta1, "\n",
          "Стандартное отклонение составляет приблизительно \u03C3 = ", input$sigma, "\n",
          "Таким образом, получено, что минимально необходимый объем", "\n",
          "выборки для проведения исследования равен ", result_sample_size_calc$total_end, "\n", 
          "(", result_sample_size_calc$n1, " в группу 1 (терапии), ", result_sample_size_calc$n2, " в группу 2 (контроль)", ")", "\n",
          "С учетом 20% выбываемости во время исследования и 10% выбываемости", "\n", 
          "во время скрининга, объем выборки составит ", result_sample_size_calc$total_study, " пациентов", "\n",
          "(", ceiling(ceiling(result_sample_size_calc$n1 / 0.8) / 0.9), " в группу 1, ", ceiling(ceiling(result_sample_size_calc$n2 / 0.8) / 0.9), " в группу 2)", "\n",
          "Расчет произведен с помощью приложения CliShi")
    }
    
  }
 
}