#' Расчет размера выборки для двухвыборочных исследований не-инфериорности/супериорности
#'
#' Эта функция рассчитывает размер выборки для двухгрупповых исследований,
#' сравнивающих доли или средние значения в дизайнах не-инфериорности/супериорности.
#' Использует пакет `TrialSize` для расчетов.
#'
#' @param input Список, содержащий следующие параметры:
#'   - `data_type`: Строка, либо "proportion" (доли), либо "mean" (средние)
#'   - `alpha`: Уровень значимости (ошибка I рода)
#'   - `beta`: Ошибка II рода (1 - мощность)
#'   - `k`: Коэффициент распределения (n2/n1)
#'   - `margin`: граница не-инфериорности/супериорности (при расчете средних это разница средних между группами)
#'   
#'   Для данных типа "proportion":
#'   - `p1`: Доля в группе 1
#'   - `p2`: Доля в группе 2
#'   
#'   Для данных типа "mean":
#'   - `mu1`: Среднее в группе 1
#'   - `mu2`: Среднее в группе 2
#'   - `sigma`: Стандартное отклонение (предполагается одинаковым в обеих группах)
#'   - `delta1`: Размер эффекта (при расчете пропорций это разница между пропорциями в разных группах)
#'
#' @return Список, содержащий:
#'   - `n1`: Размер выборки для группы 1 (округленный вверх)
#'   - `n2`: Размер выборки для группы 2 (округленный вверх)
#'   - `difference`: Рассчитанная разница (p1-p2 для долей, μ1-μ2 для средних)
#'   - `diff_name`: Название/описание разницы
#'   - `total_end`: Общий размер выборки для анализа конечных точек
#'   - `total_study`: Общий размер выборки для исследования (скорректированный на отсев)
#'   
#'   В случае ошибки возвращает список с элементом `error`, содержащим сообщение об ошибке.
#'
#' @section Предположения:
#'   - Уровень выбывания пациентов: 20% выбывания во время исследования и 10% во время скринига (деление на 0.8*0.9 = 0.72)
#'   - Одинаковая дисперсия для сравнения средних (когда `data_type == "mean"`)
#'   - Используются двусторонние тесты
#'
#' @examples
#' \dontrun{
#' # Пример для данных типа "proportion" (доли)
#' input_prop <- list(
#'   data_type = "proportion",
#'   alpha = 0.05,
#'   beta = 0.2,
#'   p1 = 0.8,
#'   p2 = 0.7,
#'   k = 1,
#'   margin = 0.1
#' )
#' create_result_sample_size_calc(input_prop)
#'
#' # Пример для данных типа "mean" (средние)
#' input_mean <- list(
#'   data_type = "mean",
#'   alpha = 0.05,
#'   beta = 0.2,
#'   mu1 = 10,
#'   mu2 = 9,
#'   sigma = 2,
#'   k = 1,
#'   delta1 = 1,
#'   margin = 0.5
#' )
#' create_result_sample_size_calc(input_mean)
#' }
#'
#' @export
#' @importFrom TrialSize TwoSampleProportion.NIS TwoSampleMean.NIS
library(TrialSize)

create_result_sample_size_calc <- function(input) {
  
  tryCatch({
    if (input$data_type == "proportion") {
      # Расчет разницы долей
      delta <- input$p1 - input$p2
      # Вызов функции расчета размера выборки для долей
      res <- TwoSampleProportion.NIS(
        alpha = input$alpha,
        beta = input$beta,
        p1 = input$p1,
        p2 = input$p2,
        k = input$k,
        delta = delta,
        margin = input$margin
      )
      # Расчет размеров выборок с учетом выбывания пациентов
      n1 <- ceiling(res[1])
      n2 <- ceiling(res[1] / input$k)
      total_end <- ceiling(n1 + n2)
      total_study <- ceiling(ceiling(n1 / 0.8) / 0.9) + ceiling(ceiling(n2 / 0.8) / 0.9)
      # Возврат результатов
      list(
        n1 = n1,
        n2 = n2,
        difference = delta,
        diff_name = "delta = p1 - p2",
        total_end = total_end,
        total_study = total_study
      )

    } else if (input$data_type == "mean") {
      # Расчет разницы средних
      margin1 <- input$mu1 - input$mu2
      # Вызов функции расчета размера выборки для средних
      res <- TwoSampleMean.NIS(
        alpha = input$alpha,
        beta = input$beta,
        sigma = input$sigma,
        k = input$k,
        delta = input$delta1,
        margin = margin1
      )
      
      n1 <- ceiling(res[1])
      n2 <- ceiling(res[1] / input$k)
      total_end <- ceiling(n1 + n2)
      total_study <- ceiling(ceiling(n1 / 0.8) / 0.9) + ceiling(ceiling(n2 / 0.8) / 0.9)
      # Возврат результатов
      list(
        n1 = n1,
        n2 = n2,
        difference = margin1,
        diff_name = "margin = μ1 - μ2",
        total_end = total_end,
        total_study = total_study
      )
    }
  }, error = function(e) {
    # Возврат сообщения об ошибке в случае исключения
    return(list(error = e$message))
  })
}