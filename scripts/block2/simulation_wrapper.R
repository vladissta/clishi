#' Параллельный прогон симуляции по сетке значений параметра
#'
#' Функция запускает пользовательскую симуляционную функцию `fun` для каждого значения
#' из `grid`, подставляя это значение в аргумент с именем `parameter`. Возвращает
#' именованный числовой вектор результатов.
#'
#' Параллелизация выполняется через `furrr` (backend `future.callr`).
#'
#' @param fun Функция-симулятор. Должна принимать аргумент с именем, указанным в
#'   `parameter`, а также любые дополнительные аргументы через `...`.
#'   Должна возвращать одно числовое значение.
#' @param parameter Строка — имя параметра, который будет изменяться (например `"alpha"`).
#' @param grid Вектор значений параметра, по которым выполняется симуляция (обычно numeric).
#' @param cores Количество воркеров для параллельных вычислений. По умолчанию
#'   `availableCores() - 1`.
#' @param seed Seed для воспроизводимости в `furrr_options(seed = seed)`.
#' @param ... Дополнительные аргументы, которые будут переданы в `fun`.
#'
#' @return Именованный числовой вектор длины `length(grid)`:
#' \itemize{
#'   \item имена — значения `grid`,
#'   \item значения — результаты вызова `fun` при соответствующем значении параметра.
#' }
#'
#' @details
#' Внутри устанавливается план `plan(future.callr::callr, workers = cores)`. 
#' Использование `callr` обеспечивает запуск каждого воркера в отдельной чистой 
#' сессии R, что повышает стабильность при работе в Shiny. 
#' Для каждого `x` из `grid` формируется список аргументов:
#' `c(setNames(list(x), parameter), list(...))`, затем выполняется `do.call(fun, args)`.
#'
#'
#' @importFrom furrr future_map_dbl furrr_options
#' @importFrom future plan
#' @importFrom future.callr callr
#' @importFrom parallelly availableCores
#' @importFrom stats setNames
#' @export

simulation_wrapper <- function(fun,
                               parameter, grid,
                               cores = availableCores() - 1,
                               seed, ...) {
  plan(future.callr::callr, workers = cores)
  
  output_vector <-
    furrr::future_map_dbl(
      grid,
      function(x) {
        args <- c(setNames(list(x), parameter), list(...))
        do.call(fun, args)
      },
      .options = furrr_options(seed = seed)
    )
  
  names(output_vector) <- grid
  return(output_vector)
}
