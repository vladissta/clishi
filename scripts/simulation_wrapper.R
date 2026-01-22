#' Параллельный прогон симуляции по сетке значений параметра
#'
#' Функция запускает пользовательскую симуляционную функцию `fun` для каждого значения
#' из `grid`, подставляя это значение в аргумент с именем `parameter`. Возвращает
#' именованный числовой вектор результатов.
#'
#' Параллелизация выполняется через `furrr` (backend `future`).
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
#' Внутри устанавливается план `plan(multisession, workers = cores)`. Для каждого `x`
#' из `grid` формируется список аргументов:
#' `c(setNames(list(x), parameter), list(...))`, затем выполняется `do.call(fun, args)`.
#'
#' Так как используется `future_map_dbl()`, функция `fun` обязана возвращать ровно одно
#' числовое значение на каждой итерации.
#'
#' @examples
#' sim_fun <- function(mu, n = 30) mean(rnorm(n, mean = mu, sd = 1))
#'
#' res <- simulation_wrapper(
#'   fun = sim_fun,
#'   parameter = "mu",
#'   grid = seq(-1, 1, by = 0.5),
#'   cores = 2,
#'   seed = 123,
#'   n = 50
#' )
#' res
#'
#' @importFrom furrr future_map_dbl furrr_options
#' @importFrom future plan multisession
#' @importFrom parallelly availableCores
#' @export

simulation_wrapper <- function(fun,
                               parameter, grid,
                               cores = availableCores() - 1,
                               seed, ...) {
  
  plan(multisession, workers = cores)
  
  output_vector <-
    furrr::future_map_dbl(
      grid,
      function(x) {
        args <- c(setNames(list(x), parameter), list(...))
        do.call(fun, args)
      },
      .options = furrr::furrr_options(seed = seed)
    )
  
  names(output_vector) <- grid
  print(output_vector)
  return(output_vector)
}
