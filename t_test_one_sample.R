#' Оценка вероятности ошибки I рода для одновыборочного t-теста
#'
#' Функция проводит Монте-Карло симуляцию одновыборочного t-теста.
#' Для каждого из n_sim экспериментов генерируется выборка заданного размера,
#' проводится t-тест и вычисляется доля случаев,
#' когда p-value оказывается меньше уровня значимости alpha.
#'
#' При заданных параметрах распределения, соответствующих нулевой гипотезе
#' (mu = mu_0), возвращаемое значение интерпретируется как оценка
#' вероятности ошибки I рода.
#'
#' @param distribution Тип распределения генеральной совокупности.
#'   Возможные значения: "normal" или "exponential".
#' @param param Список параметров распределения.
#'   Для нормального распределения: list(mu = ..., sigma = ...).
#'   Для экспоненциального распределения: list(mu = ...).
#' @param mu_0 Значение математического ожидания в нулевой гипотезе.
#' @param sample_size Размер выборки в каждом эксперименте.
#' @param n_sim Количество симуляций (независимых экспериментов).
#' @param alpha Уровень значимости теста.
#'
#' @return Числовое значение — оценка вероятности ошибки I рода,
#'   вычисленная как доля экспериментов, в которых p-value < alpha.
#'
#' @examples
#' # Оценка ошибки I рода для нормального распределения
#' t_test_one_sample(
#'   distribution = "normal",
#'   param = list(mu = 0, sigma = 1),
#'   mu_0 = 0
#' )


t_test_one_sample <- function(distribution = "normal",
                              param = list(mu = 0, sigma = 1),
                              mu_0 = 0,
                              sample_size = 30,
                              n_sim = 1000,
                              alpha = 0.05) {
  
  df <- tibble::tibble(
    experiment_ID = rep(seq_len(n_sim), each = sample_size)
  )
  
  if (distribution == "normal") {
    df <- df |>
      dplyr::mutate(
        value = stats::rnorm(n_sim * sample_size, mean = param$mu, sd = param$sigma)
      )
    
  } else if (distribution == "exponential") {
    df <- df |>
      dplyr::mutate(
        value = stats::rexp(n_sim * sample_size, rate = 1 / param$mu)
      )
    
  } else {
    stop("distribution must be 'normal' or 'exponential'")
  }
  
  results <- df |>
    dplyr::group_by(experiment_ID) |>
    tidyr::nest() |>
    dplyr::mutate(
      test_obj = purrr::map(data, \(x) stats::t.test(x$value, mu = mu_0)),
      tidy_res = purrr::map(test_obj, broom::tidy)
    ) |>
    dplyr::select(-data, -test_obj) |>
    tidyr::unnest(tidy_res)
  
  mean(results$p.value < alpha)
}

