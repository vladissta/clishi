#' Оценка вероятности ошибки I рода для двухвыборочного t-теста
#'
#' Функция проводит Монте-Карло симуляцию двухвыборочного t-теста
#' для двух независимых выборок одинакового размера.
#' В каждом эксперименте генерируются данные для двух групп,
#' проводится t-тест и вычисляется доля случаев,
#' когда p-value оказывается меньше уровня значимости alpha.
#'
#' При заданных параметрах распределений, соответствующих нулевой гипотезе
#' (mu_1 = mu_2), возвращаемое значение интерпретируется как оценка
#' вероятности ошибки I рода.
#'
#' @param distribution Тип распределения генеральной совокупности.
#'   Возможные значения: "normal" или "exponential".
#' @param param_1 Список параметров распределения для первой группы.
#'   Для нормального распределения: list(mu = ..., sigma = ...).
#'   Для экспоненциального распределения: list(mu = ...).
#' @param param_2 Список параметров распределения для второй группы.
#'   Аналогично param_1.
#' @param sample_size Размер выборки в каждой группе.
#' @param n_sim Количество симуляций (независимых экспериментов).
#' @param alpha Уровень значимости теста.
#'
#' @return Числовое значение — оценка вероятности ошибки I рода,
#'   вычисленная как доля экспериментов, в которых p-value < alpha.
#'
#' @details
#' Используется стандартная реализация функции stats::t.test
#' (Welch t-test, var.equal = FALSE).
#'
#' @examples
#' # Оценка ошибки I рода для нормального распределения
#' t_test_two_sample(
#'   distribution = "normal",
#'   param_1 = list(mu = 0, sigma = 1),
#'   param_2 = list(mu = 0, sigma = 1)
#' )


t_test_two_sample <- function(distribution = "normal",
                              param_1 = list(mu = 0, sigma = 1),
                              param_2 = list(mu = 0, sigma = 1),
                              sample_size = 30,
                              n_sim = 1000,
                              alpha = 0.05) {
  
  df_base <- tibble::tibble(
    experiment_ID = rep(seq_len(n_sim), each = sample_size)
  )
  
  if (distribution == "normal") {
    
    df <- dplyr::bind_rows(
      df_base |>
        dplyr::mutate(
          group = "1",
          value = stats::rnorm(n_sim * sample_size, mean = param_1$mu, sd = param_1$sigma)
        ),
      df_base |>
        dplyr::mutate(
          group = "2",
          value = stats::rnorm(n_sim * sample_size, mean = param_2$mu, sd = param_2$sigma)
        )
    )
    
  } else if (distribution == "exponential") {
    
    df <- dplyr::bind_rows(
      df_base |>
        dplyr::mutate(
          group = "1",
          value = stats::rexp(n_sim * sample_size, rate = 1 / param_1$mu)
        ),
      df_base |>
        dplyr::mutate(
          group = "2",
          value = stats::rexp(n_sim * sample_size, rate = 1 / param_2$mu)
        )
    )
    
  } else {
    stop("distribution must be 'normal' or 'exponential'")
  }
  
  results <- df |>
    dplyr::group_by(experiment_ID) |>
    tidyr::nest() |>
    dplyr::mutate(
      test_obj = purrr::map(data, \(x) stats::t.test(value ~ group, data = x)),
      tidy_res = purrr::map(test_obj, broom::tidy)
    ) |>
    dplyr::select(-data, -test_obj) |>
    tidyr::unnest(tidy_res)
  
  mean(results$p.value < alpha)
}


