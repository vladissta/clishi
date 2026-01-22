#' Оценка вероятности ошибки I рода для одновыборочного t-теста
#'
#' Функция проводит Монте-Карло симуляцию для оценки вероятности ошибки I рода
#' одновыборочного t-теста при различных распределениях и их параметрах.
#'
#' @param distribution Тип распределения данных. Доступные значения:
#'   "normal" (нормальное распределение) или "exponential" (экспоненциальное распределение).
#' @param mu Среднее (мат. ожидание) генеральной совокупности (для обеих моделей).
#'   Для экспоненциального распределения mu > 0.
#' @param sigma Стандартное отклонение (только для normal). По умолчанию 1.
#' @param mu_0 Значение математического ожидания в нулевой гипотезе.
#' @param sample_size Размер выборки в каждом эксперименте (по умолчанию 30).
#' @param n_sim Количество симуляций (по умолчанию 1000).
#' @param alpha Уровень значимости (по умолчанию 0.05).
#'
#' @import tidyr
#' @import dplyr
#' @importFrom purrr map
#' @importFrom checkmate assert_choice assert_number assert_count assert_true
#'
#' @return Оценка вероятности ошибки I рода: доля экспериментов, где p-value < alpha.
#'
#' @details
#' Функция генерирует n_sim независимых экспериментов. В каждом эксперименте
#' генерируется выборка размера sample_size из выбранного распределения,
#' проводится одновыборочный t-тест относительно mu_0 и считается доля случаев,
#' когда p-value < alpha.
#'
#' @examples
#' t_test_one_sample(distribution = "normal", mu = 0, sigma = 1, mu_0 = 0)
#' t_test_one_sample(distribution = "exponential", mu = 2, mu_0 = 2)
t_test_one_sample <- function(distribution = "normal",
                              mu = 0,
                              sigma = 1,
                              mu_0 = 0,
                              sample_size = 30,
                              n_sim = 1000,
                              alpha = 0.05) {
  
  # Проверка аргументов с помощью checkmate
  # ======================================
  
  # 1. Проверка distribution
  checkmate::assert_choice(
    distribution,
    choices = c("normal", "exponential"),
    .var.name = "distribution"
  )
  
  # 2. Проверка параметров распределения
  checkmate::assert_number(mu, finite = TRUE, .var.name = "mu")
  
  if (distribution == "normal") {
    checkmate::assert_number(
      sigma,
      lower = 0.001,
      finite = TRUE,
      .var.name = "sigma"
    )
  }
  
  if (distribution == "exponential") {
    # Для экспоненциального mean > 0 (rate = 1/mean)
    checkmate::assert_true(mu > 0, .var.name = "mu")
  }
  
  # 3. Проверка mu_0
  checkmate::assert_number(mu_0, finite = TRUE, .var.name = "mu_0")
  
  # 4. Проверка размера выборки
  checkmate::assert_number(sample_size, lower = 3, finite = TRUE, .var.name = "sample_size")
  
  # 5. Проверка количества симуляций
  checkmate::assert_count(n_sim, positive = TRUE, .var.name = "n_sim")
  
  # 6. Проверка уровня значимости
  checkmate::assert_number(alpha, lower = 0, upper = 1, finite = TRUE, .var.name = "alpha")
  
  # Генерация данных и проведение теста
  # ===================================
  
  df <- tibble::tibble(
    experiment_ID = rep(seq_len(n_sim), each = sample_size)
  )
  
  if (distribution == "normal") {
    df <- df |>
      dplyr::mutate(
        value = stats::rnorm(n_sim * sample_size, mean = mu, sd = sigma)
      )
  } else if (distribution == "exponential") {
    df <- df |>
      dplyr::mutate(
        value = stats::rexp(n_sim * sample_size, rate = 1 / mu)
      )
  }
  
  # Обработка данных с использованием tidyverse
  # ==========================================
  
  results <- df |>
    tidyr::nest(.by = experiment_ID) |>
    dplyr::mutate(
      test_obj = purrr::map(data, \(x) stats::t.test(x$value, mu = mu_0)),
      tidy_res = purrr::map(test_obj, broom::tidy)
    ) |>
    dplyr::select(-data, -test_obj) |>
    tidyr::unnest(tidy_res)
  
  # Вычисление вероятности ошибки I рода
  # ===================================
  return(sum(results$p.value < alpha) / n_sim)
}
