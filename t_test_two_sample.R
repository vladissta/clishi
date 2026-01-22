#' Оценка вероятности ошибки I рода для двухвыборочного t-теста
#'
#' Функция проводит Монте-Карло симуляцию для оценки вероятности ошибки I рода
#' двухвыборочного t-теста (Welch) при различных распределениях и их параметрах.
#'
#' @param distribution Тип распределения данных. Доступные значения:
#'   "normal" (нормальное распределение) или "exponential" (экспоненциальное распределение).
#' @param mu1 Среднее (мат. ожидание) в 1-й группе (для exponential: mu1 > 0).
#' @param sigma1 Стандартное отклонение в 1-й группе (только для normal). По умолчанию 1.
#' @param mu2 Среднее (мат. ожидание) во 2-й группе (для exponential: mu2 > 0).
#' @param sigma2 Стандартное отклонение во 2-й группе (только для normal). По умолчанию 1.
#' @param sample_size Размер выборки в каждой группе (по умолчанию 30).
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
#' генерируются две независимые выборки размера sample_size из выбранного распределения,
#' проводится Welch t-test (stats::t.test) и считается доля случаев, когда p-value < alpha.
#'
#' @examples
#' t_test_two_sample(distribution = "normal", mu1 = 0, sigma1 = 1, mu2 = 0, sigma2 = 1)
#' t_test_two_sample(distribution = "exponential", mu1 = 2, mu2 = 2)
t_test_two_sample <- function(distribution = "normal",
                              mu1 = 0,
                              sigma1 = 1,
                              mu2 = 0,
                              sigma2 = 1,
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
  checkmate::assert_number(mu1, finite = TRUE, .var.name = "mu1")
  checkmate::assert_number(mu2, finite = TRUE, .var.name = "mu2")
  
  if (distribution == "normal") {
    checkmate::assert_number(sigma1, lower = 0.001, finite = TRUE, .var.name = "sigma1")
    checkmate::assert_number(sigma2, lower = 0.001, finite = TRUE, .var.name = "sigma2")
  }
  
  if (distribution == "exponential") {
    checkmate::assert_true(mu1 > 0, .var.name = "mu1")
    checkmate::assert_true(mu2 > 0, .var.name = "mu2")
  }
  
  # 3. Проверка размера выборки
  checkmate::assert_number(sample_size, lower = 3, finite = TRUE, .var.name = "sample_size")
  
  # 4. Проверка количества симуляций
  checkmate::assert_count(n_sim, positive = TRUE, .var.name = "n_sim")
  
  # 5. Проверка уровня значимости
  checkmate::assert_number(alpha, lower = 0, upper = 1, finite = TRUE, .var.name = "alpha")
  
  # Генерация данных и проведение теста
  # ===================================
  
  df_base <- tibble::tibble(
    experiment_ID = rep(seq_len(n_sim), each = sample_size)
  )
  
  if (distribution == "normal") {
    
    df <- dplyr::bind_rows(
      df_base |>
        dplyr::mutate(
          group = "1",
          value = stats::rnorm(n_sim * sample_size, mean = mu1, sd = sigma1)
        ),
      df_base |>
        dplyr::mutate(
          group = "2",
          value = stats::rnorm(n_sim * sample_size, mean = mu2, sd = sigma2)
        )
    )
    
  } else if (distribution == "exponential") {
    
    df <- dplyr::bind_rows(
      df_base |>
        dplyr::mutate(
          group = "1",
          value = stats::rexp(n_sim * sample_size, rate = 1 / mu1)
        ),
      df_base |>
        dplyr::mutate(
          group = "2",
          value = stats::rexp(n_sim * sample_size, rate = 1 / mu2)
        )
    )
  }
  
  # Обработка данных с использованием tidyverse
  # ==========================================
  
  results <- df |>
    tidyr::nest(.by = experiment_ID) |>
    dplyr::mutate(
      test_obj = purrr::map(data, \(x) stats::t.test(value ~ group, data = x)),
      tidy_res = purrr::map(test_obj, broom::tidy)
    ) |>
    dplyr::select(-data, -test_obj) |>
    tidyr::unnest(tidy_res)
  
  # Вычисление вероятности ошибки I рода
  # ===================================
  return(sum(results$p.value < alpha) / n_sim)
}

