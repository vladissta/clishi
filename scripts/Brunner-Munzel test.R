#' Оценка вероятности ошибки I рода для теста Бруннер-Мюнцеля при различных распределениях и их параметрах
#'
#' Функция проводит симуляцию для оценки вероятности ошибки I рода
#' непараметрического критерия Бруннер-Мюнцеля при различных параметрах
#' распределений и размерах выборок.
#'
#' @param distribution Тип распределения данных. Доступные значения: 
#'   "normal" (нормальное распределение) или "exponential" (экспоненциальное распределение).
#' @param param_1 Список параметров для первой группы. Для нормального распределения:
#'   list(mu = среднее, sigma = стандартное отклонение). Для экспоненциального:
#'   list(mu = математическое ожидание).
#' @param param_2 Список параметров для второй группы. Аналогично param_1.
#' @param sample_size Размер выборки в каждой группе (по умолчанию 30).
#' @param n_sim Количество симуляций (по умолчанию 1000).
#' @param alpha Уровень значимости (по умолчанию 0.05).
#'
#' @return Вероятность отклонения нулевой гипотезы (вероятность ошибки I рода).
#'
#' @details
#' Функция создает симулированные данные для двух групп, применяет тест Бруннера-Мюнцеля
#' для каждой симуляции и вычисляет долю случаев,
#' когда p-value меньше заданного уровня значимости alpha.
#'
#' @examples
#' # Оценка вероятности ошибки I рода для нормального распределения
#' brunner_munzel(distribution = "normal", 
#'              param_1 = list(mu = 20, sigma = 2), 
#'              param_2 = list(mu = 20, sigma = 2))
#'
#' # Оценка вероятности ошибки I рода для экспоненциального распределения
#' brunner_munzel(distribution = "exponential", 
#'              param_1 = list(mu = 20), 
#'              param_2 = list(mu = 25), 
#'              sample_size = 50, 
#'              n_sim = 2000)
#'

library(tidyverse)
library(brunnermunzel)

brunner_munzel <- function(distribution = "normal", 
                           mu_1 = 20,
                           sigma_1=2,
                           mu_2 = 20,
                           sigma_2 = 2,
                           sample_size = 30, 
                           n_sim = 1000, 
                           alpha = 0.05) {
  # Генерация данных и проведение теста
  # ====================================
  if (distribution == "normal") {
    # Генерация данных из нормального распределения для двух групп
    # Используем репликацию для создания n_sim независимых экспериментов
    df_experiment <- rbind(
      data.frame(
        experiment_ID = rep(1:n_sim, each = sample_size), # ID эксперимента
        group = "1", # Метка группы
        value = rnorm(sample_size*n_sim, mean = mu_1, sd = sigma_1) # Параметры нормального распределения
      ),
      data.frame(
        experiment_ID = rep(1:n_sim, each = sample_size),
        group = "2",
        value = rnorm(sample_size*n_sim, mean = mu_2, sd = sigma_2)
      )
    )
    # Обработка данных с использованием tidyverse
    results <- df_experiment %>%
      # Группировка по ID эксперимента
      group_by(experiment_ID) %>% 
      # Вложение данных для каждого эксперимента
      nest() %>% 
      # Применение критерия Манна-Уитни к каждому набору данных
      mutate(test_result = purrr::map(data, ~brunnermunzel::brunnermunzel.test(value ~ group, .x))) %>% 
      # Извлечение результатов теста в табличный формат
      mutate(result = purrr::map(test_result, ~broom::tidy(.x))) %>% 
      # Удаление промежуточных столбцов
      dplyr::select(-c(data,test_result)) %>% 
      # Разворачивание вложенных результатов
      unnest(result)
    
  } else if (distribution == "exponential") {
    # Генерация данных из экспоненциального распределения
    # Для экспоненциального распределения rate = 1/mean
    df_experiment <- rbind(
      data.frame(
        experiment_ID = rep(1:n_sim, each = sample_size),
        group = "1",
        value = rexp(sample_size*n_sim, rate = 1/mu_1)
      ),
      data.frame(
        experiment_ID = rep(1:n_sim, each = sample_size),
        group = "2",
        value = rexp(sample_size*n_sim, rate = 1/mu_2)
      )
    )
    # Аналогичная обработка данных для экспоненциального распределения
    results <- df_experiment %>% 
      group_by(experiment_ID) %>% 
      nest() %>% 
      mutate(test_result = purrr::map(data, ~brunnermunzel::brunnermunzel.test(value ~ group, .x))) %>% 
      mutate(result = purrr::map(test_result, ~broom::tidy(.x))) %>% 
      dplyr::select(-c(data,test_result)) %>% 
      unnest(result)
  }
  # Вычисление вероятности ошибки I рода критерия
  # =============================
  # Вероятность ошибки I рода = доля p-значений, меньших alpha
  return(sum(results$p.value < alpha)/n_sim)
  
}
