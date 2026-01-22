#' Оценка вероятности ошибки I рода для теста Бруннер-Мюнцеля при различных распределениях и их параметрах
#'
#' Функция проводит симуляцию для оценки вероятности ошибки I рода
#' непараметрического критерия Бруннер-Мюнцеля при различных параметрах
#' распределений и размерах выборок.
#'
#' @param distribution Тип распределения данных. Доступные значения: 
#'   "normal" (нормальное распределение) или "exponential" (экспоненциальное распределение).
#' @param mu1 - среднее в первой выборке (по умолчанию 20)
#' @param sigma1 - стандартное отклонение в первой выборке (по умолчанию 2)
#' @param mu2 - среднее во второй выборке (по умолчанию 20)
#' @param sigma2 - стандартное отклонение во второй выборке (по умолчанию 2)
#' @param sample_size Размер выборки в каждой группе (по умолчанию 30).
#' @param n_sim Количество симуляций (по умолчанию 1000).
#' @param alpha Уровень значимости (по умолчанию 0.05).
#' 
#' @import tidyr
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @importFrom checkmate assert_choice assert_number assert_int assert_count assert_true
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
#'              mu1 = 20, sigma1 = 2, 
#'              mu2 = 20, sigma2 = 2)
#'
#' # Оценка вероятности ошибки I рода для экспоненциального распределения
#' brunner_munzel(distribution = "exponential", 
#'              mu1 = 20, 
#'              mu2 = 25, 
#'              sample_size = 50, 
#'              n_sim = 2000)
#'

brunner_munzel <- function(distribution = "normal", 
                            mu1 = 20, sigma1 = 2, 
                            mu2 = 20, sigma2 = 2, 
                            sample_size = 30, 
                            n_sim = 1000, 
                            alpha = 0.05) {
  # Проверка аргументов с помощью checkmate
  # =========================================
  
  # 1. Проверка distribution
  checkmate::assert_choice(
    distribution,
    choices = c("normal", "exponential"),
    .var.name = "distribution"
  )
  
  # 2. Проверка числовых параметров распределения
  # mu1 и mu2 для нормального распределения
  checkmate::assert_number(
    mu1, 
    finite = TRUE,
    .var.name = "mu1"
  )
  
  checkmate::assert_number(
    mu2, 
    finite = TRUE,
    .var.name = "mu2"
  )
  
  # sigma1 и sigma2 должны быть положительными числами
  checkmate::assert_number(
    sigma1,
    lower = 0.001,
    finite = TRUE,
    .var.name = "sigma1"
  )
  
  checkmate::assert_number(
    sigma2,
    lower = 0.001,
    finite = TRUE,
    .var.name = "sigma2"
  )
  
  # 3. Проверка размера выборки
  checkmate::assert_number(
    sample_size,
    lower = 3,
    .var.name = "sample_size"
  )
  
  # 4. Проверка количества симуляций
  checkmate::assert_count(
    n_sim,
    positive = TRUE,
    .var.name = "n_sim"
  )
  
  # 5. Проверка уровня значимости
  checkmate::assert_number(
    alpha,
    lower = 0,
    upper = 1,
    finite = TRUE,
    .var.name = "alpha"
  )
  
  # 6. Для экспоненциального распределения mu > 0
  if (distribution == "exponential") {
    # Для экспоненциального распределения rate = 1/mean, поэтому mean > 0
    checkmate::assert_true(
      mu1 > 0 & mu2 > 0)
  }
  
  # Генерация данных и проведение теста
  # ====================================
  if (distribution == "normal") {
    # Генерация данных из нормального распределения для двух групп
    # Используем репликацию для создания n_sim независимых экспериментов
    df_experiment <- rbind(
      data.frame(
        experiment_ID = rep(1:n_sim, each = sample_size), # ID эксперимента
        group = "1", # Метка группы
        value = rnorm(sample_size*n_sim, mean = mu1, sd = sigma1) # Параметры нормального распределения
      ),
      data.frame(
        experiment_ID = rep(1:n_sim, each = sample_size),
        group = "2",
        value = rnorm(sample_size*n_sim, mean = mu2, sd = sigma2)
      )
    )} else if (distribution == "exponential") {
    # Генерация данных из экспоненциального распределения
    # Для экспоненциального распределения rate = 1/mean
    df_experiment <- rbind(
      data.frame(
        experiment_ID = rep(1:n_sim, each = sample_size),
        group = "1",
        value = rexp(sample_size*n_sim, rate = 1/mu1)
      ),
      data.frame(
        experiment_ID = rep(1:n_sim, each = sample_size),
        group = "2",
        value = rexp(sample_size*n_sim, rate = 1/mu2)
      )
    )}
  # Обработка данных с использованием tidyverse
  results <- df_experiment %>%
    # Вложение и группировка данных для каждого эксперимента
    nest(.by = experiment_ID) %>% 
    # Применение критерия Манна-Уитни к каждому набору данных
    mutate(test_result = purrr::map(data, ~brunnermunzel::brunnermunzel.test(value ~ group, .x))) %>% 
    # Извлечение результатов теста в табличный формат
    mutate(result = purrr::map(test_result, ~broom::tidy(.x))) %>% 
    # Удаление промежуточных столбцов
    dplyr::select(-c(data,test_result)) %>% 
    # Разворачивание вложенных результатов
    unnest(result)
  # Вычисление вероятности ошибки I рода критерия
  # =============================
  # Вероятность ошибки I рода = доля p-значений, меньших alpha
  return(sum(results$p.value < alpha)/n_sim)
  
}

