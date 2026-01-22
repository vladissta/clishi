library(dplyr)
library(tibble)
library(stats)
library(checkmate)


get_contingency_table <- function(data) {
  experiments <- unique(data$experiment)
  result <- c()
  for (experiment_num in experiments) {
    experiment_data <- data |> filter(experiment == experiment_num)
    result[[experiment_num]] <- table(experiment_data$event, experiment_data$exposure) 
  }
  return(result)
}


cross_sectional <- function(n_sim, sample_size, event_probability = 0.5, exposure_probability = 0.5, alpha = 0.05) {
  data <- tibble(
    experiment = rep(1:n_sim, each = sample_size),
    exposure = rbinom(n_sim * sample_size, size = 1, prob = exposure_probability),
    event = rbinom(n_sim * sample_size, size = 1, prob = event_probability)
  )
  
  result <- get_contingency_table(data)
  
  for (cont_table_num in seq_along(result)) {
    result[cont_table_num] <- chisq.test(result[[cont_table_num]])$p.value
  }
  
  p_count <- sum(result < alpha)
  return(p_count)
}


cohort <- function(n_sim, sample_size, exposure_probability = 0.5, exposure_proportion, alpha = 0.05) {
  n_exp   <- as.integer(round(sample_size * exposure_proportion))
  n_unexp <- sample_size - n_exp
  
  data <- tibble(
    experiment = rep(1:n_sim, each = sample_size),
    event = rep(c(rep(1, n_exp), rep(0, n_unexp)), times = n_sim),
    exposure = rbinom(n_sim * sample_size, size = 1, prob = exposure_probability)
  )
  
  result <- get_contingency_table(data)
  
  for (cont_table_num in seq_along(result)) {
    result[cont_table_num] <- chisq.test(result[[cont_table_num]])$p.value
  }
  
  p_count <- sum(result < alpha)
  return(p_count)
}


case_control <- function(n_sim, sample_size, event_probability = 0.5, event_proportion, alpha = 0.05) {
  n_case <- as.integer(round(sample_size * event_proportion)) 
  n_ctrl <- sample_size - n_case
  
  data <- tibble(
    experiment = rep(1:n_sim, each = sample_size),
    exposure = rep(c(rep(1, n_case), rep(0, n_ctrl)), times = n_sim),
    event = c(rbinom(n_sim * sample_size, size = 1, prob = event_probability))
  )
  
  result <- get_contingency_table(data)
  
  for (cont_table_num in seq_along(result)) {
    result[cont_table_num] <- chisq.test(result[[cont_table_num]])$p.value
  }
  
  p_count <- sum(result < alpha)
  return(p_count)
}


fisher <- function(n_sim, sample_size, event_proportion, exposure_proportion, alpha = 0.05) {
  n_event <- as.integer(round(sample_size * event_proportion))
  n_noev  <- sample_size - n_event
  n_exp   <- as.integer(round(sample_size * exposure_proportion))
  n_unexp <- sample_size - n_exp
  
  data <- tibble(
    experiment = rep(1:n_sim, each = sample_size),
    exposure = as.vector(replicate(n_sim, sample(c(rep(1, n_exp), rep(0, n_unexp))))),
    event = as.vector(replicate(n_sim, sample(c(rep(1, n_event), rep(0, n_noev)))))
  )
  
  result <- get_contingency_table(data)
  
  for (cont_table_num in seq_along(result)) {
    result[cont_table_num] <- fisher.test(result[[cont_table_num]])$p.value
  }
  
  p_count <- sum(result < alpha)
  return(p_count)
}


#' Оценка частоты ошибок I рода для различных дизайнов бинарных экспериментов
#' 
#' Функция генерирует серии бинарных данных для заданного дизайна исследования 
#' (кросс-секционное, когортное, случай-контроль или точный тест Фишера), 
#' проводит статистическое тестирование (Хи-квадрат или тест Фишера) и 
#' возвращает количество симуляций, в которых p-значение оказалось меньше 
#' заданного уровня значимости alpha.
#' 
#' @param n_sim Целое число. Количество симуляций.
#' @param sample_size Целое число. Размер выборки в каждой симуляции.
#' @param event_probability Число от 0 до 1. Вероятность наступления события (event),  
#' (по умолчанию 0.5).
#' @param exposure_probability Число от 0 до 1. Вероятность воздействия (exposure),
#' (по умолчанию 0.5).
#' @param alpha Число от 0 до 1. Уровень значимости (по умолчанию 0.05).
#' @param exposure_proportion(Опционально) Доля экспонированных для когортного дизайна,
#' эксперимента Фишера.
#' @param event (Опционально) Доля случаев для дизайна случай-контроль,
#' эксперимента Фишера.
#' @param method Строка. Тип дизайна исследования. Возможные значения:
#' \code{"cross_sectional"}, \code{"cohort"}, \code{"case_control"}, \code{"fisher"}.
#'   
#' @return Возвращает одно число (integer) — количество симуляций,
#' в которых p-value оказалось меньше заданного уровня alpha.
#' 
#' @importFrom dplyr filter
#' @importFrom tibble tibble
#' @importFrom stats rbinom chisq.test fisher.test
#' @importFrom checkmate assert_int assert_number assert_choice
#' 
#' @examples
#' # Пример запуска кросс-секционного исследования
#' generation_binary_experiment(
#'   n_sim = 100,
#'   sample_size = 50,
#'   method = "cross_sectional"
#' )
generation_binary_experiment <- function(
    n_sim,
    sample_size,
    event_probability = 0.5,
    exposure_probability = 0.5,
    exposure_proportion,
    event_proportion,
    alpha = 0.05,
    method = c("cross_sectional", "cohort", "case_control", "fisher")
) {
  assert_int(n_sim, lower = 1)
  assert_int(sample_size, lower = 1)
  assert_choice(method, c("cross_sectional", "cohort", "case_control", "fisher"))
  assert_number(alpha, lower = 0.001, upper = 0.999)
  
  if (method %in% c("cross_sectional", "case_control")) {
    assert_number(event_probability, lower = 0.001, upper = 0.999)
  }
  if (method %in% c("cross_sectional", "cohort")) {
    assert_number(exposure_probability, lower = 0.001, upper = 0.999)
  }
  if (method %in% c("case_control", "fisher")) {
    assert_number(event_proportion, lower = 0.001, upper = 0.999)
  }
  if (method %in% c("cohort", "fisher")) {
    assert_number(exposure_proportion, lower = 0.001, upper = 0.999)
  }
 

  switch(
    match.arg(method),
    "cross_sectional" = cross_sectional(
      n_sim = n_sim,
      sample_size = sample_size,
      event_probability = event_probability,
      exposure_probability = exposure_probability,
      alpha = alpha
    ) / n_sim,
    
    "cohort" = cohort(
      n_sim = n_sim,
      sample_size = sample_size,
      exposure_probability = exposure_probability,
      exposure_proportion= exposure_proportion,
      alpha = alpha
    )/ n_sim,
    
    "case_control" = case_control(
      n_sim = n_sim,
      sample_size = sample_size,
      event_probability = event_probability,
      event_proportion = event_proportion,
      alpha = alpha
    )/ n_sim,
    
    "fisher" = fisher(
      n_sim = n_sim,
      sample_size = sample_size,
      event_proportion = event_proportion,
      exposure_proportion= exposure_proportion,
      alpha = alpha
    )/ n_sim
  )
}
