get_contingency_table <- function(data) {
  experiments <- unique(data$experiment)
  result <- c()
  for (experiment_num in experiments) {
    experiment_data <- data |> filter(experiment == experiment_num)
    result[[experiment_num]] <- table(experiment_data$event, experiment_data$exposure) 
  }
  return(result)
}

cross_sectional <- function(n_sim, sample_size, p_event = 0.5, p_expose = 0.5, alpha = 0.05) {
  data <- tibble(
    experiment = rep(1:n_sim, each = sample_size),
    exposure = rbinom(n_sim * sample_size, size = 1, prob = p_expose),
    event = rbinom(n_sim * sample_size, size = 1, prob = p_event)
  )
  
  result <- get_contingency_table(data)
  
  for (cont_table_num in seq_along(result)) {
    result[cont_table_num] <- chisq.test(result[[cont_table_num]])$p.value
  }
  
  p_count <- sum(result < alpha)
  return(p_count)
}


cohort <- function(n_sim, sample_size, p_expose = 0.5, exposure, alpha = 0.05) {
  n_exp   <- as.integer(round(sample_size * exposure))
  n_unexp <- sample_size - n_exp
  
  data <- tibble(
    experiment = rep(1:n_sim, each = sample_size),
    event = rep(c(rep(1, n_exp), rep(0, n_unexp)), times = n_sim),
    exposure = c(
      rbinom(n_sim * n_exp, size = 1, prob = p_expose),
      rbinom(n_sim * n_unexp, size = 1, prob = p_expose)
    )
  )
  
  result <- get_contingency_table(data)
  
  for (cont_table_num in seq_along(result)) {
    result[cont_table_num] <- chisq.test(result[[cont_table_num]])$p.value
  }
  
  p_count <- sum(result < alpha)
  return(p_count)
}

case_control <- function(n_sim, sample_size, p_event = 0.5,  event, alpha = 0.05) {
  n_case <- as.integer(round(sample_size * event)) 
  
  n_ctrl <- sample_size - n_case
  
  data <- tibble(
    experiment = rep(1:n_sim, each = sample_size),
    exposure = rep(c(rep(1, n_case), rep(0, n_ctrl)), times = n_sim),
    event = c(
      rbinom(n_sim * n_case, size = 1, prob = p_event),
      rbinom(n_sim * n_ctrl, size = 1, prob = p_event)
    )
  )
  
  result <- get_contingency_table(data)
  
  for (cont_table_num in seq_along(result)) {
    result[cont_table_num] <- chisq.test(result[[cont_table_num]])$p.value
  }
  
  p_count <- sum(result < alpha)
  return(p_count)
}

fisher <- function(n_sim, sample_size, p_event = 0.5, p_expose = 0.5, event, exposure, alpha = 0.05) {
  n_event <- as.integer(round(sample_size * event))
  n_noev  <- sample_size - n_event
  n_exp   <- as.integer(round(sample_size * exposure))
  n_unexp <- sample_size - n_exp
  
  data <- tibble(
    experiment = rep(1:n_sim, each = sample_size),
    exposure = c(
      rbinom(n_sim * n_event, size = 1, prob = p_expose),
      rbinom(n_sim * n_noev, size = 1, prob = p_expose)
    ),
    event = c(
      rbinom(n_sim * n_exp, size = 1, prob = p_event),
      rbinom(n_sim * n_unexp, size = 1, prob = p_event)
    )
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
#' #' Функция генерирует серии бинарных данных для заданного дизайна исследования 
#' (кросс-секционное, когортное, случай-контроль или точный тест Фишера), 
#' проводит статистическое тестирование (Хи-квадрат или тест Фишера) и 
#' возвращает количество симуляций, в которых p-значение оказалось меньше 
#' заданного уровня значимости alpha.
#' 
#' @param n_sim Целое число. Количество симуляций.
#' @param sample_size Целое число. Размер выборки в каждой симуляции.
#' @param p_event Число от 0 до 1. Вероятность наступления события (event),  
#' (по умолчанию 0.5).
#' @param p_expose Число от 0 до 1. Вероятность воздействия (exposure),
#' (по умолчанию 0.5).
#' @param alpha Число от 0 до 1. Уровень значимости (по умолчанию 0.05).
#' @param exposure (Опционально) Доля экспонированных для когортного дизайна,
#' эксперимента Фишера.
#' @param event (Опционально) Доля случаев для дизайна случай-контроль,
#' эксперимента Фишера.
#' @param method Строка. Тип дизайна исследования. Возможные значения:
#' \code{"cross-sectional"}, \code{"cohort"}, \code{"case-control"}, \code{"fisher"}.
#'   
#' @return Возвращает одно число (integer) — количество симуляций,
#' в которых p-value оказалось меньше заданного уровня alpha.
#' 
#' @importFrom dplyr filter
#' @importFrom tibble tibble
#' @importFrom stats rbinom chisq.test fisher.test
#' 
#' @examples
#' # Пример запуска кросс-секционного исследования
#' generation_binary_experiment(
#'   n_sim = 100,
#'   sample_size = 50,
#'   p_event = 0.5,
#'   p_expose = 0.5
#'   method = "cross-sectional"
#' )


generation_binary_experiment <- function(
    n_sim,
    sample_size,
    p_event = 0.5,
    p_expose = 0.5,
    alpha = 0.05,
    exposure,
    event,
    method = c("cross-sectional", "cohort", "case-control", "fisher")
) {
  switch(
    match.arg(method),
    "cross-sectional" = cross_sectional(
      n_sim = n_sim,
      sample_size = sample_size,
      p_event = p_event,
      p_expose = p_expose,
      alpha = alpha
    ),
    
    "cohort" = cohort(
      n_sim = n_sim,
      sample_size = sample_size,
      p_event = p_event,
      p_expose = p_expose,
      exposure = exposure,
      alpha = alpha
    ),
    
    "case-control" = case_control(
      n_sim = n_sim,
      sample_size = sample_size,
      p_event = p_event,
      p_expose = p_expose,
      event = event,
      alpha = alpha
    ),
    
    "fisher" = fisher(
      n_sim = n_sim,
      sample_size = sample_size,
      p_event = p_event,
      p_expose = p_expose,
      event = event,
      exposure = exposure,
      alpha = alpha
    )
  )
}
