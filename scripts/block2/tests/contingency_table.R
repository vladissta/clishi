library(dplyr)
library(tibble)
library(stats)
library(checkmate)


get_contingency_table <- function(data) {
  experiments <- unique(data$experiment)
  result <- c()
  
  for (experiment_num in experiments) {
    experiment_data <- data |> filter(experiment == experiment_num)
    
    # Use factor() with levels to ensure a 2x2 table even if counts are 0
    result[[experiment_num]] <- table(
      factor(experiment_data$event, levels = c(0, 1)),
      factor(experiment_data$exposure, levels = c(0, 1))
    ) 
  }
  return(result)
}

get_test <- function(method, cont_tables, alpha = 0.05, correct = FALSE){
  result <- c()
  for (cont_table_num in seq_along(cont_tables)) {
    if (method == "chi") {
      result[cont_table_num] <- chisq.test(cont_tables[[cont_table_num]], correct = correct)$p.value
    } else {
      result[cont_table_num] <- fisher.test(cont_tables[[cont_table_num]])$p.value
    }
    if (is.na(result[cont_table_num])) {
      result[cont_table_num] = 0
    }
  } 
  
  p_frac <- sum(result < alpha) / length(result)
  return(p_frac)
}


cross_sectional <- function(n_sim, sample_size, event_probability = 0.5, exposure_probability = 0.5) {
  data <- tibble(
    experiment = rep(1:n_sim, each = sample_size),
    exposure = rbinom(n_sim * sample_size, size = 1, prob = exposure_probability),
    event = rbinom(n_sim * sample_size, size = 1, prob = event_probability)
  )
  
  cont_tables <- get_contingency_table(data)
  return(cont_tables)
}

# cross_sectional_RR <- function(n_sim, sample_size, RR, basic_risk) {
#   data <- tibble(
#     experiment = rep(1:n_sim, each = sample_size),
#     
#     
#     
#   )
#   
#   cont_tables <- get_contingency_table(data)
#   return(cont_tables)
# }

cohort <- function(n_sim, sample_size, event_probability = 0.5, exposure_proportion) {
  n_exp   <- as.integer(round(sample_size * exposure_proportion))
  n_unexp <- sample_size - n_exp
  
  data <- tibble(
    experiment = rep(1:n_sim, each = sample_size),
    event = rep(c(rep(1, n_exp), rep(0, n_unexp)), times = n_sim),
    exposure = rbinom(n_sim * sample_size, size = 1, prob = event_probability)
  )
  
  cont_tables <- get_contingency_table(data)
  return(cont_tables)
}

cohort_RR <- function(n_sim, sample_size, exposure_proportion = 0.5, basic_risk, RR) {
  # (un)exposed_plus/minus <- (не)экспозированные, у которых событие произошло/не произошло
  cont_tables <- c()
  for (simulation_num in 1:n_sim) {
    unexposed <- rbinom(round(sample_size * (1 - exposure_proportion)), 1, prob = basic_risk)
    exposed = rbinom(sample_size - length(unexposed), 1, prob = min(basic_risk * RR, 1))
    
    unexposed_plus <- sum(unexposed)
    unexposed_minus <- length(unexposed) - unexposed_plus
    
    exposed_plus <- sum(exposed)
    exposed_minus <- length(exposed) - exposed_plus
    
    cont_tables[[simulation_num]] <- matrix(
      c(unexposed_minus, unexposed_plus,
        exposed_minus, exposed_plus),
      nrow = 2,
      byrow = TRUE
    )
  }
  return(cont_tables)
}

case_control <- function(n_sim, sample_size, exposure_probability = 0.5, event_proportion) {
  n_case <- as.integer(round(sample_size * event_proportion)) 
  n_ctrl <- sample_size - n_case
  
  data <- tibble(
    experiment = rep(1:n_sim, each = sample_size),
    exposure = rep(c(rep(1, n_case), rep(0, n_ctrl)), times = n_sim),
    event = c(rbinom(n_sim * sample_size, size = 1, prob = exposure_probability))
  )
  
  cont_tables <- get_contingency_table(data)
  return(cont_tables)
}


fisher <- function(n_sim, sample_size, event_proportion, exposure_proportion) {
  n_event <- as.integer(round(sample_size * event_proportion))
  n_noev  <- sample_size - n_event
  n_exp   <- as.integer(round(sample_size * exposure_proportion))
  n_unexp <- sample_size - n_exp
  
  data <- tibble(
    experiment = rep(1:n_sim, each = sample_size),
    exposure = as.vector(replicate(n_sim, sample(c(rep(1, n_exp), rep(0, n_unexp))))),
    event = as.vector(replicate(n_sim, sample(c(rep(1, n_event), rep(0, n_noev)))))
  )
  
  cont_tables <- get_contingency_table(data)
  return(cont_tables)
}


#' Оценка частоты ошибок I рода для различных дизайнов бинарных экспериментов
#' 
#' Функция генерирует серии бинарных данных для заданного дизайна исследования 
#' (кросс-секционное, когортное, случай-контроль или точный тест Фишера), 
#' проводит статистическое тестирование (Хи-квадрат или тест Фишера) и 
#' возвращает количество симуляций, в которых p-значение оказалось меньше 
#' заданного уровня значимости alpha.
#' 
#' Генерация данных для бинарных экспериментов
#'
#' @param n_sim Целое число. Количество симуляций.
#' @param sample_size Целое число. Размер выборки в каждой симуляции.
#' @param event_probability Число от 0 до 1. Вероятность наступления события (event). 
#' Используется для дизайнов \code{"cross_sectional"} и \code{"cohort"}.
#' По умолчанию 0.5.
#' @param exposure_probability Число от 0 до 1. Вероятность воздействия (exposure).
#' Используется для дизайнов \code{"cross_sectional"} и \code{"case_control"}.
#' По умолчанию 0.5.
#' @param exposure_proportion Число от 0 до 1. Фиксированная доля экспонированных. 
#' Обязательно для дизайнов \code{"cohort"} и \code{"fisher"}.
#' @param event_proportion Число от 0 до 1. Фиксированная доля исходов (случаев).
#' Обязательно для дизайнов \code{"case_control"} и \code{"fisher"}.
#' @param design Строка. Тип дизайна исследования. Возможные значения:
#' \code{"cross_sectional"} (по умолчанию), \code{"cohort"}, 
#' \code{"case_control"}, \code{"fisher"}, \code{"cross_sectional_OR"} (кросс-
#' секционное исследование с заданными отношением рисков и базовым риском)
#' @param method Строка. Тип статистического теста. Возможные значения: 
#' \code{"chi"} (хи-квадрат, по умолчанию) или \code{"fisher"} (тест Фишера).
#' @param alpha Число от 0 до 1. Уровень значимости. По умолчанию 0.05.
#' @param correct Логическое значение. Использовать ли поправку Йейтса для теста 
#' хи-квадрат? По умолчанию \code{FALSE}.
#' @return Возвращает одно число (integer) — количество экспериментов,
#' в которых p-value оказалось меньше заданного уровня alpha.
#' 
#' @importFrom dplyr filter
#' @importFrom tibble tibble
#' @importFrom stats rbinom chisq.test fisher.test
#' @importFrom checkmate assert_int assert_number assert_choice
#' 
#' @examples
#' # Пример запуска когортного исследования
#' generation_binary_experiment(
#'   n_sim = 100,
#'   sample_size = 50,
#'   exposure_proportion = 0.7,
#'   event_probability = 0.5,
#'   design = "cohort",
#'   method = "chi"
#' )
generation_binary_experiment <- function(
    n_sim,
    sample_size,
    event_probability,
    exposure_probability,
    RR,
    basic_risk,
    exposure_proportion,
    event_proportion,
    alpha = 0.05,
    design = "cross_sectional",
    correct = FALSE,
    method = "chi"
) {
  assert_int(n_sim, lower = 1)
  assert_int(sample_size, lower = 1)
  assert_choice(design, c("cross_sectional", "cohort", "case_control", "fisher"))
  assert_choice(method, c("chi", "fisher"))
  assert_number(alpha, lower = 0.001, upper = 0.999)
  
  if (design == "cohort") {
    if ((!is.null(RR) && !is.na(RR)) && (!is.null(basic_risk) && !is.na(basic_risk))) {
      is_RR <- TRUE
      assert_number(basic_risk, lower = 0.001, upper = 0.999)
      assert_number(RR, lower = 0.1, upper = 10)
    } else {
      is_RR <- FALSE
      assert_number(event_probability, lower = 0.001, upper = 0.999)
    }
    assert_number(exposure_proportion, lower = 0.001, upper = 0.999)
  }
  
  if (design %in% c("case_control", "cross_sectional")) {
    assert_number(event_probability, lower = 0.001, upper = 0.999)
  }
  if (design %in% c("cross_sectional")) {
    assert_number(exposure_probability, lower = 0.001, upper = 0.999)
  }
  if (design %in% c("case_control", "fisher")) {
    assert_number(event_proportion, lower = 0.001, upper = 0.999)
  }
  if (design %in% c("fisher")) {
    assert_number(exposure_proportion, lower = 0.001, upper = 0.999)
  } 
  
  cont_tables <- switch(
    match.arg(design, c("cross_sectional", "cohort", "case_control", "fisher")),
    "cross_sectional" = cross_sectional(
      n_sim = n_sim,
      sample_size = sample_size,
      event_probability = event_probability,
      exposure_probability = exposure_probability
    ),
    
    "cohort" = if (is_RR) {
      cohort_RR(
        n_sim = n_sim,
        sample_size = sample_size,
        exposure_proportion = exposure_proportion,
        basic_risk = basic_risk,
        RR = RR
      )
    } else {
      cohort(
        n_sim = n_sim,
        sample_size = sample_size,
        event_probability = event_probability,
        exposure_proportion = exposure_proportion
      )
    },
    "case_control" = case_control(
      n_sim = n_sim,
      sample_size = sample_size,
      exposure_probability = exposure_probability,
      event_proportion = event_proportion
    ),
    
    "fisher" = fisher(
      n_sim = n_sim,
      sample_size = sample_size,
      event_proportion = event_proportion,
      exposure_proportion= exposure_proportion
    )
  )
  
  return(get_test(method, cont_tables, alpha, correct))
}
