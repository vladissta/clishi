# 0. Общие моменты:
#   - везде выходным значением будет вероятность статистически-значимого результата (доля случаев, когда p < alpha).
# - по возможности используем tidy-style
# - после того, как заработает основной функционал, нужно будет потратить какое-то время на окультуривание кода 
# (добавить assertions, roxygen, комментарии). Для этого можно поюзать чатГПТ :)

# 3. Тесты Манна-Уитни и Бруннера-Мюнцеля
# - distribution: распределение чисел в генеральной совокупности (normal/exponential)
# - param_1: истинные параметры распределения в первой группе - named list с полями mu/sigma для normal и только mu для exponential
# - param_2: истинные параметры распределения во второй группе - named list с полями mu/sigma для normal и только mu для exponential
# - sample_size: объем выборки, будем считать группы одинакового размера (integer)
# - n_sim: количество симуляций (integer)
# - alpha: уровень значимости (numeric)
# 
# Одновыборочные варианты рассматривать не будем.
library(tidyverse)


mann_whitney <- function(distribution = "normal", 
                         param_1 = list(mu = 20, sigma = 2), 
                         param_2 = list(mu = 20, sigma = 2), 
                         sample_size = 30, 
                         n_sim = 1000, 
                         alpha = 0.05) {
  
  if (distribution == "normal") {
    df_experiment <- rbind(
      data.frame(
        experiment_ID = rep(1:n_sim, each = sample_size),
        group = "1",
        value = rnorm(sample_size*n_sim, mean = param_1$mu, sd = param_1$sigma)
      ),
      data.frame(
        experiment_ID = rep(1:n_sim, each = sample_size),
        group = "2",
        value = rnorm(sample_size*n_sim, mean = param_2$mu, sd = param_2$sigma)
      )
    )
    
    results <- df_experiment %>% 
      group_by(experiment_ID) %>% 
      nest() %>% 
      mutate(test_result = purrr::map(data, ~wilcox.test(value ~ group, .x))) %>% 
      mutate(result = purrr::map(test_result, ~broom::tidy(.x))) %>% 
      dplyr::select(-c(data,test_result)) %>% 
      unnest(result)
    
  } else if (distribution == "exponential") {
    df_experiment <- rbind(
      data.frame(
        experiment_ID = rep(1:n_sim, each = sample_size),
        group = "1",
        value = rexp(sample_size*n_sim, rate = 1/param_1$mu)
      ),
      data.frame(
        experiment_ID = rep(1:n_sim, each = sample_size),
        group = "2",
        value = rexp(sample_size*n_sim, rate = 1/param_2$mu)
      )
    )
    
    results <- df_experiment %>% 
      group_by(experiment_ID) %>% 
      nest() %>% 
      mutate(test_result = purrr::map(data, ~wilcox.test(value ~ group, .x))) %>% 
      mutate(result = purrr::map(test_result, ~broom::tidy(.x))) %>% 
      dplyr::select(-c(data,test_result)) %>% 
      unnest(result)
  }
  
  return(sum(results$p.value < alpha)/n_sim)
  
}


