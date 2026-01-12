library(tidyverse)
library(brunnermunzel)

brunner_munzel <- function(distribution = "normal", 
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
      mutate(test_result = purrr::map(data, ~brunnermunzel::brunnermunzel.test(value ~ group, .x))) %>% 
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
      mutate(test_result = purrr::map(data, ~brunnermunzel::brunnermunzel.test(value ~ group, .x))) %>% 
      mutate(result = purrr::map(test_result, ~broom::tidy(.x))) %>% 
      dplyr::select(-c(data,test_result)) %>% 
      unnest(result)
  }
  
  return(sum(results$p.value < alpha)/n_sim)
  
}