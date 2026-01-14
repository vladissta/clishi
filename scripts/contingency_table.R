library(tidyverse)

generation_binary_experiment <- function(
    n_sim,
    sample_size,
    p_event = 0.5,
    p_expose = 0.5,
    alpha = 0.05,
    exposure = 0.5,
    event = 0.5,
    method = c("cross-sectional", "cohort", "case-control", "fisher")
) {
  method <- match.arg(method)
  
  funs <- list(
    "cross-sectional" = function(n_sim, sample_size, p_event = 0.5, p_expose = 0.5, alpha = 0.05) {
      data <- tibble(
        experiment = rep(1:n_sim, each = sample_size),
        exposure = rbinom(n_sim * sample_size, size = 1, prob = p_expose),
        event = rbinom(n_sim * sample_size, size = 1, prob = p_event)
      )
      
      experiments <- unique(data$experiment)
      result <- c()
      
      for (experiment_num in experiments) {
        experiment_data <- data |> filter(experiment == experiment_num)
        result[[experiment_num]] <- table(experiment_data$event, experiment_data$exposure)
      }
      
      for (cont_table_num in seq_along(result)) {
        result[cont_table_num] <- chisq.test(result[[cont_table_num]])$p.value
      }
      
      p_count <- sum(result < alpha)
      return(p_count)
    },
    
    "cohort" = function(n_sim, sample_size, p_event = 0.5, p_expose = 0.5, exposure = 0.5, alpha = 0.05) {
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
      
      experiments <- unique(data$experiment)
      result <- c()
      
      for (experiment_num in experiments) {
        experiment_data <- data |> filter(experiment == experiment_num)
        result[[experiment_num]] <- table(experiment_data$event, experiment_data$exposure)
      }
      
      for (cont_table_num in seq_along(result)) {
        result[cont_table_num] <- chisq.test(result[[cont_table_num]])$p.value
      }
      
      p_count <- sum(result < alpha)
      return(p_count)
    },
    
    "case-control" = function(n_sim, sample_size, p_event = 0.5, p_expose = 0.5, event = 0.5, alpha = 0.05) {
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
      
      experiments <- unique(data$experiment)
      result <- c()
      
      for (experiment_num in experiments) {
        experiment_data <- data |> filter(experiment == experiment_num)
        result[[experiment_num]] <- table(experiment_data$event, experiment_data$exposure)
      }
      
      for (cont_table_num in seq_along(result)) {
        result[cont_table_num] <- chisq.test(result[[cont_table_num]])$p.value
      }
      
      p_count <- sum(result < alpha)
      return(p_count)
    },
    
    "fisher" = function(n_sim, sample_size, p_event = 0.5, p_expose = 0.5, event = 0.5, exposure = 0.5, alpha = 0.05) {
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
      
      experiments <- unique(data$experiment)
      result <- c()
      
      for (experiment_num in experiments) {
        experiment_data <- data |> filter(experiment == experiment_num)
        result[[experiment_num]] <- table(experiment_data$event, experiment_data$exposure)
      }
      
      for (cont_table_num in seq_along(result)) {
        result[cont_table_num] <- fisher.test(result[[cont_table_num]])$p.value
      }
      
      p_count <- sum(result < alpha)
      return(p_count)
    }
  )
  
  switch(
    method,
    "cross-sectional" = funs[[method]](
      n_sim = n_sim,
      sample_size = sample_size,
      p_event = p_event,
      p_expose = p_expose,
      alpha = alpha
    ),
    
    "cohort" = funs[[method]](
      n_sim = n_sim,
      sample_size = sample_size,
      p_event = p_event,
      p_expose = p_expose,
      exposure = exposure,
      alpha = alpha
    ),
    
    "case-control" = funs[[method]](
      n_sim = n_sim,
      sample_size = sample_size,
      p_event = p_event,
      p_expose = p_expose,
      event = event,
      alpha = alpha
    ),
    
    "fisher" = funs[[method]](
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
