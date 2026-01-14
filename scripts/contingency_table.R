library(tidyverse)

generation_binary_experiment <- function(n_sim, sample_size, p_event = 0.5, p_expose = 0.5) {
  data <- tibble(
    experiment = rep(1:n_sim, each = sample_size),
    exposure = rbinom(n_sim * sample_size, size = 1, prob = p_expose),
    event = rbinom(n_sim * sample_size, size = 1, prob = p_event)
  )
  return(data)
} 

get_contingency_table <- function(data) {
  experiments <- unique(data$experiment)
  result <- c()
  for (experiment_num in experiments) {
    experiment_data <- data |> filter(experiment == experiment_num)
    result[[experiment_num]] <- table(experiment_data$event, experiment_data$exposure) 

  }
  return(result)
}

calculate_chisq_test <- function(contingency_tables) { 
  result <- c()
  for (cont_table_num in seq_along(contingency_tables)) {
    result[cont_table_num] <- chisq.test(contingency_tables[[cont_table_num]])$p.value
  }
  p_count <- sum(result < 0.05) 
  return(p_count)
}
