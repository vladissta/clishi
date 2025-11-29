library(tidyverse)


generate_samples <- function(distribution_type, distribution_params) {
  samples_df <- data.frame(
      id_sample = rep(1:distribution_params[["generation_size"]], each = distribution_params[["sample_size"]]),
      value = rexp(distribution_params[["generation_size"]] * distribution_params[["sample_size"]], rate = distribution_params[["rate"]])
  )
  return(samples_df)
}


calculate_samples_mean_df <- function(samples_df, distribution_params) { 
  # normalized_mean = (mean - true_mean)* sqrt(n) / sd)
  samples_mean_df <- samples_df %>%
  group_by(id_sample) %>%
  summarise(mean_sample = mean(value)) %>%
  mutate(
    normalized_mean_sample = (mean_sample - (1/distribution_params[["rate"]])) * 
      sqrt(distribution_params[["sample_size"]]) / 
      sqrt(1/distribution_params[["rate"]]^2)
  ) %>%  
  ungroup()
  return(samples_mean_df)
}


plot_distribution <- function(samples_mean_df) {
  ggplot(data = samples_mean_df, aes(x = normalized_mean_sample)) +
    geom_histogram(
      aes(y = ..density..),
      color = 'black', 
      fill = 'grey80'
    ) +
    stat_function(
      fun = dnorm, 
      args = list(mean = 0, sd = 1), color = 'red'
    ) +
    labs(
      title = "Распределение средних в каждом эксперименте",
      x = "Нормализованное среднее в каждой выборке",
      y = "Плотность"
    ) +
    theme(
      plot.title = element_text(size = 18, margin = margin(t = 5, b = 5, unit = "pt"), hjust = 0.5, face = "bold"),
      axis.title.x = element_text(size = 16, margin = margin(t = 15, unit = "pt")),
      axis.text.x = element_text(size = 14),
      axis.title.y = element_text(size = 16, angle = 90, margin = margin(r = 15, unit = "pt")),
      axis.text.y = element_text(size = 14),
      panel.background = element_rect(fill = "white"),
      panel.grid = element_line(linewidth = 0.3, color = "grey"),
    )

}
