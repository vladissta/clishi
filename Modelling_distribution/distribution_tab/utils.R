library(tidyverse)


generate_samples <- function(distribution_type, distribution_params) {
  sample <- rexp(
    distribution_params[["sample_size"]], 
    rate = distribution_params[["rate"]]
  )
  return(sample)
}

plot_distribution <- function(sample, params) {
  ggplot(data = data.frame(x = sample), aes(x = x)) +
    stat_function(fun = dexp,
                  args =  list(rate = params[["rate"]])) +
    geom_histogram(aes(y = ..density..)) +
    xlim(0, 5/params[["rate"]]) +
    theme_bw()
}
