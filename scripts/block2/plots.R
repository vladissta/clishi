# function to plot line plot of parameter grid simulation results
parameters_grid_line_plot <- function(simulation_vector, 
                                      parameter_name, alpha){
  
  ymax <- min(c(max(simulation_vector) + 0.2, 1))
  
  param_values = as.numeric(names(simulation_vector))
  pval_frac = simulation_vector
  
  data.frame(param_values, pval_frac) %>% 
    
    ggplot(aes(x=param_values, y=pval_frac, group=1)) + 
    geom_point() + 
    geom_line() +
    scale_y_continuous(breaks = c(0, alpha, seq(0, ymax, by = 0.1)), 
                       limits = c(0, ymax)) +
    scale_x_continuous(breaks = param_values) +
    labs(x = parameter_name, y = 'Доля экспериментов со стат. значимым результатом') +
    geom_hline(aes(yintercept = alpha), col='red', lty=2) +
    theme_bw() +
    theme(axis.text = element_text(size=12),
          axis.title = element_text(size=15))
  
}