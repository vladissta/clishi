parameters_grid_line_plot <- function(simulation_vector, 
                                      parameter_name, alpha){
  
  ymax <- min(c(max(simulation_vector) + 0.2, 1))
  
  data.frame(
    x = as.numeric(names(simulation_vector)),
    y = simulation_vector) %>% 
    
    ggplot(aes(x=x, y=y, group=1)) + 
    geom_point() + 
    geom_line() +
    scale_y_continuous(breaks = c(0, alpha, seq(0, ymax, by = 0.1)), 
                       limits = c(0, ymax)) +
    labs(title = paste('Ошибка I рода в завсимости от', parameter_name),
         x = parameter_name, y = 'Ошибка I рода',) +
    geom_hline(aes(yintercept = alpha), col='red', lty=2) +
    theme_bw()
  
}