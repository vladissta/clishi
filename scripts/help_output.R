norm_plot_help <- function(dist_type){
  selected <- dist_type == "norm"
  col <- if (selected) "black" else "gray70"
  lwd <- if (selected) 3 else 1.5
  
  curve(dnorm(x, mean = 0, sd = 1),
        from = -3, to = 3,
        xlab = "x", ylab = "Плотность",
        main = if (selected) "Нормальное N(0,1) — выбрано" else "Нормальное N(0,1)",
        lwd = lwd, col = col)
}

unif_plot_help <- function(dist_type){
  selected <- dist_type == "unif"
  col <- if (selected) "black" else "gray70"
  lwd <- if (selected) 3 else 1.5
  
  curve(dunif(x, min = 0, max = 1),
        from = -0.2, to = 1.2,
        xlab = "x", ylab = "Плотность",
        main = if (selected) "Равномерное U(0,1) — выбрано" else "Равномерное U(0,1)",
        lwd = lwd, col = col)
  abline(v = c(0,1), lty = 2)
}

exp_plot_help <- function(dist_type){
  selected <- dist_type == "exp"
  col <- if (selected) "black" else "gray70"
  lwd <- if (selected) 3 else 1.5
  
  curve(dexp(x, rate = 1),
        from = 0, to = 5,
        xlab = "x", ylab = "Плотность",
        main = if (selected) "Экспоненциальное Exp(1) — выбрано" else "Экспоненциальное Exp(1)",
        lwd = lwd, col = col)
}