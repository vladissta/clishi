stripchart_one_sample_plot <- function(simulated_values_df, exp_id, mu0, true_mu, conf_level){
  
  # 1) выборка эксперимента (вектор)
  x_i <- simulated_values_df$value[simulated_values_df$experiment == exp_id]
  
  xbar <- mean(x_i)
  s    <- sd(x_i)
  n    <- length(x_i)
  
  # 2) t-interval
  alpha <- 1 - conf_level
  tcrit <- qt(1 - alpha/2, df = n - 1)
  se    <- s / sqrt(n)
  ci_l  <- xbar - tcrit * se
  ci_u  <- xbar + tcrit * se
  
  # 3) диапазон X, чтобы всё помещалось
  xlim <- range(c(x_i, mu0, true_mu, ci_l, ci_u))
  
  # 4) график точек
  stripchart(
    x_i,
    method = "jitter",
    pch = 16,
    main = paste("Наблюдения одной выборки (эксперимент №", exp_id, ")"),
    xlab = "Наблюдения Xᵢ",
    ylab = "",
    yaxt = "n",
    xlim = xlim
  )
  
  # 5) доверительный интервал как "скобка" (усики) и бледнее
  y_ci <- 1
  arrows(
    x0 = ci_l, y0 = y_ci,
    x1 = ci_u, y1 = y_ci,
    angle = 90, code = 3, length = 0.06,
    col = "gray55", lwd = 2
  )
  
  # 6) линии:
  # выборочное среднее
  abline(v = xbar, col = "darkgreen", lwd = 2)
  # гипотетическое значение mu0
  abline(v = mu0, col = "red", lwd = 2, lty = 2)
  # истинное мат. ожидание mu
  abline(v = true_mu, col = "blue", lwd = 2, lty = 3)
  
  # 7) подпись CI сверху
  mtext(sprintf("CI: [%.3f; %.3f]", ci_l, ci_u),
        side = 3, line = 0.2, cex = 0.9)
  
  # 8) легенда
  legend(
    "topright",
    legend = c(expression(bar(X)), expression(mu[0]), expression(mu), "Доверительный интервал"),
    col    = c("darkgreen", "red", "blue", "gray55"),
    lwd    = c(2, 2, 2, 2),
    lty    = c(1, 2, 3, 1),
    bty    = "n"
  )
}

means_hist_plot <- function(n, df_from_sim, true_mu, true_sd, mu0){
    # res <- sim_res()
    df <- df_from_sim
    
    hist(df$means,   # df$means — вектор из n_sim выборочных средних
         breaks = 30,
         freq = FALSE,
         main = "Распределение выборочных средних (оценок математического ожидания)",
         xlab = "Выборочное среднее \\(\\bar{X}\\)")
    
    #  Считаем теоретические параметры распределения выборочного среднего
    theor_mu <- true_mu                    # истинное математическое ожидание μ
    theor_se <- true_sd / sqrt(n)     # теоретическая SE( \bar{X} )
    
    #  Диапазон оси X: учитываем и данные, и μ, и μ0
    x_min <- min(df$means)
    x_max <- max(df$means)
    x_lim <- range(c(x_min, x_max, theor_mu, mu0))
    
    hist(df$means,
         breaks = 30,
         freq   = FALSE,
         xlim   = x_lim,   
         main   = "Распределение выборочных средних (оценок математического ожидания)",
         xlab   = "Выборочное среднее \\(\\bar{X}\\)")
    
    #  Теоретическая кривая распределения \bar{X}
    x_vals <- seq(x_lim[1], x_lim[2], length.out = 200)
    lines(x_vals,
          dnorm(x_vals, mean = theor_mu, sd = theor_se),
          lwd = 2)
    
    # Вертикальные линии: гипотетическое μ0 и истинное μ
    abline(v = mu0,  col = "red",       lwd = 2, lty = 2)  # гипотетическое значение μ0 (H0)
    abline(v = theor_mu, col = "darkgreen", lwd = 2, lty = 3)  # истинный параметр μ
    
    # Легенда
    legend("topright",
           legend = c("Теоретическое распределение \\(\\bar{X}\\) по ЦПТ",
                      "Гипотетическое μ₀",
                      "Истинное математическое ожидание μ"),
           col = c("black", "red", "darkgreen"),
           lwd = c(2, 2, 2),
           lty = c(1, 2, 3),
           bty = "n")
}

ci_func_plot <- function(df_from_sim,
                         ci_range,
                         show_only_miss,
                         mu0,
                         true_mu,
                         conf_level) {
  

    df  <- df_from_sim
    
    rng <- ci_range
    df  <- df[df$experiment >= rng[1] & df$experiment <= rng[2], ]
    
    if (isTRUE(show_only_miss)) {
      df <- df[!df$covers_true_mu, ]
    }
    
    n_sim_sub <- nrow(df)
    validate(need(n_sim_sub > 0, 
                  "В выбранном диапазоне нет доверительных интервалов для отображения"))
    
    plot(df$experiment, df$means,
         ylim = range(c(df$ci_low, df$ci_high, mu0, true_mu)),
         xlab = "Номер эксперимента",
         ylab = "Выборочное среднее и ДИ для математического ожидания",
         pch = 16,
         main = paste0("Доверительные интервалы для μ (уровень доверия = ",
                       conf_level, ")\nПоказаны эксперименты ",
                       rng[1], "–", rng[2]))
    
    for (i in 1:n_sim_sub) {
      col_i <- if (df$covers_true_mu[i]) "gray50" else "red"
      segments(x0 = df$experiment[i], y0 = df$ci_low[i],
               x1 = df$experiment[i], y1 = df$ci_high[i],
               lwd = 2, col = col_i)
    }
    
    abline(h = true_mu, col = "darkgreen", lty = 3, lwd = 2)
    abline(h = mu0,    col = "red",       lty = 2, lwd = 2)
    
    legend("topright",
           legend = c(
             "Истинное математическое ожидание μ",
             "Гипотетическое значение μ₀ (H₀)",
             "Доверительный интервал покрывает μ",
             "Доверительный интервал не покрывает μ"
           ),
           lwd = c(2, 2, 2, 2),
           col = c("darkgreen", "red", "gray50", "red"),
           lty = c(3, 2, 1, 1),
           bty = "n")
}

p_values_hist_plot <- function(df_from_sim, alpha_test){
  # res <- sim_res()
  df <- df_from_sim
  
  hist(df$p_value,
       breaks = 20,
       xlim = c(0, 1),
       main = "Распределение p-значений t-теста для H₀: μ = μ₀",
       xlab = "p-value")
  abline(v = alpha_test, lty = 2, lwd = 2, col = "red")
  
  legend("topright",
         legend = c("Граница уровня значимости α"),
         lwd = 2, lty = 2, col = "red", bty = "n")
}

t_test_crit_plot <- function(df_from_sim, exp_id, n, alpha, alt_type){

    validate(need(exp_id >= 1 && exp_id <= nrow(df_from_sim), "Неверный номер эксперимента"))
    
    if ("statistic" %in% names(df_from_sim)) {
      t_obs <- df_from_sim$statistic[exp_id]
    } else if ("t_stat" %in% names(df_from_sim)) {
      t_obs <- df_from_sim$t_stat[exp_id]
    } else if ("t" %in% names(df_from_sim)) {
      t_obs <- df_from_sim$t[exp_id]
    } else {
      validate(need(FALSE, "В df_from_sim_from_sim нет t-статистики (нужен столбец statistic / t_stat / t)"))
    }

    df_from_simree <- n - 1
    
    x <- seq(-4, 4, length = 400)
    y <- dt(x, df_from_simree)
    
    plot(
      x, y, type = "l", lwd = 2,
      main = "Проверка H₀ с использованием критических значений t-статистики",
      xlab = "t", ylab = "Плотность"
    )
    
    if (alt_type == "two.sided") {
      tcrit <- qt(1 - alpha / 2, df_from_simree)
      
      polygon(c(x[x < -tcrit], -tcrit),
              c(y[x < -tcrit], 0),
              col = rgb(1, 0, 0, 0.3), border = NA)
      
      polygon(c(x[x > tcrit], tcrit),
              c(y[x > tcrit], 0),
              col = rgb(1, 0, 0, 0.3), border = NA)
      
    } else if (alt_type == "greater") {
      tcrit <- qt(1 - alpha, df_from_simree)
      
      polygon(c(x[x > tcrit], tcrit),
              c(y[x > tcrit], 0),
              col = rgb(1, 0, 0, 0.3), border = NA)
      
    } else {
      tcrit <- qt(alpha, df_from_simree)
      
      polygon(c(x[x < tcrit], tcrit),
              c(y[x < tcrit], 0),
              col = rgb(1, 0, 0, 0.3), border = NA)
    }
    
    abline(v = t_obs, col = "blue", lwd = 2)
    
    legend(
      "topright",
      legend = c("t-распределение", "t_obs", "Критическая область"),
      col = c("black", "blue", "red"),
      lwd = 2,
      bty = "n"
    )
}


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