stripchart_one_sample_plot <- function(df_from_sim, Xmat, exp_id){
  # res <- sim_res()
  df  <- df_from_sim
  id  <- exp_id
  # validate(
  #   need(id >= 1 && id <= nrow(df), "Неверный номер эксперимента")
  # )
  x_i <- Xmat[id, ]
  
  stripchart(x_i, 
             method = "jitter", pch = 16,
             main = paste("Наблюдения одной выборки (эксперимент №", id, ")"),
             xlab = "Наблюдения Xᵢ",
             ylab = "",
             yaxt = "n")
  
  abline(v = df$mean[id], col = "blue", lwd = 2)
}

means_hist_plot <- function(n, df_from_sim, true_mu, true_sd, mu0){
    # res <- sim_res()
    df <- df_from_sim
    
    hist(df$mean,                          # df$mean — вектор из n_sim выборочных средних
         breaks = 30,
         freq = FALSE,
         main = "Распределение выборочных средних (оценок математического ожидания)",
         xlab = "Выборочное среднее \\(\\bar{X}\\)")
    
    #  Считаем теоретические параметры распределения выборочного среднего
    theor_mu <- true_mu                    # истинное математическое ожидание μ
    theor_se <- true_sd / sqrt(n)     # теоретическая SE( \bar{X} )
    
    #  Диапазон оси X: учитываем и данные, и μ, и μ0
    x_min <- min(df$mean)
    x_max <- max(df$mean)
    x_lim <- range(c(x_min, x_max, theor_mu, mu0))
    
    hist(df$mean,
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
    
    plot(df$experiment, df$mean,
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