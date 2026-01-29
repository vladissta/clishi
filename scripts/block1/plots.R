stripchart_one_sample_plot <- function(simulated_values_df, exp_id, mu0, true_mu, conf_level){
  
  # 1) выборка эксперимента
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
  
  # 3) xlim (+ небольшой запас)
  xlim <- range(c(x_i, mu0, true_mu, ci_l, ci_u))
  pad  <- 0.05 * diff(xlim)
  if (!is.finite(pad) || pad == 0) pad <- 0.5
  xlim <- c(xlim[1] - pad, xlim[2] + pad)
  
  # 4) поля (легенда внутри, но xpd=NA позволит не резать подписи)
  op <- par(no.readonly = TRUE)
  on.exit(par(op), add = TRUE)
  par(mar = c(4.5, 4.5, 4.5, 2), xpd = NA)
  
  # 5) точки: jitter по y (фиктивная ось)
  y_pts <- 0.95 + runif(n, -0.035, 0.035)
  
  plot(
    x_i, y_pts,
    pch = 16, cex = 0.9,
    xlim = xlim,
    ylim = c(0.80, 1.25),
    axes = FALSE,     # убираем оси целиком
    xlab = "Наблюдения X\u1d62",
    ylab = "",
    main = paste0("Наблюдения одной выборки (эксперимент № ", exp_id, ")")
  )
  axis(1)   # оставляем только X
  box()     # рамка (если не нужна — убери)
  
  # 6) доверительный интервал — выше точек
  y_ci <- 1.15
  segments(ci_l, y_ci, ci_u, y_ci, col = "gray45", lwd = 4)
  segments(ci_l, y_ci - 0.02, ci_l, y_ci + 0.02, col = "gray45", lwd = 4)
  segments(ci_u, y_ci - 0.02, ci_u, y_ci + 0.02, col = "gray45", lwd = 4)
  
  # 7) вертикальные линии КОРОТКИЕ
  y0_line <- 0.86
  y1_line <- 1.19
  
  segments(xbar,    y0_line, xbar,    y1_line, col = "black",   lwd = 2)
  segments(mu0,     y0_line, mu0,     y1_line, col = "red",     lwd = 2, lty = 2)
  segments(true_mu, y0_line, true_mu, y1_line, col = "#1B5E20", lwd = 3)
  
  # 8) если μ0 = μ — пояснение
  tol <- 1e-10
  if (isTRUE(all.equal(mu0, true_mu, tolerance = tol))) {
    mtext(expression(mu[0] == mu ~ "(линии совпадают)"),
          side = 3, line = 0.2, adj = 1,
          col = "gray30", cex = 0.9, font = 2)
  }
  
  # 9) легенда внутри справа сверху
  legend(
    "topright",
    inset = 0.02,
    legend = c(expression(mu), expression(mu[0]), expression(bar(X)), 
               "ДИ"),
    col    = c("#1B5E20", "red", "black", "gray45"),
    lwd    = c(3, 2, 2, 4),
    lty    = c(1, 2, 1, 1),
    bty    = "o",
    bg     = "white",
    cex    = 0.9
  )
}

means_hist_plot <- function(n, df_from_sim, true_mu, true_sd, mu0){
  
  df <- df_from_sim
  
  theor_mu <- true_mu
  theor_se <- true_sd / sqrt(n)
  
  x_lim <- range(c(df$means, theor_mu, mu0))
  pad   <- 0.06 * diff(x_lim)
  x_lim <- c(x_lim[1] - pad, x_lim[2] + pad)
  
  # поле сверху под легенду
  op <- par(no.readonly = TRUE)
  on.exit(par(op), add = TRUE)
  par(mar = c(4.5, 4.5, 7.0, 6.0), xpd = FALSE)
  
  hist(
    df$means,
    breaks = 30,
    freq   = FALSE,
    xlim   = x_lim,
    main   = "Распределение выборочных средних (оценок математического ожидания)",
    xlab   = "Выборочное среднее \\(\\bar{X}\\)"
  )
  
  # теоретическая плотность
  x_vals <- seq(x_lim[1], x_lim[2], length.out = 400)
  lines(x_vals, dnorm(x_vals, mean = theor_mu, sd = theor_se), lwd = 2)
  
  # линии μ0 и μ
  abline(v = mu0,     col = "red",     lwd = 2, lty = 2)     # μ0
  abline(v = theor_mu, col = "#1B5E20", lwd = 3, lty = 1)    # μ
  
  # подписи к линиям (внутри графика, аккуратно сверху)
  usr <- par("usr")
  y_top <- usr[4] - 0.03 * diff(usr[3:4])
  
  text(x = theor_mu, y = y_top, labels = expression(mu), col = "#1B5E20",
       pos = 3, cex = 0.9, font = 2)
  text(x = mu0, y = y_top, labels = expression(mu[0]), col = "red",
       pos = 3, cex = 0.9)
  
  # легенда ВНЕ графика сверху справа
  par(xpd = NA)
  legend(
    "topright",
    inset = c(0.02, -0.22),
    legend = c(expression(mu), expression(mu[0]), "Теоретич. плотность"),
    col    = c("#1B5E20", "red", "black"),
    lwd    = c(3, 2, 2),
    lty    = c(1, 2, 1),
    bty    = "n",
    horiz  = TRUE,
    cex    = 0.9
  )
  par(xpd = FALSE)
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
  shiny::validate(shiny::need(n_sim_sub > 0,
                              "В выбранном диапазоне нет доверительных интервалов для отображения"))
  
  plot(df$experiment, df$means,
       ylim = range(c(df$ci_low, df$ci_high, mu0, true_mu)),
       xlab = "Номер эксперимента",
       ylab = "Выборочное среднее и ДИ для математического ожидания",
       pch = 16,
       main = paste0("Доверительные интервалы для μ (уровень доверия = ",
                     conf_level, ")\nПоказаны эксперименты ",
                     rng[1], "–", rng[2]))
  
  for (i in seq_len(n_sim_sub)) {
    col_i <- if (df$covers_true_mu[i]) "gray50" else "red"
    segments(x0 = df$experiment[i], y0 = df$ci_low[i],
             x1 = df$experiment[i], y1 = df$ci_high[i],
             lwd = 2, col = col_i)
  }
  
  abline(h = true_mu, col = "#1B5E20", lty = 1, lwd = 3)
  abline(h = mu0,     col = "red",     lty = 2, lwd = 2)
  
  legend("topright",
         legend = c("Истинное математическое ожидание μ",
                    "Гипотетическое значение μ₀ (H₀)",
                    "Доверительный интервал покрывает μ",
                    "Доверительный интервал не покрывает μ"),
         lwd = c(3, 2, 2, 2),
         col = c("#1B5E20", "red", "gray50", "red"),
         lty = c(1, 2, 1, 1),
         bty = "n")
}


p_values_hist_plot <- function(df_from_sim, alpha_test){
  
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