hypothesis_text_func <- function(mu0, alt_type) {
  req(mu0)
  
  alt_symbol <- switch(
    alt_type,
    "two.sided" = "\\neq",
    "greater"   = ">",
    "less"      = "<"
  )
  
  withMathJax(HTML(paste0(
    "<h4>Нулевая гипотеза:</h4>",
    "$$H_0: \\mu = ", mu0, ".$$",
    "<h4>Альтернативная гипотеза:</h4>",
    "$$H_1: \\mu ", alt_symbol, " ", mu0, ".$$"
  )))
}

one_exp_text_func <- function(df_from_sim, exp_id, alpha_test,
                              mu0, true_mu, conf_level, use_true_mu){
  
  validate(
    need(exp_id >= 1 && exp_id <= nrow(df_from_sim), "Неверный номер эксперимента")
  )
  
  row <- df_from_sim[exp_id, , drop = FALSE]
  
  # --- аккуратно забираем поля (названия могут отличаться) ---
  xbar <- if ("means" %in% names(row)) row$means else row$mean
  s    <- if ("sd" %in% names(row)) row$sd else NA
  se   <- if ("se" %in% names(row)) row$se else NA
  ci_l <- if ("ci_low" %in% names(row)) row$ci_low else row$ci_l
  ci_u <- if ("ci_high" %in% names(row)) row$ci_high else row$ci_u
  pval <- if ("p_value" %in% names(row)) row$p_value else row$pval
  
  covers_mu    <- isTRUE(row$covers_true_mu)     # должно быть в df_from_sim
  covers_mu0   <- (mu0 >= ci_l && mu0 <= ci_u)   # считаем сами, чтобы не зависеть от df
  reject_H0    <- (pval < alpha_test)
  
  # Ошибка I рода возможна ТОЛЬКО если H0 истинна (μ0 = μ), т.е. use_true_mu = TRUE
  type1_error  <- isTRUE(use_true_mu) && reject_H0
  
  # --- Заголовок/данные эксперимента ---
  cat("Эксперимент (выборка) №", exp_id, "\n", sep = "")
  cat("Уровень доверия CI =", conf_level, "\n")
  cat("Уровень значимости α =", alpha_test, "\n\n")
  
  cat(sprintf("Выборочное среднее x̄ = %.4f\n", xbar))
  if (!is.na(s))  cat(sprintf("Выборочное SD = %.4f\n", s))
  if (!is.na(se)) cat(sprintf("Стандартная ошибка SE = %.4f\n", se))
  cat(sprintf("Доверительный интервал для μ = [%.4f; %.4f]\n", ci_l, ci_u))
  cat(sprintf("p-value t-теста для H₀: μ = μ₀ = %.4f : p = %.6f\n\n", mu0, pval))
  
  # --- Универсальные фразы про CI ---
  ci_mu_line  <- if (covers_mu)  "• Доверительный интервал для μ покрывает истинное математическое ожидание μ."
  else             "• Доверительный интервал для μ НЕ покрывает истинное математическое ожидание μ."
  ci_mu0_line <- if (covers_mu0) "• Доверительный интервал покрывает гипотетическое значение μ₀."
  else             "• Доверительный интервал НЕ покрывает гипотетическое значение μ₀."
  
  # --- Решение по H0 ---
  h0_line <- if (reject_H0) {
    sprintf("• При уровне значимости α = %.3f нулевая гипотеза H₀: μ = μ₀ отвергается (p = %.6f).",
            alpha_test, pval)
  } else {
    sprintf("• При уровне значимости α = %.3f нулевая гипотеза H₀: μ = μ₀ не отвергается (p = %.6f).",
            alpha_test, pval)
  }
  
  # --- 3 сценария интерпретации ---
  cat("Интерпретация для текущей выборки:\n")
  cat(ci_mu_line, "\n")
  cat(ci_mu0_line, "\n")
  cat(h0_line, "\n")
  
  if (isTRUE(use_true_mu)) {
    # Сценарии при H0 истинна
    if (type1_error) {
      cat("• Зафиксирована ошибка I рода: H₀ истинна (μ₀ = μ), но была отвергнута.\n")
    } else {
      cat("• Ошибки I рода нет: H₀ истинна (μ₀ = μ) и не была отвергнута.\n")
    }
  } else {
    # Сценарии при μ0 != μ (то есть H0 обычно ложная, но пользователь мог поставить любое mu0)
    cat("• Ошибка I рода в этом режиме НЕ оценивается, потому что μ₀ не приравнен к μ.\n")
  }
  
  # --- Общая информация ---
  cat("\nОбщая информация:\n")
  cat("• Истинное математическое ожидание μ является фиксированным, но неизвестным параметром генеральной совокупности и не является случайной величиной.\n")
  cat("• Каждый построенный доверительный интервал в конкретном эксперименте либо покрывает истинное значение μ, либо не покрывает его.\n")
  cat(sprintf("• Уровень доверия %.2f означает, что при многократном повторении эксперимента примерно %.0f%% доверительных интервалов будут покрывать μ,\n",
              conf_level, 100*conf_level))
  cat("  если выполнены допущения, заложенные в построение интервала.\n")
}



se_summary_text_func <- function(df_from_sim, conf_level, true_sd, n) {
  df <- df_from_sim
  
  cat("Эмпирическое распределение выборочных средних:\n")
  cat("Среднее выборочных средних  E(X̄)_emp =", round(mean(df$means), 4), "\n")
  cat("Эмпирическое SD(X) =", round(sd(df$means), 4),
      "   (теоретическая стандартная ошибка SE =",
      round(true_sd / sqrt(n), 4), ")\n\n")
  
  cat("Средняя выборочная оценка стандартной ошибки SE_i:\n")
  cat("mean(SE_i) =", round(mean(df$se), 4), "\n\n")
  
  cat("Доля доверительных интервалов, покрывающих гипотетическое значение μ₀:\n")
  cat("coverage(μ₀) =", round(mean(df$covers_mu0), 4), "\n\n")
  
  cat("Доля доверительных интервалов, покрывающих истинное математическое ожидание μ:\n")
  cat("coverage(μ_true) =", round(mean(df$covers_true_mu), 4),
      "   при уровне доверия =", conf_level, "\n\n")
  
  cat("Интерпретация:\n")
  cat("- Истинное математическое ожидание μ является фиксированным параметром и не является случайной величиной.\n")
  cat("- Каждый построенный доверительный интервал либо покрывает μ, либо не покрывает его.\n")
  cat("- Уровень доверия", conf_level,
      "означает, что при многократном повторении эксперимента\n")
  cat("примерно", conf_level * 100, "% построенных доверительных интервалов будут покрывать μ,\n")
  cat("если все допущения сделанные в процессе оценки данных интервалов, соблюдаются.\n")
}

p_values_summary_text_func <- function(df_from_sim, alpha_test){
  # res <- sim_res()
  df <- df_from_sim
  
  cat("Доля экспериментов с p < α (отклоняем H₀: μ = μ₀):\n")
  cat("freq(reject H₀) =", round(mean(df$reject_H0), 4),
      " при α =", alpha_test, "\n\n")
  
  cat("Интерпретация:\n")
  cat("- если...")
  cat("- если...")
}

t_test_crit_text_func <- function(n, alpha, alt_type){
  
  # req(input$n, input$alpha, input$alt_type)
  # req(df_from_sim())
  
  dfree <- n - 1
  
  if (alt_type == "two.sided") {
    tcrit <- qt(1 - alpha / 2, dfree)
    cat("Критические значения (двусторонний тест): ±", round(tcrit, 4), "\n")
    cat("Отклоняем H₀, если |t_obs| >", round(tcrit, 4), "\n")
  } else if (alt_type == "greater") {
    tcrit <- qt(1 - alpha, dfree)
    cat("Критическое значение (правосторонний тест):", round(tcrit, 4), "\n")
    cat("Отклоняем H₀, если t_obs >", round(tcrit, 4), "\n")
  } else {
    tcrit <- qt(alpha, dfree)
    cat("Критическое значение (левосторонний тест):", round(tcrit, 4), "\n")
    cat("Отклоняем H₀, если t_obs <", round(tcrit, 4), "\n")
  }
  
  cat("\nПримечание:\n")
  cat("- Красная область на графике — критическая область уровня α.\n")
  cat("- Синяя линия — наблюдаемое значение t-статистики для выбранной выборки.\n")
}