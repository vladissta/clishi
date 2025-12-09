library(shiny)
# library(shinyjs)

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


one_exp_text_func <- function(df_from_sim, exp_id, alpha_test){
  df  <- df_from_sim
  id  <- exp_id
  # validate(
    # need(id >= 1 && id <= nrow(df), "Неверный номер эксперимента")
  # )
  
  row <- df[df$experiment == id, ]
  
  cat("Эксперимент (выборка) №", id, "\n\n")
  cat("Выборочное среднее \\bar{X}   =", round(row$mean, 4), "\n")
  cat("Выборочное SD                  =", round(row$sd, 4), "\n")
  cat("Стандартная ошибка SE          =", round(row$se, 4), "\n")
  cat("Доверительный интервал для μ   = [", round(row$ci_low, 4), ";", round(row$ci_high, 4), "]\n")
  cat("p-value t-теста для H₀: μ = μ₀ =", signif(row$p_value, 4), "\n\n")
  
  cat("Интерпретация для данной выборки:\n")
  
  cat("- Доверительный интервал для μ ",
      ifelse(row$covers_true_mu, "покрывает", "не покрывает"),
      " истинное математическое ожидание μ.\n", sep = "")
  
  cat("- Доверительный интервал ",
      ifelse(row$covers_mu0, "покрывает", "не покрывает"),
      " гипотетическое значение μ₀.\n", sep = "")
  
  cat("- На основе p-value = ", signif(row$p_value, 4),
      " нулевая гипотеза H₀: μ = μ₀ ",
      ifelse(row$reject_H0, "отвергается", "не отвергается"),
      " при уровне α = ", alpha_test, ".\n", sep = "")
}


se_summary_text_func <- function(df_from_sim, conf_level, true_sd, n) {
  df <- df_from_sim
  
  cat("Эмпирическое распределение выборочных средних:\n")
  cat("Среднее выборочных средних  E(\\bar{X})_emp =", round(mean(df$mean), 4), "\n")
  cat("Эмпирическое SD(\\bar{X}) =", round(sd(df$mean), 4),
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