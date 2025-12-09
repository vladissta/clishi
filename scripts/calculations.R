true_mu_calc <- function(dist_type, params_list){
  req(dist_type)
  
  switch(dist_type,
         norm = {params_list$norm_mean},
         unif = {(params_list$unif_max - params_list$unif_min) / 2},
         exp = {1/(params_list$exp_rate)})
  
}

true_sd_calc <- function(dist_type, params_list){
  req(dist_type)
  
  switch(dist_type,
         norm = {params_list$norm_sd},
         unif = {(params_list$unif_max + params_list$unif_min) / sqrt(12)},
         exp = {1/(params_list$exp_rate)})
  
}


samples_values_calc <-
  function(n, n_sim, dist_type, params_list){
  req(n, n_sim, dist_type)

  data.frame(
    experiment = rep(1:n_sim, each = n),
    value = switch(dist_type,
                   norm = {rnorm(n*n_sim, mean = params_list$norm_mean, sd = params_list$norm_sd)},
                   unif = {
                     validate(need(params_list$unif_max > params_list$unif_min,
                                   "Для равномерного распределения необходимо, чтобы b > a"))
                     runif(n * n_sim, min = params_list$unif_min, max = params_list$unif_max)},
                   exp = {rexp(n*n_sim, rate = params_list$exp_rate)}))
  }


# simulate_fun_calc <- function(n, n_sim, conf_level, 
#                          alpha_test, alt_type, 
#                          mu0, true_mu, true_sd, x){
#   
#   # строка = отдельный эксперимент (выборка), столбец = наблюдение
#   Xmat     <- matrix(x, nrow = n_sim, ncol = n, byrow = TRUE)
#   
#   # Выборочные средние, выборочные SD, выборочные SE
#   means    <- rowMeans(Xmat)
#   sds      <- apply(Xmat, 1, sd)
#   se       <- sds / sqrt(n)
#   
#   # ДИ для математического ожидания по t-распределению
#   alpha_ci <- 1 - conf_level
#   t_crit   <- qt(1 - alpha_ci / 2, df = n - 1)
#   
#   ci_low   <- means - t_crit * se
#   ci_high  <- means + t_crit * se
#   
#   mu0      <- mu0
#   t_stat   <- (means - mu0) / se
#   
#   # p-value 
#   p_val <- switch(
#     alt_type,
#     "two.sided" = 2 * pt(-abs(t_stat), df = n - 1),
#     "greater"   = 1 - pt(t_stat, df = n - 1),
#     "less"      = pt(t_stat, df = n - 1)
#   )
#   
#   cover_mu0  <- (ci_low <= mu0) & (ci_high >= mu0)
#   cover_true <- (ci_low <= true_mu) & (ci_high >= true_mu)
#   reject_H0  <- p_val < alpha_test
#   
#   df <- data.frame(
#     experiment = 1:n_sim,
#     mean = means,
#     sd = sds,
#     se = se,
#     ci_low = ci_low,
#     ci_high = ci_high,
#     p_value = p_val,
#     covers_mu0 = cover_mu0,
#     covers_true_mu = cover_true,
#     reject_H0 = reject_H0
#   )
# 
#   return(
#   list(
#     df = df,
#     Xmat = Xmat,
#     true_mu = true_mu,
#     true_sd = true_sd,
#     n = n,
#     alpha_test = alpha_test,
#     conf_level = conf_level,
#     mu0 = mu0
#   )
#   )
# }

# --------------------------------------------------------------------------------

simulate_fun_calc_new <-function(simulated_values_df, n, n_sim, conf_level, 
                                 alpha_test, alt_type, 
                                 mu0, true_mu, true_sd){
  
  alpha_ci <- 1 - conf_level
  t_crit   <- qt(1 - alpha_ci / 2, 
                 df = n - 1)
  
  df <- simulated_values_df %>% 
    group_by(experiment) %>%
    summarize(
      means = mean(value),
      sds        = sd(value),
      se         = sds / sqrt(n)) %>% 
    mutate(
      ci_low     = means - t_crit * se,
      ci_high    = means + t_crit * se,
      t_stat     = (means - mu0) / se) %>% 
    mutate(
      p_value    = switch(alt_type,
                       "two.sided" = 2 * pt(-abs(t_stat), df = n - 1),
                       "greater"   = 1 - pt(t_stat, df = n - 1),
                       "less"      = pt(t_stat, df = n - 1)),
      covers_mu0  = (ci_low <= mu0) & (ci_high >= mu0),
      covers_true_mu = (ci_low <= true_mu) & (ci_high >= true_mu)) %>% 
    mutate(
      reject_H0  = p_value < alpha_test
    )
  
  return(df)
}
  
# --------------------------------------------------------------------------------
  

ci_table_calc <- function(df_from_sim){
  
  # res <- sim_res()
  df <- df_from_sim
  
  df$means    <- round(df$means, 4)
  df$se      <- round(df$se, 4)
  df$ci_low  <- round(df$ci_low, 4)
  df$ci_high <- round(df$ci_high, 4)
  df$p_value <- signif(df$p_value, 4)
  
  datatable(
    df,
    options = list(pageLength = 10),
    rownames = FALSE
  )
  
}
  