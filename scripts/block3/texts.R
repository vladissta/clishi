sample_size_results_text <- function(result_sample_size_calc){
  paste0(
    result_sample_size_calc$diff_name, " = ", round(result_sample_size_calc$difference, 3), "\n",
    "Группа терапии (n1): ", result_sample_size_calc$n1, "\n",
    "Группа контроля (n2): ", result_sample_size_calc$n2, "\n",
    "Общий объем выборки: ", result_sample_size_calc$total_end, "\n",
    "Объем выборки с учетом выбывших во время исследования (20%) и\nс учетом выбывших во время скрининга (10%): ", 
    result_sample_size_calc$total_study)
}