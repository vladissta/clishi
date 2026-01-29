show_validation_error_modal <- function(error_text) {
  showModal(modalDialog(
    title = "Ошибка",
    error_text,
    easyClose = TRUE,
    footer = modalButton("Закрыть")
  ))
}


check_sim_args <- function(test_type, sim_args, alpha) {
  if (alpha < 0.001 || alpha > 0.999 || is.na(alpha)) {
    show_validation_error_modal("Уровень значимости α должен находится в промежутке от 0.001 до 0.999.")
  }
  
  if (test_type == "contingency_tables") {
    if (sim_args$design == "cross_sectional") {
      show_validation_error_modal("Вероятность исхода должна находится в промежутке от 0.001 до 0.999.")
    }
  
    if (sim_args$design %in% c("cross_sectional", "case_control")) {
      if (sim_args$exposure_probability < 0.001 || sim_args$exposure_probability > 0.999 || is.na(sim_args$exposure_probability)) {
        show_validation_error_modal("Вероятность воздействия должна находится в промежутке от 0.001 до 0.999.")
      }
    } 
    
    if (sim_args$design %in% c("case_control", "fisher")) {
      if (sim_args$event_proportion < 0.001 || sim_args$event_proportion > 0.999 || is.na(sim_args$event_proportion)) {
        show_validation_error_modal("Доля субъектов с исходом должна находится в промежутке от 0.001 до 0.999.")
      }
    } 
    
    if (sim_args$design == "fisher") {
      if (sim_args$exposure_proportion < 0.001 || sim_args$exposure_proportion > 0.999 || is.na(sim_args$exposure_proportion)) {
        show_validation_error_modal("Доля экспонированных субъектов должна находится в промежутке от 0.001 до 0.999.")
      }
    } 
    
    if (sim_args$design == "cohort") {
      if ((!is.null(sim_args$RR) && !is.na(sim_args$RR)) && (!is.null(sim_args$basic_risk) && !is.na(sim_args$basic_risk))) {
        if (sim_args$basic_risk < 0.001 || sim_args$basic_risk > 0.999 || is.na(sim_args$basic_risk)) {
          show_validation_error_modal("Базовый риск (вероятность) должен находится в промежутке от 0.001 до 0.999.")
        } 
        
        if (sim_args$RR < 0.1 || sim_args$RR > 10 || is.na(sim_args$RR)) {
          show_validation_error_modal("Отношение рисков должно находится в промежутке от 0.1 до 10.")
        }
        
        if (sim_args$RR * sim_args$basic_risk > 1 || is.na(sim_args$basic_risk))
        {
          show_validation_error_modal("Произведение отношения рисков на базовый риск должно быть меньше 1")
        }
      } else {
        
        if (sim_args$event_probability < 0.001 || sim_args$event_probability > 0.999 || is.na(sim_args$event_probability)) {
          show_validation_error_modal("Вероятность исхода должна находится в промежутке от 0.001 до 0.999.")
        }
        
        if (sim_args$exposure_proportion < 0.001 || sim_args$exposure_proportion > 0.999 || is.na(sim_args$exposure_proportion)) {
          show_validation_error_modal("Доля экспонированных субъектов должна находится в промежутке от 0.001 до 0.999.")
        }
      }
    } 
  }
}


