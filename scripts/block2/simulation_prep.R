create_simulation_args <- function(input, parameter_name = NULL) {
  
  args <- switch(
    input$test_type,
    
    "t_one_sample" = list(
      fun = t_test_one_sample,
      distribution = input$dist_type_sim,
      mu = input$mu,
      sigma = input$sigma,
      mu_0 = input$mu_0,
      sample_size = input$sample_size
    ),
    
    "t_two_sample" = list(
      fun = t_test_two_sample,
      distribution = input$dist_type_sim,
      mu1 = input$mu1,
      sigma1 = input$sigma1,
      mu2 = input$mu2,
      sigma2 = input$sigma2,
      sample_size = input$sample_size
    ),
    
    "mann_whitney" = list(
      fun = mann_whitney,
      distribution = input$dist_type_sim,
      mu1 = input$mu1,
      sigma1 = input$sigma1,
      mu2 = input$mu2,
      sigma2 = input$sigma2,
      sample_size = input$sample_size
    ),
    
    "brunner_munzel" = list(
      fun = brunner_munzel,
      distribution = input$dist_type_sim,
      mu1 = input$mu1,
      sigma1 = input$sigma1,
      mu2 = input$mu2,
      sigma2 = input$sigma2,
      sample_size = input$sample_size
    ),
    
    "contingency_tables" = {
      req(input$trial_type, input$test_method)
      
      args <- list(
        fun = generation_binary_experiment,
        design = input$trial_type,
        method = input$test_method,
        sample_size = input$sample_size
      )
      
      if (input$trial_type == "cohort") {
        req(input$exposure_proportion)
        args$exposure_proportion <- input$exposure_proportion
        if (input$cohort_method == 'rr_mode') {
          # Отношение рисков, rr_mode
          req(input$RR, input$basic_risk)
          args$RR <- input$RR
          args$basic_risk <- input$basic_risk
        } else {
          # Вероятность события, prob_mode
          req(input$event_probability)
          args$event_probability <- input$event_probability
        }
      } else if (input$trial_type == "case_control") {
        req(input$exposure_probability, input$event_proportion)
        args$exposure_probability <- input$exposure_probability
        args$event_proportion <- input$event_proportion
        
      } else if (input$trial_type == "cross_sectional") {
        req(input$event_probability, input$exposure_probability)
        args$event_probability <- input$event_probability
        args$exposure_probability <- input$exposure_probability
        
      } else if (input$trial_type == "fisher") {
        req(input$event_proportion, input$exposure_proportion)
        args$event_proportion <- input$event_proportion
        args$exposure_proportion <- input$exposure_proportion
      }
      
      args
    }
  )

  valid_params <- switch(
    input$test_type,
    "t_one_sample" = c("fun", "distribution", "mu", "sigma", "mu_0", "sample_size"),
    "t_two_sample" = c("fun", "distribution", "mu1", "sigma1", "mu2", "sigma2", "sample_size"),
    "mann_whitney" = c("fun", "distribution", "mu1", "sigma1", "mu2", "sigma2", "sample_size"),
    "brunner_munzel" = c("fun", "distribution", "mu1", "sigma1", "mu2", "sigma2", "sample_size"),
    "contingency_tables" = c("fun", "design", "method", "event_probability", 
                             "exposure_probability", "exposure_proportion", 
                             "event_proportion", "RR", "basic_risk", "sample_size"),
    character(0)
  )
  
  # Keep only valid parameters for this test type
  args <- args[names(args) %in% valid_params]
  
  # Remove the parameter that will vary in the grid
  if (!is.null(parameter_name) && parameter_name %in% names(args)) {
    args[[parameter_name]] <- NULL
  }
  
  args
}


create_defualt_grid <- function(session, parameter_name){
  
  if (parameter_name == "sample_size") {
    updateNumericInput(session, "parameter_from", value = 10,  min = 1, step = 1)
    updateNumericInput(session, "parameter_to",   value = 100, min = 2, step = 1)
    updateNumericInput(session, "parameter_by",   value = 10,  min = 1, step = 1)
    
  } else if (startsWith(parameter_name, "sigma")) {
    updateNumericInput(session, "parameter_from", value = 1, min = 0.01, step = 0.1)
    updateNumericInput(session, "parameter_to",   value = 10,  min = 0.01, step = 0.1)
    updateNumericInput(session, "parameter_by",   value = 1, min = 0.01, step = 0.1)
    
  } else if (startsWith(parameter_name, "mu")) {
    # mu, mu_0, mu1, mu2
    updateNumericInput(session, "parameter_from", value = 0,  step = 0.1)
    updateNumericInput(session, "parameter_to",   value = 1,  step = 0.1)
    updateNumericInput(session, "parameter_by",   value = 0.1, min = 0.01, step = 0.1)
    
  } else if (parameter_name %in% c(
    "event_probability", "exposure_probability",
    "exposure_proportion", "event_proportion",
    "basic_risk"
  )) {
    # probabilities/proportions in [0,1]
    updateNumericInput(session, "parameter_from", value = 0.05, min = 0, max = 1, step = 0.1)
    updateNumericInput(session, "parameter_to",   value = 0.95, min = 0, max = 1, step = 0.1)
    updateNumericInput(session, "parameter_by",   value = 0.05, min = 0.01, step = 0.1)
    
  } else if (identical(parameter_name, "RR")) {
    updateNumericInput(session, "parameter_from", value = 0.8, min = 0.01, step = 0.1)
    updateNumericInput(session, "parameter_to",   value = 3.0, min = 0.01, step = 0.1)
    updateNumericInput(session, "parameter_by",   value = 0.2, min = 0.01, step = 0.1)
  }
}
