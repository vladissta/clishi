create_simulation_args <- function(input){
  switch(
    input$test_type,
    "t_one_sample" = list(
      fun = t_test_one_sample,
      distribution = input$dist_type_sim,
      mu = input$mu,
      sigma = input$sigma,
      mu_0 = input$mu_0
    ),
    
    "t_two_sample" = list(
      fun = t_test_two_sample,
      distribution = input$dist_type_sim,
      mu1 = input$mu1,
      sigma1 = input$sigma1,
      mu2 = input$mu2,
      sigma2 = input$sigma2
    ),
    
    "mann_whitney" = list(
      fun = mann_whitney,
      distribution = input$dist_type_sim,
      mu1 = input$mu1,
      sigma1 = input$sigma1,
      mu2 = input$mu2,
      sigma2 = input$sigma2
    ),
    
    "brunner_munzel" = list(
      fun = brunner_munzel,
      distribution = input$dist_type_sim,
      mu1 = input$mu1,
      sigma1 = input$sigma1,
      mu2 = input$mu2,
      sigma2 = input$sigma2
    ),
    
    "contingency_tables" = {
      req(input$trial_type, input$test_method)
      
      args <- list(
        fun = generation_binary_experiment,
        design = input$trial_type,
        method = input$test_method
      )
      
      if (input$trial_type == "cohort") {
        req(input$event_probability, input$exposure_proportion)
        args$event_probability <- input$event_probability
        args$exposure_proportion <- input$exposure_proportion
        
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
}