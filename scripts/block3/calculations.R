library(TrialSize)

create_result_sample_size_calc <- function(input) {
  
  tryCatch({
    if (input$data_type == "proportion") {
      delta <- input$p1 - input$p2
      res <- TwoSampleProportion.NIS(
        alpha = input$alpha,
        beta = input$beta,
        p1 = input$p1,
        p2 = input$p2,
        k = input$k,
        delta = delta,
        margin = input$margin
      )
      
      n1 <- ceiling(res[1])
      n2 <- ceiling(res[1] / input$k)
      total_end <- ceiling(n1 + n2)
      total_study <- ceiling(ceiling(n1 / 0.8) / 0.9) + ceiling(ceiling(n2 / 0.8) / 0.9)
      
      list(
        n1 = n1,
        n2 = n2,
        difference = delta,
        diff_name = "delta = p1 - p2",
        total_end = total_end,
        total_study = total_study
      )
      # if (input$hypothesis == "superiority") {
      #   border = input$margin
      #   
      # }
      
    } else if (input$data_type == "mean") {
      margin1 <- input$mu1 - input$mu2
      res <- TwoSampleMean.NIS(
        alpha = input$alpha,
        beta = input$beta,
        sigma = input$sigma,
        k = input$k,
        delta = input$delta1,
        margin = margin1
      )
      
      n1 <- ceiling(res[1])
      n2 <- ceiling(res[1] / input$k)
      total_end <- ceiling(n1 + n2)
      total_study <- ceiling(ceiling(n1 / 0.8) / 0.9) + ceiling(ceiling(n2 / 0.8) / 0.9)
      
      list(
        n1 = n1,
        n2 = n2,
        difference = margin1,
        diff_name = "margin = μ1 - μ2",
        total_end = total_end,
        total_study = total_study
      )
    }
  }, error = function(e) {
    return(list(error = e$message))
  })
}