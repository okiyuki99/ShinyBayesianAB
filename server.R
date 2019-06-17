shinyServer(function(input, output, session){
  print("Initialize Start")
  values <- reactiveValues()
  values$data <- tibble()
  values$AB <- NULL
  
  # --------------------------
  # UI
  output$ui_dlbtn <- renderUI({
    if(nrow(values$data) > 0){
      downloadButton("dl_data", "Download")
    }
  })
  
  
  output$ui_controll <- renderUI({
    if(input$test_method == "prop.test"){
      fluidRow(
        column(3, numericInput('controll_trial', "# of trials", 250, min = 0, max = Inf, step = 1)),
        column(3, numericInput('controll_success', "# of success", 50, min = 0, max = Inf, step = 1))
      )
    }else if(input$test_method == "t.test"){
      fluidRow(
        column(3, numericInput('controll_trial', "# of trials", 250, min = 0, max = Inf, step = 1)),
        column(3, numericInput('controll_mean', "mean", 3, min = 0, max = Inf, step = 1)),
        column(3, numericInput('controll_sd', "sd", 1, min = 0, max = Inf, step = 1))
      )
    }
  })
  
  output$ui_treatment <- renderUI({
    if(input$test_method == "prop.test"){
      fluidRow(
        column(3, numericInput('treatment_trial', "# of trials", 250, min = 0, max = Inf, step = 1)),
        column(3, numericInput('treatment_success', "# of success", 60, min = 0, max = Inf, step = 1))
      )
    }else if(input$test_method == "t.test"){
      fluidRow(
        column(3, numericInput('treatment_trial', "# of trials", 250, min = 0, max = Inf, step = 1)),
        column(3, numericInput('treatment_mean', "mean", 3.3, min = 0, max = Inf, step = 1)),
        column(3, numericInput('treatment_sd', "sd", 1.2, min = 0, max = Inf, step = 1))
      )
    }
  })

  output$ui_prior <- renderUI({
    if(input$test_method == "prop.test"){
      fluidRow(
        column(6, numericInput('prior_alpha', "alpha", 20, step = 1)),
        column(6, numericInput('prior_beta', "beta", 50, step = 1))
      )
    }else if(input$test_method == "t.test"){
      fluidRow(
        column(3, numericInput('prior_mu', "mu", 3, step = 1)),
        column(3, numericInput('prior_lambda', "lambda", 1, step = 1)),
        column(3, numericInput('prior_alpha', "alpha", 1, step = 1)),
        column(3, numericInput('prior_beta', "beta", 1, step = 1))
      )
    }
  })  

  # --------------------------
  # Create Data for table and plot
  observeEvent(input$btn_go, {
    
    # from input
    if(input$test_method == "prop.test"){
      c_trial <- input$controll_trial
      c_success <- input$controll_success
      t_trial <- input$treatment_trial
      t_success <- input$treatment_success
      alpha <- input$prior_alpha
      beta <- input$prior_beta
      n_simulation <- input$n_simulation
      
      c_binom <- rbinom(c_trial, 1, c_success / c_trial)
      t_binom <- rbinom(t_trial, 1, t_success / t_trial)
      
      AB <- bayesTest(t_binom, c_binom,
                      priors = c("alpha" = alpha, "beta" = beta),
                      n_samples = n_simulation,
                      distribution = "bernoulli")
      
      p_value <- fisher.test(matrix(c(c_trial, t_trial, c_success, t_success), nrow=2))$p.value
      
      work_data <- tibble(c_trial = c_trial, c_success = c_success, 
                          t_trial = t_trial, t_success = t_success, 
                          fisher_test_pvalue = p_value,
                          prior_alpha = alpha, prior_beta = beta,
                          n_simulation = n_simulation,
                          win_prob = scales::percent(sum(AB$posteriors$Probability$A - AB$posteriors$Probability$B > 0) / n_simulation)
      )
      
    }else if(input$test_method == "t.test"){
      c_trial <- input$controll_trial
      c_mean <- input$controll_mean
      c_sd <- input$controll_sd
      t_trial <- input$treatment_trial
      t_mean <- input$treatment_mean
      t_sd <- input$treatment_sd
      
      mu <- input$prior_mu
      lambda <- input$prior_lambda
      alpha <- input$prior_alpha
      beta <- input$prior_beta
      n_simulation <- input$n_simulation
      
      c_norm <- rnorm(c_trial, c_mean, c_sd)
      t_norm <- rnorm(t_trial, t_mean, t_sd)
      
      AB <- bayesTest(t_norm, c_norm, 
                      priors = c("mu" = mu, "lambda" = lambda, "alpha" = alpha, "beta" = beta), 
                      n_samples = n_simulation, distribution = "normal")
      
      p_value <- t.test(t_norm, c_norm)$p.value
      
      work_data <- tibble(c_trial = c_trial, c_mean = c_mean, c_sd = c_sd,
                          t_trial = t_trial, t_mean = t_mean, t_sd = t_sd,
                          t_test_pvalue = p_value,
                          prior_mu = mu, prior_lambda = lambda,
                          prior_alpha = alpha, prior_beta = beta,
                          n_simulation = n_simulation,
                          win_prob = scales::percent(sum(AB$posteriors$Mu$A- AB$posteriors$Mu$B > 0) / n_simulation)
      )
    }

    # save
    values$AB <- AB
    
    # bind data
    values$data <- bind_rows(values$data, work_data) 
   
  })
  
  # --------------------------
  # Remove Row of Table
  # --------------------------
  observeEvent(input$btn_remove,{
    if(nrow(values$data) > 0){
      values$data <- dplyr::slice(values$data, 1:nrow(values$data)-1)
      if(nrow(values$data) == 0){
        values$data <- tibble()
      }
    }
  })
  
  # --------------------------
  # Download CSV Table
  # --------------------------
  output$dl_data <- downloadHandler(
    filename = function() { 
      paste0("data-", format(Sys.time(),"%Y%m%d-%H%M%S"), ".csv")
    },
    content = function(file) {
      write.csv(values$data, file)
    }
  )

  # --------------------------
  # Output : summary table
  # --------------------------
  output$kable_proportion <- function() {
    shiny::req(values$data)
    
    if(nrow(values$data) == 0){
      return(NULL)
    }
    
    values$data %>%
      save_to(num_cols, ncol) %>%
      knitr::kable(align = "r", escape = F) %>% 
      kable_styling(c("striped", "bordered"), full_width = T) %>%
      collapse_rows(columns = 1:num_cols, valign = "top")
  }

  # -----------------------------
  # Output : prior distribution
  # -----------------------------
  output$prior_distribution <- renderPlot({
    if(input$test_method == "prop.test"){
      shiny::req(input$prior_alpha)
      shiny::req(input$prior_beta)
      bayesAB::plotBeta(input$prior_alpha, input$prior_beta) + ggtitle(paste0('alpha =',input$prior_alpha, ' beta = ', input$prior_beta))
    
    }else if(input$test_method == "t.test"){
      shiny::req(input$prior_mu)
      shiny::req(input$prior_lambda)
      shiny::req(input$prior_alpha)
      shiny::req(input$prior_beta)
      bayesAB::plotNormalInvGamma(input$prior_mu, input$prior_lambda, input$prior_alpha, input$prior_beta) + 
        ggtitle(paste0('mu =',input$prior_mu, ' lambda = ', input$prior_lambda, ' alpha =',input$prior_alpha, ' beta = ', input$prior_beta))
    }
  })
  
  #-----------------------------
  # Output : posterior distribution
  # -----------------------------
  output$plot_posterior <- renderPlot({
    shiny::req(values$AB)
    if(input$test_method == "prop.test"){
      plot(values$AB)[[2]]$Probability
    }else if(input$test_method == "t.test"){
      plot(values$AB)[[2]]$Mu
    }
  })

  #-----------------------------
  # Output : probability
  # -----------------------------
  output$plot_probability <- renderPlot({
    shiny::req(values$AB)
    if(input$test_method == "prop.test"){
      plot(values$AB)[[3]]$Probability
    }else if(input$test_method == "t.test"){
      plot(values$AB)[[3]]$Mu
    }
  })

})