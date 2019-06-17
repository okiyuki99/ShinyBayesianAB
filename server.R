shinyServer(function(input, output, session){
  print("Initialize Start")
  values <- reactiveValues()
  values$data <- tibble()
  values$AB <- NULL
  
  # --------------------------
  # Display Lift from as_is to to_be
  output$ui_dlbtn <- renderUI({
    if(nrow(values$data) > 0){
      downloadButton("dl_data", "Download")
    }
  })
  

  # --------------------------
  # Create Data for table and plot
  observeEvent(input$btn_go, {
    
    # from input
    c_trial <- input$controll_trial
    c_success <- input$controll_success
    t_trial <- input$test_trial
    t_success <- input$test_success
    alpha <- input$prior_alpha
    beta <- input$prior_beta
    n_simulation <- input$n_simulation
    
    c_binom <- rbinom(c_trial, 1, c_success / c_trial)
    t_binom <- rbinom(t_trial, 1, t_success / t_trial)

    AB <- bayesTest(t_binom, c_binom,
                    priors = c("alpha" = alpha, "beta" = beta),
                    n_samples = n_simulation,
                    distribution = "bernoulli")
    values$AB <- AB
    
    # save
    work_data <- tibble(c_trial = c_trial, c_success = c_success, 
                               t_trial = t_trial, t_success = t_success, 
                               prior_alpha = alpha, prior_beta = beta,
                               n_simulation = n_simulation,
                               win_prob = scales::percent(sum(AB$posteriors$Probability$A - AB$posteriors$Probability$B > 0) / n_simulation))
    
    # bind data
    values$data <- bind_rows(values$data, work_data) 
   
  })
  
  # --------------------------
  # Remove Row of Table
  # --------------------------
  observeEvent(input$btn_remove,{
    if(nrow(values$data) > 0){
      values$data <- dplyr::slice(values$data, 1:nrow(values$data)-1)
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
    bayesAB::plotBeta(input$prior_alpha, input$prior_beta) + ggtitle(paste0('alpha =',input$prior_alpha, ' beta = ', input$prior_beta))
  })
  
  #-----------------------------
  # Output : posterior distribution
  # -----------------------------
  output$plot_posterior <- renderPlot({
    shiny::req(values$AB)
    plot(values$AB)[[2]]$Probability
  })

  #-----------------------------
  # Output : probability
  # -----------------------------
  output$plot_probability <- renderPlot({
    shiny::req(values$AB)
    plot(values$AB)[[3]]$Probability
  })

})