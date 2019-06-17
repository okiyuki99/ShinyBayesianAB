shinyUI(
  dashboardPage(title = "ShinyABTesting",
    dashboardHeader(title = logo_grey_light, titleWidth = 250),
    dashboardSidebar(
      collapsed = T,
      width = 250,
      sidebarMenu(
        menuItem("Top", icon = icon("th"), tabName = "menu_top"),
        menuItem("Github", icon = icon("github"), href = "https://github.com/okiyuki99/ShinyABTesting"),
        menuItem("About", icon = icon("question-circle-o"), tabName = "menu_about")
      )
    ),
    dashboardBody(
      shinyDashboardThemes(theme = "grey_dark"),
      tabItems(
        tabItem(tabName = "menu_top",
          fluidRow(
            box(title = "Experiment", width = 6, solidHeader = F, status = "info", 
              fluidRow(
                column(12, radioButtons('test_method', "Choose test", choices  = c("prop.test"), selected = "prop.test", inline = T))
              ),
              fluidRow(
                column(12, HTML("<hr>")),
                column(12, HTML("<p><b>Controll Group</b></p>")),
                column(3, numericInput('controll_trial', "# of trials", 250, min = 0, max = Inf, step = 1)),
                column(3, numericInput('controll_success', "# of success", 50, min = 0, max = Inf, step = 1)),
                column(12, HTML("<p><b>Treatment Group</b></p>")),
                column(3, numericInput('test_trial', "Test Trials", 300, min = 0, max = Inf, step = 1)),
                column(3, numericInput('test_success', "# of success", 70, min = 0, max = Inf, step = 1))
              ),
              fluidRow(
                column(12, numericInput('n_simulation', "Simulations", 10000, min = 100, max = 10000000, step = 100)),
                column(12, actionButton("btn_go", "Add Record"))
              )
            ),
            box(title = "Prior", width = 6, solidHeader = F, status = "info", 
                fluidRow(
                  column(6, numericInput('prior_alpha', "Prior:alpha", 20, min = 2, max = 100, step = 1)),
                  column(6, numericInput('prior_beta', "Prior:beta", 50, min = 2, max = 100, step = 1))
                ),
                fluidRow(
                  column(12, plotOutput("prior_distribution") %>% withSpinner(type = 5))
                )
            ),
            
            box(title = "Result", width = 12, solidHeader = F, status = "success", 
              fluidRow(
                column(6, plotOutput("plot_posterior") %>% withSpinner(type = 5)),
                column(6, plotOutput("plot_probability") %>% withSpinner(type = 5))
              ),
              fluidRow(
                column(12, tableOutput("kable_proportion")),
                column(1, actionButton("btn_remove", "Remove Record")),
                column(1, uiOutput("ui_dlbtn")),
                column(10, uiOutput("ui_unvisible_columns"))
              )
            )
          )
        ),
        tabItem(tabName = "menu_about",
          includeMarkdown("docs/about.md")
        )
      )
    )
  )
)