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
      theme_grey_light,
      tabItems(
        tabItem(tabName = "menu_top",
          fluidRow(
            box(title = p(icon("cog"), "Experiment"), width = 6, solidHeader = F, status = "info", 
              fluidRow(
                column(12, radioButtons('test_method', "Choose test", choices  = c("prop.test","t.test"), selected = "prop.test", inline = T))
              ),
              fluidRow(
                column(12, HTML("<hr>")),
                column(12, HTML("<p><b>Controll Group</b></p>"))
              ),
              uiOutput("ui_controll"),
              fluidRow(
                column(12, HTML("<p><b>Treatment Group</b></p>"))
              ),
              uiOutput("ui_treatment"),
              fluidRow(
                column(2, numericInput('test_seed', "Set seed", 1)),
                column(10, numericInput('n_simulation', "Simulations", 10000, min = 100, max = 10000000, step = 100)),
                column(12, actionButton("btn_go", "Run"))
              )
            ),
            box(title = p(icon("info-circle"), "Prior"), width = 6, solidHeader = F, status = "info", 
                uiOutput("ui_prior"),
                fluidRow(
                  column(12, plotOutput("prior_distribution") %>% withSpinner(type = 5))
                )
            ),
            
            box(title = p(icon("area-chart"),"Result"), width = 12, solidHeader = F, status = "success", 
              fluidRow(
                column(6, plotOutput("plot_posterior") %>% withSpinner(type = 5)),
                column(6, plotOutput("plot_probability") %>% withSpinner(type = 5))
              )
            ),
            box(title = p(icon("history"),"History"), width = 12, solidHeader = F, status = "success", 
              fluidRow(
                column(12, tableOutput("kable_proportion")),
                column(12,
                  div(
                    div(style="display:inline-block", actionButton("btn_remove", "Remove Record")),
                    div(style="display:inline-block", uiOutput("ui_dlbtn"))
                  )
                )
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