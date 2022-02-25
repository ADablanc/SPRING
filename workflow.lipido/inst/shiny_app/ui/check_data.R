shinydashboard::tabItem(
  tabName = "check_data",
  shinydashboard::box(
    width = 12,
    shiny::tags$div(
      style = "float: left",
      shinyWidgets::pickerInput(inputId = "check_data_cpd",
                                label = "Choose compound",
                                choices = c(),
                                multiple = TRUE,
                                options = list(`live-search` = TRUE),
                                inline = TRUE
                                )
    ),
    shiny::column(
      width = 12,
      shiny::column(
        width = 6,
        shinycssloaders::withSpinner(
          plotly::plotlyOutput("check_data_heatmap")
        )
      ),
      shiny::column(
        width = 6,
        shinycssloaders::withSpinner(
          plotly::plotlyOutput("check_data_eic")
          # shiny::uiOutput("check_data_eics")
        )
      )
    )
  )
)
