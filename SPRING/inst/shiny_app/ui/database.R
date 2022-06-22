shinydashboard::tabItem(
  tabName = "database",
  shinydashboard::box(
    width = 12,
    # shiny::selectizeInput(
    shinyWidgets::pickerInput(
      inputId = "database_name",
      label = "Database: ",
      choices = "",
      width = "auto"
    ),
    shinycssloaders::withSpinner(
      DT::dataTableOutput("database_table")
    )
  )
)
