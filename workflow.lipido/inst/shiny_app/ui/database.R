shinydashboard::tabItem(
  tabName = "database",
  shinydashboard::box(
    width = 12,
    shinycssloaders::withSpinner(
      DT::dataTableOutput("database_table")
    )
  )
)
