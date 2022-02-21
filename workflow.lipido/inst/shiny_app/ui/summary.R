shinydashboard::tabItem(
    tabName = "summary",
    shiny::downloadLink(
        outputId = "summary_export",
        label = ""
    ),

    shinydashboard::box(
        width = 12,
        shinycssloaders::withSpinner(
            DT::dataTableOutput("summary_table")
        )
    )
)
