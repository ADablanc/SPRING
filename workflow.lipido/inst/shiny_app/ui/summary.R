shinydashboard::tabItem(
    tabName = "summary",
    shiny::downloadLink(
        outputId = "summary_export",
        label = ""
    ),

    shinydashboard::box(
        width = 12,
        shinyWidgets::radioGroupButtons(
            inputId = "summary_polarity",
            label = "Filter by polarity",
            choices = c("both", "positive", "negative"),
            checkIcon = list(
                yes = shiny::tags$i(
                    class = "fa fa-check square",
                    style = "color: steelblue"
                ),
                no = shiny::tags$i(
                    class = "fa fa-square-o",
                    style = "color: steelblue"
                )
            ),
            individual = TRUE
        ),
        shinycssloaders::withSpinner(
            DT::dataTableOutput("summary_table")
        )
    )
)
