shinydashboard::tabItem(
    tabName = "summary",
    shiny::downloadLink(
        outputId = "summary_export",
        label = ""
    ),

    shinydashboard::box(
        width = 12,
        shinyWidgets::radioGroupButtons(
            inputId = "summary_by",
            label = "Report intensities",
            choices = setNames(
                c("referent", "all"),
                c("by referent ion", "sum all ions")),
            individual = TRUE,
            checkIcon = list(
                yes = shiny::tags$i(
                    class = "fa fa-circle",
                    style = "color: steelblue"
                ),
                no = shiny::tags$i(
                    class = "fa fa-circle-o",
                    style = "color: steelblue"
                )
            )
        ),
        shinycssloaders::withSpinner(
            DT::dataTableOutput("summary_table")
        )
    )
)
