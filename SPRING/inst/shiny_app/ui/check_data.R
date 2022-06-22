shinydashboard::tabItem(
    tabName = "check_data",
    shinydashboard::box(
        width = 12,
        shiny::column(
            width = 6,
            shinyWidgets::pickerInput(
                inputId = "check_data_cpd",
                label = "Choose compound",
                choices = c(),
                multiple = TRUE,
                options = list(`live-search` = TRUE),
                inline = TRUE
            ),
            shinycssloaders::withSpinner(
                plotly::plotlyOutput(
                    "check_data_heatmap",
                    height = "75vh"
                )
            )
        ),
        shiny::column(
            width = 6,
            shinycssloaders::withSpinner(
                plotly::plotlyOutput(
                    "check_data_eic_mzdev",
                    height = "75vh"
                )
            )
        )
    )
)
