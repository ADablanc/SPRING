shinydashboard::tabItem(
    tabName = "conflicts",
    shinydashboard::box(
        width = 12,
        shiny::actionButton(
            inputId = "conflicts_left",
            label = "",
            icon = shiny::icon("arrow-left")#,
            #style = "float: left;"
        ),
        shiny::actionButton(
            inputId = "conflicts_right",
            label = "",
            icon = shiny::icon("arrow-right")#,
            #style = "float: right;"
        ),
        shiny::tags$span(
            class = "logo",
            shiny::textOutput("conflicts_info", inline = TRUE)
        ),
        shinycssloaders::withSpinner(
             DT::dataTableOutput("conflicts_table")
        )
    ),

    shinydashboard::box(
        title = "Meanned mass spectrum",
        plotly::plotlyOutput("conflicts_ms")
    )
)
