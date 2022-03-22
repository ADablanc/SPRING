appname <- "Workflow Lipido"

header <- shiny::tags$header(
    class = "main-header",
    shiny::tags$span(class = "logo", ""),
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(
        text = "shinyjs.collapse = function(boxId) {
            $('#' + boxId)
                .closest('.box')
                .find('[data-widget=collapse]')
                .click();
            }",
        functions = "collapse"
    ),
    shinyFeedback::useShinyFeedback(),
    shinyWidgets::useSweetAlert(),
    bsplus::use_bs_tooltip(),
    rintrojs::introjsUI(),
    shiny::includeCSS("www/workflow.lipido.css"),
    shiny::includeScript("www/workflow.lipido.js"),

    shiny::tags$nav(
        class = "navbar navbar-static-top",
        role = "navigation",
        shiny::tags$form(
            class = "form-inline",
            shiny::tags$div(
                class = "form-group",
                style = "float:left",
                shiny::tags$span(
                    id = "titleApp",
                    class = "logo",
                    appname
                )
            ),
            shiny::tags$div(
                class = "form-group",
                style = "float:right;",
                shiny::tags$a(
                    `data-toggle` = "tooltip",
                    `data-placement` = "left",
                    title = "Information on the different areas
                        of the active window",
                    shiny::actionButton(
                        "introjs",
                        "",
                        icon = icon("question")
                    )
                )
            ),
            shiny::tags$span(
                class = "logo project_logo",
                shiny::textOutput(
                    outputId = "project_name",
                    inline = TRUE
                )
            )
        )
    )
)

sidebar <- shinydashboard::dashboardSidebar(
    collapsed = TRUE,
    disable = TRUE,
    shinydashboard::sidebarMenu(
        id = "tabs",
        shinydashboard::menuItem(
            "Process",
            icon = shiny::icon("cog"),
            tabName = "process"
        ),
        shinydashboard::menuItem(
            text = "Explore data",
            icon = shiny::icon("table"),
            shinydashboard::menuSubItem(
                "Resolve conflicts",
                tabName = "conflicts"
            ),
            shinydashboard::menuSubItem(
                "Check data",
                tabName = "check_data"
            ),
            shinydashboard::menuSubItem(
                "Summary",
                tabName = "summary"
            )
        ),
        shinydashboard::menuItem(
            "Database",
            icon = shiny::icon("database"),
            tabName = "database"
        )
    )
)

body <- shinydashboard::dashboardBody(
    shiny::fluidPage(
        tags$div(
            id = "loader",
            class = "lds-dual-ring"
        ),
        shinyjs::hidden(
            shiny::div(
                id = "app-content",
                shinydashboard::tabItems(
                    # source("ui/process.R", local = TRUE)$value,
                    # source("ui/conflicts.R", local = TRUE)$value,
                    # source("ui/check_data.R", local = TRUE)$value,
                    # source("ui/summary.R", local = TRUE)$value,
                    # source("ui/database.R", local = TRUE)$value
                )
            )
        )
    )
)

ui <- shinydashboard::dashboardPage(
    title = appname,
    header,
    sidebar,
    body
)
