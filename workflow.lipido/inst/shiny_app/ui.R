appname <- "Workflow Lipido"

header <- shiny::tags$header(
    class = "main-header", 
    shiny::tags$span(class = "logo", ""), 
    shinyjs::useShinyjs(), 
    shinyjs::extendShinyjs(
        text = "shinyjs.collapse = function(boxId) {
        $('#' + boxId).closest('.box').find('[data-widget=collapse]').click();
        }", functions = "collapse"),
    shinyFeedback::useShinyFeedback(),
    shinyWidgets::useSweetAlert(),
    bsplus::use_bs_tooltip(),
    # bsplus::use_bs_popover(),
    rintrojs::introjsUI(),
    shiny::includeCSS("www/workflow.lipido.css"),
    shiny::includeScript("www/workflow.lipido.js"),

    shiny::tags$nav(class = "navbar navbar-static-top", role = "navigation", 
        shiny::tags$form(class = "form-inline", 
            shiny::tags$div(class = "form-group", style = "float:left", 
                shiny::tags$span(id = "titleApp", class = "logo", appname)
            ), 
            shiny::tags$div(class = "form-group", style = "float:right;",
                shiny::tags$a(`data-toggle` = "tooltip", 
                    `data-placement` = "left", 
                    title = "Information on the different areas 
                        of the active window", 
                    shiny::actionButton("introjs", "", icon = icon("question"))
                )
            )
        )
    )
)

sidebar <- shinydashboard::dashboardSidebar(collapsed = TRUE, disable = TRUE, 
    shinydashboard::sidebarMenu(id = "tabs"
    )
)

body <- shinydashboard::dashboardBody(
    shiny::fluidPage(
        tags$div(id = "loader", class = "lds-dual-ring"), 
        shinyjs::hidden(
            shiny::div(id = "app-content",
                shinydashboard::tabItems(
                ),

                list(
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
