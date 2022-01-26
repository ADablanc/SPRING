server <- function(input, output, session) {

    #' @title Clean the RAM
    #' 
    #' @description
    #' Call garbage collector every 10 sec
    shiny::observe({
        shiny::invalidateLater(10000, session)
        gc()
    })

    #' @title Event when session is ended
    #'
    #' @description
    #' At session end it remove all object in environnement & call garbage collector
    session$onSessionEnded(function() {
        gc()
        shiny::stopApp()
    })

    #' @title Print in console in which user swicth to tab
    #'
    #' @description
    #' Print each time the tab which the user switch
    #'
    #' @params input$tabs string, name of the tab associated (see file ui.R)
    shiny::observeEvent(input$tabs, {
        gc()
        print('                                                            ')
        print('############################################################')
        print(
            stringr::str_trunc(
                paste0('TAB ######################### ', input$tabs, ' ###########################'),
                60
            )
        )
        print('############################################################')
    })

    source(file.path('server', 'func.R'), local = TRUE)$value

    # hide loader & show app div
    shinyjs::hide(id='loader', anim=TRUE, animType='fade')
    shinyjs::show("app-content")

}

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
					title = 'Information on the different areas of the active window', 
					shiny::actionButton('introjs', '', icon = icon('question'))
				)
			)
		)
	)
)

sidebar <- shinydashboard::dashboardSidebar(collapsed = TRUE, disable = TRUE, 
	shinydashboard::sidebarMenu(id = 'tabs'
	)
)

body <- shinydashboard::dashboardBody(
	shiny::fluidPage(
		tags$div(id = "loader", class = "lds-dual-ring"), 
		shinyjs::hidden(
			shiny::div(id = 'app-content',
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

shinyApp(ui = ui, server = server)
