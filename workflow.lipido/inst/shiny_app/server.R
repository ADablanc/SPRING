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
    #' At session end it remove all object in environnement & 
    #'      call garbage collector
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
        print("                                                            ")
        print("############################################################")
        print(stringr::str_trunc(
            paste0(
                "TAB ######################### ", 
                        input$tabs, 
                " ###########################"),
            60)
        )
        print("############################################################")
    })

    source("server/func.R", local = TRUE)$value

    # hide loader & show app div
    shinyjs::hide(id = "loader", anim = TRUE, animType = "fade")
    shinyjs::show("app-content")

}
