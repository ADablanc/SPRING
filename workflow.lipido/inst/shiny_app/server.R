server <- function(input, output, session) {
    source("server/reactiveVal.R", local = TRUE)$value

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
        if (!is.null(isolate(db()))) RSQLite::dbDisconnect(isolate(db()))
        gc()
        shiny::stopApp()
    })

    volumes <- c(home = tools::file_path_as_absolute("~"),
        shinyFiles::getVolumes()())

    source("server/func.R", local = TRUE)$value

    source("server/project.R", local = TRUE)$value

    # source("server/plots.R", local = TRUE)$value

    source("server/process.R", local = TRUE)$value

    # source("server/conflicts.R", local = TRUE)$value

    # source("server/check_data.R", local = TRUE)$value

    source("server/summary.R", local = TRUE)$value

    source("server/database.R", local = TRUE)$value

    # hide loader & show app div
    shinyjs::hide(id = "loader", anim = TRUE, animType = "fade")
    shinyjs::show("app-content")
}
