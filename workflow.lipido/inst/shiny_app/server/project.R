#' @title Project modal
#'
#' @description
#' Project modal UI definition
project_modal <- function() {
    shiny::modalDialog(
        shiny::tags$div(
            id = "project_bttn",
            shinyFiles::shinyFilesButton(
                id = "project_load",
                title = "Load project file",
                class = "bttn-simple bttn-md bttn-success bttn-no-outline",
                multiple = FALSE,
                label = "Load project file"
            ),
            shiny::tags$hr(),
            shinyWidgets::actionBttn(
                inputId = "project_create",
                label = "Create new project",
                style = "simple",
                color = "primary"
            )
        ),
        footer = NULL,
        size = "s"
    )
}

#' @title Create project modal
#'
#' @description
#' Create project modal definition
create_project_modal <- function() {
    shiny::modalDialog(
        shiny::textInput(
            inputId = "project_create_name",
            label = "Name of the project",
            placeholder = "test"
        ),
        shiny::tags$div(
            id = "project_create_path_div",
            shinyFiles::shinyDirButton(
                id = "project_create_path",
                label = "",
                icon = shiny::icon("upload"),
                class = "action-button bttn bttn-bordered",
                title = "select directory",
            ),
            shiny::verbatimTextOutput(
                outputId = "project_create_path_display",
                placeholder = TRUE
            )
        ),
        title = "Create project",
        footer = list(
            shinyWidgets::actionBttn(
                inputId = "project_create_cancel",
                label = "Cancel",
                style = "simple",
                color = "danger"
            ),
            shinyWidgets::actionBttn(
                inputId = "project_create_valid",
                label = "Valid",
                style = "simple",
                color = "success"
            )
        )
    )
}

#' @title Project name title
#'
#' @description
#' Update the project name in the top right corner of the app when the user
#' select a sqlite file or create a new one
#'
#' @param sqlite_path `reactiveValue` contains the path to the sqlite file
output$project_name <- shiny::renderText({
    if (!is.null(sqlite_path())) {
        tools::file_path_sans_ext(basename(sqlite_path()))
    } else {
        ""
    }
})

#' @title Project load bttn
#'
#' @description
#' Definition of the project load button. It allows only to select a sqlite file
shinyFiles::shinyFileChoose(
    input,
    "project_load",
    roots = volumes,
    filetypes = "sqlite"
)

#' @title Load project file
#'
#' @description
#' Load a project file. Called when the user click on the button "Load"
#' It require the sqlite file selected by the user
#' If valid it load in the reactive values the `SQLiteConnection`,
#'     the annotation `dataframe` & the spectra_infos `dataframe`
#' It split the annotation `dataframe` with the split_conflicts `function`
#'
#' @param sqlite_path `character(1)` path to the sqlite file
observeEvent(input$project_load, {
    params <- list(
        sqlite_path = shinyFiles::parseFilePaths(
            volumes,
            input$project_load
        )$datapath
    )
    browser()
    tryCatch({
        if (length(params$sqlite_path) == 0) {
            custom_stop("invalid", "no project file loaded")
        }
        sqlite_path(params$sqlite_path)
        db(db_connect(params$sqlite_path))
        ann <- dbReadTable(db(), "ann")
        ann <- split_conflicts(ann)
        ann(ann)
        conflict_id(if (length(ann$conflicts) > 0) 1 else 0)
        spectra_infos(dbReadTable(db(), "spectra_infos"))
        shiny::removeModal()
    }, invalid = function(i) {
        NULL
    }, error = function(e) {
        print("########## Project load")
        print(params)
        print(e)
        sqlite_path(NULL)
        db(NULL)
        ann(list(conflicts = list(), no_conflicts = data.frame()))
        spectra_infos(data.frame())
        conflict_id(0)
        sweet_alert_error(e$message)
    })
})

#' @title Show create project modal
#'
#' @description
#' Show the create project modal when user click on the button "create project"
shiny::observeEvent(input$project_create, {
    shiny::showModal(create_project_modal())
})

#' @title Project path bttn
#'
#' @description
#' Project path button definition. It allows the selection of a directory
shinyFiles::shinyDirChoose(input, "project_create_path", roots = volumes)

#' @title Project directory path label
#'
#' @description
#' Show the project directory choosen by the user in a span
#'
#' @param input$project_create_path `character(1)` directory path
output$project_create_path_display <- shiny::renderText({
    if (!is.integer(input$project_create_path)) {
        shinyFiles::parseDirPath(volumes, input$project_create_path)
    } else {
        "No directory selected"
    }
})

#' @title Cancel creation of project
#'
#' @description
#' Event when user click on the cancel button on the create project modal. It
#' will show the previous project modal
shiny::observeEvent(input$project_create_cancel, {
    shiny::showModal(project_modal())
})

#' @title Valid creation of project
#'
#' @description
#' Valid the creation of a project. It will create the sqlite path with the
#' name given by the user in the directory selected
#'
#' @param name `character(1)` name of the project
#' @param path `character(1)` directory path
shiny::observeEvent(input$project_create_valid, {
    params <- list(
        name = input$project_create_name,
        path = shinyFiles::parseDirPath(volumes, input$project_create_path)
    )
    tryCatch({
        inputs <- c("project_create_name", "project_create_path_div")
        conditions <- c(params$name != "", length(params$path) > 0)
        msgs <- c("you need to give a name", "you need to select a directory")
        check_inputs(inputs, conditions, msgs)

        sqlite_path <- file.path(params$path, paste0(params$name, ".sqlite"))
        sqlite_path(sqlite_path)
        db(db_connect(sqlite_path))
        shiny::removeModal()
    }, invalid = function(i) {
        NULL
    }, error = function(e) {
        print("########## Project create")
        print(params)
        print(e)
        sqlite_path(NULL)
        db(NULL)
        sweet_alert_error(e$message)
    })
})
