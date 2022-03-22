#' @title Run Shiny app
#'
#' @description
#' Run a shiny application in the default web browser
#'
#' @param log_file_path `character(1)` path to the file were to write the stderr
#'  & stdout
#' @param converter `character(1)` path to msconvert.exe
#'
#' @export
#' @examples
#' \dontrun{
#'      run_shiny_app()
#' }
run_shiny_app <- function(log_file_path = NULL,
                          converter = "pwiz/msconvert.exe") {
    if (length(log_file_path) > 0) {
        log_file <- file(log_file_path, open = "wt")
        sink(log_file, type = "message")
        sink(log_file, type = "output")
    }
    # dont forget to get absolute path cause Shiny move
    # directly to the shiny folder by itself &
    # export in a global env the variable
    assign(
        "converter",
        normalizePath(converter),
        envir = .workflow_lipido_env
    )
    shiny::runApp(
        system.file("shiny_app", package = "workflow.lipido"),
        launch.browser = TRUE
    )
    if (length(log_file_path) > 0) {
        sink(type = "message")
        sink(type = "output")
        close(log_file)
    }
}
