#' @title Run Shiny app
#'
#' @description
#' Run a shiny application in the default web browser
#'
#' @param pwiz_dir `character(1)` path to the pwiz directory
#' @param log_file_path `character(1)` path to the file were to write the stderr
#'  & stdout
#'
#' @export
#' @examples
#' \dontrun{
#'      run_shiny_app()
#' }
run_shiny_app <- function(pwiz_dir = "pwiz", log_file_path = NULL) {
    if (class(pwiz_dir) != "character") {
        stop("pwiz_dir must be a filepath")
    } else if (length(pwiz_dir) > 1) {
        stop("pwiz_dir must be a unique filepath")
    } else if (!dir.exists(pwiz_dir)) {
        stop("the pwiz directory is not found")
    }
    if (!is.null(log_file_path)) {
        if (class(log_file_path) != "character") {
            stop("log_file_path must be NULL or a filepath")
        } else if (length(log_file_path) > 1) {
            stop("log_file_path must be a unique filepath")
        } else if (!dir.exists(dirname(log_file_path))) {
            stop("the directory of the log file doesn't exists")
        }
    }

    # test to only stop the process if no database is available
    capture.output(get_available_database())
    if (length(log_file_path) > 0) {
        log_file <- file(log_file_path, open = "wt")
        sink(log_file, type = "message")
        sink(log_file, type = "output")
    }

    # dont forget to get absolute path cause Shiny move
    # directly to the shiny folder by itself &
    # export in a global env the variable
    assign(
        "pwiz",
        normalizePath(pwiz_dir),
        envir = .SPRING_env
    )
    shiny::runApp(
        system.file("shiny_app", package = "SPRING"),
        launch.browser = TRUE
    )
    if (length(log_file_path) > 0) {
        sink(type = "message")
        sink(type = "output")
        close(log_file)
    }
}
