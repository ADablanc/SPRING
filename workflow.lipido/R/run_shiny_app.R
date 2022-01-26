#' @title Run Shiny app
#'
#' @description
#' Run a shiny application in the default web browser
#' All outputs are redirect to a file by default in the home directory
#' 
#' @param log_file_path path to the file were to write the stderr & stdout
#' 
#' @export
#' @examples
#' \dontrun{
#'      run_shiny_app()
#' }
run_shiny_app <- function(log_file_path = "~/.workflow.lipido.log") {
    if (length(log_file_path) > 0) {
        log_file <- file(log_file_path, open = "wt")
        sink(log_file, type = "message")
        sink(log_file, type = "output")
    }
    shiny::runApp(system.file("shiny_app", package = "workflow.lipido"))
    if (length(log_file_path) > 0) {
        sink(type = "message")
        sink(type = "output")
        close(log_file)
    }
}
