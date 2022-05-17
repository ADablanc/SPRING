#' @title Escape special characters
#'
#' @description
#' Escape all special characters
#' special characters are : .|()^{}+$*?[]
#'
#' @param string `character`
#'
#' @return `character`
#' @examples
#' \dontrun{escapeRegex("C:/msconvert?xcms.exe")}
escape_regex <- function(string) {
    gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", string)
}

#' @title Remove NA
#'
#' @description
#' Remove all NA values in a vector
#'
#' @param x `vector`
#'
#' @return `vector` without NA values
without_na <- function(x) {
    x[which(!is.na(x))]
}

#' @title Get available databases
#'
#' @description
#' Get all the names of the available database in package
#'
#' @return `character` vector with the name of the databases
#' @export
get_available_database <- function() {
    databases <- tools::file_path_sans_ext(list.files(system.file(
        "extdata",
        "database",
        package = "workflow.lipido"
    )))
    if (length(databases) == 0) {
        stop("No database is available in application ")
    } else {
        databases
    }
}
