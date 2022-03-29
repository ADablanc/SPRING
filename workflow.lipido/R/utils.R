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
