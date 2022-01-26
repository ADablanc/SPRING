#' @title Escape special characters
#'
#' @description
#' Escape all special characters
#' special characters are : .|()^{}+$*?[]
#'
#' @param string a character vector
#'
#' @return the character vector with the special characters escaped
#' @examples
#' \dontrun{escapeRegex("C:/msconvert?xcms.exe")}
escape_regex <- function(string) gsub(
    "([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", string)
