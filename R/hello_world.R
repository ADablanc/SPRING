#' @title Hello world
#'
#' @description
#' Just a test for the package. Will be removed very soon
#'
#' @return a list :
#' 		[[1]]
#' 			[1] "foo" "bar"
#'
#' 		[[2]]
#' 			[1] 0 1
#'
#' @export
#' @examples
#' hello_world()
hello_world <- function() {
    rcpp_hello_world()
}
