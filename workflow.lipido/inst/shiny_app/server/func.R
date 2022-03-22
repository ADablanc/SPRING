#' @title Send toastr error msg
#'
#' @description
#' Send predefined toastr error msg
#'
#' @param title `character(1)` title of the msg
#' @param msg `character(1)` content of the msg
#'
#' @examples
#' \dontrun{toastr_error("Aaaaaaargh", "A zombie bite me...")}
toastr_error <- function(msg = "")
    shinyFeedback::showToast(
        type = "error",
        msg,
        .options = list(
            closeButton = TRUE,
            newestOnTop = TRUE,
            progressBar = FALSE,
            preventDuplicates = TRUE,
            positionClass = "toast-top-center"
        )
    )

#' @title Send toastr success msg
#'
#' @description
#' Send predefined toastr success msg
#'
#' @param `character(1)` title of the msg
#' @param msg `character(1)` content of the msg
#'
#' @examples
#' \dontrun{toastr_success("yeeeeees", "Just dodged a zombie...")}
toastr_success <- function(msg = "")
    shinyFeedback::showToast(
        type = "success",
        msg,
        .options = list(
            closeButton = TRUE,
            newestOnTop = TRUE,
            progressBar = FALSE,
            preventDuplicates = TRUE,
            positionClass = "toast-top-center"
        )
    )

#' @title Send toastr warning msg
#'
#' @description
#' Send predefined toastr warning msg
#'
#' @param `character(1)` title of the msg
#' @param msg `character(1)` content of the msg
#'
#' @examples
#' \dontrun{toastr_error("Ohoh", "Is it a zombie ?...")}
toastr_warning <- function(msg = "")
    shinyFeedback::showToast(
        type = "warning",
        msg,
        .options = list(
            closeButton = TRUE,
            newestOnTop = TRUE,
            progressBar = FALSE,
            preventDuplicates = TRUE,
            positionClass = "toast-top-center"
        )
    )

#' @title Send sweet alert error msg
#'
#' @description
#' Send predefined sweet alert error msg
#'
#' @param `character(1)` title of the msg
#' @param msg `character(1)` content of the msg
#'
#' @examples
#' \dontrun{sweet_alert_error("Garrrrrrrr", "Braiiiinn...")}
sweet_alert_error <- function(title = "", msg = "")
    shinyWidgets::sendSweetAlert(
        session,
        html = TRUE,
        type = "error",
        title = title,
        text = shiny::tags$div(
            msg,
            shiny::tags$br(),
            shiny::tags$a(
                href = paste0(
                    "mailto:sebastien.hutinet@inserm.fr?",
                    "subject=Describe header of error",
                    if (exists("log_file_path")) sprintf(
                        "&body=don\'t forget to attach error log (in \"%s\")",
                        log_file_path)
                    else NULL
                ),
                "Contact me"
            )
        )
    )

condition <- function(subclass, message, call=sys.call(-1), ...)
    structure(
        class = c(subclass, "condition"),
        list(message = message, call = call),
        ...
    )
custom_stop <- function(subclass, message, call=sys.call(-1), ...)
    stop(condition(c(subclass, "error"), message, call = call, ...))

#' @title Check inputs
#'
#' @description
#' Check conditon for an input
#' If false call shinyFeedback on input, send a toastr & call an invalid error
#'
#' @param inputs `character vector` inputs name
#' @param condition `character vector` condition to respect for each input
#' @param msg `character vector`message to display in toastr & shinyFeedback
#' for each error
check_inputs <- function(inputs, conditions, msgs) {
    for (i in seq(length(inputs))) {
        if (!conditions[[i]]) {
            print(msgs[[i]])
            show_feedback(inputs[[i]], msgs[[i]])
            toastr_error(msgs[[i]])
        } else {
            hide_feedback(inputs[[i]])
        }
    }
    if (any(!conditions)) custom_stop("invalid", "invalid inputs")
}

show_feedback <- function(input, msg) {
    if (input != "") {
        shinyFeedback::showFeedbackDanger(input, msg)
    }
}
hide_feedback <- function(input) {
    if (input != "") {
        shinyFeedback::hideFeedback(input)
    }
}
