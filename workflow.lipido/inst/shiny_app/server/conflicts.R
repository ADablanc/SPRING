#' @title Conflict left event
#'
#' @description
#' Update the conflict_id() reactive value when user click on the left arrow
#'
#' @param input$conflict_left `numeric(1)`
shiny::observeEvent(input$conflicts_left, {
    conflict_id(conflict_id() - 1)
})

#' @title Conflict right event
#'
#' @description
#' Update the conflict_id() reactive value when user click on the right arrow
#'
#' @param input$conflict_right `numeric(1)`
shiny::observeEvent(input$conflicts_right, {
    conflict_id(conflict_id() + 1)
})

#' @title Conflict ID event
#'
#' @description
#' Depending on the value of conflict_id it will enable or disable the arrows
#'
#' @param conflicts() `reactive value` group IDs where a conflict was detected
#' @param conflict_id `reactiveValue` ID of the conflict
shiny::observeEvent(c(conflicts(), conflict_id()), {
    if (conflict_id() <= 1) {
        shinyjs::disable("conflicts_left")
    } else {
        shinyjs::enable("conflicts_left")
    }
    if (conflict_id() >= length(conflicts())) {
        shinyjs::disable("conflicts_right")
    } else {
        shinyjs::enable("conflicts_right")
    }
})

#' @title Conflict number ID
#'
#' @description
#' Print the actual index of the conflicts
#'
#' @param conflicts() `reactive value` group IDs where a conflict was detected
#' @param conflict_id `reactiveValue` conflict ID
#'
#' @return `character(1)` with the form "conflict ID / number of conflicts"
output$conflicts_info <- shiny::renderText({
    if (length(conflicts()) > 0) {
        sprintf("%s / %s", conflict_id(), length(conflicts()))
    } else {
        ""
    }
})

#' @title Conflicts table
#'
#' @description
#' Show all possible annotation conflicts for the same peak group
#' The selection of a line will update the value "conflict_row_selected" with
#' the row index in order to update the plot "conflicts_ms"
#'
#' @param conflicts() `reactive value` group IDs where a conflict was detected
#' @param conflict_id `reactiveValue` conflict ID
#'
#' @return `DataTable` with columns :
#' \itemize{
#'     \item name `character` name of the possible annotation
#'     \item Already seen with `character` name of all the adducts were the
#'     annotation was already found without any conflict with another
#'     \item Already seen in nSamples `character` number of samples were the
#'     annotation was already found without any conflict with another
#'     \item Diff rT (sec) `numeric` difference in time retention compared to
#'     theoretical (in sec)
#'     \item Adduct `character` name of the adduct form
#'     \item nsamples `numeric` number of samples were founded
#'     \item Best score (%) `numeric` best isotopic score
#'     \item Best m/z dev (mDa) `numeric` best m/z deviation compared to
#'     theoretical
#'     \item Max iso `numeric` max number of isopologue identified
#' }
output$conflicts_table <- DT::renderDataTable({
    default_table <- data.frame(matrix(, nrow = 0, ncol = 8, dimnames = list(
        c(), c("Already seen with", "Conflicted adduct",
                 "Already seen in nSamples", "Conflicted nSamples",
                 "Diff rT (sec)", "Best score (%)", "Best m/z dev (mDa)",
                 "Max iso"))),
        check.names = FALSE)

    params <- list(
        conflicts = conflicts(),
        conflict_id = conflict_id()
    )
    tryCatch({
        if (length(params$conflicts) == 0) {
            custom_stop("invalid", "no conflicts")
        } else if (
            params$conflict_id > length(params$conflicts) |
            params$conflict_id < 1
        ) {
            stop("something wrongs with conflict_id")
        }

        params$conflict <- db_get_annotations(
            db(),
            group_ids = conflicts()[conflict_id()]
        )
        conflict <- params$conflict[, c("name", "rtdiff", "adduct", "nsamples",
                                 "best_score", "best_deviation_mz",
                                 "best_npeak")]

        params$ann <- db_get_annotations(db(), names = conflict$name)
        ann <- params$ann[!params$ann$group_id %in% conflicts(), , drop = FALSE]
        spectra_ids <- without_na(unlist(ann[, 14:ncol(ann)]))
        spectra_infos <- db_get_spectra_infos(db(), spectra_ids)
        info_conflict <- summarise_ann(ann, spectra_infos)
        info_conflict <- info_conflict[, c("name", "Adducts", "nSamples")]
        conflict <- merge(conflict, info_conflict, all.x = TRUE)

        cpd_names <- conflict$name
        conflict <- conflict[c("Adducts", "adduct", "nSamples", "nsamples",
                               "rtdiff", "best_score", "best_deviation_mz",
                               "best_npeak")]
        conflict[, c("rtdiff", "best_score")] <- round(
            conflict[, c("rtdiff", "best_score")])
        conflict$best_deviation_mz <- round(conflict$best_deviation_mz, 2)
        colnames(conflict) <- c("Already seen with", "Conflicted adduct",
                                "Already seen in nSamples",
                                "Conflicted nSamples", "Diff rT (sec)",
                                "Best score (%)", "Best m/z dev (mDa)",
                                "Max iso")

        conflict <- cbind(
            Valid = sapply(seq(nrow(conflict)), function(bttn_val)
                as.character(
                    shiny::actionButton(
                        inputId = paste("conflicts_table_bttn", bttn_val),
                        label = "",
                        icon = shiny::icon("check"),
                        class = "btn-success",
                        value = bttn_val
                    )
                )
            ),
            conflict
        )
        rownames(conflict) <- cpd_names
        conflict
    }, invalid = function(i) {
        print("########## conflicts_table")
        print(params)
        print(i)
        default_table
    }, error = function(e) {
        print("########## conflicts_table")
        print(params)
        print(e)
        sweet_alert_error(e$message)
        default_table
    })
}, server = isFALSE(getOption("shiny.testmode")),
    escape = FALSE,
    selection = "none",
    options = list(
        dom = "frtip",
        paging = FALSE,
        bFilter = FALSE,
        ordering = FALSE,
        columnDefs = list(
            list(
                className = "dt-head-center dt-center",
                targets = "_all",
                width = 80
            )
        ),
        language = list(
            emptyTable = "no conflicts found"
        ),
        initComplete = htmlwidgets::JS('
                function(settings, json) {
                    var table = settings.oInstance.api();
                    table.columns.adjust();
                    if (table.data().length > 0) {
                        // select first row
                        $(table.row(0).node()).addClass("selected");
                        Shiny.onInputChange(
                             "conflict_row_selected",
                             table.row(0).index() + 1
                         );
                    } else {
                        Shiny.onInputChange(
                             "conflict_name_selected",
                             0
                         );
                    }
                }
        ')
    ),
    callback = htmlwidgets::JS('
         table.on("click", "tbody tr", function() {
            if (table.data().length > 0) {
                $(table.rows(".selected").nodes()).removeClass("selected");
                $(table.row(this).node()).addClass("selected");
                 Shiny.onInputChange(
                     "conflict_row_selected",
                     table.row(this).index() + 1
                 );
            }
         })
    ')
)

#' @title Conflict MS
#'
#' @description
#' Mass Spectrum plot which contains all the adducts form of the annotation
#'  selected in the table "conflict_table"
#' The plot will have in positive the observed ions & in mirror (in negative)
#' the theoretical ions
#'
#' It have two JS functions :
#' \itemize{
#'     \item when the mouse is over an observed peak we will try to show the
#'     corresponding theoretical point
#'     the points are in this order :
#'     [observed$M, observed$M1, theoretical$M, theoretical$M1]
#'     so the theoretical must be at the same index but if we start in the
#'     middle of the array !
#'     for example for the peak observed M1, the index is 1
#'     so the theoretical must be at :
#'         1 + middle = 1 + (length / 2) = 1 + (4 / 2) = 1 + 2 = 3
#'     \item second is for hiding the annotations bind to the trace and to
#'     force the relayout between the xaxis range +/- 1
#' }
#'
#' @param db `reactive value` contains the pointer to the db
#' @param conflicts() `reactive value` group IDs where a conflict was detected
#' @param conflict_id `reactive value` conflict ID
#' @param input$conflict_row_selected `numeric` row ID selected in the conflict
#'  table
#'
#' @param `plotly`
output$conflicts_ms <- plotly::renderPlotly({
    params <- list(
        db = db(),
        conflicts = conflicts(),
        conflict_id = conflict_id(),
        conflict_row_selected = input$conflict_row_selected
    )
    tryCatch({
        if (length(params$conflicts) == 0) {
            custom_stop("invalid", "no conflicts")
        } else if (
            params$conflict_id > length(params$conflicts) |
            params$conflict_id < 1
        ) {
            stop("something wrongs with conflict_id")
        } else if (length(params$conflict_row_selected) == 0) {
            custom_stop("invalid", "no row selected")
        } else if (params$conflict_row_selected == "0") {
            custom_stop("invalid", "no rows in the table")
        }
        params$conflict <- db_get_annotations(
            db(),
            group_ids = conflicts()[conflict_id()]
        )
        plot_annotation_ms(
            db(),
            params$conflict[params$conflict_row_selected, "name"]
        )
    }, invalid = function(i) {
        print("########## conflicts_ms")
        print(params)
        print(i)
        plot_empty_MS()
    }, error = function(e) {
        print("########## conflicts_ms")
        print(params)
        print(e)
        sweet_alert_error(e$message)
        plot_empty_MS()
    })
})

#' @title Valid conflict event
#'
#' @description
#' Event when validation of a conflict by user
#' It will remove all conflicts for a group of annotations and only keep one
#' In consequence it will update the ann reactive value
#'
#' @param db `reactive value` pointer to the sqlite db
#' @param conflicts() `reactive value` group IDs where a conflict was detected
#' @param conflict_id `reactive value` conflict ID
#' @param input$conflicts_table_valid `numeric` contain the row ID of the
#'  conflict were the button valid was clicked
observeEvent(input$conflicts_table_valid, {
    params <- list(
        db = db(),
        conflicts = conflicts(),
        conflict_id = conflict_id(),
        conflicts_table_valid = input$conflicts_table_valid
    )
    tryCatch({
        params$conflict <- db_get_annotations(
            db(),
            group_ids = conflicts()[conflict_id()]
        )
        i <- as.numeric(params$conflicts_table_valid$value)
        db_resolve_conflict(
            db(),
            params$conflict[i, "group_id"],
            params$conflict[i, "name"]
        )
        conflicts(conflicts()[-conflict_id()])
        conflict_id(
            if (length(conflicts()) == 0) 0
            else if (conflict_id() == 1) 1
            else conflict_id() - 1
        )
        # to force all the outputs to reload if they use the data from the db
        toastr_success(sprintf(
            "%s %s annotated",
            params$conflict[i, "name"],
            params$conflict[i, "adduct"]
        ))
    }, error = function(e) {
        print("########## conflicts_table_valid")
        print(params)
        print(e)
        sweet_alert_error(e$message)
    })
})
