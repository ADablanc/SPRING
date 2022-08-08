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
#' Show all possible annotation conflicts for the same pcgroup
#' The selection of a line will update the value "conflict_name_selected" with
#' the name of the compound in order to update the plot "conflicts_ms" & the
#' plot "conflicts_eic"s
#'
#' @param conflicts() `reactive value` group IDs where a conflict was detected
#' @param conflict_id `reactiveValue` conflict ID
#'
#' @return `DataTable` with columns :
#' \itemize{
#'     \item nSamples `numeric` number of samples were founded
#'     \item rT (min) `numeric` rT of the annotation
#'     \item Diff rT (sec) `numeric` difference in time retention compared to
#'     theoretical (in sec)
#'     \item Adducts `character`
#'     \item name `character` name of the possible annotation
#'     \item Best score (%) `numeric` best isotopic score
#'     \item Best m/z dev (mDa) `numeric` best m/z deviation compared to
#'     theoretical
#'     \item Max iso `numeric` max number of isopologue identified
#' }
output$conflicts_table <- DT::renderDataTable({
    default_table <- data.frame(matrix(, nrow = 0, ncol = 7, dimnames = list(
        c(), c("nSamples", "rT (min)", "Diff rT (sec)", "Adducts",
               "Best score (%)", "Best m/z dev (mDa)", "Max iso"))),
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

        ann <- db_get_annotations(
            db(),
            pcgroup_ids = conflicts()[conflict_id()]
        )
        nsamples <- db_get_nsamples(db())
        spectra_ids <- na.omit(unlist(
            ann[, (ncol(ann) - nsamples + 1):ncol(ann)]))
        spectra_infos <- db_get_spectra_infos(db(), spectra_ids)
        ann <- summarise_ann(ann, spectra_infos, nsamples)$resume
        ann <- ann[, c("Name", "nSamples", "rT (min)", "Diff rT (sec)",
                       "Adducts", "Best score (%)", "Best m/z dev (mDa)",
                       "Max iso")]
        ann <- cbind(
            Valid = sapply(seq(nrow(ann)), function(bttn_val) {
                as.character(
                    shiny::actionButton(
                        inputId = paste("conflicts_table_bttn-", bttn_val),
                        label = "",
                        icon = shiny::icon("check"),
                        class = "btn-success",
                        value = as.character(ann[bttn_val, "Name"])
                    )
                )
            }),
            ann
        )
        rownames(ann) <- as.character(ann$Name)
        ann[, -2]
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
                             "conflict_name_selected",
                             table.cell(0, 0).data()
                         );
                    } else {
                        Shiny.onInputChange(
                             "conflict_name_selected",
                             ""
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
                     "conflict_name_selected",
                     table.cell(".selected", 0).data()
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
#' @param input$conflict_name_selected `character` name of the conflicted cpd
#'  table
#'
#' @param `plotly`
output$conflicts_ms <- plotly::renderPlotly({
    params <- list(
        db = db(),
        conflict_name_selected = input$conflict_name_selected
    )
    tryCatch({
        if (length(params$conflict_name_selected) == 0) {
            custom_stop("invalid", "no row selected")
        } else if (params$conflict_name_selected == "") {
            custom_stop("invalid", "no rows in the table")
        }
        plot_annotation_ms(
            db(),
            params$conflict_name_selected
        )
    }, invalid = function(i) {
        print("########## conflicts_ms")
        print(params)
        print(i)
        plot_empty_ms()
    }, error = function(e) {
        print("########## conflicts_ms")
        print(params)
        print(e)
        sweet_alert_error(e$message)
        plot_empty_ms()
    })
})

#' @title Plot EIC
#'
#' @description
#' EIC of all the basepeaks for all files when a user clicked on a conflict
#'  line (only the referent ion will be shown)
#' The line dashed correspond to the area not integrated & the line colored the
#' retention time range where integrated by XCMS.
#' It contains a special behavior when the mouse hover a trace : it will display
#'  all the hovertext of all traces in a unique textbox allowing the user to
#'   differentiate all the y coordinates of the traces in one shot
#'
#' @param db `reactive value` pointer to the sqlite connection
#' @param input$conflict_name_selected `character` name of the conflicted cpd
#'  table
#'
#' @return `plotly`
output$conflicts_eic <- plotly::renderPlotly({
    params <- list(
        db = db(),
        conflict_name_selected = input$conflict_name_selected
    )
    tryCatch({
        if (length(params$conflict_name_selected) == 0) {
            custom_stop("invalid", "no row selected")
        } else if (params$conflict_name_selected == "") {
            custom_stop("invalid", "no rows in the table")
        }
        # retrieve the group ID of the referent ion for this compound
        params$group_id <- db_get_group_id(
            params$db,
            name = params$conflict_name_selected
        )
        # give a title to the EIC with the name of the cpd + adduct
        ann <- db_get_annotations(params$db, params$conflict_name_selected)
        title <- paste(
            ann[
                ann$adduct == ann[1, "referent_adduct"],
                c("name", "adduct")
            ],
            collapse = "<br />"
        )
        plot_eic(params$db, params$group_id, title)
    }, invalid = function(i) {
        print("########## conflict_eic")
        print(params)
        print(i)
        plot_empty_chromato("EIC")
    }, error = function(e) {
        print("########## conflict_eic")
        print(params)
        print(e)
        sweet_alert_error(e$message)
        plot_empty_chromato("EIC")
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
        conflicts_table_valid = input$conflicts_table_valid$value
    )
    tryCatch({
        db_resolve_conflict(
            params$db,
            params$conflicts[params$conflict_id],
            params$conflicts_table_valid
        )
        conflicts(conflicts()[-conflict_id()])
        conflict_id(
            if (length(conflicts()) == 0) 0
            else if (conflict_id() == 1) 1
            else conflict_id() - 1
        )
        actualize$peak_spot <<- runif(1)
        # to force all the outputs to reload if they use the data from the db
        toastr_success(sprintf(
            "%s annotated",
            params$conflicts_table_valid
        ))
    }, error = function(e) {
        print("########## conflicts_table_valid")
        print(params)
        print(e)
        sweet_alert_error(e$message)
    })
})
