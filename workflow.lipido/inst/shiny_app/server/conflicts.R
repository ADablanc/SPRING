# update the conflict_id() reactive value when user click on the left arrow
shiny::observeEvent(input$conflicts_left, {
    conflict_id(conflict_id() - 1)
})
# update the conflict_id() reactive value when user click on the right arrow
shiny::observeEvent(input$conflicts_right, {
    conflict_id(conflict_id() + 1)
})
# depending on the value od conflict_id it will enable or disable the arrows
shiny::observeEvent(conflict_id(), {
    if (conflict_id() <= 1) {
        shinyjs::disable("conflicts_left")
    } else {
        shinyjs::enable("conflicts_left")
    }
    if (conflict_id() >= length(ann()$conflicts)) {
        shinyjs::disable("conflicts_right")
    } else {
        shinyjs::enable("conflicts_right")
    }
})

# print the actual index of the conflicts
output$conflicts_info <- shiny::renderText({
    if (length(ann()$conflicts) > 0) {
        sprintf("%s / %s", conflict_id(), length(ann()$conflicts))
    } else {
        ""
    }
})

#' @title Conflicts table
#'
#' @description
#' Show the conflicts table
output$conflicts_table <- DT::renderDataTable({
    default_table <- data.frame(matrix(, nrow = 0, ncol = 9, dimnames = list(
        c(), c("name", "Already seen with", "Already seen in nSamples",
               "Diff rT (sec)", "Adduct", "nsamples", "Best score (%)",
               "Best m/z dev (mDa)", "Max iso"))),
        check.names = FALSE)

    conflicts <- ann()$conflicts
    ann <- ann()$no_conflicts
    params <- list(
        length_conflicts = length(conflicts),
        conflict_id = conflict_id()
    )
    tryCatch({
        if (length(conflicts) == 0) {
            custom_stop("invalid", "no conflicts")
        } else if (
            params$conflict_id > length(conflicts) |
            params$conflict_id < 1
        ) {
            stop("something wrongs with conflict_id")
        }

        conflict <- conflicts[[params$conflict_id]]
        params$conflict <- conflict
        conflict <- conflict[, c("name", "rtdiff", "adduct", "nsamples",
                                 "best_score", "best_deviation_mz",
                                 "best_npeak")]
        conflict[, c("rtdiff", "best_score")] <- round(
            conflict[, c("rtdiff", "best_score")])
        conflict$best_deviation_mz <- round(conflict$best_deviation_mz, 2)
        colnames(conflict) <- c("name", "Diff rT (sec)", "Adduct", "nSamples",
                                "Best score (%)", "Best m/z dev (mDa)",
                                "Max iso")

        info_conflict <- summarise_ann(ann[ann$name %in% conflict$name, ,
                                           drop = FALSE], spectra_infos())
        info_conflict <- info_conflict[, c("name", "Adducts", "nSamples")]
        colnames(info_conflict) <- c("name", "Already seen with",
                                     "Already seen in nSamples")
        conflict <- merge(info_conflict, conflict, all = TRUE)

        conflict <- cbind(conflict,
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
            )
        )
        rownames(conflict) <- conflict$name
        conflict[, -1]
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

output$conflicts_ms <- plotly::renderPlotly({
    conflicts <- ann()$conflicts

    params <- list(
        length_conflicts = length(conflicts),
        conflict_id = conflict_id(),
        conflict_row_selected = input$conflict_row_selected
    )
    tryCatch({
        if (length(params$conflict_row_selected) == 0) {
            custom_stop("invalid", "no row selected")
        } else if (params$conflict_row_selected == "0") {
            custom_stop("invalid", "no rows in the table")
        }
        conflict <- conflicts[[params$conflict_id]]
        params$conflict <- conflict
        # get all ions for the lipid selected
        i <- params$conflict_row_selected

        plot_annotation_ms(db(), conflict[i, "name"])
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

observeEvent(input$conflicts_table_valid, {
    conflicts <- ann()$conflicts
    params <- list(
        length_conflicts = length(conflicts),
        conflict_id = conflict_id(),
        conflicts_table_valid = input$conflicts_table_valid
    )
    tryCatch({
        conflict <- conflicts[[params$conflict_id]]
        params$conflict <- conflict
        i <- as.numeric(params$conflicts_table_valid$value)
        db_resolve_conflict(db(), conflict[i, "group_id"], conflict[i, "name"])
        conflict_id(conflict_id() - 1)
        ann(list(
            no_conflicts = rbind(
                ann()$no_conflicts,
                conflict[i, , drop = FALSE]
            ),
            conflicts = conflicts[-params$conflict_id]
        ))
        toastr_success(sprintf(
            "%s %s annotated",
            conflict[i, "name"],
            conflict[i, "adduct"]
        ))
    }, error = function(e) {
        print("########## conflicts_table_valid")
        print(params)
        print(e)
        sweet_alert_error(e$message)
    })
})
