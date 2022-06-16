#' @title Options for summary table
#'
#' @description
#' List of options for the summary table
#' This was written in this form in order to use the function
#' DT::formatCurrency which need the option list to work
summary_table_options <- list(
    rownames = FALSE,
    selection = "none",
    filter = "top",
    extensions = c("Scroller", "Buttons", "FixedColumns"),
    options = list(
        dom = "Bfrtip",
        paging = TRUE,
        scroller = TRUE,
        scrollY = "55vh",
        scrollX = TRUE,
        scrollCollapse = TRUE,
        buttons = list(
            list(
                extend = "colvis",
                text = "Hide columns",
                columns = 1:7
            ),
            list(
                extend = "collection",
                text = "Export to excel",
                action = htmlwidgets::JS("
                    function(e, dt, node, config) {
                        $(\"#summary_export\")[0].click();
                    }
                ")
            )
        ),
        fixedColumns = 1,
        columnDefs = list(
            list(
                className = "dt-head-center dt-center",
                targets = "_all",
                width = 80
            ),
            list(
                targets = "th:nth-child(n+9)",
                type = "num-fmt"
            )
        ),
        language = list(
            emptyTable = "no lipids found"
        ),
        initComplete = htmlwidgets::JS("
            function(settings, json) {
                settings.oInstance.api().columns.adjust();
            }
        ")
    )
)

#' @title Summary table
#'
#' @description
#' Summary datatable: annotations regrouped by compound instead by ion
#' it will return in the column samples the sum of intensity of ALL basepeaks
#' It use the function DT::formatCurrency in order to sort the intensities
#' even if the thousand separator is " " and not ","
#'
#' @param db `reactive value` pointer to the sqlite connection
#' @param conflicts `reactive value` group IDs where a conflict was detected
#'
#' @return `DataTable` with columns :
#' \itemize{
#'     \item class `character` cpd class
#'     \item name `character` name of the compound
#'     \item rt (min) `numeric` meanned rT
#'     \item Diff rT (sec) `numeric` rT difference between observed &
#'     theoretical
#'     \item Adducts `character` all adducts detected separated by a space
#'     \item nSamples `integer` number of samples where the compound was
#'     detected
#'     \item best score (%) `numeric` the highest isotopic score
#'     \item best m/z dev (mDa) `numeric` the minimal m/z deviation observed
#'     in mDa
#'     \item max iso `integer` the highest number of isotopologue for the ions
#'     reported
#'     \item ... `integer` a column for each sample which contain the summed
#'     intensity of ALL basepeaks
#' }
output$summary_table <- DT::renderDataTable({
    params <- list(
        db = db()
    )
    ann <- tryCatch({
    # to invalidate the summary table
        ann <- db_get_annotations(db())
        if (nrow(ann) == 0) {
            custom_stop("invalid", "no annotations in database")
        }
        nsamples <- db_get_nsamples(db())
        spectra_infos <- db_get_spectra_infos(db())

        ann <- summarise_ann(ann, spectra_infos, nsamples)$resume
        ann[, (ncol(ann) - nsamples + 1):ncol(ann)][
            ann[, (ncol(ann) - nsamples + 1):ncol(ann)] == 0] <- NA
        ann
    }, invalid = function(i) {
        print("########## check_data_mzdev")
        print(params)
        print(i)
        data.frame(matrix(, nrow = 0, ncol = 10, dimnames = list(c(),
            c("Group ID", "Class", "Name", "rT (min)", "Diff rT (sec)",
              "Adducts", "nSamples", "Best score (%)", "Best m/z dev (mDa)",
              "Max iso"))), check.names = FALSE)
    }, error = function(e) {
        print("########## check_data_mzdev")
        print(params)
        print(e)
        sweet_alert_error(e$message)
        data.frame(matrix(, nrow = 0, ncol = 9, dimnames = list(c(),
             c("Group ID", "Class", "Name", "rT (min)", "Diff rT (sec)",
               "Adducts", "nSamples", "Best score (%)", "Best m/z dev (mDa)",
               "Max iso"))), check.names = FALSE)
    })
    nsamples <- db_get_nsamples(db())
    DT::formatCurrency(
        table = do.call(
            DT::datatable,
            c(list(data = ann), summary_table_options)
        ),
        columns = (ncol(ann) - nsamples + 1):ncol(ann),
        mark = " ",
        digits = 0,
        currency = ""
    )
}, server = isFALSE(getOption("shiny.testmode")))

#' @title Summary export
#'
#' @description
#' Export all annotations in a excel file
#' First sheet will have the annotations regroup by compound, even those which
#' are in conflicts with others
#' Second will have annotations regroup by ions
#'
#' @param sqlite_path `reactiveValue` sqlite path
output$summary_export <- shiny::downloadHandler(
    filename = function() {
        if (is.null(sqlite_path())) {
            "*.xlsx"
        } else {
            paste0(tools::file_path_sans_ext(basename(sqlite_path())), ".xlsx")
        }
    },
    content = function(excel_path) {
        export_annotations(
            sqlite_path(),
            excel_path
        )
        excel_path
    }
)
