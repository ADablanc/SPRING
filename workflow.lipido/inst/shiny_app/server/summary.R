default_summary_table <- data.frame(matrix(, nrow = 0, ncol = 9,
   dimnames = list(c(), c("name", "rT (min)", "Diff rT (sec)", "Adducts", "nSamples",
                          "Most intense ion", "Best score (%)",
                          "Best m/z dev (mDa)", "Max iso"))),
   check.names = FALSE)
summary_table_options <- list(
    rownames = FALSE,
    selection = "none",
    filter = "top",
    extensions = c("Scroller", "Buttons", "FixedColumns"),
    options = list(
        dom = "Bfrtip",
        paging = TRUE,
        scroller = TRUE,
        scrollY = "65vh",
        scrollX = TRUE,
        scrollCollapse = TRUE,
        buttons = list(
            list(
                extend = "colvis",
                text = "Hide columns",
                columns = 1:8
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
                targets = "th:nth-child(n+10)",
                type = "num-fmt"
            )
        ),
        language = list(
            emptyTable = "no lipids found"
        ),
        initComplete = htmlwidgets::JS("
            function(settings, json){
                settings.oInstance.api().columns.adjust();
            }
        ")
    )
)
output$summary_table <- DT::renderDataTable({
    # to invalidate the summary table
    ann <- summarise_ann(db_get_ann(db), db_get_spectra_infos(db))
    if (nrow(ann) > 0) {
        ann[ann == 0] <- NA
        DT::formatCurrency(
            table = do.call(DT::datatable,
                            c(list(data = ann), summary_table_options)),
            columns = 10:ncol(ann),
            mark = " ",
            digits = 0,
            currency = ""
        )
    } else do.call(DT::datatable, c(list(data = default_summary_table),
                                    summary_table_options))
})

output$summary_export <- shiny::downloadHandler(
    filename = function() {
        if (is.null(sqlite_path)) "*.xlsx"
        else paste0(tools::file_path_sans_ext(basename(sqlite_path)), ".xlsx")
    },
    content = function(file) {
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "Summary")
        openxlsx::addWorksheet(wb, "Details")
        openxlsx::writeDataTable(wb, "Summary",
                                 summarise_ann(db_get_ann(db),
                                               db_get_spectra_infos(db)))
        openxlsx::writeDataTable(wb, "Details",
                                 get_int_ann(db_get_ann(db),
                                               db_get_spectra_infos(db)))
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
        return(file)
    }
)
