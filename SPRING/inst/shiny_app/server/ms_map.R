#' @title Plot MS map
#'
#' @description
#' Plot a MS map or a Kendrick plot
#' Each basepeak are plotted with their annotations
#' Hover a trace will show all popup for all basepeak with the same group ID (
#' same compound flagged by CAMERA)
#' A click on a trace will update the ms_map_eic output by returning the EIC ID
#'  to the server
#'
#' @param db `reactive value` pointer to the sqlite connection
#'
#' @return `plotly`
output$ms_map_plot <- plotly::renderPlotly({
    params <- list(
        db = db(),
        type = input$ms_map_type,
        annotation_filter = input$ms_map_annotation_filter,
        int_threshold = input$ms_map_int_threshold
    )
    tryCatch({
        htmlwidgets::onRender(
            plot_ms_map(
                params$db,
                params$annotation_filter,
                params$int_threshold,
                params$type
            ),
            'function(el, x) {
                el.on("plotly_click", function(data) {
                    Shiny.onInputChange(
                        "ms_map_eic_id",
                        data["points"][0].customdata
                    );
                })
            }'
        )
    }, error = function(e) {
        print("########## ms_map_plot")
        print(params)
        print(e)
        sweet_alert_error(e$message)
        plot_empty_ms_map()
    })
})

#' @title Plot MS map
#'
#' @description
#' EIC of all the basepeaks for all files when a user clicked on a trace on the
#'  ms_map_plot output
#' The line dashed correspond to the area not integrated & the line colored the
#' retention time range where integrated by XCMS.
#' It contains a special behavior when the mouse hover a trace : it will display
#'  all the hovertext of all traces in a unique textbox allowing the user to
#'   differentiate all the y coordinates of the traces in one shot
#' If available it will use the retention time corrected in the slot
#' `scantime_corrected` added by the function `obiwarp`
#'
#' @param db `reactive value` pointer to the sqlite connection
#' @param input$ms_map_eic_id `numeric` EIC ID of the trace clicked in the
#'  ms_map_plot output
#'
#' @return `plotly`
output$ms_map_eic <- plotly::renderPlotly({
    params <- list(
        db = db(),
        eic_id = input$ms_map_eic_id
    )
    tryCatch({
        if (is.null(params$eic_id)) {
            plot_empty_chromato("EIC")
        } else {
            plot_db_eic(params$db, params$eic_id)
        }
    }, error = function(e) {
        print("########## ms_map_eic")
        print(params)
        print(e)
        sweet_alert_error(e$message)
        plot_empty_chromato("EIC")
    })
})
