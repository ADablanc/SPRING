#' @title Plot MS map
#'
#' @description
#' Plot a MS map or a Kendrick plot
#' Each basepeak are plotted with their annotations
#' Hover a trace will show all popup for all basepeak with the same group ID (
#' same compound flagged by CAMERA)
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
        plot_ms_map(
            params$db,
            params$annotation_filter,
            params$int_threshold,
            params$type
        )
    }, error = function(e) {
        print("########## ms_map_plot")
        print(params)
        print(e)
        sweet_alert_error(e$message)
        plot_empty_ms_map()
    })
})
