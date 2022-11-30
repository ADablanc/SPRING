#' @title Plot peak spot viewer
#'
#' @description
#' Plot a Peak spot viewer or a Kendrick plot
#' Each basepeak are plotted with their annotations
#' Hover a trace will show all popup for all basepeak with the same group ID (
#' same compound flagged by CAMERA)
#' A click on a trace will update the peak_spot_row_id output by returning the
#' row id of the annotation dataframe to the server
#'
#' @param db `reactive value` pointer to the sqlite connection
#'
#' @return `plotly`
output$peak_spot_plot <- plotly::renderPlotly({
    actualize$peak_spot
    params <- list(
        db = db(),
        type = input$peak_spot_type,
        annotation_filter = input$peak_spot_annotation_filter,
        int_threshold = input$peak_spot_int_threshold
    )
    tryCatch({
        htmlwidgets::onRender(
            plot_peak_spot(
                params$db,
                params$annotation_filter,
                params$int_threshold,
                params$type
            ),
            'function(el, x) {
                el.on("plotly_click", function(data) {
                    Shiny.onInputChange(
                        "peak_spot_row_id",
                        data["points"][0].customdata
                    );
                })
            }'
        )
    }, error = function(e) {
        print("########## peak_spot_plot")
        print(params)
        print(e)
        sweet_alert_error(e$message)
        plot_empty_peak_spot()
    })
})

#' @title Plot EIC
#'
#' @description
#' EIC of all the basepeaks for all files when a user clicked on a trace on the
#'  peak_spot_plot output
#' The line dashed correspond to the area not integrated & the line colored the
#' retention time range where integrated by XCMS.
#' It contains a special behavior when the mouse hover a trace : it will display
#'  all the hovertext of all traces in a unique textbox allowing the user to
#'   differentiate all the y coordinates of the traces in one shot
#'
#' @param db `reactive value` pointer to the sqlite connection
#' @param input$peak_spot_row_id `numeric` row id of the annotation DataFrame
#'  which correspond to the ion clicked
#'
#' @return `plotly`
output$peak_spot_eic <- plotly::renderPlotly({
    actualize$peak_spot
    params <- list(
        db = db(),
        row_id = input$peak_spot_row_id
    )
    tryCatch({
        if (is.null(params$row_id)) {
            plot_empty_chromato("EIC")
        } else if (params$row_id == 0) {
            plot_empty_chromato("EIC")
        } else {
            # retrieve the group ID for this ion
            params$group_id <- db_get_group_id(
                params$db,
                row_id = params$row_id
            )
            # give a title to the EIC with the name of the cpd + adduct
            ann <- db_get_annotations(params$db, row_ids = params$row_id)
            if (!all(is.na(ann[1, "name"]))) {
                title <- paste(ann[1, c("name", "adduct")], collapse = "<br />")
            } else {
                title <- ""
            }
            p <- plot_eic(params$db, params$group_id, title)
            p <- plotly::config(p, modeBarButtons = NULL)
            p <- plotly::config(
                p,
                modeBarButtons = list(
                    list(
                        list(
                            name = "toImage",
                            title = "Download plot as a png",
                            icon = htmlwidgets::JS("Plotly.Icons.camera"),
                            click = htmlwidgets::JS(
                                paste0("function(gd) {
                                       Plotly.downloadImage(
                                            gd,
                                            {
                                                format:'png', ",
                                                "width:1200, ",
                                                "height:400, ",
                                                "filename:'Chromatogram'
                                            }
                                   )
                               }"
                            ))
                        )
                    ),
                    list("zoom2d", "autoScale2d"),
                    list(
                        list(
                            name = "resetView",
                            title = "Reset legend",
                            icon = htmlwidgets::JS("Plotly.Icons.undo"),
                            click = htmlwidgets::JS(
                                paste0("function(gd) {
                                       Plotly.restyle(gd, 'visible', true);
                                   }"
                               )
                           )
                        )
                    ),
                    list(
                        list(
                            name = "select",
                            title = "Select rT borns",
                            icon = htmlwidgets::JS("Plotly.Icons.selectbox"),
                            click = htmlwidgets::JS("
                                function (gd) {
                                    Plotly.relayout(
                                        gd,
                                        \"dragmode\",
                                        \"select\"
                                    );
                                }
                            ")
                        )
                    )
                )
            )
            htmlwidgets::onRender(
                p,
                "function (el, x) {
                    el.on(\"plotly_selected\", function (eventData) {
                        Shiny.onInputChange(
                            \"peak_spot_eic_rt\",
                            eventData.range.x
                        )
                    })
                }"
            )
        }
    }, error = function(e) {
        print("########## peak_spot_eic")
        print(params)
        print(e)
        sweet_alert_error(e$message)
        plot_empty_chromato("EIC")
    })
})

shiny::observeEvent(input$peak_spot_eic_rt, {
    actualize$peak_spot_eic
    params <- list(
        db = db(),
        row_id = input$peak_spot_row_id,
        rt_range = input$peak_spot_eic_rt * 60
    )
    tryCatch({
        if (is.null(params$row_id)) {
            custom_stop("invalid", "no ion was selected on the peak spot plot")
        }
        # retrieve the compound name for this ion
        params$ann <- db_get_annotations(params$db, row_ids = params$row_id)
        if (all(is.na(params$ann[1, "name"]))) {
            custom_stop(
                "invalid",
                "reintegration is only available for annotated ion for now"
            )
        }
        invisible(reintegrate_ann(
            params$db,
            params$ann[1, "name"],
            params$rt_range[1],
            params$rt_range[2]
        ))
        actualize$peak_spot <<- runif(1)
        toastr_success(sprintf("%s reintegrated !", params$ann[1, "name"]))
    }, invalid = function(i) {
        print("######### peak_spot_eic_rt")
        print(params)
        print(i)
        toastr_error(i$message)
    }, error = function(e) {
        print("######### peak_spot_eic_rt")
        print(params)
        print(e)
        sweet_alert_error(e$message)
    })
})
