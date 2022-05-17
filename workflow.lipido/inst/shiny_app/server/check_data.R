observeEvent(db(), {
    cpd_database <- db_get_params(db())$ann$database
    if (!is.null(cpd_database)) {
        shinyWidgets::updatePickerInput(
            session,
            inputId = "check_data_cpd",
            label = "Choose compound(s)",
            choices = load_chem_db(cpd_database)$name
        )
    }
})

#' @title Compound heatmap
#'
#' @description
#' Plot a heamap with in x compounds, in y samples and in z their cumulated
#' intensities of their basepeaks
#' It contains an event on JS where when the user click on a case it will return
#'  in the input$check_data_heatmap_click the list :
#' \itemize{
#'     \item sample `character` sample name
#'     \item cpd_name `character` name of the compound
#' }
#'
#' @param db `reactive value` pointer to the sqlite connection
#' @param input$check_data_cpd `character vector` all the compound selected
#'
#' @return `plotly`
output$check_data_heatmap <- plotly::renderPlotly({
    params <- list(
        db = db(),
        cpd_names = input$check_data_cpd
    )
    tryCatch({
        if (length(params$cpd_names) == 0) {
            custom_stop("invalid", "no compound selected")
        }
        htmlwidgets::onRender(
            plot_heatmap(db(), params$cpd_names),
            "function(el, x) {
                el.on(\"plotly_click\", function(data) {
                    Shiny.onInputChange(\"check_data_heatmap_click\", {
                        sample: data.points[0].y,
                        cpd_name: data.points[0].x
                    })
                })
            }"
        )
    }, invalid = function(i) {
        print("########## check_data_heatmap")
        print(params)
        print(i)
        plot_empty_heatmap()
    }, error = function(e) {
        print("########## check_data_heatmap")
        print(params)
        print(e)
        sweet_alert_error(e$message)
        plot_empty_heatmap()
    })
})

#' @title Plot EIC
#'
#' @description
#' Plot the EIC for a sample and a compound with the m/z deviations compared to
#' the theoreticals.
#' It will trace all the EIC in the same plot for all possible adducts used in
#' the processing workflow (same for the m/z deviation). The line dashed
#' correspond to the area not integrated & the line colored the retention time
#' range where integrated by XCMS. The two line dashed which surround the trace
#' correspond to the retention time tolerance used for the identification.
#' This special EIC help to distinguate if this is the peakpicking which turn
#' wrong or the identification (if it is the rT tolerance or the FWHM window)
#' It contains a special behavior when the mouse hover a trace : it will display
#'  all the hovertext of all traces in a unique textbox allowing the user to
#'   differentiate all the y coordinates of the traces in one shot
#' It draw also annotation to better view to which adduct correspond the trace
#' (the annotation is placed at the most intense point for the trace)
#' A JS function is added to hide the annotation when the trace is hidden
#'
#' @param db `reactive value` pointter to the sqlite connection
#' @param input$check_data_heatmap_click `character list` contains :
#' \itemize{
#'     \item sample `character` sample name of the case clicked on the heatmap
#'     \item cpd_name `character` compound name of the case clicked on the
#'     heatmap
#' }
#'
#' @return `plotly`
output$check_data_eic_mzdev <- plotly::renderPlotly({
    params <- list(
        db = db(),
        sample = input$check_data_heatmap_click$sample,
        cpd_name = input$check_data_heatmap_click$cpd_name
    )
    p1 <- plot_empty_chromato("EIC")
    p2 <- plot_empty_mzdev()
    p <- suppressWarnings(
        plotly::subplot(p1, p2, nrows = 2, shareX = TRUE))
    p <- plotly::layout(p, title = "")

    tryCatch({
        if (is.null(params$cpd_name)) {
            custom_stop("invalid", "no compound selected")
        } else if (is.null(params$sample)) {
            custom_stop("invalid", "no sample selected")
        }
        plot_eic_mzdev(db(), params$sample, params$cpd_name)
    }, invalid = function(i) {
        print("########## check_data_eic_mzdev")
        print(params)
        print(i)
        p
    }, error = function(e) {
        print("########## check_data_eic_mzdev")
        print(params)
        print(e)
        sweet_alert_error(e$message)
        p
    })
})

#' @title Plot m/z deviation
#'
#' @description
#' Plot the m/z deviation of all the m/z in the file compared to the theoretical
#'  compound with adduct (only basepeak)
#' It will trace also a box which represent the m/z tolerance and rT tolernace
#' used for the identification step
#'
#' @param db `reactive value` pointter to the sqlite connection
#' @param input$check_data_heatmap_click `character list` contains :
#' \itemize{
#'     \item sample `character` sample name of the case clicked on the heatmap
#'     \item cpd_name `character` compound name of the case clicked on the
#'     heatmap
#' }
#' @param input$check_data_adduct `character(1)` name of the adduct
#'
#' @return `plotly`
output$check_data_mzdev <- plotly::renderPlotly({
    params <- list(
        sample = input$check_data_heatmap_click$sample,
        cpd_name = input$check_data_heatmap_click$cpd_name,
        adduct_name = input$check_data_adduct
    )

    tryCatch({
        if (is.null(params$cpd_name)) {
            custom_stop("invalid", "no compound selected")
        } else if (is.null(params$sample)) {
            custom_stop("invalid", "no sample selected")
        } else if (is.null(params$adduct_name)) {
            custom_stop("invalid", "no adduct selected")
        }
        plot_mzdev(db(), params$sample, params$cpd_name, params$adduct_name)
    }, invalid = function(i) {
        print("########## check_data_mzdev")
        print(params)
        print(i)
        plot_empty_mzdev()
    }, error = function(e) {
        print("########## check_data_mzdev")
        print(params)
        print(e)
        sweet_alert_error(e$message)
        plot_empty_mzdev()
    })
})
