#' @title Construct empty MS
#'
#' @description
#' Construct an empty MS
#'
#' @param title `character(1)` title of the plot
#' @param y_title `character(1)` title of the Y axis
#'
#' @return `plotly` object
plot_empty_ms <- function(title = "Mass Spectra", y_title = "Intensity") {
    p <- plotly::plot_ly(
        type = "scatter",
        mode = "markers"
    )
    p <- plotly::layout(
        p,
        title = list(
            text = sprintf("<b>%s</b>", title),
            y = .95,
            x = .95,
            font = list(
                family = '"Open Sans",verdana,arial,sans-serif',
                size = 18
            ),
            xanchor = "right",
            yanchor = "bottom"
        ),
        margin = list(t = 50),
        xaxis = list(
            title = "m/z",
            titlefont = list(
                family = "\"Open Sans\",verdana,arial,sans-serif",
                size = 18
            ),
            showspikes = FALSE,
            showticksuffix = "all",
            hoverformat = ".5f"
        ),
        yaxis = list(
            exponentformat = "e",
            title = "",
            hoverformat = ".2e"
        ),
        hoverlabel = list(
            namelength = -1
        ),
        annotations = list(list(
            xref = "paper",
            yref = "paper",
            x = 0,
            y = 1,
            xanchor = "left",
            yanchor = "bottom",
            text = y_title,
            showarrow = FALSE,
            font = list(
                family = "\"Open Sans\",verdana,arial,sans-serif",
                size = 18
            )
        ))
    )
    plotly::config(
        p,
        responsive = TRUE,
        scrollZoom = FALSE,
        displaylogo = FALSE,
        edits = list(
            annotationTail = TRUE
        ),
        modeBarButtons = list(
            list(list(
                name = "toImage",
                title = "Download plot as a png",
                icon = htmlwidgets::JS("Plotly.Icons.camera"),
                click = htmlwidgets::JS(paste0("function(gd) {Plotly.downloadI",
                                               "mage(gd, {format:'png', width:",
                                               "1200, height:400, filename:'MS",
                                               "'})}"))
            )),
            list("zoom2d", "autoScale2d")
        )
    )
}

#' @title Plot Multiple Mass Spectras
#'
#' @description
#' Plot multiple mass spectras on the same plot.
#' It have in positive the observed ions & in mirror (in negative) the
#' theoretical ions.
#' It add two JS functions :
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
#' @param spectras `dataframe list` each named item (the names are use to give
#' a name to the trace) contains a dataframe where each line correspond to a
#' peak annotated with its corresponding theoretical peak or the theoretical
#' peak missed, with the columns :
#' \itemize{
#'     \item mz `numeric` m/z
#'     \item int `numeric` area integrated
#'     \item mz_theo `numeric` theoretical m/z
#'     \item abd_theo `numeric` theoretical relative abundance
#'     \item iso_theo `character` theoretical isotopologue annotation
#' }
#'
#' @return `plotly`
plot_composite_ms <- function(spectras) {
    p <- plot_empty_ms(title = "Hybrid mass spectra")

    mz_range <- c(Inf, 0)
    for (i in seq(length(spectras))) {
        spectra <- spectras[[i]]
        mz_range[1] <- min(
            mz_range[1],
            spectra$mz,
            spectra$mz_theo,
            na.rm = TRUE
        )
        mz_range[2] <- max(
            mz_range[2],
            spectra$mz,
            spectra$mz_theo,
            na.rm = TRUE
        )
        # create the column int_theo which represent the theoretic intensity
        spectra$int_theo <- spectra$abd_theo *
            spectra[which(spectra$iso_theo == "M"), "int"] / 100
        matched <- spectra[!is.na(spectra$mz) & !is.na(spectra$mz_theo),
                           , drop = FALSE]
        not_matched <- spectra[is.na(spectra$mz) | is.na(spectra$mz_theo),
                               , drop = FALSE]
        p <- plotly::add_segments(
            p,
            x = c(matched$mz, matched$mz_theo),
            xend = c(matched$mz, matched$mz_theo),
            y = 0,
            yend = c(matched$int, -matched$int_theo),
            name = names(spectras)[i],
            legendgroup = names(spectras)[i],
            showlegend = TRUE,
            hoverinfo = "text",
            text = c(
                sprintf(
                    paste(
                        "observed",
                        "adduct: %s",
                        "iso: %s",
                        "m/z: %s",
                        "abd: %s%%",
                        sep = "<br />"
                    ),
                    names(spectras)[i],
                    matched$iso_theo,
                    round(matched$mz, 5),
                    round(matched$abd)
                ),
                sprintf(
                    paste(
                        "theoretical",
                        "adduct: %s",
                        "iso: %s",
                        "m/z: %s",
                        "abd: %s%%",
                        sep = "<br />"
                    ),
                    names(spectras)[i],
                    matched$iso_theo,
                    round(matched$mz_theo, 5),
                    round(matched$abd_theo)
                )
            )
        )
        if (nrow(not_matched) > 0) {
            p <- plotly::add_segments(
                p,
                x = c(not_matched$mz, not_matched$mz_theo),
                xend = c(not_matched$mz, not_matched$mz_theo),
                y = 0,
                yend = c(not_matched$int, -not_matched$int_theo),
                color = I("black"),
                legendgroup = names(spectras)[i],
                showlegend = FALSE,
                hoverinfo = "text",
                text = c(
                    sprintf(
                        "observed<br /><br />m/z: %s",
                        round(not_matched$mz, 5)
                    ),
                    sprintf(
                        paste(
                            "theoretical",
                            "adduct: %s",
                            "iso: %s",
                            "m/z: %s",
                            "abd: %s%%",
                            sep = "<br />"
                        ),
                        names(spectras)[i],
                        not_matched$iso_theo,
                        round(not_matched$mz_theo, 5),
                        round(not_matched$abd_theo)
                    )
                )
            )
        }
        p <- plotly::add_annotations(
            p,
            x = matched[matched$iso_theo == "M", "mz"],
            y = matched[matched$iso_theo == "M", "int"],
            text = names(spectras)[i],
            xref = "x",
            yref = "y",
            valign = "bottom",
            arrowhead = 0
        )
    }

    p <- plotly::layout(p, xaxis = list(
        range = c(mz_range[1] - 1, mz_range[2] + 1)))

    # add 2 functions on JS
    # first is when the mouse is over an observed peak
    # we will try to show the corresponding theoretical point
    # the points are in this order :
    #     [observed$M, observed$M1, theoretical$M, theoretical$M1]
    # so the theoretical must be at the same index but if we start in the
    #     middle of the array !
    # for example for the peak observed M1, the index is 1
    # so the theoretical must be at :
    #     1 + middle = 1 + (length / 2) = 1 + (4 / 2) = 1 + 2 = 3
    # second is for hiding the annotations bind to the trace
    # and to force the relayout between the xaxis range +/- 1
    htmlwidgets::onRender(p, '
        function(el, x) {
            el.on("plotly_hover", function(eventdata) {
                var plot_id = $(eventdata.event.srcElement).closest
                    (".plotly.html-widget").get(0).id;
                if (eventdata.points[0].data.showlegend) {
                    Plotly.Fx.hover(plot_id, [
                        {
                            curveNumber: eventdata.points[0].curveNumber,
                            pointNumber: eventdata.points[0].pointNumber
                        },
                        {
                        curveNumber: eventdata.points[0].curveNumber,
                        pointNumber: eventdata.points[0].pointIndex +
                            (eventdata.points[0].fullData.x.length + 1) / 2
                        }
                    ])
                }
            });
            el.on("plotly_restyle", () => {
                annotations = el.layout.annotations;
                for (var i = 1; i < annotations.length; i++) {
                    annotations[i].visible = el._fullData[i * 2 - 1].visible !=
                        "legendonly"
                }
                Plotly.relayout(el, {
                    annotations: annotations,
                    xaxis: {
                        range: [
                            Math.min(...el._fullData
                                .filter(x => x.visible == true)
                                .map(x => Math.min(...x.x
                                    .filter(y =>  y!= null))
                            )) - 1,
                            Math.max(...el._fullData
                                .filter(x => x.visible == true)
                                .map(x => Math.max(...x.x
                                    .filter(y =>  y!= null))
                                )) + 1
                        ]
                    }
                });
            });
        }
    ')
}

#' @title Plot MS annotated
#'
#' @description
#' Plot a mass spectrum with all the spectras annotated for a compound
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
#' @param db `SQLiteConnection`
#' @param name `character(1)` name of the compound
#'
#' @return `plotly` a plotly object
#'
#' @export
#' @examples
#' \dontrun{
#' plot_annotation_ms(db, "LPC 11a:0")
#' }
plot_annotation_ms <- function(db, name) {
    if (class(db) != "SQLiteConnection") {
        stop("db must be a connection to the sqlite database")
    } else if (class(name) != "character") {
        stop("name must be a character")
    } else if (length(name) != 1) {
        stop("name must be only ONE compound")
    }

    ann <- db_get_annotations(db, names = name)
    if (nrow(ann) == 0) {
        return(plot_empty_ms(title = "Hybrid mass spectra"))
    }
    nsamples <- db_get_nsamples(db)
    spectra_ids <- na.omit(unlist(
        ann[, (ncol(ann) - nsamples + 1):ncol(ann)]
    ))
    spectra_infos <- db_get_spectra_infos(db, spectra_ids)

    # for each line (adduct) select the most intense spectra
    ann_int <- get_int_ann(ann, spectra_infos, nsamples)
    spectras <- lapply(seq(nrow(ann)), function(i) {
        db_get_spectras(
            db,
            ann[i, which.max(
                ann_int[i, (ncol(ann_int) - nsamples + 1):ncol(ann_int)]
            ) + ncol(ann) - nsamples]
        )
    })
    names(spectras) <- ann$adduct
    plot_composite_ms(spectras)
}

#' @title Plot empty chromatogram
#'
#' @description
#' Plot an empty chromatogram
#' It contains a special behavior when the mouse hover a trace : it will display
#'  all the hovertext of all traces in a unique textbox allowing the user to
#'   differentiate all the y coordinates of the traces in one shot
#
#' @param title `character(1)` plot title
#'
#' @return `plotly`
plot_empty_chromato <- function(title = "Total Ion Chromatogram(s)") {
    p <- plotly::plot_ly(
        type = "scatter",
        mode = "markers"
    )
    p <- plotly::layout(
        p,
        title = list(
            text = sprintf("<b>%s</b>", title),
            y = .95,
            x = .95,
            font = list(
                family = "\"Open Sans\",verdana,arial,sans-serif",
                size = 18
            ),
            xanchor = "right",
            yanchor = "bottom"
            ),
        margin = list(t = 50),
        spikedistance = -1,
        hovermode = "x unified",
        hoverdistance = 1,
        xaxis = list(
            title = "Retention time",
            titlefont = list(
                family = "\"Open Sans\",verdana,arial,sans-serif",
                size = 18
                ),
            showspikes = TRUE,
            spikemode = "across",
            spikedash = "dash",
            spikecolor = "#000000",
            spikethickness = 1,
            ticksuffix = " min",
            showticksuffix = "all",
            hoverformat = ".2f"
        ),
        yaxis = list(
            exponentformat = "e",
            title = "",
            hoverformat = ".2e"
        ),
        hoverlabel = list(
            namelength = -1
        ),
        legend = list(
            orientation = "h",
            y = -.3
        ),
        selectdirection = "h",
        annotations = list(
            list(
                xref = "paper",
                yref = "paper",
                x = 0,
                y = 1,
                xanchor = "left",
                yanchor = "bottom",
                text = "Intensity",
                showarrow = FALSE,
                font = list(
                    family = "\"Open Sans\",verdana,arial,sans-serif",
                    size = 18
                    )
                )
        )
    )
    plotly::config(
        p,
        responsive = TRUE,
        displaylogo = FALSE,
        scrollZoom = FALSE,
        edits = list(
            annotationTail = TRUE
        ),
        modeBarButtons = list(
            list(
                list(
                    name = "toImage",
                    title = "Download plot as a png",
                    icon = htmlwidgets::JS("Plotly.Icons.camera"),
                    click = htmlwidgets::JS(paste0("function(gd) {Plotly.down",
                                                   "loadImage(gd, {format:'png",
                                                   "', width:1200, height:400,",
                                                   " filename:'Chromatogram'})}"
                                           ))
                )
            ),
            list("zoom2d", "autoScale2d"),
            list(
                list(
                    name = "resetView",
                    title = "Reset legend",
                    icon = htmlwidgets::JS("Plotly.Icons.undo"),
                    click = htmlwidgets::JS(paste0("function(gd) {Plotly.resty",
                                                   "le(gd, 'visible', true);}"))
                )
            )
        )
    )
}

#' @title Plot DB EIC
#'
#' @description
#' Plot the EIC for all the sample for a basepeak mass recorded in the database
#' The line dashed correspond to the area not integrated & the line colored the
#' retention time range where integrated by XCMS.
#' It contains a special behavior when the mouse hover a trace : it will display
#'  all the hovertext of all traces in a unique textbox allowing the user to
#'   differentiate all the y coordinates of the traces in one shot
#'
#' @param db `SQLiteConnection`
#' @param group_id `numeric(1)` group ID
#' @param title `character(1)` title of the plot (by default it show "EIC")
#'
#' @return `plotly`
#'
#' @export
#' @examples
#' \dontrun{
#' plot_eic(db, 2)
#' }
plot_eic <- function(db, group_id, title = "EIC") {
    if (class(db) != "SQLiteConnection") {
        stop("db must be a connection to the sqlite database")
    } else if (length(group_id) > 1) {
        stop("only one group_id is required")
    } else if (!is.numeric(group_id)) {
        stop("group_id must be numerical")
    }

    p <- plot_empty_chromato(title)
    eics <- db_get_eic(db, group_id)
    if (nrow(eics) == 0) {
        return(p)
    }

    for (i in 2:ncol(eics)) {
        eic <- eics[, c(1, i)]
        # search if the ion was integrated
        peaks <- unlist(db_get_query(db, sprintf(
            "SELECT rtmin, rtmax
            FROM peaks
            WHERE ROWID == (
                SELECT \"%s\" FROM peakgroups WHERE group_id == %s
            )", colnames(eic)[2], group_id
        )))
        if (length(peaks) > 0) {
            idx <- which(eic$rt >= peaks[1] & eic$rt <= peaks[2])
            idx2 <- c(
                if (idx[1] != 1) idx[1] - 1 else NULL,
                idx,
                if (idx[length(idx)] != nrow(eic)) idx[length(idx)] + 1
                else NULL
            )
            integrated <- eic
            integrated[-idx2, 2] <- NA
            eic[idx, 2] <- NA
        } else {
            integrated <- data.frame(rt = NA, int = NA)
        }
        # trace
        p <- plotly::add_trace(
            p,
            mode = "lines",
            x = round(integrated[, 1] / 60, 3),
            y = integrated[, 2],
            name = colnames(eic)[2],
            showlegend = FALSE
        )
        p <- plotly::add_trace(
            p,
            mode = "lines",
            x = round(eic[, 1] / 60, 3),
            y = eic[, 2],
            name = colnames(eic)[2],
            showlegend = FALSE,
            line = list(
                color = "rgb(0,0,0)",
                width = 1,
                dash = "dash"
            )
        )
    }
    p
}

#' @title Plot TICs
#'
#' @description
#' Plot TICs from raw files (not converted). It use in background the msaccess
#' executable from ProteoWizard
#'
#' @param raw_files `character vector` filepaths to the raw files
#' @param msaccess `character(1)` filepath to the msaccess executable file
#' @param polarity `character(1)` should be "positive" or "negative" only
#'
#' @return `plotly`
#'
#' @export
#' @examples
#' \dontrun{
#' plot_raw_tic(
#'    c("SPRING/inst/testdata/220221CCM_global_POS_01_ssleu_filtered.mzML",
#'      "SPRING/inst/testdata/220221CCM_global_POS_01_ssleu_filtered.mzML"),
#'     "pwiz/msaccess.exe",
#'     "positive"
#' )
#' }
plot_raw_tic <- function(raw_files, msaccess, polarity = "positive") {
    if (class(raw_files) != "character") {
        stop("raw_files must be a vector of filepaths")
    } else if (any(!file.exists(raw_files))) {
        stop(sprintf("cannot find %s", paste(
            raw_files[!file.exists(raw_files)],
            collapse = ", ")
        ))
    } else if (class(msaccess) != "character") {
        stop("msaccess must be a filepath to msaccess.exe")
    } else if (length(msaccess) > 1) {
        stop("msaccess must be a unique filepath")
    } else if (!file.exists(msaccess)) {
        stop("cannot find msaccess executable")
    } else if (class(polarity) != "character") {
        stop("polarity must be a character")
    } else if (length(polarity) > 1) {
        stop("only one polarity is authorized")
    } else if (polarity != "positive" && polarity != "negative") {
        stop("polarity must be \"positive\" or \"negative\"")
    }

    raw_files <- normalizePath(raw_files)
    msaccess <- normalizePath(msaccess)

    p <- plot_empty_chromato("")
    data <- get_raw_tic(raw_files, msaccess, polarity)
    for (i in seq(length(data))) {
        p <- plotly::add_trace(
            p,
            mode = "lines",
            x = round(data[[i]]$rt / 60, 3),
            y = data[[i]]$int,
            name = names(data)[i]
        )
    }
    p
}

#' @title Plot empty Peak spot
#'
#' @description
#' Plot an empty Peak spot with custom axis
#'
#' @param title `character(1)` title of the plot
#' @param xaxis_title `character(1)` title of the x axis
#' @param yaxis_title `character(1)` title of the y axis
#'
#' @return `plotly` object
plot_empty_peak_spot <- function(title = "Peak spot viewer",
                                 xaxis_title = "Retention time",
                                 yaxis_title = "m/z") {
    p <- plotly::plot_ly(
        type = "scatter",
        mode = "markers"
    )
    p <- plotly::layout(
        p,
        title = list(
            text = sprintf("<b>%s</b>", title),
            y = .95,
            x = .95,
            font = list(
                family = "\"Open Sans\",verdana,arial,sans-serif",
                size = 18
            ),
            xanchor = "right",
            yanchor = "bottom"
        ),
        margin = list(t = 50),
        xaxis = list(
            title = xaxis_title,
            titlefont = list(
                family = "\"Open Sans\",verdana,arial,sans-serif",
                size = 18
            )
        ),
        yaxis = list(
            title = ""
        ),
        annotations = list(
            list(
                xref = "paper",
                yref = "paper",
                x = 0,
                y = 1,
                xanchor = "left",
                yanchor = "bottom",
                text = yaxis_title,
                showarrow = FALSE,
                font = list(
                    family = "\"Open Sans\",verdana,arial,sans-serif",
                    size = 18
                )
            )
        )
    )
    plotly::config(
        p,
        responsive = TRUE,
        displaylogo = FALSE,
        # scrollZoom = FALSE,
        edits = list(
            annotationTail = TRUE
        ),
        modeBarButtons = list(
            list(
                list(
                    name = "toImage",
                    title = "Download plot as a png",
                    icon = htmlwidgets::JS("Plotly.Icons.camera"),
                    click = htmlwidgets::JS(paste0("function(gd) {Plotly.down",
                                                   "loadImage(gd, {format:'png",
                                                   "', width:1200, height:400,",
                                                   " filename:'Chromatogram'})}"
                    ))
                )
            )
        )
    )
}

#' @title Plot Peak spot viewer
#'
#' @description
#' Plot a peak spot or Kendrick plot
#' Each basepeak are plotted with their annotations
#' Hover a trace will show all popup for all basepeak with the same group ID (
#' same compound flagged by CAMERA)
#' Possibility to trace the "Peak spot" (m/z fct(rT)) or a Kendrick plot
#' Each trace will contain the ROWID of the annotation line in the DB in their
#'  `customdata` slot
#'
#' @param db `SQLiteConnection`
#' @param annotation_filter `character(1)` annotation to filter (can be
#' "no annotated", "annotated" or "all")
#' @param int_threshold `numeric(1)` intensity threshold
#' @param type `character(1)` "Peak spot" or "Kendrick plot"
#'
#' @return `plotly`
#'
#' @export
#' @examples
#' \dontrun{
#' plot_peak_spot(db)
#' }
plot_peak_spot <- function(db, annotation_filter = "all", int_threshold = 0,
                        type = "Peak spot") {
    if (class(db) != "SQLiteConnection") {
        stop("db must be a connection to the sqlite database")
    } else if (!is.numeric(int_threshold)) {
        stop("int_treshold must be a numeric")
    } else if (class(annotation_filter) != "character") {
        stop("annotation_filter must be a character")
    } else if (!annotation_filter %in% c("no annotated", "annotated", "all")) {
        stop("annotation_filter must be \"no annotated\", \"annotated\" or
            \"all\"")
    } else if (!type %in% c("Peak spot", "Kendrick plot")) {
        stop("type must be \"Peak spot\" or \"Kendrick plot\"")
    }

    if (type == "Peak spot") {
        xaxis <- "Retention time"
        yaxis <- "m/z"
    } else {
        xaxis <- "Kendrick mass"
        yaxis <- "Kendrick mass defect"
    }
    p <- plot_empty_peak_spot(
        title = type,
        xaxis_title = xaxis,
        yaxis_title = yaxis
    )

    # get data
    nsamples <- db_get_nsamples(db)
    int_ann <- get_int_ann(
        db_get_annotations(db),
        db_get_spectra_infos(db),
        nsamples
    )
    mz_ann <- get_int_ann(
        db_get_annotations(db),
        db_get_spectra_infos(db),
        nsamples,
        val = "mz"
    )
    if (nrow(int_ann) == 0) {
        return(p)
    }

    # get the lines which respect the filters
    if (annotation_filter == "no annotated") {
        idx <- which(is.na(int_ann$Name))
    } else if (annotation_filter == "annotated") {
        idx <- which(!is.na(int_ann$Name))
    } else {
        idx <- seq(nrow(int_ann))
    }
    ints <- apply(int_ann[, (ncol(int_ann) - nsamples + 1):ncol(int_ann)],
                  1, max, na.rm = TRUE)
    idx <- intersect(idx, which(ints >= int_threshold))
    if (length(idx) == 0) {
        return(p)
    }

    # get data points
    row_ids <- seq(nrow(mz_ann))[idx]
    mz_ann <- mz_ann[idx, , drop = FALSE]
    ints <- ints[idx]
    mzs <- apply(mz_ann[, (ncol(mz_ann) - nsamples + 1):ncol(mz_ann)],
                 1, median, na.rm = TRUE)
    rts <- mz_ann[, "rT (min)"]
    if (type == "Kendrick plot") {
        km <- get_kendrick_mass(mzs)
        x <- km$x
        y <- km$y
    } else {
        x <- rts
        y <- mzs
    }

    # trace
    p <- plotly::add_trace(
        p,
        x = x,
        y = y,
        customdata = row_ids,
        name = mz_ann[, "PCGroup ID"],
        color = ints,
        hoverinfo = "text",
        text = paste0(
            apply(mz_ann, 1, function(x) {
                if (is.na(x["Name"])) ""
                else paste0(x["Name"], "<br />", x["Adduct"], "<br />")
            }),
            sprintf(
                "m/z: %s<br />rT: %smin<br />Intensity: %s",
                round(mzs, 5),
                round(rts, 2),
                prettyNum(round(ints), big.mark = " ")
            )
        ),
        showlegend = FALSE
    )
    if (any(!is.na(mz_ann$Name))) {
        idx <- which(!is.na(mz_ann$Name))
        p <- plotly::add_annotations(
            p,
            x = x[idx],
            y = y[idx],
            text = sprintf(
                "%s<br />%s",
                mz_ann[idx, "Name"],
                mz_ann[idx, "Adduct"]
            ),
            xref = "x",
            yref = "y",
            showarrow = TRUE
        )
    }
    htmlwidgets::onRender(p, '
        function(el, x) {
            el.on("plotly_hover", function(eventdata) {
                var group_id_index = el._fullData.findIndex(
                    obj => obj.name == eventdata.points[0].data.name);
                if (group_id_index != -1) {
                    Plotly.Fx.hover(
                        el,
                        Array(el._fullData[group_id_index].x.length)
                            .fill()
                            .map((_, i) =>
                                ({
                                    curveNumber: group_id_index,
                                    pointNumber: i
                                })
                            )
                        );
                }
            });
        }
    ')
}
