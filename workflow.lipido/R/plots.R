#' @title Construct empty MS
#'
#' @description
#' Construct an empty MS
#'
#' @param title `character(1)` title of the plot
#' @param yTitle `character(1)` title of the Y axis
#'
#' @return `plotly` object
plot_empty_MS <- function(title = "Mass Spectra", yTitle = "Intensity") {
    p <- plotly::plot_ly(
        type = "scatter",
        mode = "markers"
    )
    p <- plotly::layout(
        p,
        title = list(
            text = sprintf("<b>%s</b>", title),
            y = .95,
            x = .5,
            font = list(
                family = '"Open Sans",verdana,arial,sans-serif',
                size = 18
            ),
            xanchor = "center",
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
            text = yTitle,
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
    p <- plot_empty_MS(title = "Hybrid mass spectra")

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
plot_annotation_ms <- function(db, name) {
    if (class(db) != "SQLiteConnection") {
        stop("db must be a connection to the sqlite database")
    } else if (class(name) != "character") {
        stop("name must be a character")
    } else if (length(name) != 1) {
        stop("name must be only ONE compound")
    }

    annotations <- db_get_annotations(db, names = name)
    if (nrow(annotations) == 0) {
        return(plot_empty_MS(title = "Hybrid mass spectra"))
    }
    spectra_infos <- db_get_spectra_infos(db)

    # for each line (adduct) select the most intense spectra
    annotation_int <- get_int_ann(annotations, spectra_infos)
    spectras <- lapply(seq(nrow(annotations)), function(i)
        db_get_spectras(
            db,
            annotations[i, which.max(
                annotation_int[i, 9:ncol(annotation_int)]
            ) + 13]
        )
    )
    names(spectras) <- annotations$adduct
    plot_composite_ms(spectras)
}

#' @title Plot empty heatmap
#'
#' @description
#' Plot an empty heatmap
#'
#' @return `plotly`
plot_empty_heatmap <- function() {
    p <- plotly::plot_ly(type = "heatmap")
    p <- plotly::layout(p, hoverlabel = list(namelength = -1))
    plotly::config(
        p,
        responsive = TRUE,
        displaylogo = FALSE,
        modeBarButtons = list(
            list(list(
                name = "toImage",
                title = "Download plot as a png",
                icon = htmlwidgets::JS("Plotly.Icons.camera"),
                click = htmlwidgets::JS(paste0("function(gd) {Plotly.downloadI",
                                               "mage(gd, {format:'png', width:",
                                               "1200, height:400, filename:'he",
                                               "atmap'})}"))
            )),
            list("zoom2d", "autoScale2d")
        )
    )
}

#' @title Plot compound heatmap
#'
#' @description
#' Plot a heamap with in x compounds, in y samples and in z their cumulated
#' intensities of their basepeaks
#'
#' @param db `SQLiteConnection`
#' @param names `character vector` vector of compound names
#'
#' @return `plotly`
#'
#' @export
plot_heatmap <- function(db, names) {
    if (class(db) != "SQLiteConnection") {
        stop("db must be a connection to the sqlite database")
    } else if (class(names) != "character") {
        stop("name must be a character")
    } else if (length(names) < 1) {
        stop("name must contain at least ONE compound")
    }

    ann <- db_get_annotations(db, names = names)
    if (ncol(ann) == 0) {
        return(plot_empty_heatmap())
    }
    spectra_ids <- without_na(unlist(ann[, 14:ncol(ann)]))
    spectra_infos <- db_get_spectra_infos(db, spectra_ids)

    int_ann <- summarise_ann(ann, spectra_infos)
    int_ann <- merge(
        int_ann,
        data.frame(name = names),
        by = "name",
        all = TRUE
    )
    suppressWarnings(int_ann[is.na(int_ann)] <- 0)
    plotly::add_trace(
        plot_empty_heatmap(),
        x = colnames(int_ann)[10:ncol(int_ann)],
        y = int_ann$name,
        z = as.matrix(int_ann[, 10:ncol(int_ann)]),
        hoverinfo = "text",
        text = matrix(
            sprintf(
                "sample: %s<br />cpd: %s<br />intensity: %s",
                rep(colnames(int_ann)[10:ncol(int_ann)], each = nrow(int_ann)),
                rep(int_ann$name, times = nrow(int_ann)),
                formatC(
                    round(unlist(int_ann[, 10:ncol(int_ann)])),
                    big.mark = " ",
                    format = "d"
                )
            ),
            nrow = nrow(int_ann)
        )
    )
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
            x = .5,
            font = list(
                family = "\"Open Sans\",verdana,arial,sans-serif",
                size = 18
            ),
            xanchor = "center",
            yanchor = "bottom"
            ),
        margin = list(t = 50),
        spikedistance = -1,
        hovermode = "x unified",
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

#' @title Plot EIC
#'
#' @description
#' Plot the EIC for a sample and a compound. It will trace all the EIC in the
#' same  plot for all possible adducts used in the processing workflow.
#' The line dashed correspond to the area not integrated & the line colored the
#' retention time range where integrated by XCMS. The two line dashed which
#' surround the trace correspond to the retention time tolerance used for the
#' identification
#' This special EIC help to distinguate if this is the peakpicking which turn
#' wrong or the identification (if it is the rT tolerance or the FWHM window)
#' It contains a special behavior when the mouse hover a trace : it will display
#'  all the hovertext of all traces in a unique textbox allowing the user to
#'   differentiate all the y coordinates of the traces in one shot
#' It is useful when combined with the `plotmzdev` plot next to it
#' It draw also annotation to better view to which adduct correspond the trace
#' (the annotation is placed at the most intense point for the trace)
#' A JS function is added to hide the annotation when the trace is hidden
#'
#' @param db `SQLiteConnection`
#' @param sample_name `character(1)` sample name of the file recorded in db
#' @param name `character(1)` compound name
#'
#' @return `plotly`
#'
#' @export
plot_eic <- function(db, sample_name, name) {
    if (class(db) != "SQLiteConnection") {
        stop("db must be a connection to the sqlite database")
    } else if (class(sample_name) != "character") {
        stop("sample name must be a character")
    } else if (length(sample_name) != 1) {
        stop("sample name must contain only ONE sample name")
    } else if (class(name) != "character") {
        stop("name must be a character")
    } else if (length(name) != 1) {
        stop("name must contain only ONE compound")
    }

    p <- plot_empty_chromato("EIC")

    # load files
    ms_file_pos <- db_read_ms_file(db, sample_name, "positive")
    ms_file_neg <- db_read_ms_file(db, sample_name, "negative")
    # if never processed or file doesn't exists
    if (is.null(ms_file_pos) && is.null(ms_file_neg)) {
        return(p)
    }

    # get params used in process & load all the basepeaks for the compound name
    params <- db_get_params(db)
    adduct_names <- strsplit(params$ann$adduct_names, ";")[[1]]
    chem_db <- load_db(adduct_names, params$ann$instrument, name)
    chem_db <- chem_db[chem_db$iso == "M", , drop = FALSE]

    max_int <- 0
    for (i in seq(nrow(chem_db))) {
        mz_range <- get_mz_range(chem_db[i, "mz"], params$cwt$ppm)
        rt_range <- chem_db[i, "rt"] + c(-params$cwt$peakwidth_max * 3.5,
                                         params$cwt$peakwidth_max * 3.5)
        # get the eic
        eic <- get_eic(
            if (chem_db[i, "charge"] > 0) ms_file_pos else ms_file_neg,
            mz_range,
            rt_range
        )
        if (all(eic$int == 0)) {
            next
        }
        max_int <- max(max_int, eic$int)
        # search if the ion was integrated
        peaks <- dbGetQuery(db, sprintf(
            "SELECT rtmin, rtmax
            FROM peaks
            WHERE sample == \"%s\" AND
                mzmin >= %s AND mzmax <= %s AND
                rtmin >= %s AND rtmax <= %s;",
            sample_name,
            mz_range[1],
            mz_range[2],
            rt_range[1],
            rt_range[2]
        ))
        if (nrow(peaks) > 0) {
            idx <- lapply(seq(nrow(peaks)), function(j)
                which(eic$rt >= peaks[j, "rtmin"] &
                          eic$rt <= peaks[j, "rtmax"])
            )
            idx2 <- do.call(c, lapply(idx, function(id)
                c(if (id[1] != 1) id[1] - 1 else NULL,
                      id,
                      if (id[length(id)] != nrow(eic)) id[length(id)] + 1
                      else NULL)
            ))
            integrated <- eic
            integrated[-idx2, "int"] <- NA
            eic[do.call(c, idx), "int"] <- NA
        } else {
            integrated <- data.frame(rt = NA, int = NA)
        }
        # trace
        p <- plotly::add_trace(
            p,
            mode = "lines",
            x = integrated$rt / 60,
            y = integrated$int,
            name = chem_db[i, "adduct"],
            legendgroup = chem_db[i, "adduct"],
            showlegend = nrow(peaks) > 0
        )
        p <- plotly::add_trace(
            p,
            mode = "lines",
            x = eic$rt / 60,
            y = eic$int,
            name = chem_db[i, "adduct"],
            legendgroup = chem_db[i, "adduct"],
            showlegend = nrow(peaks) == 0,
            line = list(
                color = "rgb(0,0,0)",
                width = 1,
                dash = "dash"
            )
        )
        p <- plotly::add_annotations(
            p,
            x = if (nrow(peaks) == 0) eic[which.max(eic$int), "rt"] / 60
                else integrated[which.max(integrated$int), "rt"] / 60,
            y = if (nrow(peaks) == 0) max(eic$int, na.rm = TRUE)
                else max(integrated$int, na.rm = TRUE),
            text = chem_db[i, "adduct"],
            xref = "x",
            yref = "y",
            valign = "bottom",
            arrowhead = 0
        )
    }
    p <- plotly::add_trace(
        p,
        mode = "lines",
        x = c(rep(chem_db[1, "rt"] - params$ann$rt_tol, 2), NA,
              rep(chem_db[1, "rt"] + params$ann$rt_tol, 2)) / 60,
        y = c(0, max_int, NA, 0, max_int),
        showlegend = FALSE,
        line = list(
            color = "rgb(0,0,0)",
            width = 2,
            dash = "dash"
        ),
        hoverinfo = "skip"
    )
    htmlwidgets::onRender(
        p,
        paste0(
            "function(el, x) {",
                "el.on(\"plotly_restyle\", () => {",
                    "annotations = el.layout.annotations;",
                    "for (var i = 1; i < annotations.length; i++) {",
                        "annotations[i].visible = el._fullData[i * 2 - 1]",
                            ".visible != \"legendonly\"",
                    "}",
                    "Plotly.relayout(el, {",
                        "annotations: annotations",
                    "});",
                "});",
            "}"
        )
    )
}

#' @title Plot empty m/z dev
#'
#' @description
#' Plot an empty plot to use for the m/z deviation
#'
#' @param title `character(1)` title
#'
#' @return `plotly`
plot_empty_mzdev <- function(title = "m/z deviation") {
    p <- plotly::plot_ly(
        type = "scatter",
        mode = "markers"
    )
    p <- plotly::layout(
        p,
        title = list(
            text = sprintf("<b>%s</b>", title),
            y = .95,
            x = .5,
            font = list(
                family = "\"Open Sans\",verdana,arial,sans-serif",
                size = 18
            ),
            xanchor = "center",
            yanchor = "bottom"
        ),
        margin = list(t = 50),
        xaxis = list(
            title = "Retention time",
            titlefont = list(
                family = "\"Open Sans\",verdana,arial,sans-serif",
                size = 18
            ),
            hoverformat = ".2f"
        ),
        yaxis = list(
            exponentformat = "e",
            title = "",
            hoverformat = ".2f"
        ),
        hoverlabel = list(
            namelength = -1
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
                text = "m/z deviation (mDa)",
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
        modeBarButtons = list(
            list(
                list(
                    name = "toImage",
                    title = "Download plot as a png",
                    icon = htmlwidgets::JS("Plotly.Icons.camera"),
                    click = htmlwidgets::JS(paste0("function(gd) {Plotly.downl",
                                                   "oadImage(gd, {format:'png'",
                                                   ", width:1200, height:400, ",
                                                   "filename:'mzdev'})}"))
                )
            ),
            list("zoom2d", "autoScale2d")
        )
    )
}

#' @title Plot m/z deviation
#'
#' @description
#' Plot the m/z deviation of all the m/z in the file compared to the theoretical
#'  compound with adduct (only basepeak)
#' It will trace also a box which represent the m/z tolerance and rT tolernace
#' used for the identification step
#' It is useful to use it with the `plot_eic` plot in order to see if there is
#' a problem during the peak picking or the identification step
#'
#' @param db `SQLiteConnection`
#' @param sample_name `character(1)` sample name of the file recorded in db
#' @param name `character(1)` compound name
#' @param adduct_name `character(1)` name of the adduct used in the
#' identification step
#'
#' @return `plotly`
plot_mzdev <- function(db, sample_name, name, adduct_name) {
    if (class(db) != "SQLiteConnection") {
        stop("db must be a connection to the sqlite database")
    } else if (class(sample_name) != "character") {
        stop("sample name must be a character")
    } else if (length(sample_name) != 1) {
        stop("sample name must contain only ONE sample name")
    } else if (class(name) != "character") {
        stop("name must be a character")
    } else if (length(name) != 1) {
        stop("name must contain only ONE compound")
    } else if (class(adduct_name) != "character") {
        stop("adduct name must be a character")
    } else if (length(adduct_name) != 1) {
        stop("adduct name must contain only adduct name")
    }

    p <- plot_empty_mzdev()

    # get params used in process & load all the basepeaks for the compound name
    params <- db_get_params(db)
    # if never processed
    if (ncol(params$ann) == 0) {
        return(p)
    }
    chem_db <- load_db(adduct_name, params$ann$instrument, name)
    chem_db <- chem_db[chem_db$iso == "M", ]

    # load file
    ms_file <- db_read_ms_file(
        db,
        sample_name,
        if (chem_db$charge > 0) "positive" else "negative"
    )
    # if file doesn't exists
    if (is.null(ms_file)) {
        return(p)
    }

    mz_dev <- xcms::rawMat(ms_file,
        get_mz_range(chem_db$mz, params$cwt$ppm),
        chem_db$rt + c(-params$cwt$peakwidth_max * 3.5,
                       params$cwt$peakwidth_max * 3.5)
    )
    # trace
    p <- plotly::add_trace(
        p,
        mode = "markers",
        x = mz_dev[, "time"] / 60,
        y = (chem_db$mz - mz_dev[, "mz"]) * 10**3,
        showlegend = FALSE,
        hoverinfo = "text",
        name = adduct_name,
        text = sprintf(
            "rT: %s min<br />m/z deviation: %s mDa",
            round(mz_dev[, "time"] / 60, 2),
            round((chem_db$mz - mz_dev[, "mz"]) * 10**3, 2)
        )
    )
    # plot the limit parameters square
    plotly::add_trace(
        p,
        mode = "lines",
        x = c(rep(chem_db[1, "rt"] - params$ann$rt_tol, 2),
              rep(chem_db[1, "rt"] + params$ann$rt_tol, 2),
              chem_db[1, "rt"] - params$ann$rt_tol) / 60,
        y = c(-params$ann$da_tol, params$ann$da_tol, params$ann$da_tol,
              -params$ann$da_tol, -params$ann$da_tol) * 10**3,
        showlegend = FALSE,
        line = list(
            color = "rgb(0,0,0)",
            width = 2,
            dash = "dash"
        ),
        hoverinfo = "none"
    )
}
