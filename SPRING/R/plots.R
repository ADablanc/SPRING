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
#' Plot a heamap with in x compounds, in y samples and in z the intensity of the
#' basepeak with the referent adduct
#'
#' @param db `SQLiteConnection`
#' @param names `character vector` vector of compound names
#'
#' @return `plotly`
#'
#' @export
#' @examples
#' \dontrun{
#' plot_heatmap(db, c("LPC 11:0", "PS 24:0"))
#' }
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
    nsamples <- db_get_nsamples(db)
    spectra_ids <- na.omit(unlist(
        ann[, (ncol(ann) - nsamples + 1):ncol(ann)]
    ))
    spectra_infos <- db_get_spectra_infos(db, spectra_ids)

    int_ann <- merge(
        summarise_ann(ann, spectra_infos, nsamples)$resume,
        data.frame(Name = names),
        all = TRUE
    )
    plotly::add_trace(
        plot_empty_heatmap(),
        x = int_ann$Name,
        y = colnames(int_ann)[(ncol(int_ann) - nsamples + 1):ncol(int_ann)],
        z = t(int_ann[, (ncol(int_ann) - nsamples + 1):ncol(int_ann)]),
        hoverinfo = "text",
        text = matrix(
            sprintf(
                "sample: %s<br />cpd: %s<br />intensity: %s",
                rep(colnames(int_ann)[
                    (ncol(int_ann) - nsamples + 1):ncol(int_ann)
                    ], times = nrow(int_ann)
                ),
                rep(int_ann$Name, each = nsamples),
                formatC(
                    round(unlist(t(
                        int_ann[, (ncol(int_ann) - nsamples + 1):ncol(int_ann)]
                    ))),
                    big.mark = " ",
                    format = "d"
                )
            ),
            ncol = nrow(int_ann)
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
#' same  plot for all possible adducts detected in the processing workflow.
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
#' If available it will use the retention time corrected in the slot
#' `scantime_corrected` added by the function `obiwarp`
#'
#' @param db `SQLiteConnection`
#' @param sample_name `character(1)` sample name of the file recorded in db
#' @param name `character(1)` compound name
#'
#' @return `plotly`
#'
#' @export
#' @examples
#' \dontrun{
#' plot_eic(db, "220221CCM_global__01_ssleu_filtered", "LPC 11:0")
#' }
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
    ms_file <- db_read_ms_file(db, sample_name)
    if (is.null(ms_file)) {
        return(p)
    }

    # get params used in process & load all the basepeaks for the compound name
    params <- db_get_params(db)
    chem_db <- load_ion_db(
        database = params$ann$database,
        instrument = params$ann$instrument,
        polarity = params$ann$polarity,
        cpd_names = name
    )
    # if the compound is detected
    adduct_names <- db_get_annotations(db, names = name)$adduct
    if (length(adduct_names) > 1) {
        # compound was detected with more than one adduct !
            # plot other EICs
        ions <- do.call(rbind, lapply(adduct_names, function(adduct_name) {
            get_ions(
                chem_db[1, "formula"],
                adducts[adducts$name == adduct_name, ],
                params$ann$instrument
            )[[1]]
        }))
    } else {
        ions <- chem_db
    }
    ions <- ions[ions$iso == "M", , drop = FALSE]

    max_int <- 0
    for (i in seq(nrow(ions))) {
        mz_range <- get_mz_range(ions[i, "mz"], params$cwt$ppm)
        rt_range <- chem_db[1, "rt"] + c(-params$cwt$peakwidth_max * 3.5,
                                         params$cwt$peakwidth_max * 3.5)
        # get the eic
        eic <- get_eic(ms_file, mz_range, rt_range)
        if (all(eic$int == 0)) {
            next
        }
        max_int <- max(max_int, eic$int)
        # search if the ion was integrated
        peaks <- db_get_query(db, sprintf(
            "SELECT rtmin, rtmax
            FROM spectras
            WHERE mzmin >= %s AND mzmax <= %s AND
                rtmin >= %s AND rtmax <= %s AND
                spectra_id in (
                    SELECT spectra_id
                        FROM spectra_infos
                        WHERE SAMPLE == \"%s\" AND
                        rt BETWEEN %s and %s
                );",
            mz_range[1],
            mz_range[2],
            rt_range[1],
            rt_range[2],
            sample_name,
            rt_range[1],
            rt_range[2]
        ))
        if (nrow(peaks) > 0) {
            idx <- lapply(seq(nrow(peaks)), function(j) {
                which(eic$rt >= peaks[j, "rtmin"] &
                          eic$rt <= peaks[j, "rtmax"])
            })
            idx2 <- do.call(c, lapply(idx, function(id) {
                c(if (id[1] != 1) id[1] - 1 else NULL,
                      id,
                      if (id[length(id)] != nrow(eic)) id[length(id)] + 1
                      else NULL)
            }))
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
            x = round(integrated$rt / 60, 3),
            y = integrated$int,
            name = ions[i, "adduct"],
            legendgroup = ions[i, "adduct"],
            showlegend = nrow(peaks) > 0
        )
        p <- plotly::add_trace(
            p,
            mode = "lines",
            x = round(eic$rt / 60, 3),
            y = eic$int,
            name = ions[i, "adduct"],
            legendgroup = ions[i, "adduct"],
            showlegend = nrow(peaks) == 0,
            line = list(
                color = "rgb(0,0,0)",
                width = 1,
                dash = "dash"
            )
        )
        p <- plotly::add_annotations(
            p,
            x = if (nrow(peaks) == 0) round(
                    eic[which.max(eic$int), "rt"] / 60, 3)
                else round(integrated[which.max(integrated$int), "rt"] / 60, 3),
            y = if (nrow(peaks) == 0) max(eic$int, na.rm = TRUE)
                else max(integrated$int, na.rm = TRUE),
            text = ions[i, "adduct"],
            xref = "x",
            yref = "y",
            valign = "bottom",
            arrowhead = 0
        )
    }
    p <- plotly::add_trace(
        p,
        mode = "lines",
        x = round(c(rep(chem_db[1, "rt"] - params$ann$rt_tol, 2), NA,
              rep(chem_db[1, "rt"] + params$ann$rt_tol, 2)) / 60, 3),
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

#' @title Plot DB EIC
#'
#' @description
#' Plot the EIC for all the sample for a basepeak mass recorded in the database
#' The line dashed correspond to the area not integrated & the line colored the
#' retention time range where integrated by XCMS.
#' It contains a special behavior when the mouse hover a trace : it will display
#'  all the hovertext of all traces in a unique textbox allowing the user to
#'   differentiate all the y coordinates of the traces in one shot
#' If available it will use the retention time corrected in the slot
#' `scantime_corrected` added by the function `obiwarp`
#'
#' @param db `SQLiteConnection`
#' @param eic_id `numeric(1)` eic ID in database, it corresponds to the row ID
#' for the annotation table in the database
#'
#' @return `plotly`
#'
#' @export
#' @examples
#' \dontrun{
#' plot_db_eic(db, eic_id = 1)
#' }
plot_db_eic <- function(db, eic_id) {
    if (class(db) != "SQLiteConnection") {
        stop("db must be a connection to the sqlite database")
    } else if (class(eic_id) != "numeric" && class(eic_id) != "integer") {
        stop("eic_id must be a numeric")
    } else if (length(eic_id) != 1) {
        stop("eic_id must contain only ONE ID")
    }

    p <- plot_empty_chromato("EIC")

    # get eic data
    ann <- db_get_annotations(db, row = eic_id)
    data <- db_get_eic(db, eic_id)
    nsamples <- db_get_nsamples(db)

    for (i in 2:ncol(data)) {
        eic <- data[, c(1, i)]

        # search the born rT where the peak was integrated
        spec_id <- ann[1, ncol(ann) - nsamples + i - 1]
        if (!is.na(spec_id)) {
            spectra <- db_get_spectras(db, spec_id)
            basepeak <- spectra[which(spectra$abd == 100), ]
            idx <- which(eic$rt >= basepeak$rtmin & eic$rt <= basepeak$rtmax)
            idx <- seq(
                if (idx[1] != 1) idx[1] <- idx[1] - 1 else 1,
                if (idx[length(idx)] < nrow(eic)) idx[length(idx)] + 1
                else nrow(eic)
            )
            integrated <- eic
            integrated[-idx, 2] <- NA
            eic[idx, 2] <- NA
        } else {
            integrated <- eic[0, ]
        }

        # find spec id in database
        p <- plotly::add_trace(
            p,
            mode = "lines",
            x = round(integrated$rt / 60, 3),
            y = integrated[, 2],
            name = colnames(data)[i],
            showlegend = FALSE
        )
        p <- plotly::add_trace(
            p,
            mode = "lines",
            x = round(eic$rt / 60, 3),
            y = eic[, 2],
            name = colnames(data)[i],
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
#' If available it will use the retention time corrected in the slot
#' `scantime_corrected` added by the function `obiwarp`
#'
#' @param db `SQLiteConnection`
#' @param sample_name `character(1)` sample name of the file recorded in db
#' @param name `character(1)` compound name
#'
#' @return `plotly`
#' @export
#' @examples
#' \dontrun{
#' plot_mzdev(
#'      db,
#'      "220221CCM_global__01_ssleu_filtered",
#'      "LPC 11:0",
#'      "[M+H]+"
#' )
#' }
plot_mzdev <- function(db, sample_name, name) {
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

    p <- plot_empty_mzdev()

    # load file
    ms_file <- db_read_ms_file(db, sample_name)
    # if file doesn't exists
    if (is.null(ms_file)) {
        return(p)
    }

    # get params used in process & load all the basepeaks for the compound name
    params <- db_get_params(db)
    chem_db <- load_ion_db(
        database = params$ann$database,
        instrument = params$ann$instrument,
        polarity = params$ann$polarity,
        cpd_names = name
    )
    # if the compound is detected
    adduct_names <- db_get_annotations(db, names = name)$adduct
    if (length(adduct_names) > 1) {
        # compound was detected with more than one adduct !
        # plot other EICs
        ions <- do.call(rbind, lapply(adduct_names, function(adduct_name) {
            get_ions(
                chem_db[1, "formula"],
                adducts[adducts$name == adduct_name, ],
                params$ann$instrument
            )[[1]]
        }))
    } else {
        ions <- chem_db
    }
    ions <- ions[ions$iso == "M", , drop = FALSE]

    for (i in seq(nrow(ions))) {
        mz_range <- get_mz_range(ions[i, "mz"], params$cwt$ppm)
        rt_range <- chem_db[1, "rt"] + c(-params$cwt$peakwidth_max * 3.5,
                                         params$cwt$peakwidth_max * 3.5)
        mz_dev <- get_mzdev(ms_file, mz_range, rt_range)

        p <- plotly::add_trace(
            p,
            mode = "markers",
            x = round(mz_dev$rt / 60, 3),
            y = round((ions[i, "mz"] - mz_dev$mz) * 10**3, 3),
            name = ions[i, "adduct"],
            showlegend = TRUE,
            hoverinfo = "text",
            text = sprintf(
                "rT: %s min<br />m/z deviation: %s mDa",
                round(mz_dev$rt / 60, 2),
                round((ions[i, "mz"] - mz_dev$mz) * 10**3, 2)
            )
        )
    }
    # plot the limit parameters square
    plotly::add_trace(
        p,
        mode = "lines",
        x = round(c(rep(chem_db[1, "rt"] - params$ann$rt_tol, 2),
              rep(chem_db[1, "rt"] + params$ann$rt_tol, 2),
              chem_db[1, "rt"] - params$ann$rt_tol) / 60, 3),
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

#' @title Plot EIC & m/z deviation
#'
#' @description
#' Plot the EIC for a sample and a compound with the m/z deviations compared to
#' the theoreticals.
#' It will trace all the EIC in the same plot for all possible adducts detected
#'  in the processing workflow (same for the m/z deviation). The line dashed
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
#' If available it will use the retention time corrected in the slot
#' `scantime_corrected` added by the function `obiwarp`
#'
#' @param db `SQLiteConnection`
#' @param sample_name `character(1)` sample name of the file recorded in db
#' @param name `character(1)` compound name
#'
#' @return `plotly`
#'
#' @export
#' @examples
#' \dontrun{
#' plot_eic_mzdev(db, "220221CCM_global__01_ssleu_filtered", "LPC 11:0")
#' }
plot_eic_mzdev <- function(db, sample_name, name) {
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

    p1 <- plot_empty_chromato("EIC")
    p2 <- plot_empty_mzdev()

    # load files
    ms_file <- db_read_ms_file(db, sample_name)
    if (is.null(ms_file)) {
        return(suppressWarnings(
            plotly::subplot(p1, p2, nrows = 2, shareX = TRUE)
        ))
    }

    # get params used in process & load all the basepeaks for the compound name
    params <- db_get_params(db)

    chem_db <- load_ion_db(
        database = params$ann$database,
        instrument = params$ann$instrument,
        polarity = params$ann$polarity,
        cpd_names = name
    )
    # if the compound is detected
    adduct_names <- db_get_annotations(db, names = name)$adduct
    if (length(adduct_names) > 1) {
        # compound was detected with more than one adduct !
        ions <- do.call(rbind, lapply(adduct_names, function(adduct_name) {
            get_ions(
                chem_db[1, "formula"],
                adducts[adducts$name == adduct_name, ],
                params$ann$instrument
            )[[1]]
        }))
    } else {
        ions <- chem_db
    }
    ions <- ions[ions$iso == "M", , drop = FALSE]

    max_int <- 0
    colors <- suppressWarnings(RColorBrewer::brewer.pal(nrow(ions), "Set2"))
    for (i in seq(nrow(ions))) {
        mz_range <- get_mz_range(ions[i, "mz"], params$cwt$ppm)
        rt_range <- chem_db[1, "rt"] + c(-params$cwt$peakwidth_max * 3.5,
                                         params$cwt$peakwidth_max * 3.5)
        # get the eic
        eic <- get_eic(ms_file, mz_range, rt_range)
        if (all(eic$int == 0)) {
            next
        }
        mz_dev <- get_mzdev(ms_file, mz_range, rt_range)
        max_int <- max(max_int, eic$int)
        # search if the ion was integrated
        peaks <- db_get_query(db, sprintf(
            "SELECT rtmin, rtmax
            FROM spectras
            WHERE mzmin >= %s AND mzmax <= %s AND
                rtmin >= %s AND rtmax <= %s AND
                spectra_id in (
                    SELECT spectra_id
                        FROM spectra_infos
                        WHERE sample == \"%s\" AND
                            rt BETWEEN %s and %s
                );",
            mz_range[1],
            mz_range[2],
            rt_range[1],
            rt_range[2],
            sample_name,
            rt_range[1],
            rt_range[2]
        ))
        if (nrow(peaks) > 0) {
            idx <- lapply(seq(nrow(peaks)), function(j) {
                which(eic$rt >= peaks[j, "rtmin"] &
                          eic$rt <= peaks[j, "rtmax"])
            })
            idx2 <- do.call(c, lapply(idx, function(id) {
                c(if (id[1] != 1) id[1] - 1 else NULL,
                  id,
                  if (id[length(id)] != nrow(eic)) id[length(id)] + 1
                  else NULL)
            }))
            integrated <- eic
            integrated[-idx2, "int"] <- NA
            eic[do.call(c, idx), "int"] <- NA
        } else {
            integrated <- data.frame(rt = NA, int = 0)
        }
        # trace
        p1 <- plotly::add_trace(
            p1,
            mode = "lines",
            x = round(integrated$rt / 60, 3),
            y = integrated$int,
            name = ions[i, "adduct"],
            color = colors[i],
            legendgroup = ions[i, "adduct"],
            showlegend = nrow(peaks) > 0
        )
        p1 <- plotly::add_trace(
            p1,
            mode = "lines",
            x = round(eic$rt / 60, 3),
            y = eic$int,
            name = ions[i, "adduct"],
            color = colors[i],
            legendgroup = ions[i, "adduct"],
            showlegend = nrow(peaks) == 0,
            line = list(
                color = "black",
                width = 1,
                dash = "dash"
            )
        )
        p1 <- plotly::add_annotations(
            p1,
            x = if (nrow(peaks) == 0) round(
                eic[which.max(eic$int), "rt"] / 60, 3)
            else round(integrated[which.max(integrated$int), "rt"] / 60, 3),
            y = if (nrow(peaks) == 0) max(eic$int, na.rm = TRUE)
            else max(integrated$int, na.rm = TRUE),
            text = ions[i, "adduct"],
            xref = "x",
            yref = "y",
            valign = "bottom",
            arrowhead = 0
        )
        p2 <- plotly::add_trace(
            p2,
            mode = "markers",
            x = round(mz_dev$rt / 60, 3),
            y = round((ions[i, "mz"] - mz_dev$mz) * 10**3, 3),
            name = ions[i, "adduct"],
            color = colors[i],
            legendgroup = ions[i, "adduct"],
            showlegend = TRUE,
            hoverinfo = "text",
            text = sprintf(
                "rT: %s min<br />m/z deviation: %s mDa",
                round(mz_dev$rt / 60, 2),
                round((ions[i, "mz"] - mz_dev$mz) * 10**3, 2)
            )
        )
    }
    p1 <- plotly::add_trace(
        p1,
        mode = "lines",
        x = round(c(rep(chem_db[1, "rt"] - params$ann$rt_tol, 2), NA,
              rep(chem_db[1, "rt"] + params$ann$rt_tol, 2)) / 60, 3),
        y = c(0, max_int, NA, 0, max_int),
        showlegend = FALSE,
        line = list(
            color = "rgb(0,0,0)",
            width = 2,
            dash = "dash"
        ),
        hoverinfo = "skip"
    )
    p2 <- plotly::add_trace(
        p2,
        mode = "lines",
        x = round(c(rep(chem_db[1, "rt"] - params$ann$rt_tol, 2),
              rep(chem_db[1, "rt"] + params$ann$rt_tol, 2),
              chem_db[1, "rt"] - params$ann$rt_tol) / 60, 3),
        y = c(-params$ann$da_tol, params$ann$da_tol, params$ann$da_tol,
              -params$ann$da_tol, -params$ann$da_tol) * 10**3,
        showlegend = FALSE,
        line = list(
            color = "rgb(0,0,0)",
            width = 2,
            dash = "dash"
        ),
        hoverinfo = "skip"
    )
    p <- suppressWarnings(plotly::subplot(p1, p2, nrows = 2, shareX = TRUE))
    htmlwidgets::onRender(
        plotly::layout(p, title = ""),
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

#' @title Plot empty MS map
#'
#' @description
#' Plot an empty MS map with custom axis
#'
#' @param title `character(1)` title of the plot
#' @param xaxis_title `character(1)` title of the x axis
#' @param yaxis_title `character(1)` title of the y axis
#'
#' @return `plotly` object
plot_empty_peak_spot <- function(title = "MS map", xaxis_title = "Retention time",
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

#' @title Plot MS map
#'
#' @description
#' Plot a MS map or Kendrick plot
#' Each basepeak are plotted with their annotations
#' Hover a trace will show all popup for all basepeak with the same group ID (
#' same compound flagged by CAMERA)
#' Possibility to trace the "MS map" (m/z fct(rT)) or a Kendrick plot
#' Each trace will contain the EIC ID in their `customdata` slot
#'
#' @param db `SQLiteConnection`
#' @param annotation_filter `character(1)` annotation to filter (can be
#' "no annotated", "annotated" or "all")
#' @param int_threshold `numeric(1)` intensity threshold
#' @param type `character(1)` "MS map" or "Kendrick plot"
#'
#' @return `plotly`
#'
#' @export
#' @examples
#' \dontrun{
#' plot_peak_spot(db)
#' }
plot_peak_spot <- function(db, annotation_filter = "all", int_threshold = 0,
                        type = "MS map") {
    if (class(db) != "SQLiteConnection") {
        stop("db must be a connection to the sqlite database")
    } else if (!is.numeric(int_threshold)) {
        stop("int_treshold must be a numeric")
    } else if (class(annotation_filter) != "character") {
        stop("annotation_filter must be a character")
    } else if (!annotation_filter %in% c("no annotated", "annotated", "all")) {
        stop("annotation_filter must be \"no annotated\", \"annotated\" or
            \"all\"")
    } else if (!type %in% c("MS map", "Kendrick plot")) {
        stop("type must be \"MS map\" or \"Kendrick plot\"")
    }

    if (type == "MS map") {
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
    eic_ids <- seq(nrow(mz_ann))

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
    mz_ann <- mz_ann[idx, , drop = FALSE]
    ints <- ints[idx]
    eic_ids <- eic_ids[idx]
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
        customdata = eic_ids,
        name = mz_ann[, "Group ID"],
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
