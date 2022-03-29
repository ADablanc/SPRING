#' @title Construct empty MS
#'
#' @description
#' Construct an empty MS
#'
#' @param title `character(1)` title of the plot
#' @param yTitle `character(1)` title of the Y axis
#'
#' @return plotly object
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

    annotations <- db_get_annotations(db, name)
    if (nrow(annotations) == 0) {
        return(plot_empty_MS(title = "Hybrid mass spectra"))
    }
    spectra_infos <- db_get_spectra_infos(db)

    # select a referent sample, the one where all the adducts were founded
    annotation_int <- get_int_ann(annotations, spectra_infos)
    j <- which(sapply(annotation_int[, 9:ncol(annotation_int)], function(x)
        all(!is.na(x))))
    # now the sample where the intensity was the highest
    # dont forget that the index begin at +8 for annotation_int !!!
    if (length(j) == 0) {
        j <- which.max(apply(annotation_int[, 9:ncol(annotation_int),
                                            drop = FALSE], 2, sum,
                             na.rm = TRUE))
    } else if (length(j) > 1) {
        j <- j[which.max(sapply(annotation_int[, j + 8], sum))]
    }

    spectras <- lapply(annotations[, j + 13], function(spectra_id)
        if (is.na(spectra_id)) NULL
        else db_get_spectras(db, spectra_id))
    names(spectras) <- annotations$adduct
    spectras <- spectras[lengths(spectras) > 0]
    plot_composite_ms(spectras)
}
