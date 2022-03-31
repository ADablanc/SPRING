testthat::test_that("plot empty MS", {
    config <- list(
        modeBarButtonsToAdd = c("hoverclosest", "hovercompare"),
        showSendToCloud = FALSE,
        responsive = TRUE,
        scrollZoom = FALSE,
        displaylogo = FALSE,
        edits = list(
            annotationTail = TRUE
        ),
        modeBarButtons = list(
            list(
                list(
                    name = "toImage",
                    title = "Download plot as a png",
                    icon = "Plotly.Icons.camera",
                    click = paste0("function(gd) {Plotly.downloadImage(gd, ",
                                   "{format:'png', width:1200, height:400, ",
                                   "filename:'MS'})}"
                    )
                ), list(
                    "zoom2d",
                    "autoScale2d"
                )
            )
        )
    )
    layout_attrs <- list(
        list(
            title = list(
                text = "<b>Mass Spectra</b>",
                y = 0.95,
                x = 0.5,
                font = list(
                    family = "\"Open Sans\",verdana,arial,sans-serif",
                    size = 18
                ),
                xanchor = "center",
                yanchor = "bottom"
            ),
            margin = list(
                t = 50
            ),
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
    )
    plot <- plot_empty_MS()
    # test also the config
    testthat::expect_true(all(
        unlist(plot[[1]]$config) == unlist(config),
        na.rm = TRUE
    ))
    # test also the layout
    testthat::expect_true(all(
        unlist(plot[[1]]$layoutAttrs) == unlist(layout_attrs),
        na.rm = TRUE
    ))
})

testthat::test_that("plot composite MS", {
    traces <- list(
        list(
            mode = "markers",
            alpha_stroke = 1,
            sizes = c(10, 100),
            spans = c(1, 20),
            type = "scatter"
        ),
        list(
            mode = "lines",
            alpha_stroke = 1,
            sizes = c(10, 100),
            spans = c(1, 20),
            type = "scatter",
            x = c(408.251325886321, 408.25095),
            y = 0,
            xend = c(408.251325886321, 408.25095),
            yend = c(88824.635233072, -88824.635233072),
            name = "[M+H-H2O]+",
            legendgroup = "[M+H-H2O]+",
            showlegend = TRUE,
            hoverinfo = "text",
            text = c(
                paste("observed", "adduct: [M+H-H2O]+", "iso: M",
                      "m/z: 408.25133", "abd: 100%", sep = "<br />"),
                paste("theoretical", "adduct: [M+H-H2O]+", "iso: M",
                      "m/z: 408.25095", "abd: 100%", sep = "<br />")
            ),
            inherit = TRUE
        ),
        list(
            mode = "lines",
            alpha_stroke = 1,
            sizes = c(10, 100),
            spans = c(1, 20),
            type = "scatter",
            x = c(NA, NA, NA, 411.25935, 409.25427, 410.25672),
            y = 0,
            xend = c(NA, NA, NA, 411.25935, 409.25427, 410.25672),
            yend = c(NA, NA, NA, -337.533613885674, -19230.5335279601,
                     -2886.80064507484),
            color = "black",
            legendgroup = "[M+H-H2O]+",
            showlegend = FALSE,
            hoverinfo = "text",
            text = c(
                "observed<br /><br />m/z: NA",
                "observed<br /><br />m/z: NA",
                "observed<br /><br />m/z: NA",
                paste("theoretical", "adduct: [M+H-H2O]+", "iso: M+3",
                      "m/z: 411.25935", "abd: 0%", sep = "<br />"),
                paste("theoretical", "adduct: [M+H-H2O]+", "iso: M+1",
                      "m/z: 409.25427", "abd: 22%", sep = "<br />"),
                paste("theoretical", "adduct: [M+H-H2O]+", "iso: M+2",
                      "m/z: 410.25672", "abd: 3%", sep = "<br />")
            ),
            inherit = TRUE
        ),
        list(
            mode = "lines",
            alpha_stroke = 1,
            sizes = c(10, 100),
            spans = c(1, 20),
            type = "scatter",
            x = c(426.262333104945, 427.265704881008, 426.26152, 427.26484),
            y = 0,
            xend = c(426.262333104945, 427.265704881008, 426.26152,
                     427.26484),
            yend = c(6201250.27168528, 1186767.56444882, -6201250.27168528,
                     -1345671.30895571),
            name = "[M+H]+",
            legendgroup = "[M+H]+",
            showlegend = TRUE,
            hoverinfo = "text",
            text = c(
                paste("observed", "adduct: [M+H]+", "iso: M",
                      "m/z: 426.26233", "abd: 100%", sep = "<br />"),
                paste("observed", "adduct: [M+H]+", "iso: M+1",
                      "m/z: 427.2657", "abd: 19%", sep = "<br />"),
                paste("theoretical", "adduct: [M+H]+", "iso: M",
                      "m/z: 426.26152", "abd: 100%", sep = "<br />"),
                paste("theoretical", "adduct: [M+H]+", "iso: M+1",
                      "m/z: 427.26484", "abd: 22%", sep = "<br />")
            ),
            inherit = TRUE
        ),
        list(
            mode = "lines",
            alpha_stroke = 1,
            sizes = c(10, 100),
            spans = c(1, 20),
            type = "scatter",
            x = c(NA, NA, 428.26719, 429.26984),
            y = 0,
            xend = c(NA, NA, 428.26719, 429.26984),
            yend = c(NA, NA, -214563.259400311, -26045.2511410782),
            color = "black",
            legendgroup = "[M+H]+",
            showlegend = FALSE,
            hoverinfo = "text",
            text = c(
                "observed<br /><br />m/z: NA",
                "observed<br /><br />m/z: NA",
                paste("theoretical", "adduct: [M+H]+", "iso: M+2",
                      "m/z: 428.26719", "abd: 3%", sep = "<br />"),
                paste("theoretical", "adduct: [M+H]+", "iso: M+3",
                      "m/z: 429.26984", "abd: 0%", sep = "<br />")
            ),
            inherit = TRUE
        ),
        list(
            mode = "lines",
            alpha_stroke = 1,
            sizes = c(10, 100),
            spans = c(1, 20),
            type = "scatter",
            x = c(448.244170162955, 448.24346),
            y = 0,
            xend = c(448.244170162955, 448.24346),
            yend = c(288290.748778874, -288290.748778874),
            name = "[M+Na]+",
            legendgroup = "[M+Na]+",
            showlegend = TRUE,
            hoverinfo = "text",
            text = c(
                paste("observed", "adduct: [M+Na]+", "iso: M",
                      "m/z: 448.24417", "abd: 100%", sep = "<br />"),
                paste("theoretical", "adduct: [M+Na]+", "iso: M",
                      "m/z: 448.24346", "abd: 100%", sep = "<br />")
            ),
            inherit = TRUE
        ),
        list(
            mode = "lines",
            alpha_stroke = 1,
            sizes = c(10, 100),
            spans = c(1, 20),
            type = "scatter",
            x = c(NA, NA, NA, 449.24678, 450.24914, 451.25178),
            y = 0,
            xend = c(NA, NA, NA, 449.24678, 450.24914, 451.25178),
            yend = c(NA, NA, NA, -62530.2634101378, -9946.03083287115,
                     -1210.82114487127),
            color = "black",
            legendgroup = "[M+Na]+",
            showlegend = FALSE,
            hoverinfo = "text",
            text = c(
                "observed<br /><br />m/z: NA",
                "observed<br /><br />m/z: NA",
                "observed<br /><br />m/z: NA",
                paste("theoretical", "adduct: [M+Na]+", "iso: M+1",
                      "m/z: 449.24678", "abd: 22%", sep = "<br />"),
                paste("theoretical", "adduct: [M+Na]+", "iso: M+2",
                      "m/z: 450.24914", "abd: 3%", sep = "<br />"),
                paste("theoretical", "adduct: [M+Na]+", "iso: M+3",
                      "m/z: 451.25178", "abd: 0%", sep = "<br />")
            ),
            inherit = TRUE
        )
    )
    config <- list(
        modeBarButtonsToAdd = c("hoverclosest", "hovercompare"),
        showSendToCloud = FALSE,
        responsive = TRUE,
        scrollZoom = FALSE,
        displaylogo = FALSE,
        edits = list(
            annotationTail = TRUE
        ),
        modeBarButtons = list(
            list(
                list(
                    name = "toImage",
                    title = "Download plot as a png",
                    icon = "Plotly.Icons.camera",
                    click = paste0("function(gd) {Plotly.downloadImage(gd, ",
                                   "{format:'png', width:1200, height:400, ",
                                   "filename:'MS'})}"
                    )
                ), list(
                    "zoom2d",
                    "autoScale2d"
                )
            )
        )
    )
    layout_attrs <- list(
        list(
            title = list(
                text = "<b>Hybrid mass spectra</b>",
                y = 0.95,
                x = 0.5,
                font = list(
                    family = "\"Open Sans\",verdana,arial,sans-serif",
                    size = 18
                ),
                xanchor = "center",
                yanchor = "bottom"
            ),
            margin = list(
                t = 50
            ),
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
        ),
        list(
            annotations = list(
                text = "[M+H-H2O]+",
                x = 408.251325886321,
                y = 88824.635233072,
                xref = "x",
                yref = "y",
                valign = "bottom",
                arrowhead = 0
            )
        ),
        list(
            annotations = list(
                text = "[M+H]+",
                x = 426.262333104945,
                y = 6201250.27168528,
                xref = "x",
                yref = "y",
                valign = "bottom",
                arrowhead = 0
            )
        ),
        list(
            annotations = list(
                text = "[M+Na]+",
                x = 448.244170162955,
                y = 288290.748778874,
                xref = "x",
                yref = "y",
                valign = "bottom",
                arrowhead = 0
            )
        ),
        list(
            xaxis = list(
                range = c(407.25095, 452.25178)
            )
        )
    )
    js_func <- list(
        render = list(
            list(
                code = paste0(
                    "\n        function(el, x) {\n            el.on(\"plotly_h",
                    "over\", function(eventdata) {\n                var plot_i",
                    "d = $(eventdata.event.srcElement).closest\n              ",
                    "      (\".plotly.html-widget\").get(0).id;\n             ",
                    "   if (eventdata.points[0].data.showlegend) {\n          ",
                    "          Plotly.Fx.hover(plot_id, [\n                   ",
                    "     {\n                            curveNumber: eventdat",
                    "a.points[0].curveNumber,\n                            poi",
                    "ntNumber: eventdata.points[0].pointNumber\n              ",
                    "          },\n                        {\n                ",
                    "        curveNumber: eventdata.points[0].curveNumber,\n  ",
                    "                      pointNumber: eventdata.points[0].po",
                    "intIndex +\n                            (eventdata.points",
                    "[0].fullData.x.length + 1) / 2\n                        }",
                    "\n                    ])\n                }\n            ",
                    "});\n            el.on(\"plotly_restyle\", () => {\n     ",
                    "           annotations = el.layout.annotations;\n        ",
                    "        for (var i = 1; i < annotations.length; i++) {\n ",
                    "                   annotations[i].visible = el._fullData[",
                    "i * 2 - 1].visible !=\n                        \"legendon",
                    "ly\"\n                }\n                Plotly.relayout(",
                    "el, {\n                    annotations: annotations,\n   ",
                    "                 xaxis: {\n                        range:",
                    " [\n                            Math.min(...el._fullData",
                    "\n                                .filter(x => x.visible ",
                    "== true)\n                                .map(x => Math.",
                    "min(...x.x\n                                    .filter(y",
                    " =>  y!= null))\n                            )) - 1,\n   ",
                    "                         Math.max(...el._fullData\n      ",
                    "                          .filter(x => x.visible == true)",
                    "\n                                .map(x => Math.max(...x",
                    ".x\n                                    .filter(y =>  y!=",
                    " null))\n                                )) + 1\n        ",
                    "                ]\n                    }\n               ",
                    " });\n            });\n        }\n    "),
                data = NULL
            )
        )
    )
    spectras <- data.frame(
        spectra_id = c(1, 1, 1, 1, 3, 3, 3, 3, 4, 4, 4, 4),
        feature_id = c(NA, 17, NA, NA, 20, 19, NA, NA, 18, NA, NA, NA),
        mz = c(NA, 408.251325886321, NA, NA, 426.262333104945, 427.265704881008,
               NA, NA, 448.244170162955, NA, NA, NA),
        int = c(NA, 88824.635233072, NA, NA, 6201250.27168528, 1186767.56444882,
                NA, NA, 288290.748778874, NA, NA, NA),
        abd = c(NA, 100, NA, NA, 100, 19.1375531135642, NA, NA, 100, NA, NA,
                NA),
        ion_id_theo = c(62, 62, 62, 62, 66, 66, 66, 66, 64, 64, 64, 64),
        mz_theo = c(411.25935, 408.25095, 409.25427, 410.25672, 426.26152,
                    427.26484, 428.26719, 429.26984, 448.24346, 449.24678,
                    450.24914, 451.25178),
        abd_theo = c(0.38, 100, 21.65, 3.25, 100, 21.7, 3.46, 0.42, 100, 21.69,
                     3.45, 0.42),
        iso_theo = c("M+3", "M", "M+1", "M+2", "M", "M+1", "M+2", "M+3", "M",
                     "M+1", "M+2", "M+3")
    )
    spectras <- split(spectras, spectras$spectra_id)
    names(spectras) <- c("[M+H-H2O]+", "[M+H]+", "[M+Na]+")
    plot <- plot_composite_ms(spectras)
    # test if we get all traces
    testthat::expect_true(all(
        unlist(plot[[1]]$attrs) == unlist(traces),
        na.rm = TRUE
    ))
    # test also the config
    testthat::expect_true(all(
        unlist(plot[[1]]$config) == unlist(config),
        na.rm = TRUE
    ))
    # test also the layout
    testthat::expect_true(all(
        unlist(plot[[1]]$layoutAttrs) == unlist(layout_attrs),
        na.rm = TRUE
    ))
    # test also the js func
    testthat::expect_true(all(
        unlist(plot[[8]]) == unlist(js_func),
        na.rm = TRUE
    ))
})

testthat::test_that("plot annotation MS", {
    db <- db_connect(system.file(
        "testdata",
        "220221CCM_global.sqlite",
        package = "workflow.lipido"
    ))
    traces <- list(
        list(
            mode = "markers",
            alpha_stroke = 1,
            sizes = c(10, 100),
            spans = c(1, 20),
            type = "scatter"
        ),
        list(
            mode = "lines",
            alpha_stroke = 1,
            sizes = c(10, 100),
            spans = c(1, 20),
            type = "scatter",
            x = c(408.251325886321, 408.25095),
            y = 0,
            xend = c(408.251325886321, 408.25095),
            yend = c(88824.635233072, -88824.635233072),
            name = "[M+H-H2O]+",
            legendgroup = "[M+H-H2O]+",
            showlegend = TRUE,
            hoverinfo = "text",
            text = c(
                paste("observed", "adduct: [M+H-H2O]+", "iso: M",
                      "m/z: 408.25133", "abd: 100%", sep = "<br />"),
                paste("theoretical", "adduct: [M+H-H2O]+", "iso: M",
                      "m/z: 408.25095", "abd: 100%", sep = "<br />")
            ),
            inherit = TRUE
        ),
        list(
            mode = "lines",
            alpha_stroke = 1,
            sizes = c(10, 100),
            spans = c(1, 20),
            type = "scatter",
            x = c(NA, NA, NA, 411.25935, 409.25427, 410.25672),
            y = 0,
            xend = c(NA, NA, NA, 411.25935, 409.25427, 410.25672),
            yend = c(NA, NA, NA, -337.533613885674, -19230.5335279601,
                     -2886.80064507484),
            color = "black",
            legendgroup = "[M+H-H2O]+",
            showlegend = FALSE,
            hoverinfo = "text",
            text = c(
                "observed<br /><br />m/z: NA",
                "observed<br /><br />m/z: NA",
                "observed<br /><br />m/z: NA",
                paste("theoretical", "adduct: [M+H-H2O]+", "iso: M+3",
                      "m/z: 411.25935", "abd: 0%", sep = "<br />"),
                paste("theoretical", "adduct: [M+H-H2O]+", "iso: M+1",
                      "m/z: 409.25427", "abd: 22%", sep = "<br />"),
                paste("theoretical", "adduct: [M+H-H2O]+", "iso: M+2",
                      "m/z: 410.25672", "abd: 3%", sep = "<br />")
            ),
            inherit = TRUE
        ),
        list(
            mode = "lines",
            alpha_stroke = 1,
            sizes = c(10, 100),
            spans = c(1, 20),
            type = "scatter",
            x = c(426.262333104945, 427.265704881008, 426.26152, 427.26484),
            y = 0,
            xend = c(426.262333104945, 427.265704881008, 426.26152,
                     427.26484),
            yend = c(6201250.27168528, 1186767.56444882, -6201250.27168528,
                     -1345671.30895571),
            name = "[M+H]+",
            legendgroup = "[M+H]+",
            showlegend = TRUE,
            hoverinfo = "text",
            text = c(
                paste("observed", "adduct: [M+H]+", "iso: M",
                      "m/z: 426.26233", "abd: 100%", sep = "<br />"),
                paste("observed", "adduct: [M+H]+", "iso: M+1",
                      "m/z: 427.2657", "abd: 19%", sep = "<br />"),
                paste("theoretical", "adduct: [M+H]+", "iso: M",
                      "m/z: 426.26152", "abd: 100%", sep = "<br />"),
                paste("theoretical", "adduct: [M+H]+", "iso: M+1",
                      "m/z: 427.26484", "abd: 22%", sep = "<br />")
            ),
            inherit = TRUE
        ),
        list(
            mode = "lines",
            alpha_stroke = 1,
            sizes = c(10, 100),
            spans = c(1, 20),
            type = "scatter",
            x = c(NA, NA, 428.26719, 429.26984),
            y = 0,
            xend = c(NA, NA, 428.26719, 429.26984),
            yend = c(NA, NA, -214563.259400311, -26045.2511410782),
            color = "black",
            legendgroup = "[M+H]+",
            showlegend = FALSE,
            hoverinfo = "text",
            text = c(
                "observed<br /><br />m/z: NA",
                "observed<br /><br />m/z: NA",
                paste("theoretical", "adduct: [M+H]+", "iso: M+2",
                      "m/z: 428.26719", "abd: 3%", sep = "<br />"),
                paste("theoretical", "adduct: [M+H]+", "iso: M+3",
                      "m/z: 429.26984", "abd: 0%", sep = "<br />")
            ),
            inherit = TRUE
        ),
        list(
            mode = "lines",
            alpha_stroke = 1,
            sizes = c(10, 100),
            spans = c(1, 20),
            type = "scatter",
            x = c(448.244170162955, 448.24346),
            y = 0,
            xend = c(448.244170162955, 448.24346),
            yend = c(288290.748778874, -288290.748778874),
            name = "[M+Na]+",
            legendgroup = "[M+Na]+",
            showlegend = TRUE,
            hoverinfo = "text",
            text = c(
                paste("observed", "adduct: [M+Na]+", "iso: M",
                      "m/z: 448.24417", "abd: 100%", sep = "<br />"),
                paste("theoretical", "adduct: [M+Na]+", "iso: M",
                      "m/z: 448.24346", "abd: 100%", sep = "<br />")
            ),
            inherit = TRUE
        ),
        list(
            mode = "lines",
            alpha_stroke = 1,
            sizes = c(10, 100),
            spans = c(1, 20),
            type = "scatter",
            x = c(NA, NA, NA, 449.24678, 450.24914, 451.25178),
            y = 0,
            xend = c(NA, NA, NA, 449.24678, 450.24914, 451.25178),
            yend = c(NA, NA, NA, -62530.2634101377, -9946.03083287114,
                     -1210.82114487127),
            color = "black",
            legendgroup = "[M+Na]+",
            showlegend = FALSE,
            hoverinfo = "text",
            text = c(
                "observed<br /><br />m/z: NA",
                "observed<br /><br />m/z: NA",
                "observed<br /><br />m/z: NA",
                paste("theoretical", "adduct: [M+Na]+", "iso: M+1",
                      "m/z: 449.24678", "abd: 22%", sep = "<br />"),
                paste("theoretical", "adduct: [M+Na]+", "iso: M+2",
                      "m/z: 450.24914", "abd: 3%", sep = "<br />"),
                paste("theoretical", "adduct: [M+Na]+", "iso: M+3",
                      "m/z: 451.25178", "abd: 0%", sep = "<br />")
            ),
            inherit = TRUE
        )
    )
    config <- list(
        modeBarButtonsToAdd = c("hoverclosest", "hovercompare"),
        showSendToCloud = FALSE,
        responsive = TRUE,
        scrollZoom = FALSE,
        displaylogo = FALSE,
        edits = list(
            annotationTail = TRUE
        ),
        modeBarButtons = list(
            list(
                list(
                    name = "toImage",
                    title = "Download plot as a png",
                    icon = "Plotly.Icons.camera",
                    click = paste0("function(gd) {Plotly.downloadImage(gd, ",
                                   "{format:'png', width:1200, height:400, ",
                                   "filename:'MS'})}"
                    )
                ), list(
                    "zoom2d",
                    "autoScale2d"
                )
            )
        )
    )
    layout_attrs <- list(
        list(
            title = list(
                text = "<b>Hybrid mass spectra</b>",
                y = 0.95,
                x = 0.5,
                font = list(
                    family = "\"Open Sans\",verdana,arial,sans-serif",
                    size = 18
                ),
                xanchor = "center",
                yanchor = "bottom"
            ),
            margin = list(
                t = 50
            ),
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
        ),
        list(
            annotations = list(
                text = "[M+H-H2O]+",
                x = 408.251325886321,
                y = 88824.635233072,
                xref = "x",
                yref = "y",
                valign = "bottom",
                arrowhead = 0
            )
        ),
        list(
            annotations = list(
                text = "[M+H]+",
                x = 426.262333104945,
                y = 6201250.27168528,
                xref = "x",
                yref = "y",
                valign = "bottom",
                arrowhead = 0
            )
        ),
        list(
            annotations = list(
                text = "[M+Na]+",
                x = 448.244170162955,
                y = 288290.748778874,
                xref = "x",
                yref = "y",
                valign = "bottom",
                arrowhead = 0
            )
        ),
        list(
            xaxis = list(
                range = c(407.25095, 452.25178)
            )
        )
    )
    js_func <- list(
        render = list(
            list(
                code = paste0(
                    "\n        function(el, x) {\n            el.on(\"plotly_h",
                    "over\", function(eventdata) {\n                var plot_i",
                    "d = $(eventdata.event.srcElement).closest\n              ",
                    "      (\".plotly.html-widget\").get(0).id;\n             ",
                    "   if (eventdata.points[0].data.showlegend) {\n          ",
                    "          Plotly.Fx.hover(plot_id, [\n                   ",
                    "     {\n                            curveNumber: eventdat",
                    "a.points[0].curveNumber,\n                            poi",
                    "ntNumber: eventdata.points[0].pointNumber\n              ",
                    "          },\n                        {\n                ",
                    "        curveNumber: eventdata.points[0].curveNumber,\n  ",
                    "                      pointNumber: eventdata.points[0].po",
                    "intIndex +\n                            (eventdata.points",
                    "[0].fullData.x.length + 1) / 2\n                        }",
                    "\n                    ])\n                }\n            ",
                    "});\n            el.on(\"plotly_restyle\", () => {\n     ",
                    "           annotations = el.layout.annotations;\n        ",
                    "        for (var i = 1; i < annotations.length; i++) {\n ",
                    "                   annotations[i].visible = el._fullData[",
                    "i * 2 - 1].visible !=\n                        \"legendon",
                    "ly\"\n                }\n                Plotly.relayout(",
                    "el, {\n                    annotations: annotations,\n   ",
                    "                 xaxis: {\n                        range:",
                    " [\n                            Math.min(...el._fullData",
                    "\n                                .filter(x => x.visible ",
                    "== true)\n                                .map(x => Math.",
                    "min(...x.x\n                                    .filter(y",
                    " =>  y!= null))\n                            )) - 1,\n   ",
                    "                         Math.max(...el._fullData\n      ",
                    "                          .filter(x => x.visible == true)",
                    "\n                                .map(x => Math.max(...x",
                    ".x\n                                    .filter(y =>  y!=",
                    " null))\n                                )) + 1\n        ",
                    "                ]\n                    }\n               ",
                    " });\n            });\n        }\n    "),
                data = NULL
            )
        )
    )

    # 1st test if we have a connection
    testthat::expect_error(
        plot_annotation_ms(NULL, NULL),
        "db must be a connection to the sqlite database"
    )

    # 2nd test if we have a name
    testthat::expect_error(
        plot_annotation_ms(db, NULL),
        "name must be a character"
    )

    # 3rd test if we have a single name
    testthat::expect_error(
        plot_annotation_ms(db, c("FA 17:0", "LPC 11a:0")),
        "name must be only ONE compound"
    )

    # 4th test if the compound doesn't exist in database
    # the plot should be empty
    plot <- plot_annotation_ms(db, "methanol")
    testthat::expect_true(all(
        unlist(plot[[1]]$attrs) == unlist(traces[1]),
        na.rm = TRUE
    ))
    # test also the config
    testthat::expect_true(all(
        unlist(plot[[1]]$config) == unlist(config),
        na.rm = TRUE
    ))
    # test also the layout
    testthat::expect_true(all(
        unlist(plot[[1]]$layoutAttrs) == unlist(layout_attrs[1]),
        na.rm = TRUE
    ))
    # test also the js func
    testthat::expect_true(all(
        unlist(plot[[8]]) == unlist(js_func),
        na.rm = TRUE
    ))

    # 5th test : normal
    plot <- plot_annotation_ms(db, "LPC 11a:0")
    # test if we get all traces
    testthat::expect_true(all(
        unlist(plot[[1]]$attrs) == unlist(traces),
        na.rm = TRUE
    ))
    # test also the config
    testthat::expect_true(all(
        unlist(plot[[1]]$config) == unlist(config),
        na.rm = TRUE
    ))
    # test also the layout
    testthat::expect_true(all(
        unlist(plot[[1]]$layoutAttrs) == unlist(layout_attrs),
        na.rm = TRUE
    ))
    # test also the js func
    testthat::expect_true(all(
        unlist(plot[[8]]) == unlist(js_func),
        na.rm = TRUE
    ))

    # 6th test : normal test but two referent sample with all the adducts is
    # found
    sqlite_path <- db@dbname
    RSQLite::dbDisconnect(db)
    sqlite_path2 <- tempfile(fileext = ".sqlite")
    invisible(file.copy(sqlite_path, sqlite_path2))
    # modify a column in the database just to create the example
    db <- db_connect(sqlite_path2)
    dbExecute(
        db,
        "UPDATE ann
        SET \"220221CCM_global__01_ssleu_filtered\" = 7
        WHERE group_id == 1"
    )
    dbExecute(
        db,
        "UPDATE ann
        SET \"220221CCM_global__01_ssleu_filtered\" = 8
        WHERE group_id == 9"
    )
    dbExecute(
        db,
        "UPDATE spectra_infos
        SET basepeak_int = 0, sum_int = 0
        WHERE spectra_id == 7 OR spectra_id == 8"
    )
    plot <- plot_annotation_ms(db, "LPC 11a:0")
    # test if we get all traces
    testthat::expect_true(all(
        unlist(plot[[1]]$attrs) == unlist(traces),
        na.rm = TRUE
    ))
    # test also the config
    testthat::expect_true(all(
        unlist(plot[[1]]$config) == unlist(config),
        na.rm = TRUE
    ))
    # test also the layout
    testthat::expect_true(all(
        unlist(plot[[1]]$layoutAttrs) == unlist(layout_attrs),
        na.rm = TRUE
    ))
    # test also the js func
    testthat::expect_true(all(
        unlist(plot[[8]]) == unlist(js_func),
        na.rm = TRUE
    ))

    # 7th test : normal test but no referent sample with all the adducts is
    # found
    dbExecute(
        db,
        "UPDATE ann
        SET \"220221CCM_global__01_ssleu_filtered\" = null,
            \"220221CCM_global__02_ssleu_filtered\" = null
        WHERE group_id == 2"
    )
    dbExecute(
        db,
        "UPDATE ann
        SET \"220221CCM_global__01_ssleu_filtered\" = null
        WHERE group_id == 9"
    )
    plot <- plot_annotation_ms(db, "LPC 11a:0")
    RSQLite::dbDisconnect(db)
    # test if we get all traces
    testthat::expect_true(all(
        unlist(plot[[1]]$attrs) == unlist(traces[-c(4, 5)]),
        na.rm = TRUE
    ))
    # test also the config
    testthat::expect_true(all(
        unlist(plot[[1]]$config) == unlist(config),
        na.rm = TRUE
    ))
    # test also the layout
    testthat::expect_true(all(
        unlist(plot[[1]]$layoutAttrs) == unlist(layout_attrs[-3]),
        na.rm = TRUE
    ))
    # test also the js func
    testthat::expect_true(all(
        unlist(plot[[8]]) == unlist(js_func),
        na.rm = TRUE
    ))
})

testthat::test_that("plot empty heatmap", {
    traces <- list(
        alpha_stroke = 1,
        sizes = c(10, 100),
        spans = c(1, 20),
        type = "heatmap"
    )
    config <- list(
        modeBarButtonsToAdd = c("hoverclosest", "hovercompare"),
        showSendToCloud = FALSE,
        responsive = TRUE,
        displaylogo = FALSE,
        modeBarButtons = list(
            list(
                list(
                    name = "toImage",
                    title = "Download plot as a png",
                    icon = "Plotly.Icons.camera",
                    click = paste0("function(gd) {Plotly.downloadImage(gd, ",
                                   "{format:'png', width:1200, height:400, ",
                                   "filename:'heatmap'})}"
                    )
                ), list(
                    "zoom2d",
                    "autoScale2d"
                )
            )
        )
    )
    layout_attrs <- list(
        list(
            hoverlabel = list(
                namelength = -1
            )
        )
    )
    p <- plot_empty_heatmap()
    testthat::expect_true(all(
        unlist(p[[1]]$attrs) == unlist(traces),
        na.rm = TRUE
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$config) == unlist(config),
        na.rm = TRUE
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$layoutAttrs) == unlist(layout_attrs),
        na.rm = TRUE
    ))
})

testthat::test_that("plot heatmap", {
    traces <- list(
        list(
            alpha_stroke = 1,
            sizes = c(10, 100),
            spans = c(1, 20),
            type = "heatmap"
        ),
        list(
            alpha_stroke = 1,
            sizes = c(10, 100),
            spans = c(1, 20),
            type = "heatmap"
        ),
        x = c("220221CCM_global__01_ssleu_filtered",
              "220221CCM_global__02_ssleu_filtered"),
        y = c("LPC 11:0", "PS 24:0"),
        z = matrix(c(6214416.44108707, 0, 6578365.65569723, 0),
                   nrow = 2, ncol = 2,
                   dimnames = list(c(), c("220221CCM_global__01_ssleu_filtered",
                                          "220221CCM_global__02_ssleu_filtered")
                   )),
        hoverinfo = "text",
        text = matrix(c(
            paste0("sample: 220221CCM_global__01_ssleu_filtered<br />cpd: LPC ",
                   "11:0<br />intensity: 6 214 416"),
            paste0("sample: 220221CCM_global__01_ssleu_filtered<br />cpd: PS 2",
                   "4:0<br />intensity: 0"),
            paste0("sample: 220221CCM_global__02_ssleu_filtered<br />cpd: LPC ",
                   "11:0<br />intensity: 6 578 366"),
            paste0("sample: 220221CCM_global__02_ssleu_filtered<br />cpd: PS 2",
                   "4:0<br />intensity: 0")), nrow = 2, ncol = 2),
        inherit = TRUE
    )
    config <- list(
        modeBarButtonsToAdd = c("hoverclosest", "hovercompare"),
        showSendToCloud = FALSE,
        responsive = TRUE,
        displaylogo = FALSE,
        modeBarButtons = list(
            list(
                list(
                    name = "toImage",
                    title = "Download plot as a png",
                    icon = "Plotly.Icons.camera",
                    click = paste0("function(gd) {Plotly.downloadImage(gd, ",
                                   "{format:'png', width:1200, height:400, ",
                                   "filename:'heatmap'})}"
                    )
                ), list(
                    "zoom2d",
                    "autoScale2d"
                )
            )
        )
    )
    layout_attrs <- list(
        list(
            hoverlabel = list(
                namelength = -1
            )
        )
    )
    db_empty <- db_connect(":memory:")
    db <- db_connect(system.file(
        "testdata",
        "220221CCM_global.sqlite",
        package = "workflow.lipido"
    ))

    # 1st test : without db
    testthat::expect_error(
        plot_heatmap(NULL, NULL),
        "db must be a connection to the sqlite database"
    )

    # 2nd test : without compounds
    testthat::expect_error(
        plot_heatmap(db, NULL),
        "name must be a character"
    )

    # 3rd test : without no compounds
    testthat::expect_error(
        plot_heatmap(db, character(0)),
        "name must contain at least ONE compound"
    )

    # 4th test : with no samples in database (no processing yet)
    # must return an empty heatmap
    p <- plot_heatmap(db_empty, "LPC 11:0")
    RSQLite::dbDisconnect(db_empty)
    testthat::expect_true(all(
        unlist(p[[1]]$attrs) == unlist(traces[1]),
        na.rm = TRUE
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$config) == unlist(config),
        na.rm = TRUE
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$layoutAttrs) == unlist(layout_attrs),
        na.rm = TRUE
    ))

    # 5th test : normal
    p <- plot_heatmap(db, c("LPC 11:0", "PS 24:0"))
    RSQLite::dbDisconnect(db)
    testthat::expect_true(all(
        unlist(p[[1]]$attrs) == unlist(traces),
        na.rm = TRUE
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$config) == unlist(config),
        na.rm = TRUE
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$layoutAttrs) == unlist(layout_attrs),
        na.rm = TRUE
    ))
})

testthat::test_that("empty chromato", {
    traces <- list(
        mode = "markers",
        alpha_stroke = 1,
        sizes = c(10, 100),
        spans = c(1, 20),
        type = "scatter"
    )
    layout <- list(
        title = list(
            text = "<b>Total Ion Chromatogram(s)</b>",
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
    config <- list(
        modeBarButtonsToAdd = c("hoverclosest", "hovercompare"),
        showSendToCloud = FALSE,
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
                    click = htmlwidgets::JS(paste0("function(gd) {Plotly.downl",
                                                   "oadImage(gd, {format:'png'",
                                                   ", width:1200, height:400, ",
                                                   "filename:'Chromatogram'})}"
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
    p <- plot_empty_chromato()
    testthat::expect_true(all(
        unlist(p[[1]]$attrs) == unlist(traces),
        na.rm = TRUE
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$layoutAttrs) == unlist(layout),
        na.rm = TRUE
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$config) == unlist(config),
        na.rm = TRUE
    ))
})

testthat::test_that("plot EIC", {
    traces <- list(
        list(
            mode = "markers",
            alpha_stroke = 1,
            sizes = c(10, 100),
            spans = c(1, 20),
            type = "scatter"
        ),
        list(
            mode = "lines",
            alpha_stroke = 1,
            sizes = c(10, 100),
            spans = c(1, 20),
            type = "scatter",
            x = c(2.70905, 2.71786666666667, 2.7355, 2.74431666666667,
                  2.75311666666667, 2.76193333333333, 2.77956666666667,
                  2.78836666666667, 2.79718333333333, 2.81481666666667,
                  2.82363333333333, 2.83243333333333, 2.84125, 2.85006666666667,
                  2.85888333333333, 2.8765, 2.88531666666667, 2.89413333333333,
                  2.90295, 2.91175, 2.92938333333333, 2.9382, 2.947,
                  2.95581666666667, 2.96463333333333, 2.98225, 2.99106666666667,
                  2.99988333333333, 3.0087, 3.01751666666667, 3.02631666666667,
                  3.03513333333333, 3.04395, 3.05276666666667, 3.06158333333333,
                  3.07038333333333, 3.0792, 3.09683333333333, 3.10563333333333,
                  3.11445, 3.12326666666667, 3.13208333333333, 3.14088333333333,
                  3.1497, 3.15851666666667, 3.16733333333333, 3.17615, 3.2026,
                  3.21141666666667, 3.22023333333333, 3.22905, 3.23785,
                  3.24666666666667, 3.2643, 3.2731, 3.28191666666667,
                  3.29073333333333, 3.29955, 3.30836666666667, 3.31716666666667,
                  3.32598333333333, 3.3348, 3.34361666666667, 3.35241666666667,
                  3.36123333333333, 3.37005, 3.37886666666667, 3.38768333333333,
                  3.39648333333333, 3.4053, 3.42293333333333, 3.43173333333333,
                  3.44055, 3.44936666666667, 3.45818333333333, 3.467, 3.4758,
                  3.48461666666667, 3.49343333333333, 3.50225, 3.51105,
                  3.52868333333333, 3.54631666666667, 3.55511666666667,
                  3.56393333333333, 3.57275, 3.58156666666667, 3.59036666666667,
                  3.59918333333333, 3.608, 3.61681666666667, 3.62563333333333,
                  3.63443333333333, 3.64325, 3.65206666666667, 3.66088333333333,
                  3.66968333333333, 3.68731666666667, 3.69613333333333,
                  3.70493333333333, 3.71375, 3.72256666666667, 3.73138333333333,
                  3.7402, 3.749, 3.75781666666667, 3.76663333333333, 3.77545,
                  3.78425, 3.79306666666667, 3.80188333333333, 3.8107,
                  3.81951666666667, 3.82831666666667, 3.83713333333333, 3.84595,
                  3.85476666666667, 3.86356666666667, 3.87238333333333, 4.18965,
                  4.19846666666667, 4.20728333333333, 4.21608333333333, 4.2249,
                  4.23371666666667, 4.24253333333333, 4.25135, 4.26015,
                  4.26896666666667, 4.27778333333333, 4.2866, 4.2954,
                  4.30421666666667, 4.31303333333333, 4.32185, 4.33066666666667,
                  4.33946666666667, 4.34828333333333, 4.3571, 4.36591666666667,
                  4.37471666666667, 4.38353333333333, 4.39235, 4.40116666666667,
                  4.40996666666667, 4.41878333333333, 4.4276, 4.43641666666667,
                  4.44523333333333, 4.45403333333333, 4.46285, 4.47166666666667,
                  4.48048333333333, 4.48928333333333, 4.4981, 4.50691666666667,
                  4.51573333333333, 4.52455, 4.5334, 4.54221666666667,
                  4.55103333333333, 4.55985, 4.56866666666667, 4.57746666666667,
                  4.58628333333333, 4.5951, 4.60391666666667, 4.61271666666667,
                  4.62153333333333, 4.63035, 4.63916666666667, 4.64796666666667,
                  4.65678333333333, 4.6656, 4.67441666666667, 4.68323333333333,
                  4.69203333333333, 4.70085, 4.70966666666667, 4.71848333333333,
                  4.72728333333333, 4.7361, 4.74491666666667, 4.75373333333333,
                  4.76255, 4.77135, 4.78016666666667, 4.78898333333333, 4.7978,
                  4.8066, 4.81541666666667, 4.82423333333333, 4.83305,
                  4.84186666666667, 4.85066666666667, 4.85948333333333, 4.8683,
                  4.87711666666667, 4.88593333333333, 4.89473333333333, 4.90355,
                  4.91236666666667, 4.92118333333333, 4.92998333333333, 4.9388,
                  4.94761666666667, 4.95643333333333, 4.96525, 4.97405,
                  4.98286666666667, 4.99168333333333, 5.0005, 5.0093,
                  5.01811666666667, 5.02693333333333, 5.03575, 5.04456666666667,
                  5.05336666666667, 5.08861666666667, 5.09745, 5.10631666666667,
                  5.11513333333333, 5.12395, 5.13276666666667, 5.14156666666667,
                  5.15038333333333, 5.1592, 5.17683333333333, 5.18563333333333,
                  5.19445, 5.20326666666667, 5.21208333333333, 5.22088333333333,
                  5.2297, 5.23851666666667, 5.24733333333333, 5.26495,
                  5.27376666666667, 5.28258333333333, 5.2914, 5.30021666666667,
                  5.30901666666667, 5.31783333333333, 5.32665, 5.33546666666667,
                  5.34428333333333, 5.3531),
            y = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                  NA, 0, 0, 273337.25, 2152744, 3501824, 1892321, 852784.5,
                  566891.5, 437327.25, 474893.5, 653133, 463749.5, 217667.5,
                  124145.0625, 83991.9375, 57429.65625, 39043.34375, NA, NA, NA,
                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                  NA),
            name = "[M+H]+",
            legendgroup = "[M+H]+",
            showlegend = TRUE,
            inherit = TRUE
        ),
        list(
            mode = "lines",
            alpha_stroke = 1,
            sizes = c(10, 100),
            spans = c(1, 20),
            type = "scatter",
            x = c(2.70905, 2.71786666666667, 2.7355, 2.74431666666667,
                  2.75311666666667, 2.76193333333333, 2.77956666666667,
                  2.78836666666667, 2.79718333333333, 2.81481666666667,
                  2.82363333333333, 2.83243333333333, 2.84125, 2.85006666666667,
                  2.85888333333333, 2.8765, 2.88531666666667, 2.89413333333333,
                  2.90295, 2.91175, 2.92938333333333, 2.9382, 2.947,
                  2.95581666666667, 2.96463333333333, 2.98225, 2.99106666666667,
                  2.99988333333333, 3.0087, 3.01751666666667, 3.02631666666667,
                  3.03513333333333, 3.04395, 3.05276666666667, 3.06158333333333,
                  3.07038333333333, 3.0792, 3.09683333333333, 3.10563333333333,
                  3.11445, 3.12326666666667, 3.13208333333333, 3.14088333333333,
                  3.1497, 3.15851666666667, 3.16733333333333, 3.17615, 3.2026,
                  3.21141666666667, 3.22023333333333, 3.22905, 3.23785,
                  3.24666666666667, 3.2643, 3.2731, 3.28191666666667,
                  3.29073333333333, 3.29955, 3.30836666666667, 3.31716666666667,
                  3.32598333333333, 3.3348, 3.34361666666667, 3.35241666666667,
                  3.36123333333333, 3.37005, 3.37886666666667, 3.38768333333333,
                  3.39648333333333, 3.4053, 3.42293333333333, 3.43173333333333,
                  3.44055, 3.44936666666667, 3.45818333333333, 3.467, 3.4758,
                  3.48461666666667, 3.49343333333333, 3.50225, 3.51105,
                  3.52868333333333, 3.54631666666667, 3.55511666666667,
                  3.56393333333333, 3.57275, 3.58156666666667, 3.59036666666667,
                  3.59918333333333, 3.608, 3.61681666666667, 3.62563333333333,
                  3.63443333333333, 3.64325, 3.65206666666667, 3.66088333333333,
                  3.66968333333333, 3.68731666666667, 3.69613333333333,
                  3.70493333333333, 3.71375, 3.72256666666667, 3.73138333333333,
                  3.7402, 3.749, 3.75781666666667, 3.76663333333333, 3.77545,
                  3.78425, 3.79306666666667, 3.80188333333333, 3.8107,
                  3.81951666666667, 3.82831666666667, 3.83713333333333, 3.84595,
                  3.85476666666667, 3.86356666666667, 3.87238333333333, 4.18965,
                  4.19846666666667, 4.20728333333333, 4.21608333333333, 4.2249,
                  4.23371666666667, 4.24253333333333, 4.25135, 4.26015,
                  4.26896666666667, 4.27778333333333, 4.2866, 4.2954,
                  4.30421666666667, 4.31303333333333, 4.32185, 4.33066666666667,
                  4.33946666666667, 4.34828333333333, 4.3571, 4.36591666666667,
                  4.37471666666667, 4.38353333333333, 4.39235, 4.40116666666667,
                  4.40996666666667, 4.41878333333333, 4.4276, 4.43641666666667,
                  4.44523333333333, 4.45403333333333, 4.46285, 4.47166666666667,
                  4.48048333333333, 4.48928333333333, 4.4981, 4.50691666666667,
                  4.51573333333333, 4.52455, 4.5334, 4.54221666666667,
                  4.55103333333333, 4.55985, 4.56866666666667, 4.57746666666667,
                  4.58628333333333, 4.5951, 4.60391666666667, 4.61271666666667,
                  4.62153333333333, 4.63035, 4.63916666666667, 4.64796666666667,
                  4.65678333333333, 4.6656, 4.67441666666667, 4.68323333333333,
                  4.69203333333333, 4.70085, 4.70966666666667, 4.71848333333333,
                  4.72728333333333, 4.7361, 4.74491666666667, 4.75373333333333,
                  4.76255, 4.77135, 4.78016666666667, 4.78898333333333, 4.7978,
                  4.8066, 4.81541666666667, 4.82423333333333, 4.83305,
                  4.84186666666667, 4.85066666666667, 4.85948333333333, 4.8683,
                  4.87711666666667, 4.88593333333333, 4.89473333333333, 4.90355,
                  4.91236666666667, 4.92118333333333, 4.92998333333333, 4.9388,
                  4.94761666666667, 4.95643333333333, 4.96525, 4.97405,
                  4.98286666666667, 4.99168333333333, 5.0005, 5.0093,
                  5.01811666666667, 5.02693333333333, 5.03575, 5.04456666666667,
                  5.05336666666667, 5.08861666666667, 5.09745, 5.10631666666667,
                  5.11513333333333, 5.12395, 5.13276666666667, 5.14156666666667,
                  5.15038333333333, 5.1592, 5.17683333333333, 5.18563333333333,
                  5.19445, 5.20326666666667, 5.21208333333333, 5.22088333333333,
                  5.2297, 5.23851666666667, 5.24733333333333, 5.26495,
                  5.27376666666667, 5.28258333333333, 5.2914, 5.30021666666667,
                  5.30901666666667, 5.31783333333333, 5.32665, 5.33546666666667,
                  5.34428333333333, 5.3531),
            y = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2238.388671875, 0,
                  0, 995.25439453125, 0, 0, 0, 2053.673828125, 0, 0, 0,
                  1594.673828125, 801.8935546875, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                  1491.44921875, 0, 0, 0, 3075.4765625, 0, 0, 2155.884765625, 0,
                  1937.55078125, 0, 1387.880859375, 0, 2478.916015625, 0, 0, 0,
                  0, 0, 1786.4609375, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1236.244140625,
                  0, 0, 1809.7236328125, 2053.55859375, 3575.376953125, 0, 0, 0,
                  0, 0, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                  NA, 39043.34375, 28808.65625, 0, 14321.7578125, 0, 0,
                  7536.08984375, 0, 0, 0, 0, 5073.15625, 0, 0, 0, 0, 0, 1760, 0,
                  2812.58984375, 0, 0, 0, 3052.845703125, 0, 0, 0, 0, 0, 0, 0,
                  0, 2240, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
            name = "[M+H]+",
            legendgroup = "[M+H]+",
            showlegend = FALSE,
            line = list(
                color = "rgb(0,0,0)",
                width = 1,
                dash = "dash"
            ),
            inherit = TRUE
        ),
        list(
            mode = "lines",
            alpha_stroke = 1,
            sizes = c(10, 100),
            spans = c(1, 20),
            type = "scatter",
            x = NA,
            y = NA,
            name = "[M+Na]+",
            legendgroup = "[M+Na]+",
            showlegend = FALSE,
            inherit = TRUE
        ),
        list(
            mode = "lines",
            alpha_stroke = 1,
            sizes = c(10, 100),
            spans = c(1, 20),
            type = "scatter",
            x = c(2.70905, 2.71786666666667, 2.7355, 2.74431666666667,
                  2.75311666666667, 2.76193333333333, 2.77956666666667,
                  2.78836666666667, 2.79718333333333, 2.81481666666667,
                  2.82363333333333, 2.83243333333333, 2.84125, 2.85006666666667,
                  2.85888333333333, 2.8765, 2.88531666666667, 2.89413333333333,
                  2.90295, 2.91175, 2.92938333333333, 2.9382, 2.947,
                  2.95581666666667, 2.96463333333333, 2.98225, 2.99106666666667,
                  2.99988333333333, 3.0087, 3.01751666666667, 3.02631666666667,
                  3.03513333333333, 3.04395, 3.05276666666667, 3.06158333333333,
                  3.07038333333333, 3.0792, 3.09683333333333, 3.10563333333333,
                  3.11445, 3.12326666666667, 3.13208333333333, 3.14088333333333,
                  3.1497, 3.15851666666667, 3.16733333333333, 3.17615, 3.2026,
                  3.21141666666667, 3.22023333333333, 3.22905, 3.23785,
                  3.24666666666667, 3.2643, 3.2731, 3.28191666666667,
                  3.29073333333333, 3.29955, 3.30836666666667, 3.31716666666667,
                  3.32598333333333, 3.3348, 3.34361666666667, 3.35241666666667,
                  3.36123333333333, 3.37005, 3.37886666666667, 3.38768333333333,
                  3.39648333333333, 3.4053, 3.42293333333333, 3.43173333333333,
                  3.44055, 3.44936666666667, 3.45818333333333, 3.467, 3.4758,
                  3.48461666666667, 3.49343333333333, 3.50225, 3.51105,
                  3.52868333333333, 3.54631666666667, 3.55511666666667,
                  3.56393333333333, 3.57275, 3.58156666666667, 3.59036666666667,
                  3.59918333333333, 3.608, 3.61681666666667, 3.62563333333333,
                  3.63443333333333, 3.64325, 3.65206666666667, 3.66088333333333,
                  3.66968333333333, 3.68731666666667, 3.69613333333333,
                  3.70493333333333, 3.71375, 3.72256666666667, 3.73138333333333,
                  3.7402, 3.749, 3.75781666666667, 3.76663333333333, 3.77545,
                  3.78425, 3.79306666666667, 3.80188333333333, 3.8107,
                  3.81951666666667, 3.82831666666667, 3.83713333333333, 3.84595,
                  3.85476666666667, 3.86356666666667, 3.87238333333333, 4.18965,
                  4.19846666666667, 4.20728333333333, 4.21608333333333, 4.2249,
                  4.23371666666667, 4.24253333333333, 4.25135, 4.26015,
                  4.26896666666667, 4.27778333333333, 4.2866, 4.2954,
                  4.30421666666667, 4.31303333333333, 4.32185, 4.33066666666667,
                  4.33946666666667, 4.34828333333333, 4.3571, 4.36591666666667,
                  4.37471666666667, 4.38353333333333, 4.39235, 4.40116666666667,
                  4.40996666666667, 4.41878333333333, 4.4276, 4.43641666666667,
                  4.44523333333333, 4.45403333333333, 4.46285, 4.47166666666667,
                  4.48048333333333, 4.48928333333333, 4.4981, 4.50691666666667,
                  4.51573333333333, 4.52455, 4.5334, 4.54221666666667,
                  4.55103333333333, 4.55985, 4.56866666666667, 4.57746666666667,
                  4.58628333333333, 4.5951, 4.60391666666667, 4.61271666666667,
                  4.62153333333333, 4.63035, 4.63916666666667, 4.64796666666667,
                  4.65678333333333, 4.6656, 4.67441666666667, 4.68323333333333,
                  4.69203333333333, 4.70085, 4.70966666666667, 4.71848333333333,
                  4.72728333333333, 4.7361, 4.74491666666667, 4.75373333333333,
                  4.76255, 4.77135, 4.78016666666667, 4.78898333333333, 4.7978,
                  4.8066, 4.81541666666667, 4.82423333333333, 4.83305,
                  4.84186666666667, 4.85066666666667, 4.85948333333333, 4.8683,
                  4.87711666666667, 4.88593333333333, 4.89473333333333, 4.90355,
                  4.91236666666667, 4.92118333333333, 4.92998333333333, 4.9388,
                  4.94761666666667, 4.95643333333333, 4.96525, 4.97405,
                  4.98286666666667, 4.99168333333333, 5.0005, 5.0093,
                  5.01811666666667, 5.02693333333333, 5.03575, 5.04456666666667,
                  5.05336666666667, 5.08861666666667, 5.09745, 5.10631666666667,
                  5.11513333333333, 5.12395, 5.13276666666667, 5.14156666666667,
                  5.15038333333333, 5.1592, 5.17683333333333, 5.18563333333333,
                  5.19445, 5.20326666666667, 5.21208333333333, 5.22088333333333,
                  5.2297, 5.23851666666667, 5.24733333333333, 5.26495,
                  5.27376666666667, 5.28258333333333, 5.2914, 5.30021666666667,
                  5.30901666666667, 5.31783333333333, 5.32665, 5.33546666666667,
                  5.34428333333333, 5.3531),
            y = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 323.08447265625,
                  0, 0, 407.562255859375, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                  0, 0, 619.58056640625, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                  365.888427734375, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 688.90234375, 0, 0, 0, 0, 0, 0, 0,
                  0, 0, 98869.75, 183977.625, 85087.0625, 0, 21917.546875,
                  14974.5078125, 18791.125, 33766.9375, 23409.625,
                  11075.6484375, 0, 0, 0, 0, 0, 0, 1174.189453125,
                  1174.189453125, 0, 0, 0, 1124.8154296875, 0, 0, 0, 0, 0, 0, 0,
                  0, 0, 0, 429.42431640625, 429.42431640625, 1138.537109375,
                  1138.537109375, 0, 667.7509765625, 0, 0, 722.4072265625, 0, 0,
                  0, 0, 500.68310546875, 0, 0, 0, 0, 0, 959.27587890625, 0, 0,
                  0, 0, 0, 620.76220703125, 0, 0, 0, 0, 0),
            name = "[M+Na]+",
            legendgroup = "[M+Na]+",
            showlegend = TRUE,
            line = list(
                color = "rgb(0,0,0)",
                width = 1,
                dash = "dash"
            ),
            inherit = TRUE
        ),
        list(
            mode = "lines",
            alpha_stroke = 1,
            sizes = c(10, 100),
            spans = c(1, 20),
            type = "scatter",
            x = NA,
            y = NA,
            name = "[M+H-H2O]+",
            legendgroup = "[M+H-H2O]+",
            showlegend = FALSE,
            inherit = TRUE
        ),
        list(
            mode = "lines",
            alpha_stroke = 1,
            sizes = c(10, 100),
            spans = c(1, 20),
            type = "scatter",
            x = c(2.70905, 2.71786666666667, 2.7355, 2.74431666666667,
                  2.75311666666667, 2.76193333333333, 2.77956666666667,
                  2.78836666666667, 2.79718333333333, 2.81481666666667,
                  2.82363333333333, 2.83243333333333, 2.84125, 2.85006666666667,
                  2.85888333333333, 2.8765, 2.88531666666667, 2.89413333333333,
                  2.90295, 2.91175, 2.92938333333333, 2.9382, 2.947,
                  2.95581666666667, 2.96463333333333, 2.98225, 2.99106666666667,
                  2.99988333333333, 3.0087, 3.01751666666667, 3.02631666666667,
                  3.03513333333333, 3.04395, 3.05276666666667, 3.06158333333333,
                  3.07038333333333, 3.0792, 3.09683333333333, 3.10563333333333,
                  3.11445, 3.12326666666667, 3.13208333333333, 3.14088333333333,
                  3.1497, 3.15851666666667, 3.16733333333333, 3.17615, 3.2026,
                  3.21141666666667, 3.22023333333333, 3.22905, 3.23785,
                  3.24666666666667, 3.2643, 3.2731, 3.28191666666667,
                  3.29073333333333, 3.29955, 3.30836666666667, 3.31716666666667,
                  3.32598333333333, 3.3348, 3.34361666666667, 3.35241666666667,
                  3.36123333333333, 3.37005, 3.37886666666667, 3.38768333333333,
                  3.39648333333333, 3.4053, 3.42293333333333, 3.43173333333333,
                  3.44055, 3.44936666666667, 3.45818333333333, 3.467, 3.4758,
                  3.48461666666667, 3.49343333333333, 3.50225, 3.51105,
                  3.52868333333333, 3.54631666666667, 3.55511666666667,
                  3.56393333333333, 3.57275, 3.58156666666667, 3.59036666666667,
                  3.59918333333333, 3.608, 3.61681666666667, 3.62563333333333,
                  3.63443333333333, 3.64325, 3.65206666666667, 3.66088333333333,
                  3.66968333333333, 3.68731666666667, 3.69613333333333,
                  3.70493333333333, 3.71375, 3.72256666666667, 3.73138333333333,
                  3.7402, 3.749, 3.75781666666667, 3.76663333333333, 3.77545,
                  3.78425, 3.79306666666667, 3.80188333333333, 3.8107,
                  3.81951666666667, 3.82831666666667, 3.83713333333333, 3.84595,
                  3.85476666666667, 3.86356666666667, 3.87238333333333, 4.18965,
                  4.19846666666667, 4.20728333333333, 4.21608333333333, 4.2249,
                  4.23371666666667, 4.24253333333333, 4.25135, 4.26015,
                  4.26896666666667, 4.27778333333333, 4.2866, 4.2954,
                  4.30421666666667, 4.31303333333333, 4.32185, 4.33066666666667,
                  4.33946666666667, 4.34828333333333, 4.3571, 4.36591666666667,
                  4.37471666666667, 4.38353333333333, 4.39235, 4.40116666666667,
                  4.40996666666667, 4.41878333333333, 4.4276, 4.43641666666667,
                  4.44523333333333, 4.45403333333333, 4.46285, 4.47166666666667,
                  4.48048333333333, 4.48928333333333, 4.4981, 4.50691666666667,
                  4.51573333333333, 4.52455, 4.5334, 4.54221666666667,
                  4.55103333333333, 4.55985, 4.56866666666667, 4.57746666666667,
                  4.58628333333333, 4.5951, 4.60391666666667, 4.61271666666667,
                  4.62153333333333, 4.63035, 4.63916666666667, 4.64796666666667,
                  4.65678333333333, 4.6656, 4.67441666666667, 4.68323333333333,
                  4.69203333333333, 4.70085, 4.70966666666667, 4.71848333333333,
                  4.72728333333333, 4.7361, 4.74491666666667, 4.75373333333333,
                  4.76255, 4.77135, 4.78016666666667, 4.78898333333333, 4.7978,
                  4.8066, 4.81541666666667, 4.82423333333333, 4.83305,
                  4.84186666666667, 4.85066666666667, 4.85948333333333, 4.8683,
                  4.87711666666667, 4.88593333333333, 4.89473333333333, 4.90355,
                  4.91236666666667, 4.92118333333333, 4.92998333333333, 4.9388,
                  4.94761666666667, 4.95643333333333, 4.96525, 4.97405,
                  4.98286666666667, 4.99168333333333, 5.0005, 5.0093,
                  5.01811666666667, 5.02693333333333, 5.03575, 5.04456666666667,
                  5.05336666666667, 5.08861666666667, 5.09745, 5.10631666666667,
                  5.11513333333333, 5.12395, 5.13276666666667, 5.14156666666667,
                  5.15038333333333, 5.1592, 5.17683333333333, 5.18563333333333,
                  5.19445, 5.20326666666667, 5.21208333333333, 5.22088333333333,
                  5.2297, 5.23851666666667, 5.24733333333333, 5.26495,
                  5.27376666666667, 5.28258333333333, 5.2914, 5.30021666666667,
                  5.30901666666667, 5.31783333333333, 5.32665, 5.33546666666667,
                  5.34428333333333, 5.3531),
            y = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 245.137573242188, 0, 0, 0, 0,
                  0, 553.31005859375, 0, 0, 0, 0, 0, 0, 0, 146.30126953125, 0,
                  0, 0, 0, 0, 0, 0, 221.503295898438, 612.8369140625, 0, 0, 0,
                  0, 0, 0, 0, 0, 0, 261.576904296875, 0, 946.1162109375,
                  658.98828125, 0, 0, 0, 284.175048828125, 0, 0, 0, 0, 0, 0,
                  45487.375, 76420.25, 38193.125, 0, 0, 0, 8173.96484375, 0, 0,
                  0, 2323.3515625, 0, 0, 0, 0, 402.296142578125, 0, 0, 0, 0, 0,
                  440.52294921875, 0, 0, 0, 0, 548.2548828125, 0, 0, 0, 0, 0, 0,
                  0, 0, 0, 507.15234375, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                  617.48681640625, 0, 0, 0, 0, 245.449096679688,
                  335.229736328125, 0, 0, 0, 500.802001953125, 0,
                  258.686279296875, 0, 0, 0, 0),
            name = "[M+H-H2O]+",
            legendgroup = "[M+H-H2O]+",
            showlegend = TRUE,
            line = list(
                color = "rgb(0,0,0)",
                width = 1,
                dash = "dash"
            ),
            inherit = TRUE
        ),
        list(
            mode = "lines",
            alpha_stroke = 1,
            sizes = c(10, 100),
            spans = c(1, 20),
            type = "scatter",
            x = c(4.76333333333333, 4.76333333333333, NA, 5.09666666666667,
                  5.09666666666667),
            y = c(0, 3501824, NA, 0, 3501824),
            showlegend = FALSE,
            line = list(
                color = "rgb(0,0,0)",
                width = 2, dash = "dash"
            ),
            hoverinfo = "skip",
            inherit = TRUE
        )
    )
    layout <- list(
        list(
            title = list(
                text = "<b>EIC</b>",
                y = 0.95,
                x = 0.5,
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
            hoverlabel = list(namelength = -1),
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
        ),
        list(
            annotations = list(
                text = "[M+H]+",
                x = 4.77135,
                y = 3501824,
                xref = "x",
                yref = "y",
                valign = "bottom",
                arrowhead = 0
            )
        ),
        list(
            annotations = list(
                text = "[M+Na]+",
                x = 4.77135,
                y = 183977.625,
                xref = "x",
                yref = "y",
                valign = "bottom",
                arrowhead = 0
            )
        ),
        list(
            annotations = list(
                text = "[M+H-H2O]+",
                x = 4.77135,
                y = 76420.25,
                xref = "x",
                yref = "y",
                valign = "bottom",
                arrowhead = 0
            )
        )
    )
    config <- list(
        modeBarButtonsToAdd = c("hoverclosest", "hovercompare"),
        showSendToCloud = FALSE,
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
                    click = htmlwidgets::JS(paste0("function(gd) {Plotly.downl",
                                                   "oadImage(gd, {format:'png'",
                                                   ", width:1200, height:400, ",
                                                   "filename:'Chromatogram'})}"
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
    js_func <- list(
        list(
            code = paste0("function(el, x) {el.on(\"plotly_restyle\", () => {a",
                          "nnotations = el.layout.annotations;for (var i = 1; ",
                          "i < annotations.length; i++) {annotations[i].visibl",
                          "e = el._fullData[i * 2 - 1].visible != \"legendonly",
                          "\"}Plotly.relayout(el, {annotations: annotations});",
                          "});}"),
            data = NULL
        )
    )
    db_empty <- db_connect(":memory:")
    db <- db_connect(system.file(
        "testdata",
        "220221CCM_global.sqlite",
        package = "workflow.lipido"
    ))

    # 1st test : with no db
    testthat::expect_error(
        plot_eic(NULL, NULL, NULL),
        "db must be a connection to the sqlite database"
    )

    # 2nd test : with no sample name
    testthat::expect_error(
        plot_eic(db, NULL, NULL),
        "sample name must be a character"
    )

    # 3rd test : with more than one sample name
    testthat::expect_error(
        plot_eic(
            db,
            c("220221CCM_global__01_ssleu_filtered",
              "220221CCM_global__02_ssleu_filtered"),
            NULL
        ),
        "sample name must contain only ONE sample name"
    )

    # 4th test : with no compound name
    testthat::expect_error(
        plot_eic(db, "220221CCM_global__01_ssleu_filtered", NULL),
        "name must be a character"
    )

    # 5th test : with more than one compound name
    testthat::expect_error(
        plot_eic(
            db,
            "220221CCM_global__01_ssleu_filtered",
            c("LPC 11:0", "PS 24:0")
        ),
        "name must contain only ONE compound"
    )

    # 6th test : no processing was made (no files in database yet)
    p <- plot_eic(db_empty, "220221CCM_global__01_ssleu_filtered", "LPC 11:0")
    RSQLite::dbDisconnect(db_empty)
    testthat::expect_true(all(
        unlist(p[[1]]$attrs) == unlist(traces[1]),
        na.rm = TRUE
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$layoutAttrs) == unlist(layout[1]),
        na.rm = TRUE
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$config) == unlist(config),
        na.rm = TRUE
    ))
    testthat::expect_true(all(
        unlist(p[[8]]) == unlist(js_func),
        na.rm = TRUE
    ))

    # 7th test : normal
    p <- plot_eic(db, "220221CCM_global__01_ssleu_filtered", "LPC 11:0")
    RSQLite::dbDisconnect(db)
    testthat::expect_true(all(
        unlist(p[[1]]$attrs) == unlist(traces),
        na.rm = TRUE
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$layoutAttrs) == unlist(layout),
        na.rm = TRUE
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$config) == unlist(config),
        na.rm = TRUE
    ))
    testthat::expect_true(all(
        unlist(p[[8]]) == unlist(js_func),
        na.rm = TRUE
    ))
})

testthat::test_that("plot empty m/z dev", {
    traces <- list(list(
        mode = "markers",
        alpha_stroke = 1,
        sizes = c(10, 100),
        spans = c(1, 20),
        type = "scatter"
    ))
    layout <- list(list(
        title = list(
            text = "<b>m/z deviation</b>",
            y = 0.95,
            x = 0.5,
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
        hoverlabel = list(namelength = -1),
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
    ))
    config <- list(
        modeBarButtonsToAdd = c("hoverclosest", "hovercompare"),
        showSendToCloud = FALSE,
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
    p <- plot_empty_mzdev()
    testthat::expect_true(all(
        unlist(p[[1]]$attrs) == unlist(traces),
        na.rm = TRUE
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$layoutAttrs) == unlist(layout),
        na.rm = TRUE
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$config) == unlist(config),
        na.rm = TRUE
    ))
})

testthat::test_that("plot m/z dev", {
    traces <- list(
        list(
            mode = "markers",
            alpha_stroke = 1,
            sizes = c(10, 100),
            spans = c(1, 20),
            type = "scatter"
        ),
        list(
            mode = "markers",
            alpha_stroke = 1,
            sizes = c(10, 100),
            spans = c(1, 20),
            type = "scatter",
            x = c(4.18965, 4.21608333333333, 4.25135, 4.2866, 4.2954, 4.39235,
                  4.4276, 4.45403333333333, 4.47166666666667, 4.48928333333333,
                  4.50691666666667, 4.55985, 4.64796666666667, 4.67441666666667,
                  4.68323333333333, 4.69203333333333, 4.75373333333333, 4.76255,
                  4.77135, 4.78016666666667, 4.78898333333333, 4.7978, 4.8066,
                  4.81541666666667, 4.82423333333333, 4.83305, 4.84186666666667,
                  4.85066666666667, 4.85948333333333, 4.8683, 4.87711666666667,
                  4.88593333333333, 4.90355, 4.92998333333333, 4.97405,
                  5.02693333333333, 5.04456666666667, 5.10631666666667,
                  5.19445),
            y = c(2.02903320314363, 0.625224609393626, -0.259785156231374,
                  0.411601562518626, -0.168232421856374, -1.93825195310637,
                  0.777812500018626, 0.289531250018626, 0.503154296893626,
                  1.47971679689363, -1.14479492185637, -1.02272460935637,
                  0.686259765643626, 0.991435546893626, -0.0461621093563735,
                  -0.351337890606374, -0.107197265606374, 0.0759082031436265,
                  -0.320820312481374, -0.900654296856374, -0.473408203106374,
                  -0.473408203106374, -0.503925781231374, -0.748066406231374,
                  -0.473408203106374, -0.442890624981374, -0.381855468731374,
                  0.0148730468936265, 0.167460937518626, 0.991435546893626,
                  0.350566406268626, 0.503154296893626, 1.29661132814363,
                  0.899882812518626, -0.839619140606374, -1.60255859373137,
                  2.09006835939363, -2.02980468748137, 0.777812500018626),
            showlegend = FALSE,
            hoverinfo = "text",
            name = "[M+H]+",
            text = c("rT: 4.19 min<br />m/z deviation: 2.03 mDa",
                     "rT: 4.22 min<br />m/z deviation: 0.63 mDa",
                     "rT: 4.25 min<br />m/z deviation: -0.26 mDa",
                     "rT: 4.29 min<br />m/z deviation: 0.41 mDa",
                     "rT: 4.3 min<br />m/z deviation: -0.17 mDa",
                     "rT: 4.39 min<br />m/z deviation: -1.94 mDa",
                     "rT: 4.43 min<br />m/z deviation: 0.78 mDa",
                     "rT: 4.45 min<br />m/z deviation: 0.29 mDa",
                     "rT: 4.47 min<br />m/z deviation: 0.5 mDa",
                     "rT: 4.49 min<br />m/z deviation: 1.48 mDa",
                     "rT: 4.51 min<br />m/z deviation: -1.14 mDa",
                     "rT: 4.56 min<br />m/z deviation: -1.02 mDa",
                     "rT: 4.65 min<br />m/z deviation: 0.69 mDa",
                     "rT: 4.67 min<br />m/z deviation: 0.99 mDa",
                     "rT: 4.68 min<br />m/z deviation: -0.05 mDa",
                     "rT: 4.69 min<br />m/z deviation: -0.35 mDa",
                     "rT: 4.75 min<br />m/z deviation: -0.11 mDa",
                     "rT: 4.76 min<br />m/z deviation: 0.08 mDa",
                     "rT: 4.77 min<br />m/z deviation: -0.32 mDa",
                     "rT: 4.78 min<br />m/z deviation: -0.9 mDa",
                     "rT: 4.79 min<br />m/z deviation: -0.47 mDa",
                     "rT: 4.8 min<br />m/z deviation: -0.47 mDa",
                     "rT: 4.81 min<br />m/z deviation: -0.5 mDa",
                     "rT: 4.82 min<br />m/z deviation: -0.75 mDa",
                     "rT: 4.82 min<br />m/z deviation: -0.47 mDa",
                     "rT: 4.83 min<br />m/z deviation: -0.44 mDa",
                     "rT: 4.84 min<br />m/z deviation: -0.38 mDa",
                     "rT: 4.85 min<br />m/z deviation: 0.01 mDa",
                     "rT: 4.86 min<br />m/z deviation: 0.17 mDa",
                     "rT: 4.87 min<br />m/z deviation: 0.99 mDa",
                     "rT: 4.88 min<br />m/z deviation: 0.35 mDa",
                     "rT: 4.89 min<br />m/z deviation: 0.5 mDa",
                     "rT: 4.9 min<br />m/z deviation: 1.3 mDa",
                     "rT: 4.93 min<br />m/z deviation: 0.9 mDa",
                     "rT: 4.97 min<br />m/z deviation: -0.84 mDa",
                     "rT: 5.03 min<br />m/z deviation: -1.6 mDa",
                     "rT: 5.04 min<br />m/z deviation: 2.09 mDa",
                     "rT: 5.11 min<br />m/z deviation: -2.03 mDa",
                     "rT: 5.19 min<br />m/z deviation: 0.78 mDa"),
            inherit = TRUE
        ),
        list(
            mode = "lines",
            alpha_stroke = 1,
            sizes = c(10, 100),
            spans = c(1, 20),
            type = "scatter",
            x = c(4.76333333333333, 4.76333333333333, 5.09666666666667,
                  5.09666666666667, 4.76333333333333),
            y = c(-15, 15, 15, -15, -15),
            showlegend = FALSE,
            line = list(
                color = "rgb(0,0,0)",
                width = 2,
                dash = "dash"
            ),
            hoverinfo = "none",
            inherit = TRUE
        )
    )
    layout <- list(
        list(
            title = list(
                text = "<b>m/z deviation</b>",
                y = 0.95,
                x = 0.5,
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
            hoverlabel = list(namelength = -1),
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
    )
    config <- list(
        modeBarButtonsToAdd = c("hoverclosest", "hovercompare"),
        showSendToCloud = FALSE,
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
    db_empty <- db_connect(":memory:")
    db <- db_connect(system.file(
        "testdata",
        "220221CCM_global.sqlite",
        package = "workflow.lipido"
    ))

    # 1st test : without db
    testthat::expect_error(
        plot_mzdev(NULL, NULL, NULL, NULL),
        "db must be a connection to the sqlite database"
    )

    # 2nd test : without sample name
    testthat::expect_error(
        plot_mzdev(db, NULL, NULL, NULL),
        "sample name must be a character"
    )

    # 3rd test : with more than one sample name
    testthat::expect_error(
        plot_mzdev(
            db,
            c("220221CCM_global__01_ssleu_filtered",
              "220221CCM_global__02_ssleu_filtered"),
            NULL,
            NULL
        ),
        "sample name must contain only ONE sample name"
    )

    # 4th test : without compound name
    testthat::expect_error(
        plot_mzdev(db, "220221CCM_global__01_ssleu_filtered", NULL, NULL),
        "name must be a character"
    )

    # 5th test : with more than one compound name
    testthat::expect_error(
        plot_mzdev(
            db,
            "220221CCM_global__01_ssleu_filtered",
            c("LPC 11:0", "PS 24:0"),
            NULL
        ),
        "name must contain only ONE compound"
    )

    # 6th test : without adduct name
    testthat::expect_error(
        plot_mzdev(db, "220221CCM_global__01_ssleu_filtered", "LPC 11:0", NULL),
        "adduct name must be a character"
    )

    # 7th test : with more than one adduct
    testthat::expect_error(
        plot_mzdev(
            db,
            "220221CCM_global__01_ssleu_filtered",
            "LPC 11:0",
            c("[M+H]+", "[M+Na]+")
        ),
        "adduct name must contain only adduct name"
    )

    # 8th test : with no sample files in database (not yet processed)
    p <- plot_mzdev(
            db_empty,
            "220221CCM_global__01_ssleu_filtered",
            "LPC 11:0",
            "[M+H]+"
    )
    RSQLite::dbDisconnect(db_empty)
    testthat::expect_true(all(
        unlist(p[[1]]$attrs) == unlist(traces[1]),
        na.rm = TRUE
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$layoutAttrs) == unlist(layout),
        na.rm = TRUE
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$config) == unlist(config),
        na.rm = TRUE
    ))

    # 9th test : file doesn't exist
    p <- plot_mzdev(
        db,
        "220221CCM_global__03_ssleu_filtered",
        "LPC 11:0",
        "[M+H]+"
    )
    testthat::expect_true(all(
        unlist(p[[1]]$attrs) == unlist(traces[1]),
        na.rm = TRUE
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$layoutAttrs) == unlist(layout),
        na.rm = TRUE
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$config) == unlist(config),
        na.rm = TRUE
    ))

    # 10th test : normal
    p <- plot_mzdev(
        db,
        "220221CCM_global__01_ssleu_filtered",
        "LPC 11:0",
        "[M+H]+"
    )
    RSQLite::dbDisconnect(db)
    testthat::expect_true(all(
        unlist(p[[1]]$attrs) == unlist(traces),
        na.rm = TRUE
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$layoutAttrs) == unlist(layout),
        na.rm = TRUE
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$config) == unlist(config),
        na.rm = TRUE
    ))
})
