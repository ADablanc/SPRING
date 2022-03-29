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
