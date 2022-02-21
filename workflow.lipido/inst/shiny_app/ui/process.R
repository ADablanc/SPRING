shinydashboard::tabItem(
    tabName = "process",
    shinydashboard::box(
        width = 3,
        shinyWidgets::actionBttn(
            inputId = 'process_launch',
            label = 'Launch deconvolution process',
            style = 'minimal',
            color = 'primary'
        ),
        shiny::tabsetPanel(
            shiny::tabPanel(
                title = "Conversion",
                shiny::tags$div(
                    class = "form-group shiny-input-container",
                    shiny::tags$div(
                        id = "div_conversion_bttn",
                        class = "input-group",
                        shiny::tags$label(
                            class = "input-group-bttn",
                            shiny::tags$a(
                                class = paste("bttn", "bttn-no-fill",
                                    "bttn-primary", "bttn-no-outline"),
                                shiny::icon("upload"),
                                shinyFiles::shinyFilesButton(
                                    id = "process_files",
                                    title = "select file(s)",
                                    class = "shinyjs-resettable",
                                    multiple = TRUE,
                                    label = "",
                                    style = "display: none;"
                                )
                            )
                        )
                    )
                ),
                DT::dataTableOutput("process_dt_files_imported")
            ),
            shiny::tabPanel(
                title = "Filters",
                shiny::tags$table(class = "table-params",
                    shiny::tags$tr(
                        shiny::tags$td(
                            bsplus::shinyInput_label_embed(
                                tag = shiny::numericInput(
                                    inputId = "process_filter_mz_min",
                                    label = "m/z min (Da)",
                                    value = 200
                                ),
                                element = bsplus::bs_embed_tooltip(
                                    tag = bsplus::shiny_iconlink(),
                                    placement = "top",
                                    title = "Born min in m/z to trimm file"
                                )
                            )
                        ),
                        shiny::tags$td(
                            bsplus::shinyInput_label_embed(
                                tag = shiny::numericInput(
                                    inputId = "process_filter_mz_max",
                                    label = "m/z max (Da)",
                                    value = 1000
                                ),
                                element = bsplus::bs_embed_tooltip(
                                    tag = bsplus::shiny_iconlink(),
                                    placement = "top",
                                    title = "Born max in m/z to trimm file"
                                )
                            )
                        )
                    ),
                    shiny::tags$tr(
                        shiny::tags$td(
                            bsplus::shinyInput_label_embed(
                                tag = shiny::numericInput(
                                    inputId = "process_filter_rt_min",
                                    label = "rT min (min)",
                                    value = 0.7
                                ),
                                element = bsplus::bs_embed_tooltip(
                                    tag = bsplus::shiny_iconlink(),
                                    placement = "top",
                                    title = "Born min in rT to trimm file"
                                )
                            )
                        ),
                        shiny::tags$td(
                            bsplus::shinyInput_label_embed(
                                tag = shiny::numericInput(
                                    inputId = "process_filter_rt_max",
                                    label = "rT max (min)",
                                    value = 8.6
                                ),
                                element = bsplus::bs_embed_tooltip(
                                    tag = bsplus::shiny_iconlink(),
                                    placement = "top",
                                    title = "Born max in rT to trimm file"
                                )
                            )
                        )
                    )
                )
            ),
            shiny::tabPanel(
                title = "Peak Picking",
                shiny::tabsetPanel(
                    shiny::tabPanel("General",
                        shiny::tags$table(class = "table-params",
                            shiny::tags$tr(
                                shiny::tags$td(
                                    bsplus::shinyInput_label_embed(
                                        tag = shiny::numericInput(
                                            inputId = "process_ppm",
                                            label = "m/z tolerance (ppm)",
                                            value = 30
                                        ),
                                        element = bsplus::bs_embed_tooltip(
                                            tag = bsplus::shiny_iconlink(),
                                            placement = "top",
                                            title = "Maximal tolerated m/z
                                                deviation in consecutive scans
                                                in parts per million (ppm)"
                                        )
                                    )
                                ),
                                shiny::tags$td(
                                    bsplus::shinyInput_label_embed(
                                        tag = shiny::numericInput(
                                            inputId = "process_snthresh",
                                            label = "s/n",
                                            value = 6.5
                                        ),
                                        bsplus::bs_embed_tooltip(
                                            tag = bsplus::shiny_iconlink(),
                                            placement = "top",
                                            title = "Signal to noise ratio
                                                cutoff"
                                        )
                                    )
                                )
                            ),
                            shiny::tags$tr(
                                shiny::tags$td(
                                    bsplus::shinyInput_label_embed(
                                        tag = shiny::numericInput(
                                            inputId = "process_peakwidth_min",
                                            label = "Peakwidth min (s)",
                                            value = 4
                                        ),
                                        element = bsplus::bs_embed_tooltip(
                                            tag = bsplus::shiny_iconlink(),
                                            placement = "top",
                                            title = "Expected approximate peak
                                                width in chromatographic space"
                                        )
                                    )
                                ),
                                shiny::tags$td(
                                    bsplus::shinyInput_label_embed(
                                        tag = shiny::numericInput(
                                            inputId = "process_peakwidth_max",
                                            label = "Peakwidth max (s)",
                                            value = 39
                                        ),
                                        bsplus::bs_embed_tooltip(
                                            tag = bsplus::shiny_iconlink(),
                                            placement = "top",
                                            title = "Expected approximate peak
                                                width in chromatographic space"
                                        )
                                    )
                                )
                            ),
                            shiny::tags$tr(
                                shiny::tags$td(
                                    bsplus::shinyInput_label_embed(
                                        tag = shiny::numericInput(
                                            inputId = "process_prefilter_step",
                                            label = "Prefilter step",
                                            value = 2
                                        ),
                                        element = bsplus::bs_embed_tooltip(
                                            tag = bsplus::shiny_iconlink(),
                                            placement = "top",
                                            title = "Mass traces are only
                                                retained if they contain at
                                                least k peaks with intensity
                                                >= I"
                                        )
                                    )
                                ),
                                shiny::tags$td(
                                    bsplus::shinyInput_label_embed(
                                        tag = shiny::numericInput(
                                            inputId = "process_prefilter_level",
                                            label = "Prefilter level",
                                            value = 815
                                        ),
                                        element = bsplus::bs_embed_tooltip(
                                            tag = bsplus::shiny_iconlink(),
                                            placement = "top",
                                            title = "Mass traces are only
                                                retained if they contain at
                                                least k peaks with intensity
                                                >= I"
                                        )
                                    )
                                )
                            )
                        )
                    ),
                    shiny::tabPanel("Advanced",
                        shiny::tags$table(
                            class = "table-params",
                            shiny::tags$tr(
                                shiny::tags$td(
                                    bsplus::shinyInput_label_embed(
                                        tag = shiny::selectInput(
                                            inputId = "process_mz_center_fun",
                                            label = "m/z center function",
                                            choices = c("wMean", "mean",
                                                "apex", "wMeanApex3",
                                                "meanApex3"),
                                            selected = "wMean"
                                        ),
                                        element = bsplus::bs_embed_tooltip(
                                            tag = bsplus::shiny_iconlink(),
                                            placement = "top",
                                            title = "Name of the function to
                                                calculate the m/z center of
                                                the chromatographic peak"
                                        )
                                    )
                                ),
                                shiny::tags$td()
                            ),
                            shiny::tags$tr(
                                shiny::tags$td(
                                    bsplus::shinyInput_label_embed(
                                        tag = shinyWidgets::awesomeCheckbox(
                                            inputId =
                                                "process_first_baseline_check",
                                            label = tags$b("Baseline check"),
                                            value = FALSE
                                        ),
                                        element = bsplus::bs_embed_tooltip(
                                            tag = bsplus::shiny_iconlink(),
                                            placement = "top",
                                            title = "Continuous data within
                                                regions of interest is checked
                                                to be above the first baseline"
                                        )
                                    )
                                ),
                                shiny::tags$td(
                                    bsplus::shinyInput_label_embed(
                                        tag = shinyWidgets::awesomeCheckbox(
                                            inputId = "process_integrate",
                                            label = tags$b(
                                                "Integration by CWT"
                                            ),
                                            value = TRUE
                                        ),
                                        element = bsplus::bs_embed_tooltip(
                                            tag = bsplus::shiny_iconlink(),
                                            placement = "top",
                                            title = "Integration method.
                                                If unchecked the descent is
                                                done on the real data, if
                                                checked peak limits are found
                                                through descent on the mexican
                                                hat filtered data. Method 1 is
                                                very accurate but prone to
                                                noise, while method 2 is more
                                                robust to noise but less exact"
                                        )
                                    )
                                )
                            ),
                            shiny::tags$tr(
                                shiny::tags$td(
                                    bsplus::shinyInput_label_embed(
                                        tag = shiny::numericInput(
                                            inputId = "process_noise",
                                            label = "Noise",
                                            value = 0
                                        ),
                                        element = bsplus::bs_embed_tooltip(
                                            tag = bsplus::shiny_iconlink(),
                                            placement = "top",
                                            title = "Optional argument which
                                                is useful for data that was
                                                centroided without any
                                                intensity threshold, centroids
                                                with intensity < noise are
                                                omitted from ROI detection"
                                        )
                                    )
                                ),
                                shiny::tags$td(
                                    bsplus::shinyInput_label_embed(
                                        tag = shiny::numericInput(
                                            inputId = "process_mzdiff",
                                            label = "m/z difference",
                                            value = 0.041
                                        ),
                                        element = bsplus::bs_embed_tooltip(
                                            tag = bsplus::shiny_iconlink(),
                                            placement = "top",
                                            title = "Minimum difference in m/z
                                                for peaks with overlapping
                                                retention times, can be
                                                negative to allow overlap"
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            ),
            shiny::tabPanel(
                title = "Alignment",
                shiny::tabsetPanel(
                    shiny::tabPanel(
                        title = "General",
                        shiny::tags$table(
                            class = "table-params",
                            shiny::tags$tr(
                                shiny::tags$td(
                                    bsplus::shinyInput_label_embed(
                                        tag = shiny::numericInput(
                                            inputId = "process_bw",
                                            label = "rT deviation (s)",
                                            value = 5
                                        ),
                                        element = bsplus::bs_embed_tooltip(
                                            tag = bsplus::shiny_iconlink(),
                                            placement = "top",
                                            title = "retention time standard
                                                deviation (s) allowed"
                                        )
                                    )
                                ),
                                tags$td(
                                    bsplus::shinyInput_label_embed(
                                        tag = shiny::numericInput(
                                            inputId = "process_mzwid",
                                            label = "m/z group slices (mDa)",
                                            value = 10
                                        ),
                                        element = bsplus::bs_embed_tooltip(
                                            bsplus::shiny_iconlink(),
                                            placement = "top",
                                            title = "slice of overlapping m/z
                                                groups"
                                        )
                                    )
                                )
                            )
                        )
                    ),
                    shiny::tabPanel("Advanced",
                        bsplus::shinyInput_label_embed(
                            tag = shiny::selectInput(
                                inputId = "process_dist_fun",
                                label = "Distance function",
                                choices = c("cor", "cor_opt", "cov", "prd",
                                    "euc"),
                                selected = "cor_opt"),
                            element = bsplus::bs_embed_tooltip(
                                tag = bsplus::shiny_iconlink(),
                                placement = "top",
                                title = "Distance function to be used.
                                    Allowed values are \"cor\"(Pearson\'s
                                    correlation), \"cor_opt\"(calculate only
                                    10% diagonal band of distance matrix; better
                                    runtime), \"cov\"(covariance), \"prd\"
                                    (product) and \"euc\"(Euclidian distance)"
                            )
                        ),
                        bsplus::shinyInput_label_embed(
                            tag = shiny::sliderInput(
                                inputId = "process_response",
                                label = "Response",
                                min = 0,
                                max = 100,
                                value = 1,
                                step = 1
                            ),
                            element = bsplus::bs_embed_tooltip(
                                tag = bsplus::shiny_iconlink(),
                                placement = "top",
                                title = "Defining the responsiveness of
                                    warping with response = 0 giving linear
                                    warping on start and end points and
                                    response = 100 warping using all bijective
                                    anchors"
                            )
                        ),
                        shiny::tags$table(
                            class = "table-params",
                            shiny::tags$tr(
                                shiny::tags$td(
                                    bsplus::shinyInput_label_embed(
                                        tag = shiny::numericInput(
                                            inputId = "process_gap_init",
                                            label = "Gap init",
                                            value = .3
                                        ),
                                        element = bsplus::bs_embed_tooltip(
                                            tag = bsplus::shiny_iconlink(),
                                            placement = "top",
                                            title = "Defining the penalty for
                                                gap opening"
                                        )
                                    )
                                ),
                                shiny::tags$td(
                                    bsplus::shinyInput_label_embed(
                                        tag = shiny::numericInput(
                                            inputId = "process_gap_extend",
                                            label = "Gap extend",
                                            value = 2.4
                                        ),
                                        element = bsplus::bs_embed_tooltip(
                                            tag = bsplus::shiny_iconlink(),
                                            placement = "top",
                                            title = "Defining the penalty for
                                                gap enlargement"
                                        )
                                    )
                                )
                            ),
                            shiny::tags$tr(
                                shiny::tags$td(
                                    bsplus::shinyInput_label_embed(
                                        tag = shiny::numericInput(
                                            inputId = "process_factor_diag",
                                            label = "Factor diagonal",
                                            value = 2
                                        ),
                                        element = bsplus::bs_embed_tooltip(
                                            tag = bsplus::shiny_iconlink(),
                                            placement = "top",
                                            title = "Defining the local weight
                                                applied to diagonal moves in
                                                the alignment"
                                        )
                                    )
                                ),
                                shiny::tags$td(
                                    bsplus::shinyInput_label_embed(
                                        tag = shiny::numericInput(
                                            inputId = "process_factor_gap",
                                            label = "Factor gap",
                                            value = 1
                                        ),
                                        element = bsplus::bs_embed_tooltip(
                                            tag = bsplus::shiny_iconlink(),
                                            placement = "top",
                                            title = "Defining the local weight
                                                for gap moves in the alignment"
                                        )
                                    )
                                )
                            ),
                            shiny::tags$tr(
                                shiny::tags$td(
                                    bsplus::shinyInput_label_embed(
                                        tag = shiny::numericInput(
                                            inputId = "process_init_penalty",
                                            label = "Initiating penalty",
                                            value = 0
                                        ),
                                        element = bsplus::bs_embed_tooltip(
                                            tag = bsplus::shiny_iconlink(),
                                            placement = "top",
                                            title = "Defining the penalty for
                                                initiating an alignment (for
                                                local alignment only)"
                                        )
                                    )
                                ),
                                shiny::tags$td(
                                    bsplus::shinyInput_label_embed(
                                        tag = shinyWidgets::awesomeCheckbox(
                                            inputId = "process_local_alignment",
                                            label = shiny::tags$b(
                                                "Local alignment"
                                            ),
                                            value = FALSE
                                        ),
                                        element = bsplus::bs_embed_tooltip(
                                            tag = bsplus::shiny_iconlink(),
                                            placement = "top",
                                            title = "Whether a local alignment
                                                should be performed instead of
                                                the default global alignment"
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            ),
            shiny::tabPanel(
                title = "Annotation",
                shiny::tags$table(
                    class = "table-params",
                    shiny::tags$tr(
                        shiny::tags$td(
                            bsplus::shinyInput_label_embed(
                                tag = shiny::numericInput(
                                    inputId = "process_mda_tol",
                                    label = "m/z tolerance (mDa)",
                                    value = 15
                                ),
                                element = bsplus::bs_embed_tooltip(
                                    tag = bsplus::shiny_iconlink(),
                                    placement = "top",
                                    title = "m/z tolerance to use for
                                        identification"
                                )
                            )
                        ),
                        shiny::tags$td(
                            bsplus::shinyInput_label_embed(
                                tag = shiny::numericInput(
                                    inputId = "process_rt_tol",
                                    label = "rT tolerance (s)",
                                    value = 10
                                ),
                                element = bsplus::bs_embed_tooltip(
                                    tag = bsplus::shiny_iconlink(),
                                    placement = "top",
                                    title = "rT tolerance to use for
                                        identification"
                                )
                            )
                        )
                    )
                ),
                bsplus::shinyInput_label_embed(
                    tag = shiny::sliderInput(
                        inputId = "process_abd_tol",
                        label = "Relative abundance tolerance (%)",
                        min = 0,
                        max = 100,
                        value = 25,
                        step = 1
                    ),
                    element = bsplus::bs_embed_tooltip(
                        tag = bsplus::shiny_iconlink(),
                        placement = "top",
                        title = "Peaks which have a difference of this
                            percentage compared to the theoretical are not
                            considered part of the isotopic profile"
                    )
                ),
                shinyWidgets::pickerInput(
                    inputId = "process_adducts",
                    label = "adducts",
                    choices = c(),
                    multiple = TRUE,
                    options = list(
                        `live-search` = TRUE
                    )
                ),
                shiny::selectInput(
                    inputId = "process_instrument",
                    label = "Instrument",
                    choices = c()
                )
            ),
            shiny::tabPanel(
                title = "Misc",
                bsplus::shinyInput_label_embed(
                    tag = shiny::numericInput(
                        inputId = "process_cores",
                        label = "cores",
                        value = 1,
                        min = 1,
                        max = 1,
                        step = 1,
                        width = "50%"
                    ),
                    element = bsplus::bs_embed_tooltip(
                        tag = bsplus::shiny_iconlink(),
                        placement = "top",
                        title = "Number of cores to use for the processing.
                            Detect automatically the maximum possible."
                    )
                )
            )
        )
    )
)
