# Declaration of shinyFilesChoose button
shinyFiles::shinyFileChoose(input, 'process_files', roots = volumes,
	filetypes = c('mzML', 'mzXML', 'CDF', 'RAW', 'd', 'YEP', 'BAF',
        'FID', 'WIFF', 'MGF'))

output$process_dt_files_imported <- DT::renderDataTable(
    if (is.integer(input$process_files)) data.frame(matrix(,
        nrow = 0, ncol = 1, dimnames = list(
            c(), c("File"))), check.names = FALSE)
    else data.frame(File = shinyFiles::parseFilePaths(
        volumes, input$process_files)$name)
,
    selection = "none",
    rownames = FALSE,
    extensions = "Scroller",
    options = list(
        dom = "frtip",
        bFilter = FALSE,
        ordering = FALSE,
        scroller = TRUE,
        scrollY = "50vh",
        scrollX = TRUE,
        scrollCollapse = TRUE
    )
)

shinyWidgets::updatePickerInput(session, inputId = "process_adducts",
                                label = "Adducts", choices = adducts$Name,
                                selected = c("[M+Na]+", "[M+NH4]+", "[M+H-H2O]+", "[M-H]-", "[M+H]+"))

shiny::updateSelectInput(session, inputId = "process_instrument",
                         label = "Instrument", choices = names(resolution_list),
                         selected = "QTOF_XevoG2-S_R25000@200")

shiny::updateNumericInput(session, inputId = "process_cores",
                          label = "Cores", value = parallel::detectCores(), min = 1,
                          max = parallel::detectCores(), step = 1)

shiny::observeEvent(input$process_launch, {
    params <- list(
        process_files = input$process_files
    )
    tryCatch({

        # check specifically the files parameter
        if (is.integer(input$process_files)) custom_stop("invalid_2", "No files selected")

        params <- list(
            sqlite_path = sqlite_path(),
            Cores = input$process_cores,
            Files = shinyFiles::parseFilePaths(volumes,
                input$process_files)$datapath,
            `m/z min` = input$process_filter_mz_min,
            `m/z max`= input$process_filter_mz_max,
            `rT min` = input$process_filter_rt_min,
            `rT max` = input$process_filter_rt_max,
            `m/z tolerance (peakpicking)` = input$process_ppm,
            `Peakwidth min` = input$process_peakwidth_min,
            `Peakwidth max` = input$process_peakwidth_max,
            `s/n` = input$process_snthresh,
            `Prefilter step` = input$process_prefilter_step,
            `Prefilter level` = input$process_prefilter_level,
            `m/z center function` = input$process_mz_center_fun,
            `Integration by CWT` = input$process_integrate,
            `m/z difference` = input$process_mzdiff,
            Noise = input$process_noise,
            `Baseline check` = input$process_first_baseline_check,
            Response = input$process_response,
            `Distance function` = input$process_dist_fun,
            `Gap init` = input$process_gap_init,
            `Gap extend` = input$process_gap_extend,
            `Factor diag` = input$process_factor_diag,
            `Factor gap` = input$process_factor_gap,
            `Local alignment` = input$process_local_alignment,
            `Initiating penalty` = input$process_init_penalty,
            `rT deviation` = input$process_bw,
            `m/z group slices` = input$process_mzwid * 10**-3,
            `m/z tolerance (annotation)` = input$process_mda_tol * 10**-3,
            `rT tolerance` = input$process_rt_tol,
            `Relative abundance tolerance` = input$process_abd_tol,
            Adducts = input$process_adducts,
            Instrument = input$process_instrument
        )
        inputs <- paste("process", c("cores", "files", "filter_mz_min",
            "filter_mz_max", "filter_rt_min", "filter_rt_max", "ppm",
            "peakwidth_min", "peakwidth_max", "snthresh", "prefilter_step",
            "prefilter_level", "mz_center_fun", "integrate", "mzdiff", "noise",
            "first_baseline_check", "response", "dist_fun",
            "gap_init", "gap_extend", "factor_diag", "factor_gap",
            "local_alignment", "init_penalty", "bw", "mzwid", "mda_tol", "rt_tol",
            "abd_tol", "adducts", "instrument"), sep = "_")

        # check which are missing
        conditions <- !is.na(params) & lengths(params) > 0
        msgs <- paste(names(params), "is missing or incorrect")
        check_inputs(inputs, conditions, msgs)

        # check which is negative
        idx <- which(names(params) %in% c("Cores", "m/z min", "m/z max",
            "rT min", "rT max", "m/z tolerance (peakpicking)",
            "Peakwidth min", "Peakwidth max", "s/n", "Prefilter step",
            "Prefilter level", "Noise", "Gap init", "Gap extend",
            "Factor diag", "Factor gap", "Initiating penalty", "rT deviation",
            "m/z group slices", "m/z tolerance (annotation)", "rT tolerance"))
        conditions <- unlist(params[idx]) >= 0
        msgs <- paste(names(params[idx]), "need to be a positive number or 0")
        check_inputs(inputs[idx], conditions, msgs)

        # check parameters ranges
        idx <- which(names(params) %in% c("m/z min", "m/z max", "rT min",
            "rT max", "Peakwidth min", "Peakwidth max"))
        conditions <- rep(sapply(
            split(params[idx], ceiling(seq(length(idx)) / 2)),
                function(x) x[[1]] < x[[2]]), each = 2)
        msgs <- unlist(lapply(
            split(names(params[idx]), ceiling(seq(length(idx)) / 2)),
                function(x) c(
                    paste(x[1], "cannot be over than", x[2]),
                    paste(x[2], "cannot be under than", x[1]))))
        check_inputs(inputs[idx], conditions, msgs)

        # create the parameters objects
        filter_params <- FilterParam(
            mz_range = c(params[["m/z min"]], params[["m/z max"]]),
            rt_range = c(params[["rT min"]] * 60, params[["rT max"]] * 60)
        )
        cwt_params <- xcms::CentWaveParam(
            ppm = params[["m/z tolerance (peakpicking)"]],
            peakwidth = c(params[["Peakwidth min"]],
                params[["Peakwidth max"]]),
            snthresh = params[["s/n"]],
            prefilter = c(params[["Prefilter step"]],
                params[["Prefilter level"]]),
            mzCenterFun = params[["m/z center function"]],
            integrate = params[["Integration by CWT"]],
            mzdiff = params[["m/z difference"]],
            # don't know why but i experienced some issues with fitgauss
            fitgauss = FALSE,
            noise = params[["Noise"]],
            firstBaselineCheck = params[["Baseline check"]]
        )
        obw_params <- xcms::ObiwarpParam(
            binSize = 1,
            centerSample = integer(),
            response = params[["Response"]],
            distFun = params[["Distance function"]],
            gapInit = params[["Gap init"]],
            gapExtend = params[["Gap extend"]],
            factorDiag = params[["Factor diag"]],
            factorGap = params[["Factor gap"]],
            localAlignment = params[["Local alignment"]],
            initPenalty = params[["Initiating penalty"]]
        )
        pd_params <- xcms::PeakDensityParam(
            sampleGroups = 0,
            minFraction = 10**-9,
            minSamples = 1,
            maxFeatures = 500,
            bw = params[["rT deviation"]],
            binSize = params[["m/z group slices"]]
        )
        ann_params <- AnnotationParam(
            da_tol = params[["m/z tolerance (annotation)"]],
            rt_tol = params[["rT tolerance"]],
            abd_tol = params[["Relative abundance tolerance"]],
            adduct_names = params[["Adducts"]],
            instrument = params[["Instrument"]]
        )

        shinyWidgets::progressSweetAlert(session, 'pb',
            title = '', value = 0)
        infos <- ms_process(params$Files, params$sqlite_path,
                            .workflow_lipido_env$converter, filter_params,
                            cwt_params, obw_params, pd_params, ann_params,
                            cores = params$Cores, show_txt_pb = FALSE,
                            pb_fct = function(n, total, title)
                                shinyWidgets::updateProgressBar(
                                    session,
                                    id = "pb",
                                    value = (n - 1) * 100 / total,
                                    title = title))

        shinyWidgets::closeSweetAlert()
        if (class(infos) != "character") stop(infos)

        db(db_connect(sqlite_path()))
        ann <- db_get_ann(db())
        if (nrow(ann) > 0) {
            ann(split_conflicts(ann))
            spectra_infos(db_get_spectra_infos(db()))
        } else {
            ann(list())
            spectra_infos(data.frame())
        }
    }, invalid = function(i) NULL
    , invalid_2 = function(i) {
        print("########## PROCESS")
        print(params)
        print(i)
        toastr_error(i$message)
    }, error = function(e) {
        print("########## PROCESS")
        print(params)
        print(e)
        shinyWidgets::closeSweetAlert()
        sweet_alert_error(e$message)
    })
})
