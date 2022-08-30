# Declaration of shinyFilesChoose button
shinyFiles::shinyFileChoose(
    input,
    "process_files",
    roots = volumes,
    filetypes = c("mzML", "mzXML", "CDF", "RAW", "d", "YEP", "BAF", "FID",
                  "WIFF", "MGF")
)

#' @title Table of the files to process
#'
#' @description
#' Table with all the files selected by the user to process
#'
#' @param input$process_files `character` filepath to the files
output$process_dt_files_imported <- DT::renderDataTable({
    if (is.integer(input$process_files)) {
        data.frame(matrix(, nrow = 0, ncol = 1, dimnames = list(
            c(), c("File"))), check.names = FALSE)
    } else {
        data.frame(File = shinyFiles::parseFilePaths(
            volumes,
            input$process_files
        )$name)
    }
}, server = isFALSE(getOption("shiny.testmode")),
    selection = "none",
    rownames = FALSE,
    extensions = "Scroller",
    options = list(
        dom = "frtip",
        bFilter = FALSE,
        ordering = FALSE,
        scroller = TRUE,
        scrollY = "45vh",
        scrollX = TRUE,
        scrollCollapse = TRUE
    )
)

shiny::updateSelectInput(
    session,
    inputId = "process_database",
    label = "Database",
    choices = get_available_database()
)

shinyWidgets::updatePickerInput(
    session,
    inputId = "process_adducts",
    label = "Adducts",
    choices = adducts$name,
    selected = c("[M+Na]+", "[M+NH4]+", "[M+H-H2O]+", "[M-H]-", "[M+H]+")
)

shiny::updateSelectInput(
    session,
    inputId = "process_instrument",
    label = "Instrument",
    choices = names(resolution_list),
    selected = "QTOF_XevoG2-S_R25000@200"
)

observeEvent(input$process_database, {
    if (input$process_database == "") {
        return(0)
    }
    database <- system.file(
        "extdata",
        "database",
        paste(input$process_database, "csv", sep = "."),
        package = "SPRING"
    )
    cpd_classes <- unique(load_chem_db(database)$class)
    shinyWidgets::updatePickerInput(
        session,
        inputId = "process_cpd_classes",
        label = "Compound classes",
        choices = cpd_classes,
        selected = cpd_classes
    )
})

shiny::updateNumericInput(
    session,
    inputId = "process_cores",
    label = "Cores",
    value = parallel::detectCores(),
    min = 1,
    max = parallel::detectCores(),
    step = 1
)

#' @title TIC of raw files
#'
#' @description
#' Plot the TIC of the raw files selected by the user
#'
#' @param input$process_polarity `character(1)` polarity to use ("positive" or
#' "negative")
#' @param input$process_files `character` filepath to the files
output$process_tic <- plotly::renderPlotly({
    params <- list(
        raw_files = shinyFiles::parseFilePaths(
            volumes,
            input$process_files
        )$datapath,
        polarity = input$process_polarity
    )
    tryCatch({
        if (is.integer(input$process_files)) {
            custom_stop("invalid", "No files selected")
        } else if (any(is.na(params$raw_files)) |
                   length(params$raw_files) == 0) {
            custom_stop("invalid", "No files selected")
        } else {
            plot_raw_tic(
                params$raw_files,
                file.path(.SPRING_env$pwiz, "msaccess.exe"),
                params$polarity
            )
        }
    }, invalid = function(i) {
        plot_empty_chromato("")
    }, error = function(e) {
        print("########## process_tic")
        print(params)
        print(e)
        sweet_alert_error(e$message)
        plot_empty_chromato("")
    })
})

#' @title Launch process event
#'
#' @description
#' Launch the process event. See the `function` process for more info
#'
#' @param sqlite_path,
#' @param input$process_cores `numeric(1)` number of cores to use
#' @param input$process_polarity `character(1)` polarity to use ("positive" or
#' "negative")
#' @param input$process_files `character` filepath to the files
#' @param input$process_ppm `numeric(1)` Maximal tolerated m/z deviation in
#' consecutive scans in parts sper million (ppm)
#' @param input$process_peakwidth_min `numeric(1)` Expected approximate peak
#' width min in chromatographic space
#' @param input$process_peakwidth_max `numeric(1)` Expected approximate peak
#' width max in chromatographic space
#' @param input$process_snthresh `numeric(1)` Signal to noise ratio cutoff
#' @param input$process_prefilter_step `numeric(1)` Mass traces are only
#' retained if they contain at least k peaks with intensity >= I
#' @param input$process_prefilter_level `numeric(1)` Mass traces are only
#' retained if they contain at least k peaks with intensity >= I
#' @param input$process_mz_center_fun `character(1)`Name of the function to
#' calculate the m/z center of the chromatographic peak
#' @param input$process_integrate `logical(1)` Integration method. If unchecked
#' the descent is done on the real data, if checked peak limits are found
#'  through descent on the mexican hat filtered data. Method 1 is very accurate
#'  prone to noise, while method 2 is more robust to noise but less exact
#' @param input$process_mzdiff `numeric(1)` Minimum difference in m/z for peaks
#'  with overlapping retention times, can be negative to allow overlap
#' @param input$process_noise `numeric(1)` Optional argument which is useful for
#'  data that was centroided without any intensity threshold, centroids with
#'   intensity < noise are omitted from ROI detection
#' @param input$process_first_baseline_check Continuous data within regions of
#' interest is checked to be above the first baseline
#' @param input$process_response `numeric(1)` Defining the responsiveness of
#'  warping with response = 0 giving linear warping on start and end points and
#'  response = 100 warping using all bijective anchors
#' @param input$process_dist_fun `character(1)` Distance function to be used.
#' Allowed values are :
#' \itemize{
#'     \item cor : Pearson's correlation
#'     \item cor_opt : calculate only 10% diagonal band of distance matrix(
#'     better runtime)
#'     \item cov : covariance
#'     \item prd : product
#'     \item euc : Euclidian distance
#' }
#' @param input$process_gap_init `numeric(1)` Defining the penalty for gap
#' opening
#' @param input$process_gap_extend `numeric(1)` Defining the penalty for gap
#' enlargement
#' @param input$process_factor_diag `numeric(1)` Defining the local weight
#'  applied to diagonal moves in the alignment
#' @param input$process_factor_gap `numeric(1)` Defining the local weight for
#'  gap moves in the alignment
#' @param input$process_local_alignment `logical(1)` Whether a local alignment
#'  should be performed instead of the default global alignment
#' @param input$process_init_penalty `numeric(1)` Defining the penalty for
#'  initiating an alignment (for local alignment only)
#' @param input$process_bw `numeric(1)` retention time standard deviation (s)
#'  allowed
#' @param input$process_mzwid `numeric(1)` slice of overlapping m/z groups (mda)
#' @param input$process_mda_tol `numeric(1)` m/z tolerance (mda)
#' @param input$process_rt_tol `numeric(1)` rT tolerance in sec
#' @param input$process_abd_tol `numeric(1)` relative abundance tolerance, each
#'  peak which have an higher difference of relative abundance with its
#'  corresponding theoretical peak will be discarded
#' @param input$process_database `character` database to use for annotation
#' @param input$process_instrument `character(1)` instrument names from the
#'  enviPat package
#' @param input$process_cpd_classes `character vector` compound classes to
#' restrict the annotation from the database
#' @param input$process_perfwhm `numeric(1)` percentage of the FWHM to use when
#'  grouping peaks
#' @param input$process_cor_eic_th `numeric(1)` correlation threshold for EICs
#' @param input$process_graph_method `character(1)` method to use for grouping
#' peaks
#' @param input$process_sigma `numeric(1)` multiplier of the standard deviation
#' @param input$process_pval `numeric(1)` significant correlation threshold
shiny::observeEvent(input$process_launch, {
    params <- list(
        process_files = input$process_files
    )
    tryCatch({

        # check specifically the files parameter
        if (is.integer(input$process_files)) {
            custom_stop("invalid_2", "No files selected")
        }

        params <- list(
            sqlite_path = sqlite_path(),
            Cores = input$process_cores,
            polarity = input$process_polarity,
            Files = shinyFiles::parseFilePaths(
                volumes,
                input$process_files
            )$datapath,
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
            Database = input$process_database,
            Instrument = input$process_instrument,
            `Compound classes` = input$process_cpd_classes,
            `Percentage of the FWHM` = input$process_perfwhm,
            `EIC correlation threshold` = input$process_cor_eic_th,
            `Grouping method` = input$process_graph_method,
            Sigma = input$process_sigma,
            `p-value` = input$process_pval
        )
        inputs <- paste("process", c("sqlite_path", "cores", "polarity",
             "files", "ppm", "peakwidth_min", "peakwidth_max", "snthresh",
             "prefilter_step", "prefilter_level", "mz_center_fun", "integrate",
             "mzdiff", "noise", "first_baseline_check", "response", "dist_fun",
             "gap_init", "gap_extend", "factor_diag", "factor_gap",
             "local_alignment", "init_penalty", "bw", "mzwid", "mda_tol",
             "rt_tol", "abd_tol", "database", "instrument", "cpd_classes",
             "perfwhm", "cor_eic_th", "graph_method", "sigma", "pval"),
             sep = "_")

        # check which are missing
        conditions <- !is.na(params) & lengths(params) > 0
        msgs <- paste(names(params), "is missing or incorrect")
        check_inputs(inputs, conditions, msgs)

        # check which is negative
        idx <- which(names(params) %in% c("Cores",
            "m/z tolerance (peakpicking)", "Peakwidth min", "Peakwidth max",
            "s/n", "Prefilter step", "Prefilter level", "Noise", "Gap init",
            "Gap extend", "Factor diag", "Factor gap", "Initiating penalty",
            "rT deviation", "m/z group slices", "m/z tolerance (annotation)",
            "rT tolerance", "Sigma", "p-value"))
        conditions <- unlist(params[idx]) >= 0
        msgs <- paste(names(params[idx]), "need to be a positive number or 0")
        check_inputs(inputs[idx], conditions, msgs)

        # check parameters ranges
        idx <- which(names(params) %in% c("Peakwidth min", "Peakwidth max"))
        conditions <- rep(sapply(
            split(params[idx], ceiling(seq(length(idx)) / 2)),
                function(x) x[[1]] < x[[2]]), each = 2)
        msgs <- unlist(lapply(
            split(names(params[idx]), ceiling(seq(length(idx)) / 2)),
                function(x) {
                    c(
                        paste(x[1], "cannot be over than", x[2]),
                        paste(x[2], "cannot be under than", x[1]))
                }))
        check_inputs(inputs[idx], conditions, msgs)

        cwt_params <- xcms::CentWaveParam(
            ppm = params[["m/z tolerance (peakpicking)"]],
            peakwidth = c(
                params[["Peakwidth min"]],
                params[["Peakwidth max"]]),
            snthresh = params[["s/n"]],
            prefilter = c(
                params[["Prefilter step"]],
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
            binSize = .1,
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
            instrument = params[["Instrument"]],
            database = params[["Database"]],
            polarity = params[["polarity"]],
            cpd_classes = params[["Compound classes"]]
        )
        camera_params <- CameraParam(
            ann_params = ann_params,
            cores = params[["Cores"]],
            sigma = params[["Sigma"]],
            perfwhm = params[["Percentage of the FWHM"]],
            cor_eic_th = params[["EIC correlation threshold"]],
            pval = params[["p-value"]],
            graph_method = params[["Grouping method"]]
        )

        shinyWidgets::progressSweetAlert(
            session,
            "pb",
            title = "",
            value = 0
        )
        infos <- ms_process(
            params$Files,
            params$sqlite_path,
            file.path(.SPRING_env$pwiz, "msconvert.exe"),
            cwt_params,
            obw_params,
            pd_params,
            camera_params,
            ann_params,
            cores = params$Cores,
            show_txt_pb = FALSE,
            pb_fct = function(n, title, total = 8) {
                shinyWidgets::updateProgressBar(
                    session,
                    id = "pb",
                    value = (n - 1) / total,
                    title = title
                )
            }
        )

        shinyWidgets::closeSweetAlert()
        if (any(class(infos) == "error")) {
            stop(infos)
        }

        db(db_connect(sqlite_path()))
        ann <- db_get_annotations(db())
        if (nrow(ann) > 0) {
            conflicts <- split_conflicts(ann)$conflicts
            conflicts(sapply(conflicts, function(x) x[[1]][1, "pcgroup_id"]))
            if (length(conflicts) > 0) conflict_id(1)
            else conflict_id(0)
            shiny::updateSliderInput(
                session,
                inputId = "peak_spot_int_threshold",
                max = db_get_query(
                    db(),
                    "SELECT ROUND(MAX(basepeak_int))
                FROM spectra_infos"
                )[1, 1]
            )
            actualize$peak_spot <<- runif(1)
        } else {
            conflicts(c())
            conflict_id(0)
            shiny::updateSliderInput(
                session,
                inputId = "peak_spot_int_threshold",
                max = 0
            )
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
