#' @title Launch workflow
#'
#' @description
#' Launch the workflow on the raw files, which consist of the steps :
#' \itemize{
#'     \item check the parameters given + creation of the sqlite database where
#'     the result will be recorded
#'     \item convert each raw file in the polarity desired ("positive" or
#'     "negative" mode with msConvert. It also trim the file according the rt
#'     according the m/z & rT of compounds to discover with msconvert. Check
#'     with MSnbase package if the file can be converted. If the file is a CDF
#'     it will copy the file instead. If the conversion failed & the file is a
#'     mzML or mzXML it will copy the file instead and try to trim only the rt
#'     range. Record then a `xcmsRaw` file and its corresponding profile matrix
#'     in the database, this object is compress into a blob object before
#'     inserting in the database.
#'      \item peak peacking with CentWave algorithm
#'      \item alignment with obiwarp which is based on the complete mz-rt
#'      data
#'      \item group peaklists from a `xcmsSet` object using the density
#'      method
#'      \item annotate isotopologues & adducts with the CAMERA package
#'      \item annotate peaklists from a `xsAnnotate` object from CAMERA
#'      it loop through the pcgroups
#'      if one of the peak match with one of the theoretical monoisotopic
#'       from database it will compare the pseudo spectra obtained from
#'       CAMERA against the theoretical spectra & compute an isotopic
#'       score
#'       The scoring algorithm will search each corresponding observed peak
#'       with theoreticals. Therefore it contains some important rules :
#'       \itemize{
#'            \item an observed peak can only correspond to ONE theoretical
#'            peak and vice versa
#'            \item the relative abundance peak must not be under a tolerance
#'            compared to the theoretical
#'            but it can be higher since a peak can hide another
#'            \item the A+x is not searched if the A+x-1 is not found
#'            (the loop search is stopped)
#'     }
#'     \item record all EICs & m/z foreach basepeak foreach sample
#'     \item record all results in the sqlite database
#' }
#'
#' @param raw_files `character vector` filepaths to the raw files
#' @param sqlite_path `character(1)` filepath to the database to create
#' @param converter `character(1)` filepath to the msconvert.exe
#' @param cwt_params `CentwaveParam` object
#' @param obw_params `ObiwarpParam` object
#' @param pd_params `PeakdensityParam` object
#' @param camera_params `CameraParam` object
#' @param ann_params `AnnotationParam` object
#' @param cores `integer(1)` number of cores to use to parallelize process
#' @param show_txt_pb `boolean` should print a progress bar on the console ?
#' @param pb_fct `function` used to update the progress bar. Only give if you
#' intend to use a specific progress bar you created !!!
#'
#' @export
#' @examples
#' \dontrun{
#' # initialize parameters
#' raw_files <- c("file1.raw", "file2.raw")
#' sqlite_path <- "20220513_global_test.sqlite"
#' converter <- "pwiz/msconvert.exe"
#' cwt_params <- xcms::CentWaveParam(
#'      ppm = 3,
#'      peakwidth = c(4, 39),
#'      snthresh = 10,
#'      prefilter = c(2, 1000),
#'      mzdiff = .01
#' )
#' obw_params <- xcms::ObiwarpParam()
#' pd_params <- xcms::PeakDensityParam(
#'      bw = 5,
#'      binSize = 0.01,
#' )
#' ann_params <- AnnotationParam(
#'      da_tol = .015,
#'      rt_tol = 10,
#'      abd_tol = 25,
#'      database = "test",
#'      instrument = "QTOF_XevoG2-S_R25000@200",
#'      polarity = "positive"
#' )
#' ms_process(
#'      raw_files,
#'      sqlite_path,
#'      converter,
#'      cwt_params,
#'      obw_params,
#'      pd_params,
#'      ann_params
#' )
#' }
ms_process <- function(raw_files,
                       sqlite_path,
                       converter,
                       cwt_params,
                       obw_params,
                       pd_params,
                       camera_params,
                       ann_params,
                       cores = parallel::detectCores(),
                       show_txt_pb = TRUE,
                       pb_fct = NULL) {
    tryCatch({
        ########## INITIALIZE
        check_ms_process_args(
            raw_files,
            sqlite_path,
            converter,
            cwt_params,
            obw_params,
            pd_params,
            camera_params,
            ann_params,
            cores
        )
        raw_files <- sapply(raw_files, normalizePath)
        sqlite_path <- suppressWarnings(normalizePath(sqlite_path))
        converter <- normalizePath(converter)

        # XCMS will use only one core per file
        cores <- min(length(raw_files), cores)
        cwt_params@verboseColumns <- TRUE
        obw_params@binSize <- .1
        pd_params@sampleGroups <- seq(length(raw_files))
        pd_params@minFraction <- 10**-9
        pd_params@minSamples <- 1
        pd_params@maxFeatures <- 500
        filter_params <- FilterParam(cwt_params, ann_params)

        BiocParallel::register(BiocParallel::SnowParam(cores))
        BiocParallel::bpstart()
        parallel::clusterExport(
            BiocParallel::bpbackend(),
            list("convert_file", "check_ms_file", "filter_ms_file"),
            envir = pryr::where("convert_file")
        )

        ################ CONVERSION
        print("Conversion")
        if (show_txt_pb) {
            pb <- utils::txtProgressBar(
                min = 0,
                max = 1,
                style = 3,
                title = ""
            )
            if (is.null(pb_fct)) {
                pb_total <- 8
                pb_fct <- function(n, total = pb_total, title) {
                    utils::setTxtProgressBar(
                        pb,
                        value = (n - 1) / total,
                        title = title
                    )
                }
            }
        }

        if (!is.null(pb_fct)) {
            pb_val <- 1
            pb_fct(n = pb_val, title = "Conversion")
        }
        tmp_fun <- function(raw_file, converter, filter_params) {
            tryCatch({
                cbind(
                    filepath = convert_file(
                        raw_file,
                        converter,
                        filter_params
                    ),
                    success = "success"
                )
            }, error = function(e) {
                cbind(filepath = e$message, success = "error")
            })
        }
        filepaths <- suppressWarnings(do.call(
            rbind,
            BiocParallel::bplapply(
                raw_files,
                tmp_fun,
                converter,
                filter_params
            )
        ))
        if (any(filepaths[, "success"] == "error")) {
            print(filepaths[filepaths[, "success"] == "error", ])
            stop("some of the file was not imported correctly")
        } else {
            filepaths <- filepaths[, "filepath"]
        }

        ############ PEAK PICKING
        print("Peak picking")
        if (!is.null(pb_fct)) {
            pb_val <- pb_val + 1
            pb_fct(n = pb_val, title = "PeakPicking")
        }
        ms_files <- MSnbase::readMSData(filepaths, mode = "onDisk")
        suppressWarnings(suppressMessages(xset <- xcms::findChromPeaks(
            ms_files,
            cwt_params,
            return.type = "xcmsSet"
        )))
        rm(ms_files)

        ######### ALIGNMENT
        print("Obiwarp")
        if (!is.null(pb_fct)) {
            pb_val <- pb_val + 1
            pb_fct(n = pb_val, title = "Obiwarp")
        }
        suppressMessages(invisible(capture.output(xset <- xcms::retcor.obiwarp(
            xset,
            plottype = "none",
            profStep = obw_params@binSize,
            center = as.integer(names(which.max(
                table(xset@peaks[, "sample"])))),
            response = obw_params@response,
            distFunc = obw_params@distFun,
            gapInit = obw_params@gapInit,
            gapExtend = obw_params@gapExtend,
            factorDiag = as.numeric(obw_params@factorDiag),
            factorGap = as.numeric(obw_params@factorGap),
            localAlignment = obw_params@localAlignment,
            initPenalty = as.numeric(obw_params@initPenalty)
        ))))

        ########### GROUP
        print("Alignment")
        if (!is.null(pb_fct)) {
            pb_val <- pb_val + 1
            pb_fct(n = pb_val, title = "Alignment")
        }
        suppressMessages(xset <- xcms::group.density(
            xset,
            minfrac = pd_params@minFraction,
            minsamp = pd_params@minSamples,
            bw = pd_params@bw,
            mzwid = pd_params@binSize,
            max = pd_params@maxFeatures,
        ))

        ########## CAMERA
        print("Group")
        if (!is.null(pb_fct)) {
            pb_val <- pb_val + 1
            pb_fct(n = pb_val, title = "Group")
        }
        invisible(utils::capture.output(xsa <- camera_annotate(
            xset,
            camera_params
        )))

        ########## ANNOTATE
        print("Annotate")
        if (!is.null(pb_fct)) {
            pb_val <- pb_val + 1
            pb_fct(n = pb_val, title = "Annotate")
        }
        xsf <- annotate_pcgroups(xsa, ann_params)
        rm(xsa)

        ############ RECORD
        print("Record results")
        if (!is.null(pb_fct)) {
            pb_fct(n = 7, title = "Record results")
        }
        db <- db_connect(sqlite_path)
        db_record_xsf(db, xsf)
        db_record_params(
            db,
            filter_params,
            cwt_params,
            obw_params,
            pd_params,
            camera_params,
            ann_params
        )

        ########### GENERATE EICs & mzMat
        if (nrow(xsf$peakgroups) > 0) {
            print("GENERATE EICs & mzMat")
            if (!is.null(pb_fct)) {
                pb_val <- pb_val + 1
                pb_fct(n = pb_val, title = "GENERATE EICs & mzMat")
            }
            db_record_mzdata(db, xset)
        }
        rm(xset)
        RSQLite::dbDisconnect(db)
        if (show_txt_pb) {
            pb_val <- pb_val + 1
            pb_fct(n = pb_val)
            close(pb)
        }
        BiocParallel::bpstop()
        NULL
    }, error = function(e) {
        try(suppressWarnings(file.remove(sqlite_path)))
        if (exists("pb")) {
            close(pb)
        }
        BiocParallel::bpstop()
        stop(e)
    })
}

#' @title Export annotations
#'
#' @description
#' Export all annotations in a excel file
#' First sheet will have the annotations regroup by compound
#' Second will have annotations regroup by ions
#'
#' @param sqlite_path `character(1)` sqlite path to the annotation results
#' @param excel_path `character(1)` path to the excel file to create
#' @param by `character(1)` should be `referent` to report only the intensity of
#'  the referent ion or `all` to sum all the intensity for the compound
#'
#' @export
export_annotations <- function(sqlite_path, excel_path, by = "referent") {
    if (class(sqlite_path) != "character") {
        stop("sqlite file arg must be a filepath to a database file")
    } else if (!file.exists(sqlite_path)) {
        stop("the path in the sqlite file arg doesn't exist")
    } else if (class(excel_path) != "character") {
        stop("excel file arg must be a filepath to a database file")
    } else if (!dir.exists(dirname(excel_path))) {
        stop("the directory path to the excel file arg doesn't exist")
    }

    db <- db_connect(sqlite_path)
    ann <- db_get_annotations(db)
    if (nrow(ann) == 0) {
        stop("no annotations in database")
    }
    nsamples <- db_get_nsamples(db)
    spectra_ids <- na.omit(unlist(
        ann[, (ncol(ann) - nsamples + 1):ncol(ann)]))
    spectra_infos <- db_get_spectra_infos(db, spectra_ids)
    RSQLite::dbDisconnect(db)

    summarised_ann <- summarise_ann(ann, spectra_infos, nsamples, by)
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Summary")
    openxlsx::addWorksheet(wb, "Details")
    openxlsx::writeDataTable(
        wb,
        "Summary",
        summarised_ann$resume
    )
    openxlsx::writeDataTable(
        wb,
        "Details",
        summarised_ann$details
    )
    openxlsx::saveWorkbook(wb, excel_path, overwrite = TRUE)
}

camera_annotate <- function(xset, camera_params) {
    invisible(utils::capture.output({
        xsa <- CAMERA::xsAnnotate(xset, polarity = camera_params@polarity)
        xsa <- CAMERA::groupFWHM(
            xsa,
            sigma = camera_params@sigma,
            perfwhm = camera_params@perfwhm,
            intval = camera_params@intval
        )
        tryCatch(xsa <- CAMERA::findIsotopes(
            xsa,
            maxcharge = camera_params@maxcharge,
            maxiso = camera_params@maxiso,
            ppm = camera_params@ppm,
            mzabs = camera_params@mzabs,
            intval = camera_params@intval,
            minfrac = camera_params@minfrac
        ), error = function(e) NULL)
        xsa <- CAMERA::groupCorr(
            xsa,
            cor_eic_th = camera_params@cor_eic_th,
            pval = camera_params@pval,
            graphMethod = camera_params@graph_method,
            calcIso = camera_params@calcIso,
            calcCiS = camera_params@calcCiS,
            calcCaS = camera_params@calcCaS,
            cor_exp_th = camera_params@cor_eic_th,
            intval = camera_params@intval
        )
        xsa <- CAMERA::findAdducts(
            xsa,
            ppm = camera_params@ppm,
            mzabs = camera_params@mzabs,
            multiplier = camera_params@multiplier,
            polarity = camera_params@polarity,
            rules = camera_params@rules,
            max_peaks = camera_params@max_peaks,
            intval = camera_params@intval
        )
    }))
    xsa
}
