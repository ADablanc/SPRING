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
#'     with XCMS package if the file can be converted. If the file is a CDF it
#'     will copy the file instead. If the conversion failed & the file is a mzML
#'      or mzXML it will copy the file instead and try to trim only the rt
#'     range. Record then a `xcmsRaw` file and its corresponding profile matrix
#'     in the database, this object is compress into a blob object before
#'     inserting in the database.
#'      \item launch workflow foreach polarity, this workflow consist of the
#'      steps:
#'      \itemize{
#'         \item peak peacking with CentWave algorithm (it will create a list of
#'         `xcmsSet` objects with the `xcmsRaw` loaded in the database)
#'         \item alignment with obiwarp which is based on the complete mz-rt
#'         data
#'         \item group peaklists from a `xcmsSet` object using the density
#'         method
#'         \item annotate isotopologues & adducts with the CAMERA package
#'         \item annotate peaklists from a `xsAnnotate` object from CAMERA
#'         it loop through the pcgroups
#'         if one of the peak match with one of the theoretical monoisotopic
#'             from database it will compare the pseudo spectra obtained from
#'             CAMERA against the theoretical spectra & compute an isotopic
#'             score
#'         The scoring algorithm will search each corresponding observed peak
#'         with theoreticals. Therefore it contains some important rules :
#'         \itemize{
#'              \item an observed peak can only correspond to ONE theoretical
#'              peak and vice versa
#'              \item the relative abundance peak must not be under a tolerance
#'              compared to the theoretical
#'              but it can be higher since a peak can hide another
#'              \item the A+x is not searched if the A+x-1 is not found
#'              (the loop search is stopped)
#'         }
#'     }
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
    msg <- tryCatch({
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
        sample_names <- tools::file_path_sans_ext(basename(raw_files))

        # order alphabetically raw files & sample names
        raw_files <- raw_files[order(sample_names)]
        sample_names <- sort(unique(sample_names))

        cwt_params@verboseColumns <- TRUE
        obw_params@binSize <- .1
        pd_params@sampleGroups <- seq(length(unique(sample_names)))
        pd_params@minFraction <- 10**-9
        pd_params@minSamples <- 1
        pd_params@maxFeatures <- 500
        filter_params <- FilterParam(cwt_params, ann_params)

        if (cores > 1) {
            cl <- parallel::makeCluster(cores)
            parallel::clusterExport(
                cl,
                list("sqlite_path", "db_connect", "dbExecute", "dbWriteTable",
                     "dbGetQuery", "import_ms_file", "convert_file",
                     "check_ms_file", "db_record_ms_file", "compress",
                     "filter_ms_file", "filter_params", "db_read_ms_file",
                     "decompress", "find_chrompeaks"),
                envir = pryr::where("sqlite_path")
            )
            doSNOW::registerDoSNOW(cl)
            operator <- foreach::"%dopar%"
        } else {
            operator <- foreach::"%do%"
        }

        db <- db_connect(sqlite_path)
        db_record_samples(db, sample_names)
        RSQLite::dbDisconnect(db)

        if (show_txt_pb) {
            pb <- utils::txtProgressBar(
                min = 0,
                max = 100,
                style = 3,
                title = ""
            )
            if (is.null(pb_fct)) {
                pb_fct <- function(n, total, title) {
                    utils::setTxtProgressBar(
                        pb,
                        value = (n - 1) / total * 100,
                        title = title
                    )
                }
            }
        }
        raw_file <- NULL # just to get rid of the NOTE
        if (!is.null(pb_fct)) {
            pb_fct(n = 0, total = length(raw_files), title = "Conversion")
        }
        ("Conversion")
        infos <- operator(
            foreach::foreach(
                raw_file = iterators::iter(raw_files),
                .combine = rbind,
                .options.snow = list(
                    progress = if (is.null(pb_fct)) {
                        NULL
                    } else {
                        function(n) {
                            pb_fct(n, length(raw_files), "Conversion")
                        }
                    }
                )
            ), {
                db <- db_connect(sqlite_path)
                sample_name <- tools::file_path_sans_ext(basename(raw_file))
                msg <- cbind(
                    sample = sample_name,
                    imported = import_ms_file(
                        db,
                        sample_name,
                        raw_file,
                        converter,
                        filter_params
                    )
                )
                RSQLite::dbDisconnect(db)
                msg
            }
        )
        if (!is.null(pb_fct)) {
            pb_fct(n = 1, total = 1, title = "Conversion")
        }
        if (any(infos[, "imported"] != "success")) {
            print(infos)
            stop("some of the file was not imported correctly")
        }

        if (!is.null(pb_fct)) {
            pb_fct(n = 0, total = 1, title = "PeakPicking")
        }
        print("Peak picking")
        xsets <- operator(
            foreach::foreach(
                sample_name = iterators::iter(sample_names),
                .combine = append,
                .options.snow = list(
                    progress = if (is.null(pb_fct)) {
                        NULL
                    } else {
                        function(n) {
                            pb_fct(n, length(sample_names), "PeakPicking")
                        }
                    }
                )
            ),
            tryCatch({
                db <- db_connect(sqlite_path)
                ms_file <- db_read_ms_file(db, sample_name)
                RSQLite::dbDisconnect(db)
                list(find_chrompeaks(ms_file, cwt_params, sample_name))
            }, error = function(e) {
                e$message
            })
        )
        if (!is.null(pb_fct)) {
            pb_fct(n = 1, total = 1, title = "PeakPicking")
        }
        test_error <- which(sapply(xsets, class) != "xcmsSet")
        if (length(test_error) > 0) {
            stop(paste(unlist(xsets[test_error]), collapse = "\n"))
        }

        print("Obiwarp")
        xset <- obiwarp(
            sqlite_path,
            sample_names,
            xsets,
            obw_params,
            operator,
            pb_fct
        )

        print("Alignment")
        xset <- group_peaks(xset, pd_params, operator, pb_fct)

        print("Group")
        invisible(utils::capture.output(xsa <- CAMERA::annotate(
            xset,
            sigma = camera_params@sigma,
            perfwhm = camera_params@perfwhm,
            cor_eic_th = camera_params@cor_eic_th,
            graphMethod = camera_params@graphMethod,
            pval = camera_params@pval,
            calcCiS = camera_params@calcCiS,
            calcIso = camera_params@calcIso,
            calcCaS = camera_params@calcCaS,
            maxcharge = camera_params@maxcharge,
            maxiso = camera_params@maxiso,
            minfrac = camera_params@minfrac,
            ppm = camera_params@ppm,
            mzabs = camera_params@mzabs,
            quick = FALSE,
            rules = camera_params@rules,
            polarity = camera_params@polarity,
            multiplier = camera_params@multiplier,
            max_peaks = camera_params@max_peaks,
            intval = camera_params@intval
        )))

        print("Annotate")
        xsa <- annotate_pcgroups(xsa, ann_params, pb_fct)

        if (!is.null(pb_fct)) {
            pb_fct(n = 1, total = 1, title = "Record results")
        }
        db <- db_connect(sqlite_path)
        db_record_xsa(db, xsa, sample_names[1])
        db_record_ann(
            db,
            xsa@ann,
            xsa@spectras,
            xsa@spectra_infos
        )
        db_record_params(
            db,
            filter_params,
            cwt_params,
            obw_params,
            pd_params,
            camera_params,
            ann_params
        )
        RSQLite::dbDisconnect(db)
        if (show_txt_pb) {
            close(pb)
        }
        if (exists("cl")) {
            parallel::stopCluster(cl)
        }
        NULL
    }, error = function(e) {
        try(suppressWarnings(file.remove(sqlite_path)))
        if (exists("pb")) {
            close(pb)
        }
        if (exists("cl")) {
            parallel::stopCluster(cl)
        }
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
#' Warning ! It export only the annotations with no conflicts !
#' (Conflicts are when for a group of peaks multiple annotations are possible
#' (it happens often when for an ion formula refers to multiple compounds))
#'
#' @param sqlite_path `character(1)` sqlite path to the annotation results
#' @param excel_path `character(1)` path to the excel file to create
#'
#' @export
export_annotations <- function(sqlite_path, excel_path) {
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

    summarised_ann <- summarise_ann(ann, spectra_infos, nsamples)
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
