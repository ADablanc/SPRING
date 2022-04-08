#' @title Launch workflow
#'
#' @description
#' Launch the workflow on the raw files, which consist of the steps :
#' \itemize{
#'     \item check the parameters given + creation of the sqlite database where
#'     the result will be recorded
#'     \item convert each raw file in "positive" & "negative" mode with
#'     msConvert. It also trim the file according the rt range and m/z range
#'     specified in the filter_params with msconvert. Check with XCMS package
#'     if the file can be converted. If the file is a CDF it will copy the file
#'     instead. If the conversion failed & the file is a mzML or mzXML it will
#'     copy the file instead and try to trim only the rt range. Record then a
#'     `xcmsRaw` file and its corresponding profile matrix in the database, this
#'      two object are compress into a blob object before inserting in the
#'      database. These object are recorded in their corresponding "polarity"
#'      column (a sample could contain a `xcmsRaw` in positive AND a `xcmsRaw`
#'      in negative !).
#'      \item launch workflow foreach polarity, this workflow consist of the
#'      steps:
#'      \itemize{
#'         \item peak peacking with CentWave algorithm (it will create a list of
#'         `xcmsSet` objects with the `xcmsRaw` loaded in the database)
#'         \item alignment with obiwarp which is based on the complete mz-rt
#'         data
#'         \item group peaklists from a `xcmsSet` object using the density
#'         method
#'         \item annotate peaklists from a `xcmsSet`
#'         it loop through the peaks grouped dataframe
#'         if the peak match with one of the theoretical monoisotopic from
#'             database it will search all isotopologue grouped in the rt window
#'              of the peak +/- fwhm
#'         it compare then the spectra obtained against the theoretical spectra
#'             & compute an isotopic score
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
#'     \item merge the `xcmsSet` in positive polarity & the `xcmsSet` in
#'     negative polarity. For that it will bind all the annotation results from
#'     both & assign new IDs to distingate if the feature, the group or the
#'     spectra was detected in positive or negative mode. Then it will filtrate
#'     the annotation `DataFrame`. Foreach compound annotation it will check if
#'     the same annotations fall in the same rT. For that it will choose a
#'     referenced spectra (the one which as the most adduct forms, then the
#'     number of samples where founded, then the number of isotopologues, then
#'     the less retention time difference & the best isotopic). It will
#'     calculate a retention time window which correspond to the rT of the
#'     referenced spectra +/- fwhm. It will also regroup lines which correspond
#'     to the same annotations but were not grouped by XCMS
#'     \item record all results in the sqlite database
#' }
#'
#' @param raw_files `character vector` filepaths to the raw files
#' @param sqlite_path `character(1)` filepath to the database to create
#' @param converter `character(1)` filepath to the msconvert.exe
#' @param filter_params `FilterParam` object
#' @param cwt_params `CentwaveParam` object
#' @param obw_params `ObiwarpParam` object
#' @param pd_params `PeakdensityParam` object
#' @param ann_params `AnnotationParam` object
#' @param cores `integer(1)` number of cores to use to parallelize process
#' @param show_txt_pb `boolean` should print a progress bar on the console ?
#' @param pb_fct `function` used to update the progress bar. Only give if you
#' intend to use a specific progress bar you created !!!
#'
#' @export
ms_process <- function(raw_files,
                       sqlite_path,
                       converter,
                       filter_params,
                       cwt_params,
                       obw_params,
                       pd_params,
                       ann_params,
                       cores = parallel::detectCores(),
                       show_txt_pb = TRUE,
                       pb_fct = NULL) {
    msg <- tryCatch({
        check_ms_process_args(
            raw_files,
            sqlite_path,
            converter,
            filter_params,
            cwt_params,
            obw_params,
            pd_params,
            ann_params,
            cores
        )
        raw_files <- sapply(raw_files, normalizePath)
        sqlite_path <- suppressWarnings(normalizePath(sqlite_path))
        converter <- normalizePath(converter)
        sample_names <- gsub(
            "(positive)|(negative)|(pos)|(neg)",
            "",
            tools::file_path_sans_ext(basename(raw_files)),
            ignore.case = TRUE
        )
        # order alphabetically raw files & sample names
        raw_files <- raw_files[order(sample_names)]
        sample_names <- sort(unique(sample_names))

        cwt_params@verboseColumns <- TRUE
        obw_params@binSize <- .1
        pd_params@sampleGroups <- seq(length(sample_names))
        pd_params@minFraction <- 10**-9
        pd_params@minSamples <- 1
        pd_params@maxFeatures <- 500

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
                sample_name <- gsub(
                    "(positive)|(negative)|(pos)|(neg)", "",
                    tools::file_path_sans_ext(basename(raw_file)),
                    ignore.case = TRUE
                )
                msg <- cbind(
                    sample = sample_name,
                    positive = import_ms_file(
                        db,
                        sample_name,
                        raw_file,
                        converter,
                        "positive",
                        filter_params
                    ),
                    negative = import_ms_file(
                        db,
                        sample_name,
                        raw_file,
                        converter,
                        "negative",
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
        if (!any(c(infos[, c("positive", "negative")]) == "success")) {
            print(infos)
            stop("none of the file was imported correctly")
        }

        if (!is.null(pb_fct)) {
            pb_fct_pos <- function(n, total, title) {
                pb_fct(n = n, total = total, title = paste(title, "positive"))
            }
        }
        xset_pos <- ms_process_polarity(
            sqlite_path,
            sample_names,
            "positive",
             cwt_params,
            obw_params,
            pd_params,
            ann_params,
            operator,
            pb_fct_pos
        )

        if (!is.null(pb_fct)) {
            pb_fct_neg <- function(n, total, title) {
                pb_fct(n = n, total = total, title = paste(title, "negative"))
            }
        }
        xset_neg <- ms_process_polarity(
            sqlite_path,
            sample_names,
            "negative",
            cwt_params,
            obw_params,
            pd_params,
            ann_params,
            operator,
            pb_fct_neg
        )

        if (!is.null(pb_fct)) {
            pb_fct(n = 1, total = 1, title = "Record results")
        }
        merged_results <- merge_xsets(xset_pos, xset_neg)
        db <- db_connect(sqlite_path)
        db_record_xset(db, xset_pos, xset_neg, infos[1, "sample"])
        db_record_ann(
            db,
            merged_results$ann,
            merged_results$spectras,
            merged_results$spectra_infos,
            merged_results$peaks,
            merged_results$peak_groups
        )
        db_record_params(
            db,
            filter_params,
            cwt_params,
            obw_params,
            pd_params,
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

#' @title Launch workflow for a polarity
#'
#' @description
#' Launch the workflow for only a unique polarity on xcmsRaws recorded on
#' a sqlite database
#' it will launch the workflow which consists in 4 steps :
#' \itemize{
#'     \item peak peacking with CentWave algorithm (it will create a list of
#'     `xcmsSet` objects with the `xcmsRaw` loaded in the database)
#'     \item alignment with obiwarp which is based on the complete mz-rt data
#'     \item group peaklists from a `xcmsSet` object using the density method
#'     \item annotate peaklists from a `xcmsSet`
#'     it loop through the peaks grouped dataframe
#'     if the peak match with one of the theoretical monoisotopic from database
#'         it will search all isotopologue grouped in the rt window of the peak
#'         +/- fwhm
#'     it compare then the spectra obtained against the theoretical spectra
#'         & compute an isotopic score
#'     The scoring algorithm will search each corresponding observed peak with
#'         theoreticals. Therefore it contains some important rules :
#'     \itemize{
#'          \item an observed peak can only correspond to ONE theoretical peak
#'           and vice versa
#'          \item the relative abundance peak must not be under a tolerance
#'          compared to the theoretical
#'          but it can be higher since a peak can hide another
#'          \item the A+x is not searched if the A+x-1 is not found
#'          (the loop search is stopped)
#'     }
#' }
#'
#' @param sqlite_path `SQLiteConnection`
#' @param samples `character vector` sample names in database
#' @param polarity `character(1)` muste be "negative" or "positive", used to
#' load the corresponding `xcmsRaw` & profile matrix
#' @param cwt_params `CentwaveParam` object
#' @param obw_params `ObiwarpParam` object
#' @param pd_params `PeakdensityParam` object
#' @param ann_params `AnnotationParam` object
#' @param operator `function` to use for parallelization (`\%dopar\%`)
#'      or not (`\%do\%`)
#' @param pb_fct `function` used to update the progress bar
#'
#' @return `xcmsSet` with three additional slots :
#' \itemize{
#'     \item ann `DataFrame` each line represent an hypothesis annotation
#'     it contains the columns :
#'     \itemize{
#'         \item group_id `integer` group ID
#'         \item name `character` name
#'         \item formula `character` chemical formula
#'         \item adduct `character` adduct form
#'         \item ion_formula `character` ion chemical formula
#'         \item rtdiff `numeric` retention time difference between the measured
#'          & the expected
#'         \item rt `numeric` retention time measured meanned accross the
#'         samples
#'         \item rtmin `numeric` born min of retention time measured accross the
#'         samples
#'         \item rtmax `numeric` born max of the retention time measured accross
#'          the samples
#'         \item nsamples `integer` number of samples where the compound was
#'         found
#'         \item best_score `numeric` best isotopic score seen
#'         \item best_deviation_mz `numeric` best m/z deviation seen
#'         \item best_npeak `integer` best number of isotopologues found
#'         \item ... `integer` a column for each sample which contain the
#'         spectra ID
#'     }
#'     \item spectras `DataFrame`, each line correspond to a peak annotated with
#'      its corresponding theoretical peak or the theoretical peak missed,
#'      with the columns :
#'     \itemize{
#'         \item spectra_id `integer` spectra ID
#'         \item feature_id `integer` feature ID
#'         \item mz `numeric` m/z
#'         \item int `numeric` area integrated
#'         \item abd `numeric` relative abundance
#'         \item ion_id_theo `integer` ignore
#'         \item mz_theo `numeric` theoretical m/z
#'         \item abd_theo `numeric` theoretical relative abundance
#'         \item iso_theo `character` theoretical isotopologue annotation
#'     }
#'     \item spectra_infos `DataFrame`, each line correspond to a spectra
#'     annotated, with the columns :
#'     \itemize{
#'         \item spectra_id `integer` spectra ID
#'         \item score `numeric` isotopic score observed
#'         \item deviation_mz `numeric` m/z deviation observed
#'         \item npeak `integer` number of isotopologue annotated
#'         \item basepeak_int `numeric` area of the basepeak annotated
#'         \item sum_int `numeric` cumulative sum off all the area of the
#'         isotopologues annotated
#'         \item rt `numeric` retention time
#'     }
#' }
ms_process_polarity <- function(sqlite_path,
                                samples,
                                polarity,
                                cwt_params,
                                obw_params,
                                pd_params,
                                ann_params,
                                operator = foreach::"%do%",
                                pb_fct = NULL) {
    if (!is.null(pb_fct)) {
        pb_fct(n = 0, total = 1, title = "PeakPicking")
    }
    xsets <- operator(
        foreach::foreach(
            sample = iterators::iter(samples),
            .combine = append,
            .options.snow = list(
                progress = if (is.null(pb_fct)) {
                    NULL
                } else {
                    function(n) {
                        pb_fct(n, length(samples), "PeakPicking")
                    }
                }
            )
        ),
        tryCatch({
            db <- db_connect(sqlite_path)
            ms_file <- db_read_ms_file(db, sample, polarity)
            RSQLite::dbDisconnect(db)
            list(find_chrompeaks(ms_file, cwt_params, sample))
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
    pd_params@sampleGroups <- seq(length(unique(samples)))
    ann_params <- restrict_adducts_polarity(ann_params, polarity)
    xset <- obiwarp(
        sqlite_path,
        samples,
        polarity,
        xsets,
        obw_params,
        operator,
        pb_fct
    )
    xset <- group_peaks(xset, pd_params, operator, pb_fct)
    xset <- annotate_peaklists(xset, ann_params, pb_fct)
}

#' @title Merge `xcmsSets`
#'
#' @description
#' Merge the `xcmsSet` in positive polarity & the `xcmsSet` in negative polarity
#' For that it will bind all the annotation results from both & assign new IDs
#' to distingate if the feature, the group or the spectra was detected
#' in positive or negative mode
#' Then it will filtrate the annotation `DataFrame`:
#' Foreach compound annotation it will check if
#'      the same annotations fall in the same rT
#' For that it will choose a referenced spectra
#'      (the one which as the most adduct forms, then the number of samples
#'      where founded, then the number of isotopologues, then the less
#'      retention time difference & the best isotopic)
#' It will calculate a retention time window which correspond to
#'      the rT of the referenced spectra +/- fwhm
#' It will also regroup lines which correspond
#'      to the same annotations but were not grouped by XCMS
#'
#' @param xset_pos `xcmSet` in positive mode
#' @param xset_neg `xcmSet` in negative mode
#'
#' @return `list` with items:
#' \itemize{
#'     \item ann `DataFrame` each line correspond to a compound found
#'     with the columns:
#'     \itemize{
#'         \item group_id `integer` group ID
#'         \item name `character` name
#'         \item formula `character` chemical formula
#'         \item adduct `character` adduct form
#'         \item ion_formula `character` ion chemical formula
#'         \item rtdiff `numeric` retention time difference between the measured
#'          & the expected
#'         \item rt `numeric` retention time measured meanned accross the
#'         samples
#'         \item rtmin `numeric` born min of retention time measured accross the
#'         samples
#'         \item rtmax `numeric` born max of the retention time measured accross
#'         the samples
#'         \item nsamples `integer` number of samples where the compound was
#'         found
#'         \item best_score `numeric` best isotopic score seen
#'         \item best_deviation_mz `numeric` best m/z deviation seen
#'         \item best_npeak `integer` best number of isotopologues found
#'         \item ... `integer` a column for each sample which contain the
#'         spectra ID
#'     }
#'     \item spectras `DataFrame`, each line correspond to a peak annotated with
#'     its corresponding theoretical peak or the theoretical peak missed,
#'     with the columns :
#'     \itemize{
#'         \item spectra_id `integer` spectra ID
#'         \item feature_id `integer` feature ID
#'         \item mz `numeric` m/z
#'         \item int `numeric` area integrated
#'         \item abd `numeric` relative abundance
#'         \item ion_id_theo `integer` ignore
#'         \item mz_theo `numeric` theoretical m/z
#'         \item abd_theo `numeric` theoretical relative abundance
#'         \item iso_theo `character` theoretical isotopologue annotation
#'     }
#'     \item spectra_infos `DataFrame`, each line correspond to a spectra
#'     annotated, with the columns :
#'     \itemize{
#'         \item spectra_id `integer` spectra ID
#'         \item score `numeric` isotopic score observed
#'         \item deviation_mz `numeric` m/z deviation observed
#'         \item npeak `integer` number of isotopologue annotated
#'         \item basepeak_int `numeric` area of the basepeak annotated
#'         \item sum_int `numeric` cumulative sum off all the area of the
#'         isotopologues annotated
#'         \item rt `numeric` retention time
#'     }
#'     \item peaks `DataFrame` obtained from XCMS, it contains all peaks peak
#'     picked, it contains the columns :
#'     \itemize{
#'         \item feature_id `integer` feature ID
#'         \item mz `numeric` m/z
#'         \item mz_min `numeric` m/z born min
#'         \item mz_max `numeric` m/z born max
#'         \item rt `numeric` retention time
#'         \item rtmin `numeric` retention time born min
#'         \item rtmax `numeric` retention time born max
#'         \item int `numeric` area integrated
#'         \item intb `numeric` area integrated with baseline substracted
#'         \item maxo `numeric` maximum of intensity
#'         \item sn `numeric` signal / noise
#'         \item egauss `numeric` ignore
#'         \item mu `numeric` ignore
#'         \item sigma `numeric` ignore,
#'         \item h `numeric` ignore
#'         \item f `integer` ID of the ROI in the ROI list constructed by XCMS,
#'         no use
#'         \item dppm `numeric` m/z deviation of the peak in ppm
#'         \item scale `integer` centwave scale used for the integration
#'         (in scans & not in sec !!!)
#'         \item scpos `integer` scan position
#'         \item scmin `integer` scan born min before the optimization of the
#'         area integrated
#'         \item scmax `integer` scan born max before the optimization of the
#'         area integrated
#'         \item lmin `integer` scan born min after the optimization of the area
#'         integrated
#'         \item lmax `integer` scan born max after the optimization of the area
#'         integrated
#'         \item sample `character` sample name
#'         \item polarity `character` polarity
#'     }
#'     \item peak_groups `DataFrame`, each line correspond to a group from XCMS,
#'     the columns are :
#'     \itemize{
#'         \item group_id `integer` group ID
#'         \item polarity `character` polarity
#'         \item mzmed `numeric` m/z median
#'         \item mzmin `numeric` m/z born min
#'         (not the mzmin column from the peaklist !)
#'         \item mzmax `numeric` m/z born max
#'         (not the mzmax column from the peaklist !)
#'         \item rtmed `numeric` rt median
#'         \item rtmin `numeric` rt born min
#'         (not the rtmin column from the peaklist !)
#'         \item rtmax `numeric` rt born max
#'         (not the rtmax column from the peaklist !)
#'         \item npeaks `integer` number of peaks grouped
#'         \item ... `integer` a column for each sample which contain the
#'         feature ID
#'     }
#' }
merge_xsets <- function(xset_pos, xset_neg) {
    ann <- xset_pos@ann
    spectras <- xset_pos@spectras
    spectra_infos <- xset_pos@spectra_infos
    samples <- colnames(ann)[14:ncol(ann)]
    peaks <- data.frame(xset_pos@peaks)
    colnames(peaks)[which(colnames(peaks) == "into")] <- "int"
    peak_groups <- data.frame(xset_pos@groups)
    colnames(peak_groups)[8:ncol(peak_groups)] <- samples

    if (nrow(peaks) > 0) {
        peaks <- cbind(
            feature_id = seq(nrow(peaks)),
            peaks,
            polarity = "positive"
        )
        peaks$sample <- samples[peaks$sample]
        peak_groups <- cbind(
            group_id = seq(nrow(peak_groups)),
            polarity = "positive",
            peak_groups
        )
        peak_groups[, 10:ncol(peak_groups)] <- xcms::groupval(xset_pos)
    } else {
        peaks <- cbind(
            feature_id = logical(0),
            peaks,
            polarity = logical(0)
        )
        peak_groups <- cbind(
            group_id = logical(0),
            polarity = logical(0),
            peak_groups
        )
    }

    feature_id_offset <- nrow(peaks)
    group_id_offset <- nrow(peak_groups)
    spectra_id_offset <- nrow(spectra_infos)

    ann_neg <- xset_neg@ann
    spectras_neg <- xset_neg@spectras
    spectra_infos_neg <- xset_neg@spectra_infos
    samples <- colnames(ann_neg)[14:ncol(ann_neg)]
    peaks_neg <- data.frame(xset_neg@peaks)
    colnames(peaks_neg)[which(colnames(peaks_neg) == "into")] <- "int"
    peak_groups_neg <- data.frame(xset_neg@groups)
    colnames(peak_groups_neg)[8:ncol(peak_groups_neg)] <- samples

    if (nrow(peaks_neg) > 0) {
        peaks_neg <- cbind(
            feature_id = seq(nrow(peaks_neg)) + feature_id_offset,
            peaks_neg,
            polarity = "negative"
        )
        peaks_neg$sample <- samples[peaks_neg$sample]
        peak_groups_neg <- cbind(
            group_id = seq(nrow(peak_groups_neg)) + group_id_offset,
            polarity = "negative",
            peak_groups_neg
        )
        peak_groups_neg[, 10:ncol(peak_groups_neg)] <- xcms::groupval(
            xset_neg) + feature_id_offset
    } else {
        peaks_neg <- cbind(
            feature_id = logical(0),
            peaks_neg,
            polarity = logical(0)
        )
        peak_groups_neg <- cbind(
            group_id = logical(0),
            polarity = logical(0),
            peak_groups_neg
        )
    }

    if (nrow(ann_neg) > 0) {
        ann_neg$group_id <- ann_neg$group_id + group_id_offset
        ann_neg[, 14:ncol(ann_neg)] <- ann_neg[, 14:ncol(ann_neg)] +
            spectra_id_offset
        spectras_neg$spectra_id <- spectras_neg$spectra_id +
            spectra_id_offset
        spectras_neg$feature_id <- spectras_neg$feature_id +
            feature_id_offset
        spectra_infos_neg$spectra_id <- spectra_infos_neg$spectra_id +
            spectra_id_offset
    }

    ann <- rbind(ann, ann_neg)
    spectras <- rbind(spectras, spectras_neg)
    spectra_infos <- rbind(spectra_infos, spectra_infos_neg)
    peaks <- rbind(peaks, peaks_neg)
    peak_groups <- rbind(peak_groups, peak_groups_neg)

    ann <- filtrate_ann(ann, spectra_infos)
    ann <- ann[order(ann$group_id), ]
    rownames(ann) <- NULL
    list(
        ann = ann,
        spectras = spectras,
        spectra_infos = spectra_infos,
        peaks = peaks,
        peak_groups = peak_groups
    )
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
#' @param polarity `character(1)` polarity to filter annotations, not mandatory
#'
#' @export
export_annotations <- function(sqlite_path, excel_path, polarity = "both") {
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
    ann <- db_get_annotations(db, polarity = polarity)
    if (nrow(ann) == 0) {
        stop("no annotations in database")
    }
    ann <- split_conflicts(ann)$no_conflicts
    if (nrow(ann) == 0) {
        stop("no annotations with 0 conflicts")
    }
    nsamples <- db_get_nsamples(db)
    spectra_ids <- without_na(unlist(
        ann[, (ncol(ann) - nsamples - 1):ncol(ann)]))
    spectra_infos <- db_get_spectra_infos(db, spectra_ids)

    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Summary")
    openxlsx::addWorksheet(wb, "Details")
    openxlsx::writeDataTable(
        wb,
        "Summary",
        summarise_ann(ann, spectra_infos)
    )
    openxlsx::writeDataTable(
        wb,
        "Details",
        get_int_ann(ann, spectra_infos)
    )
    openxlsx::saveWorkbook(wb, excel_path, overwrite = TRUE)
}
