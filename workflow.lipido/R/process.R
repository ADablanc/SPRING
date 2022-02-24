#' @title Execute The step "Process"
#'
#' @description
#' Execute the step "Process" of the workflow.
#' it launch the substeps:
#' \itemize{
#'      \item conversion
#'      \item peakpicking
#'      \item rT correction
#'      \item alignment
#'      \item annotation
#' }
#' Some parameters have a default parameter. This is the case for :
#' \itemize{
#'      \item verboseColumns is TRUE cause we want all the infos
#'          exported by XCMS
#'      \item binSize is 0.1 cause i find it sufficient &
#'          1 is too high to perform a well alignment of the spectras
#'      \item sampleGroups is equal to the length of the provided files
#'          I prefer to apply some rules to filter later & that will allow
#'          the possibility to change this anytime instead of re-run
#'          the entire process
#'      \item minFraction is the minimum integer possible for the same
#'          reason than `sampleGroups`
#'      \item minSamples is at 1, same reason than `sampleGroups`
#'      \item maxFeatures is at 500 cause the workflow will probably
#'          need to deal with some huge datasets
#' }
#' The database used is incorporated by default in the package,
#'     see the function `load_db` for more informations
#'
#' @param raw_files vector of filepaths
#' @param converter filepath to the msconvert executable
#' @param filter_params FilterParam object
#' @param cwt_params CentWaveParam object created by xcms
#' @param obw_params ObiwarpParam object created by xcms
#' @param pd_params PeakDensityParam object created by xcms
#' @param ann_params AnnotationParam object
#' @param cores numeric number of cores to use for the peakpicking
#'      (one core = one file peakpicked)
#' @param show_txt_pb boolean print a progress bar or not on the console
#' @param pb_fct function used to update the progress bar
#'
#' @return XCMSnExp object with two additional slots :
#' \begin{
#'     \itemize `conflicts` list of annotations,
#'                  each item correspond to multiple hypothesis annotation for
#'                  the same isotopic pattern grouped
#'     \itemize `ann` dataframe containing the hypothesis annotations
#' }
#' each hypothesis annotation is represent by a line in the dataframe
#'     (one line = one isotopic profile) with the different items :
#' \begin{
#'     \itemize `name` name of the compound
#'     \itemize `rt` theoretical rT
#'     \itemize `formula` chemical formula
#'     \itemize `adduct` adduct
#'     \itemize `ion_formula` chemical ion formula
#'     \itemize `diff_rt` difference in time retention between observed and
#'                        theoretical
#'     \itemize `nsamples` number of samples were the annotation is found
#'     \itemize `best_score` best isotopic score of the annotation in all the
#'                           samples
#'     \itemize `best_mz_deviation` best m/z deviation of the annotation in all
#'                                  the samples
#'     \itemize `best_npeak` maximum number of isotopologues found for the
#'                           annotation in all the samples
#'     \itemize `group_id` group ID given by xcms
#'     \itemize `mzmed` m/z median of the basepeak in the samples
#'     \itemize `mzmin` m/z born min of the basepeak in the samples
#'     \itemize `mzmax`m/z born max of the basepeak in the samples
#'     \itemize `rtmed` rT median of the basepeak in the samples
#'     \itemize `rtmin` rT born min of the basepeak in the samples
#'     \itemize `rtmax` rT born max of the basepeak in the samples
#'     \itemize ... a column for each samples containing NULL values or a list
#'                      with the different items :
#'                  \begin{
#'                      \itemize `score` the isotopic score for the annotation
#'                                           in the sample
#'                      \itemize `deviation_mz` the m/z deviation for the
#'                                                  annotation in the sample
#'                      \itemize `npeak` the number of isotopologues for the
#'                                           annotation in the sample
#'                      \itemize `spectra` a dataframe which represent the
#'                                             spectra annotated with the
#'                                             columns:
#'                      \begin{
#'                          \itemize `feature_id` the row ID of the feature in
#'                                                    the peaklist of XCMS
#'                          \itemize `rt` the retention time
#'                          \itemize `sample` the name of the sample
#'                          \itemize `mz` the m/z
#'                          \itemize `int` the intensity (area)
#'                          \itemize `abd` the relative abundance
#'                          \itemize `spectra_id_theo` ignore
#'                          \itemize `mz_theo` the theoretical m/z
#'                          \itemize `rt_theo` the theoretical rT
#'                          \itemize `abd_theo` the theoretical relative
#'                                                  abundance
#'                          \itemize `iso_theo` the isotopologue annotation
#'                      }
#'                  }
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' mzxml_files <- c(
#'   system.file("testdata", "200204PLF_QC01_pos_filtered.mzML",
#'     package = "workflow.lipido"
#'   ),
#'   system.file("testdata", "200204PLF_QC02_pos_filtered.mzML",
#'     package = "workflow.lipido"
#'   )
#' )
#' cwt_params <- xcms::CentWaveParam(
#'   ppm = 30,
#'   peakwidth = c(4, 39),
#'   snthresh = 6.5,
#'   prefilter = c(2, 815),
#'   mzdiff = .041,
#'   noise = 0,
#'   firstBaselineCheck = FALSE
#' )
#' obw_params <- xcms::ObiwarpParam()
#' pd_params <- xcms::PeakDensityParam(
#'   bw = 5,
#'   binSize = .010
#' )
#' ms_process(mzxml_files, cwt_params, obw_params, pd_params)
#' }
ms_process <- function(raw_files, sqlite_path, converter, filter_params,
                       cwt_params, obw_params, pd_params, ann_params,
                       cores = parallel::detectCores(), show_txt_pb = TRUE,
                       pb_fct = NULL) {
    sqlite_path <- tryCatch({
    check_ms_process_args(raw_files, sqlite_path, converter, filter_params,
                          cwt_params, obw_params, pd_params, ann_params, cores)
    raw_files <- sapply(raw_files, tools::file_path_as_absolute)
    sqlite_path <- file.path(
        tools::file_path_as_absolute(dirname(sqlite_path)),
        basename(sqlite_path))
    converter <- tools::file_path_as_absolute(converter)
    xcms::verboseColumns(cwt_params) <- TRUE
    xcms::binSize(obw_params) <- .1
    xcms::sampleGroups(pd_params) <- seq(length(raw_files))
    xcms::minFraction(pd_params) <- 10**-9
    xcms::minSamples(pd_params) <- 1
    xcms::maxFeatures(pd_params) <- 500

    if (cores > 1) {
        cl <- parallel::makeCluster(cores)
        parallel::clusterExport(cl, list("sqlite_path", "db_connect",
                                         "dbExecute", "dbWriteTable",
                                         "dbGetQuery", "import_ms_file",
                                         "convert_file", "check_ms_file",
                                         "db_record_ms_file", "compress",
                                         "filter_ms_file", "filter_params",
                                         "db_read_ms_file", "decompress",
                                         "findChromPeaks", "db_get_profile"),
                                envir = pryr::where("sqlite_path"))
        parallel::clusterEvalQ(cl, getClass("xcmsSet", where = "xcms"))
        doSNOW::registerDoSNOW(cl)
        operator <- foreach::"%dopar%"
    } else operator <- foreach::"%do%"

    db <- db_connect(sqlite_path)
    db_record_samples(
        db, unique(gsub("[positive]|[negative]|[pos]|[neg]", "",
            tools::file_path_sans_ext(basename(raw_files)),
            ignore.case = TRUE)))
    RSQLite::dbDisconnect(db)

    if (show_txt_pb)
        pb <- utils::txtProgressBar(min = 0, max = 100, style = 3, title = "")
    if (show_txt_pb & is.null(pb_fct))
        pb_fct <- function(n, total, title)
            utils::setTxtProgressBar(pb, value = (n - 1) / total * 100,
                                     title = title)
    infos <- operator(
        foreach::foreach(
            raw_file = iterators::iter(raw_files),
            .combine = rbind,
            .options.snow = if (is.null(pb_fct))
                                NULL
                            else list(progress = function(n)
                                pb_fct(n, length(raw_files), "Conversion"))),
        {
            db <- db_connect(sqlite_path)
            sample_name <- gsub("[positive]|[negative]|[pos]|[neg]", "",
                                tools::file_path_sans_ext(basename(raw_file)),
                                ignore.case = TRUE)
            msg <- cbind(
                sample = sample_name,
                positive = import_ms_file(raw_file, converter, "positive",
                                          filter_params, obw_params@binSize,
                                          db, sample_name),
                negative = import_ms_file(raw_file, converter, "negative",
                                          filter_params, obw_params@binSize,
                                          db, sample_name))
            RSQLite::dbDisconnect(db)
            msg
        })
    if (!any(c(infos[, c("positive", "negative")]) == "success")) stop(
        "none of the file was imported correctly")

    ann_params_pos <- restrict_adducts_polarity(ann_params, "positive")
    if (any("success" %in% infos[, "positive"]) &
        length(ann_params_pos@adduct_names) > 0) {
        xsets_pos <- ms_process_polarity(sqlite_path,
             infos[infos[, "positive"] == "success", "sample"], "positive",
             cwt_params, obw_params, pd_params, ann_params_pos,
             operator, pb_fct)
        if (class(xsets_pos) != "xcmsSet") stop(xsets_pos)
    } else xsets_pos <- NULL

    ann_params_neg <- restrict_adducts_polarity(ann_params, "negative")
    if (any("success" %in% infos[, "negative"]) &
        length(ann_params_neg@adduct_names) > 0) {
        xsets_neg <- ms_process_polarity(sqlite_path,
             infos[infos[, "negative"] == "success", "sample"], "negative",
             cwt_params, obw_params, pd_params, ann_params_neg,
             operator, pb_fct)
        if (class(xsets_neg) != "xcmsSet") stop(xsets_neg)
    } else xsets_neg <- NULL

    merged_results <- merge_xsets(xsets_pos, xsets_neg)
    db <- db_connect(sqlite_path)
    db_record_xsets(db, merged_results$ann, merged_results$spectras,
                    merged_results$spectra_infos, merged_results$peaks,
                    merged_results$peak_groups)
    db_record_params(db, filter_params, cwt_params, obw_params, pd_params,
                     ann_params)
    RSQLite::dbDisconnect(db)

    sqlite_path
    }, error = function(e) {
        file.remove(sqlite_path)
        e
    })
    if (show_txt_pb) close(pb)
    if (exists("cl")) parallel::stopCluster(cl)
    sqlite_path
}

ms_process_polarity <- function(sqlite_path, samples, polarity,
                                cwt_params, obw_params, pd_params, ann_params,
                                operator, pb_fct) {
    xsets <- operator(
        foreach::foreach(
            sample = iterators::iter(samples),
            .combine = append,
            .options.snow = list(
                progress = function(n) {
                    pb_fct(n, length(samples), "PeakPicking")
                }
            )
        ),
        tryCatch({
            db <- db_connect(sqlite_path)
            ms_file <- db_read_ms_file(db, sample, polarity)
            RSQLite::dbDisconnect(db)
            list(findChromPeaks(ms_file, cwt_params))
        }, error = function(e) {
            e$message
        })
    )
    ids <- !sapply(xsets, is.null)
    samples <- samples[ids]
    xsets <- xsets[ids]
    test_error <- which(sapply(xsets, class) != "xcmsSet")
    if (length(test_error) > 0) {
        stop(paste(unlist(xsets[test_error]), collapse = "\n"))
    }
    pd_params@sampleGroups <- seq(length(unique(samples)))
    annotate_peaklists(
        group_peaks(
            obiwarp(sqlite_path, samples, polarity, xsets, obw_params,
                    operator, pb_fct),
            pd_params,
            operator,
            pb_fct
        ),
        samples,
        ann_params,
        pb_fct
    )
}

merge_xsets <- function(xsets_pos, xsets_neg) {
    feature_id_offset <- 0
    group_id_offset <- 0
    spectra_id_offset <- 0

    if (!is.null(xsets_pos)) {
        ann <- xsets_pos@ann
        spectras <- xsets_pos@spectras
        spectra_infos <- xsets_pos@spectra_infos
        samples <- colnames(ann)[14:ncol(ann)]
        peaks <- data.frame(xsets_pos@peaks)
        peaks <- cbind(feature_id = seq(nrow(peaks)),
                       peaks,
                       polarity = "positive")
        peaks$sample <- samples[peaks$sample]
        colnames(peaks)[which(colnames(peaks) == "into")] <- "int"
        peak_groups <- data.frame(xsets_pos@groups)
        peak_groups <- cbind(group_id = seq(nrow(peak_groups)),
                             polarity = "positive",
                             peak_groups)
        peak_groups[, 10:ncol(peak_groups)] <- xcms::groupval(xsets_pos)
        colnames(peak_groups)[10:ncol(peak_groups)] <- samples
        feature_id_offset <- nrow(peaks)
        group_id_offset <- nrow(peak_groups)
        spectra_id_offset <- nrow(spectra_infos)
    }
    if (!is.null(xsets_neg)) {
        ann_neg <- xsets_neg@ann
        spectras_neg <- xsets_neg@spectras
        spectra_infos_neg <- xsets_neg@spectra_infos
        samples <- colnames(ann_neg)[14:ncol(ann_neg)]
        peaks_neg <- data.frame(xsets_neg@peaks)
        peaks_neg <- cbind(feature_id = seq(nrow(peaks_neg)),
                           peaks_neg,
                           polarity = "negative")
        peaks_neg$sample <- samples[peaks_neg$sample]
        colnames(peaks_neg)[which(colnames(peaks_neg) == "into")] <- "int"
        peak_groups_neg <- data.frame(xsets_neg@groups)
        peak_groups_neg <- cbind(group_id = seq(nrow(peak_groups_neg)),
                                 polarity = "negative",
                                 peak_groups_neg)
        peak_groups_neg[, 10:ncol(peak_groups_neg)] <- xcms::groupval(xsets_neg)
        colnames(peak_groups_neg)[10:ncol(peak_groups_neg)] <- samples

        if (feature_id_offset > 0) {
            ann_neg$group_id <- ann_neg$group_id + group_id_offset
            ann_neg[, 14:ncol(ann_neg)] <- ann_neg[, 14:ncol(ann_neg)] +
                spectra_id_offset
            spectras_neg$spectra_id <- spectras_neg$spectra_id +
                spectra_id_offset
            spectras_neg$feature_id <- spectras_neg$feature_id +
                feature_id_offset
            spectra_infos_neg$spectra_id <- spectra_infos_neg$spectra_id +
                spectra_id_offset
            peaks_neg$feature_id <- peaks_neg$feature_id + feature_id_offset
            peak_groups_neg$group_id <- peak_groups_neg$group_id +
                group_id_offset
            peak_groups_neg[, 10:ncol(peak_groups_neg)] <- peak_groups_neg[
                , 10:ncol(peak_groups_neg)] + feature_id_offset
            ann <- plyr::rbind.fill(ann, ann_neg)
            spectras <- rbind(spectras, spectras_neg)
            spectra_infos <- rbind(spectra_infos, spectra_infos_neg)
            peaks <- rbind(peaks, peaks_neg)
            peak_groups <- plyr::rbind.fill(peak_groups, peak_groups_neg)
        } else {
            ann <- ann_neg
            spectras <- spectras_neg
            spectra_infos <- spectra_infos_neg
            peaks <- peaks_neg
            peak_groups <- peak_groups_neg
        }
    }

    ann <- filtrate_ann(ann, spectra_infos)
    list(
        ann = ann,
        spectras = spectras,
        spectra_infos = spectra_infos,
        peaks = peaks,
        peak_groups = peak_groups
    )
}
