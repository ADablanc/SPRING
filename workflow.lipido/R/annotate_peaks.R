#' @title Annotate peaklists
#' 
#' @description 
#' Annotate peaklists from an XCMSnExp with grouping information
#' it loop through the peaks grouped dataframe
#' if the peak match with one of the theoretical monoisotopic from database 
#'      it will search all isotopologue grouped in 
#'      the rt window of the peak +/- fwhm
#' it compare then the spectra obtained against the theoretical spectra 
#'      & compute an isotopic score
#' at the end it will merge rows which corresponds to the same annotation but 
#'      were not grouped together by XCMS (parameters not right ?)
#' 
#' @param ms_files XCMSnExp
#' @param ann_params AnnotationParameter object
#' @param show_pb boolean print progression bar
#' @param sigma ignore
#' @param perfwhm ignore
#' 
#' @return a XCMSnExp with a new slot `ann` which contains a dataframe
annotate_peaklists <- function(ms_files, ann_params, show_pb, 
        sigma = 6, perfwhm = .6) {
    db <- load_db(ann_params@adduct_names, ann_params@instrument)
    # the rt in database is in min !!
    db$rt <- db$rt * 60
    l_spectras <- unique(db[, c("spectra_id", "mz", "abd", "iso")])
    l_spectras <- split(l_spectras, l_spectras$spectra_id)
    db <- unique(db[db$abd == 100, 
        c("name", "formula", "adduct", "ion_formula", 
            "mz", "rt", "spectra_id")])
    db_spectras <- unique(db[, c("spectra_id", "mz", "rt"), drop = FALSE])
    
    # add column "feature_id" without dropping alignment
    peaks <- xcms::chromPeaks(ms_files)
    peaks <- cbind(feature_id = seq(nrow(peaks)), peaks)
    feature_definitions <- xcms::featureDefinitions(ms_files)
    xcms::chromPeaks(ms_files) <- peaks
    xcms::featureDefinitions(ms_files) <- feature_definitions
    
    peaks_grouped <- xcms::featureDefinitions(ms_files)
    peaks_int <- xcms::featureValues(ms_files, missing = 0)
    peaks <- cbind(
        peaks_grouped[, c("peakidx", "mzmed", "mzmin", "mzmax", "rtmed", 
            "rtmin", "rtmax")], 
        peaks_int)
    peaks <- cbind(group_id = seq(nrow(peaks)), peaks)
    feature_ids <- xcms::featureValues(ms_files, value = "feature_id")
    sample_names <- tools::file_path_sans_ext(basename(
        MSnbase::fileNames(ms_files)))
    
    if (show_pb) pb <- utils::txtProgressBar(min = 0, 
        max = nrow(peaks), style = 3)
    ann <- data.frame()
    for (i in seq(nrow(peaks))) {
        if (show_pb) utils::setTxtProgressBar(pb, i)
        # unique is used because a spectra id represent 
            # an ion formula of multiple possible compounds 
            # which can be at different rT
        spectra_ids <- unique(db_spectras[which(
            db_spectras$mz >= peaks[i, "mzmed"] - ann_params@da_tol & 
            db_spectras$mz <= peaks[i, "mzmed"] + ann_params@da_tol & 
            db_spectras$rt >= peaks[i, "rtmed"] - ann_params@rt_tol & 
            db_spectras$rt <= peaks[i, "rtmed"] + ann_params@rt_tol), 
            "spectra_id"])
        if (length(spectra_ids) == 0) next
        
        fwhm <- abs(peaks[i, "rtmax"] - peaks[i, "rtmin"]) / 
            sigma * 2.35 * perfwhm
        rows <- peaks$mzmed >= peaks[i, "mzmed"] - 6 &  
                peaks$mzmed <= peaks[i, "mzmed"] + 6 & 
                peaks$rtmed >= peaks[i, "rtmed"] - fwhm & 
                peaks$rtmed <= peaks[i, "rtmed"] + fwhm
        cluster <- peaks[rows, , drop = FALSE]
        cluster_feature_ids <- feature_ids[rows, , drop = FALSE]
        # idx represent the columns (samples) not empty
        idx <- which(unlist(peaks[i, 9:ncol(peaks)]) > 0) + 8
        q_spectras <- lapply(seq(length(idx)), function(j)
            data.frame(
                feature_id = cluster_feature_ids[
                    cluster[, idx[j]] > 0, idx[j] - 8], 
                rt = cluster[cluster[, idx[j]] > 0, "rtmed"],
                sample = sample_names[idx[j] - 8], 
                mz = cluster[cluster[, idx[j]] > 0, "mzmed"],
                int = cluster[cluster[, idx[j]] > 0, idx[j]], 
                abd = cluster[cluster[, idx[j]] > 0, idx[j]] / 
                    peaks[i, idx[j]] * 100
            ))
                
        tmp <- compare_spectras(q_spectras, 
            l_spectras[spectra_ids], 
            ann_params@da_tol, 
            ann_params@abd_tol)
            
        for (j in seq(length(tmp))) {
            # idx_2 represent the columns (samples) not empty & 
                # with a correct score
            idx_2 <- which(sapply(tmp[[j]], function(x) 
                x$score > 0 & x$npeak > 0))
            if (length(idx_2) == 0) next
            tmp_spectras <- tmp[[j]][idx_2]
            ann_tmp <- peaks[i, -2]
            ann_tmp[, 8:ncol(ann_tmp)] <- NA
            # idx - 1 cause we delete one column 
            ann_tmp[, idx[idx_2] - 1] <- t(data.frame(a = I(tmp_spectras)))
            ann_tmp <- cbind(
                diff_rt = Inf, 
                nadducts = 1, 
                nsamples = length(tmp_spectras), 
                best_score = max(sapply(tmp_spectras, 
                    function(x) x$score)), 
                best_mz_deviation = max(sapply(tmp_spectras, 
                    function(x) x$deviation_mz)), 
                best_npeak = max(sapply(tmp_spectras, 
                    function(x) x$npeak)),  
                ann_tmp)
            hypo_candidate <- db[db$spectra_id == spectra_ids[j], 
                , drop = FALSE]
            hypo_candidate <- hypo_candidate[
                abs(hypo_candidate$rt - peaks[i, "rtmed"]) <= 
                    ann_params@rt_tol, , drop = FALSE]
            # if multiple hypothesis lipid at same rT, duplicate
            ann_tmp <- cbind(hypo_candidate[, 
                    c("name", "rt", "formula", "adduct", "ion_formula"), 
                    drop = FALSE], 
                rep(ann_tmp, nrow(hypo_candidate)))
            ann_tmp[, "diff_rt"] <- abs(ann_tmp[["rtmed"]] - ann_tmp[["rt"]])
            ann <- rbind(ann, ann_tmp)
        }
    }
    if (show_pb) close(pb)
    ann <- filtrate_ann(ann)
    attributes(ms_files)$ann <- ann
    return(ms_files)
}

#' @title Filtrate annotation dataframe
#'
#' @description
#' Filtrate annotation dataframe
#' Foreach compound annotation it will check if 
#'      the same annotations fall in the same rT
#' For that it will choose a referenced spectra 
#'      (the one which as the most isotopologue & 
#'          the best isotopic score & the less retention time difference)
#' It will calculate a retention time window which correspond to 
#'      the rT of the referenced spectra +/- fwhm
#' It will also regroup lines which correspond 
#'      to the same annotations but were not grouped by XCMS
#' 
#' @param ann the annotation dataframe obtained 
#'      from the function `annotate_peaklists`
#' @param sigma ignore
#' @param perfwhm ignore
#' 
#' @return the annotation dataframe filtered & regrouped
filtrate_ann <- function(ann, sigma = 6, perfwhm = .6) {
    do.call(rbind, lapply(split(ann, ann$name), function(x) {
        if (nrow(x) == 1) return(x)
        best_peak <- x[order(
            -x$best_npeak, 
            -x$best_score, 
            x$diff_rt, 
            x$best_mz_deviation)[1], , drop = FALSE]
        # eject the annotations where the same compound is not at the same rT
        fwhm <- (abs(best_peak$rtmax - best_peak$rtmin) / 
            sigma * 2.35 * perfwhm)
        x <- x[x$rtmed >= best_peak$rtmed - fwhm & 
            x$rtmed <= best_peak$rtmed + fwhm, , drop = FALSE]
        # merge rows where the alignment fail    
        x <- do.call(rbind, lapply(split(x, x$adduct), function(y) {
            if (nrow(y) == 1) return(y)
            new_y <- y[which.max(y$best_score), , drop = FALSE]
            new_y$diff_rt <- mean(y$diff_rt)
            new_y$best_score <- max(y$best_score)
            new_y$best_mz_deviation <- max(y$best_mz_deviation)
            new_y$best_npeak <- max(y$best_npeak)
            suppressWarnings(new_y[, 19:ncol(y)] <- t(data.frame(a = I(
                lapply(y[, 19:ncol(y)], function(z) {
                    id <- which(!is.na(z))
                    if (length(id) == 0) NA
                    else if (length(id) == 1) z[id]
                    else z[id][which.max(sapply(z[id], function(z_) z_$score))]
                })))))
            new_y$nsamples <- sum(!is.na(new_y[, 19:ncol(y)]))
            new_y
        }))
        x$nadducts <- nrow(x)
        x
    }))
}

#' @title Resolve annotations conflicts
#' 
#' @description
#' Resolve annotations conflicts which corresponds to multiple annotations 
#'      for the same group of peaks
#' it will priorize those which were found in the maximum of sample 
#'      then the annotations with the maximum of different adducts
#'      then those with the minimum of retention time compared to theoretical
#'      finally by the isotopic score
#'
#' @param ann the annotation dataframe obtained 
#'      from the function `annotate_peaklists`
#' 
#' @return the annotation dataframe with all annotations conflicts solved
resolve_conflicts <- function(ann) {
    do.call(rbind, lapply(split(ann, ann$group_id), function(x) 
    if (nrow(x) == 1) x
        else x[order(-x$nsamples, -x$nadducts, x$diff_rt, 
            -x$best_score)[1], , drop = FALSE]
    ))
}

#' @title Resolve annotations conflicts
#' 
#' @description
#' Summarise the annotations dataframe by compound instead by ion
#' it will return in the column samples the intensity of 
#'      the monoisotopic measured for the best adduct form
#' The best adduct form is the one which was found in the maximum of samples & 
#'      the minimum of retention time & the best isotopic score
#'
#' @param ann the annotation dataframe obtained 
#'      from the function `annotate_peaklists`
#' 
#' @return the annotation dataframe grouped by compound
summarise_ann <- function(ann) {
    do.call(rbind, lapply(split(ann, ann$name), function(x) 
        cbind.data.frame(
            name = x[1, "name"], 
            rt = mean(x$rtmed), 
            diff_rt = min(x$diff_rt), 
            adducts = paste(x$adduct, collapse = " "), 
            best_score = max(x$best_score), 
            best_mz_deviation = min(x$best_mz_deviation), 
            best_npeak = max(x$best_npeak), 
            ` ` = "", 
            adduct = x[order(-x$nsamples, -x$nadducts, x$diff_rt, 
                -x$best_score)[1], "adduct", drop = FALSE], 
            do.call(cbind, lapply(
                x[order(-x$nsamples, x$diff_rt, -x$best_score)[1], 19:ncol(x)], 
                    function(y) 
                        if (length(y[[1]]) <= 1) NA
                        else y[[1]]$spectra[
                            which(y[[1]]$spectra$iso_theo == "M"), "int"]))
        )
    ))
}
