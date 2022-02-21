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
annotate_peaklists <- function(xsets, samples, ann_params,
                               pb_fct = NULL,
                               sigma = 6, perfwhm = .6) {
    db <- load_db(ann_params@adduct_names, ann_params@instrument)
    # the rt in database is in min !!
    db$rt <- db$rt * 60
    l_spectras <- unique(db[, c("ion_id", "mz", "abd", "iso")])
    l_spectras <- split(l_spectras, l_spectras$ion_id)
    db <- unique(db[db$abd == 100,
                    c("name", "formula", "adduct", "ion_formula",
                      "mz", "rt", "ion_id")])

    peaks <- data.frame(xsets@peaks)
    peaks <- cbind(feature_id = seq(nrow(peaks)), peaks)
    colnames(peaks)[which(colnames(peaks) == "into")] <- "int"
    peak_groups <- data.frame(xsets@groups)
    peak_groups[, 8:ncol(peak_groups)] <- xcms::groupval(xsets)
    colnames(peak_groups)[8:ncol(peak_groups)] <- samples

    spectra_id <- 0
    spectras <- data.frame()
    spectra_infos <- data.frame()
    ann <- data.frame()
    sample_matrix <- matrix(, nrow = 1, ncol = length(samples),
                            dimnames = list(c(), samples))
    # i represent a group of peaks
    for (i in seq(nrow(peak_groups))) {
        if (!is.null(pb_fct)) pb_fct(i, nrow(peak_groups), "Annotate")
        db_match <- db[which(
            db$mz >= peak_groups[i, "mzmed"] - ann_params@da_tol &
                db$mz <= peak_groups[i, "mzmed"] + ann_params@da_tol &
                db$rt >= peak_groups[i, "rtmed"] - ann_params@rt_tol &
                db$rt <= peak_groups[i, "rtmed"] + ann_params@rt_tol),
            , drop = FALSE]
        if (nrow(db_match) == 0) next
        l_spectras2 <- l_spectras[unique(db_match$ion_id)]

        peak_rows <- unlist(peak_groups[i, 8:ncol(peak_groups)])
        peak_rows <- peak_rows[!is.na(peak_rows)]
        basepeaks <- peaks[peak_rows, , drop = FALSE]
        mz_ranges <- range(unlist(lapply(l_spectras2, function(x) range(x$mz))))
        tmp_ann <- cbind(group_id = i,
                         db_match[, c("name", "formula", "adduct",
                                        "ion_formula")],
                           rtdiff = abs(peak_groups[i, "rtmed"] -
                                            db_match$rt),
                           rt = peak_groups[i, "rtmed"],
                           rtmin = median(basepeaks$rtmin),
                           rtmax = median(basepeaks$rtmax),
                           nsamples = sum(!is.na(
                               peak_groups[i, 8:ncol(peak_groups)])),
                           best_score = 0,
                           best_deviation_mz = Inf,
                           best_npeak = 0,
                           sample_matrix)
        # j represent ONE sample
        for (j in which(peak_groups[i, 8:ncol(peak_groups)] > 0)) {
            # get the rt range where all isotpologue must fall
                # it corresponds to the fwhm of the basepeak
            basepeak <- basepeaks[basepeaks$sample == j, ]
            fwhm <- abs(median(basepeak$rtmax) -
                            median(basepeak$rtmin)) /
                sigma * 2.35 * perfwhm
            rt_range <- basepeak$rt + c(-fwhm, fwhm)

            # get the mz range where all isotopologue must fall
                # it corresponds to 10 times the maximum deviation observed
                # between the basepeaks & the theoretical
            da_tol_iso <- max(abs(basepeak$mz - db_match$mz)) * 10
            mz_range <- c(mz_ranges[1] - da_tol_iso,
                          mz_ranges[2] + da_tol_iso)

            # now search all isotopologues
            q_spectra <- peaks[
                peaks$mz >= mz_range[1] &
                    peaks$mz <= mz_range[2] &
                    peaks$rt >= rt_range[1] &
                    peaks$rt <= rt_range[2] &
                    peaks$sample == j,
                c("feature_id", "mz", "int"), drop = FALSE]
            q_spectra <- cbind(q_spectra,
                               abd = q_spectra$int /
                                   basepeak$int * 100)
            tmp <- compare_spectras(q_spectra,
                                    l_spectras2,
                                    da_tol_iso,
                                    ann_params@abd_tol)
            # dont forget that there is the possibility where
                # tmp_ann contains multiple time the same formula
                # and tmp contains only UNIQUE formula
            k_2 <- split(seq(nrow(tmp_ann)), db_match$ion_id)
            # k represent ONE ion formula, so multiple rows on tmp_ann
            for (k in seq(length(tmp))) {
                if (tmp[[k]]$score == 0) next
                spectra_id <- spectra_id + 1
                spectra <- tmp[[k]]$spectra
                spectras <- rbind(spectras,
                                  cbind(spectra_id = spectra_id, spectra))
                spectra_infos <- rbind(spectra_infos,
                                       data.frame(
                                             spectra_id = spectra_id,
                                             score = tmp[[k]]$score,
                                             deviation_mz = tmp[[k]]$deviation_mz,
                                             npeak = tmp[[k]]$npeak,
                                             basepeak_int = basepeak$int,
                                             sum_int = sum(spectra[which(
                                                 !is.na(spectra$mz_theo) &
                                                 !is.na(spectra$mz)),
                                                 "int"]),
                                             sample = samples[j],
                                             rt = basepeak$rt))
                if (tmp[[k]]$score > tmp_ann[k_2[[k]][1], "best_score"]) {
                    tmp_ann[k_2[[k]], j + 13] <- spectra_id
                    tmp_ann[k_2[[k]], "best_score"] <- tmp[[k]]$score
                    tmp_ann[k_2[[k]], "best_deviation_mz"] <- tmp[[k]]$deviation_mz
                    tmp_ann[k_2[[k]], "best_npeak"] <- tmp[[k]]$npeak
                }
            }
        }
        ann <- rbind(ann, tmp_ann)
    }

    attributes(xsets)$ann <- ann
    attributes(xsets)$spectra_infos <- spectra_infos
    attributes(xsets)$spectras <- spectras
    return(xsets)
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
#' It reject also the annotation if the best ion has only one isotopologue
#'      (only "M")
#'
#' @param ann the annotation dataframe obtained
#'      from the function `annotate_peaklists`
#' @param sigma ignore
#' @param perfwhm ignore
#'
#' @return the annotation dataframe filtered & regrouped
filtrate_ann <- function(ann, spectra_infos, sigma = 6, perfwhm = .6) {
    do.call(rbind, lapply(split(ann, ann$name), function(x) {
        if (max(x$best_npeak) == 1) return(NULL)
        else if (nrow(x) == 1) return(x)
        best_peak <- x[order(
            -x$best_npeak,
            -x$best_score,
            x$rtdiff,
            x$best_deviation_mz)[1], , drop = FALSE]
        # eject the annotations where the same compound is not at the same rT
        fwhm <- (abs(best_peak$rtmax - best_peak$rtmin) /
                     sigma * 2.35 * perfwhm)
        x <- x[x$rt >= best_peak$rt - fwhm &
                   x$rt <= best_peak$rt + fwhm, , drop = FALSE]
        # merge rows where the peak picking or the alignment fail
        if (any(duplicated(x$adduct)))
            do.call(rbind, lapply(split(x, x$adduct), function(y) {
                if (nrow(y) == 1) return(y)
                new_y <- y[which.max(y$best_score), , drop = FALSE]
                new_y[, c("rtdiff", "rt", "rtmin", "rtmax")] <- apply(
                    new_y[, c("rtdiff", "rt", "rtmin", "rtmax")], 2, median)
                new_y[, c("best_score", "best_npeak")] <- apply(
                    new_y[, c("best_score", "best_npeak")], 2, max)
                new_y$best_deviation_mz <- y[which.min(
                    abs(y$best_deviation_mz)), "best_deviation_mz"]
                new_y[, 14:ncol(y)] <- apply(y[, 14:ncol(y)], 2, function(z)
                    if (length(which(!is.na(z))) == 1) z[!is.na(z)]
                    else z[!is.na(z)][which.max(spectra_infos[
                        spectra_infos$spectra_id %in% z[!is.na(z)], "score"])])
                new_y$nsamples <- sum(!is.na(new_y[, 14:ncol(new_y)]))
                new_y
            }))
    }))
}

split_conflicts <- function(ann) {
    conflicts <- split(ann, ann$group_id)
    conflicts_nrow <- sapply(conflicts, nrow)
    return(list(
        no_conflicts = if (all(conflicts_nrow > 1)) data.frame()
        else do.call(rbind, conflicts[
            which(conflicts_nrow == 1)]),
        conflicts = conflicts[which(conflicts_nrow > 1)]
    ))
}

#' @title Resolve annotations conflicts
#'
#' @description
#' Summarise the annotations dataframe by compound instead by ion
#' it will return in the column samples the sum of intensity of all ions
#'
#' @param ann the annotation dataframe obtained
#'      from the function `annotate_peaklists`
#'
#' @return the annotation dataframe grouped by compound
summarise_ann <- function(ann, spectra_infos) {
    int_ann <- get_int_ann(ann, spectra_infos)
    do.call(rbind, lapply(split(int_ann, int_ann$name), function(x)
        cbind.data.frame(
            name = x[1, "name"],
            `rT (min)` = round(mean(x[, "rT (min)"]), 2),
            `Diff rT (sec)` = min(x[, "Diff rT (sec)"]),
            Adducts = paste(x$Adduct, collapse = " "),
            nSamples = sum(sapply(x[, 9:ncol(x)], function(y) any(y > 0))),
            `Most intense ion` = as.factor(x[which.max(
                apply(x[, 9:ncol(x)], 1, max)), "Adduct"]),
            `Best score (%)` = max(x[, "Best score (%)"]),
            `Best m/z dev (mDa)` = min(x[, "Best m/z dev (mDa)"]),
            `Max iso` = max(x[, "Max iso"]),
            lapply(x[, 9:ncol(x)], sum)
        )
    ))
}

get_int_ann <- function(ann, spectra_infos) {
    # split first ann informations
    ann <- split_conflicts(ann)$no_conflicts
    # extract intensity of basepeaks
    cbind.data.frame(
        name = ann$name,
        `rT (min)` = round(ann$rt / 60, 2),
        `Diff rT (sec)` = round(ann$rtdiff),
        Adduct = ann$adduct,
        nSamples = ann$nsamples,
        `Best score (%)` = round(ann$best_score),
        `Best m/z dev (mDa)` = round(ann$best_deviation_mz),
        `Max iso` = ann$best_npeak,
        apply(ann[, 14:ncol(ann)], c(1, 2), function(x)
            if (is.na(x)) NA
            else spectra_infos[as.numeric(x), "basepeak_int"]
        )
    )
}
