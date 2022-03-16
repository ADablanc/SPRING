#' @title Annotate peaklists
#'
#' @description
#' Annotate peaklists from a `xcmsSet` with grouping information
#' it loop through the peaks grouped dataframe
#' if the peak match with one of the theoretical monoisotopic from database
#'      it will search all isotopologue grouped in
#'      the rt window of the peak +/- fwhm
#' it compare then the spectra obtained against the theoretical spectra
#'      & compute an isotopic score
#' The scoring algorithm will search each corresponding observed peak
#'      with theoreticals
#' Therefore it contains some important rules :
#' \itemize{
#'      \item an observed peak can only correspond to ONE theoretical peak
#'       and vice versa
#'      \item the relative abundance peak must not be under a tolerance
#'      compared to the theoretical
#'      but it can be higher since a peak can hide another
#'      \item the A+x is not searched if the A+x-1 is not found
#'      (the loop search is stopped)
#' }
#'
#' @param xset `xcmsSet`
#' @param ann_params `AnnotationParameter`
#' @param pb_fct `function` used to update the progress bar
#' @param sigma `numeric(1)` ignore
#' @param perfwhm `numeric(1)` ignore
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
annotate_peaklists <- function(xset,
                               ann_params,
                               pb_fct = NULL,
                               sigma = 6,
                               perfwhm = .6) {
    db <- load_db(ann_params@adduct_names, ann_params@instrument)
    l_spectras <- unique(db[, c("ion_id", "mz", "abd", "iso")])
    l_spectras <- split(l_spectras, l_spectras$ion_id)
    db <- unique(db[db$abd == 100,
                    c("name", "formula", "adduct", "ion_formula",
                      "mz", "rt", "ion_id")])

    spectra_id <- 0
    samples <- rownames(xset@phenoData)
    spectras <- data.frame(matrix(, nrow = 0, ncol = 9, dimnames = list(
        c(), c("spectra_id", "feature_id", "mz", "int", "abd", "ion_id_theo",
               "mz_theo", "abd_theo", "iso_theo")
    )))
    spectra_infos <- data.frame(matrix(, nrow = 0, ncol = 8, dimnames = list(
        c(), c("spectra_id", "score", "deviation_mz", "npeak", "basepeak_int",
               "sum_int", "sample", "rt")
    )))
    ann <- data.frame(
        matrix(, nrow = 0, ncol = 13 + length(samples), dimnames = list(
            c(), c("group_id", "name", "formula", "adduct", "ion_formula",
                   "rtdiff", "rt", "rtmin", "rtmax", "nsamples", "best_score",
                   "best_deviation_mz", "best_npeak", samples))
        ),
        check.names = FALSE
    )
    sample_matrix <- matrix(, nrow = 1, ncol = length(samples),
                            dimnames = list(c(), samples))

    peaks <- data.frame(xset@peaks)
    if (nrow(peaks) == 0 || nrow(db) == 0) {
        attributes(xset)$ann <- ann
        attributes(xset)$spectra_infos <- spectra_infos
        attributes(xset)$spectras <- spectras
        return(xset)
    }
    peaks <- cbind(feature_id = seq(nrow(peaks)), peaks)
    colnames(peaks)[which(colnames(peaks) == "into")] <- "int"
    # replace the value in peak_groups by the feature id
    # instead of the intensity
    peak_groups <- data.frame(xset@groups)
    peak_groups[, 8:ncol(peak_groups)] <- xcms::groupval(xset)
    colnames(peak_groups)[8:ncol(peak_groups)] <- samples

    # i represent a group of peaks
    for (i in seq(nrow(peak_groups))) {
        if (!is.null(pb_fct)) {
            pb_fct(i, nrow(peak_groups), "Annotate")
        }
        # check if we have a match in the database with basepeaks
        db_match <- db[which(
            db$mz >= peak_groups[i, "mzmed"] - ann_params@da_tol &
                db$mz <= peak_groups[i, "mzmed"] + ann_params@da_tol &
                db$rt >= peak_groups[i, "rtmed"] - ann_params@rt_tol &
                db$rt <= peak_groups[i, "rtmed"] + ann_params@rt_tol),
            , drop = FALSE]
        if (nrow(db_match) == 0) {
            next
        }
        # get the corresponding spectras
        l_spectras2 <- l_spectras[unique(db_match$ion_id)]

        # get the feature_ids of all basepeaks
        peak_rows <- unlist(peak_groups[i, 8:ncol(peak_groups)])
        peak_rows <- peak_rows[!is.na(peak_rows)]
        basepeaks <- peaks[peak_rows, , drop = FALSE]
        l_mz_ranges <- range(unlist(
            lapply(l_spectras2, function(x) range(x$mz))
        ))
        # create the same base for the annotation DataFrame for all basepeaks
        tmp_ann <- cbind(
            group_id = i,
            db_match[, c("name", "formula", "adduct", "ion_formula")],
            rtdiff = abs(peak_groups[i, "rtmed"] - db_match$rt),
            rt = peak_groups[i, "rtmed"],
            rtmin = stats::median(basepeaks$rtmin),
            rtmax = stats::median(basepeaks$rtmax),
            nsamples = sum(!is.na(peak_groups[i, 8:ncol(peak_groups)])),
            best_score = 0,
            best_deviation_mz = Inf,
            best_npeak = 0,
            sample_matrix
        )
        # j represent ONE sample
        for (j in which(peak_groups[i, 8:ncol(peak_groups)] > 0)) {
            # get the rt range where all isotpologues must fall
            # it corresponds to the fwhm of the basepeak
            basepeak <- basepeaks[basepeaks$sample == j, ]
            fwhm <- abs(stats::median(basepeak$rtmax) -
                            stats::median(basepeak$rtmin)
                    ) / sigma * 2.35 * perfwhm
            rt_range <- basepeak$rt + c(-fwhm, fwhm)

            # get the mz range where all isotopologue must fall
            # it corresponds to 10 times the maximum deviation observed
            # between the basepeaks & the theoretical
            da_tol_iso <- max(abs(basepeak$mz - db_match$mz)) * 10
            mz_range <- c(l_mz_ranges[1] - da_tol_iso,
                          l_mz_ranges[2] + da_tol_iso)

            # now search all isotopologues
            q_spectra <- peaks[
                peaks$mz >= mz_range[1] &
                    peaks$mz <= mz_range[2] &
                    peaks$rt >= rt_range[1] &
                    peaks$rt <= rt_range[2] &
                    peaks$sample == j,
                c("feature_id", "mz", "int"), drop = FALSE]
            q_spectra <- cbind(
                q_spectra,
                abd = q_spectra$int / basepeak$int * 100
            )
            tmp <- compare_spectras(
                q_spectra,
                l_spectras2,
                da_tol_iso,
                ann_params@abd_tol
            )
            # dont forget that there is the possibility where
            # tmp_ann contains multiple time the same formula
            # and tmp contains only a UNIQUE formula
            k_2 <- split(seq(nrow(tmp_ann)), db_match$ion_id)
            # k represent ONE ion formula, so multiple rows on tmp_ann
            for (k in seq(length(tmp))) {
                if (tmp[[k]]$score == 0) {
                    next
                }
                spectra_id <- spectra_id + 1
                spectra <- tmp[[k]]$spectra
                spectras <- rbind(
                    spectras,
                    cbind(spectra_id = spectra_id, spectra)
                )
                spectra_infos <- rbind(
                    spectra_infos,
                    data.frame(
                        spectra_id = spectra_id,
                        score = tmp[[k]]$score,
                        deviation_mz = tmp[[k]]$deviation_mz,
                        npeak = tmp[[k]]$npeak,
                        basepeak_int = basepeak$int,
                        sum_int = sum(spectra[
                            which(!is.na(spectra$mz_theo) & !is.na(spectra$mz)),
                            "int"]
                        ),
                        sample = samples[j],
                        rt = basepeak$rt
                    )
                )
                tmp_ann[k_2[[k]], j + 13] <- spectra_id
                if (tmp[[k]]$score > tmp_ann[k_2[[k]][1], "best_score"]) {
                    tmp_ann[k_2[[k]], "best_score"] <- tmp[[k]]$score
                    tmp_ann[k_2[[k]], "best_deviation_mz"] <-
                        tmp[[k]]$deviation_mz
                    tmp_ann[k_2[[k]], "best_npeak"] <- tmp[[k]]$npeak
                }
            }
        }
        ann <- rbind(ann, tmp_ann)
    }

    rownames(ann) <- NULL
    rownames(spectras) <- NULL
    attributes(xset)$ann <- ann
    attributes(xset)$spectra_infos <- spectra_infos
    attributes(xset)$spectras <- spectras
    xset
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
#' @param ann `DataFrame` each line correspond to a compound found
#' with the columns:
#' \itemize{
#'     \item group_id `integer` group ID
#'     \item name `character` name
#'     \item formula `character` chemical formula
#'     \item adduct `character` adduct form
#'     \item ion_formula `character` ion chemical formula
#'     \item rtdiff `numeric` retention time difference between the measured &
#'     the expected
#'     \item rt `numeric` retention time measured meanned accross the samples
#'     \item rtmin `numeric` born min of retention time measured accross the
#'     samples
#'     \item rtmax `numeric` born max of the retention time measured accross the
#'      samples
#'     \item nsamples `integer` number of samples where the compound was found
#'     \item best_score `numeric` best isotopic score seen
#'     \item best_deviation_mz `numeric` best m/z deviation seen
#'     \item best_npeak `integer` best number of isotopologues found
#'     \item ... `integer` a column for each sample which contain the spectra ID
#' }
#' @param spectra_infos `DataFrame`, each line correspond to a spectra
#' annotated, with the columns :
#' \itemize{
#'     \item spectra_id `integer` spectra ID
#'     \item score `numeric` isotopic score observed
#'     \item deviation_mz `numeric` m/z deviation observed
#'     \item npeak `integer` number of isotopologue annotated
#'     \item basepeak_int `numeric` area of the basepeak annotated
#'     \item sum_int `numeric` cumulative sum off all the area of the
#'     isotopologues annotated
#'     \item rt `numeric` retention time
#' }

#' @param sigma `numeric(1)` ignore
#' @param perfwhm `numeric(1)` ignore
#'
#' @return the annotation dataframe filtered & regrouped
filtrate_ann <- function(ann, spectra_infos, sigma = 6, perfwhm = .6) {
    if (nrow(ann) == 0) {
        return(ann)
    }
    ann <- do.call(rbind, lapply(split(ann, ann$name), function(x) {
        if (nrow(x) == 1) {
            return(x)
        }
        x <- x[order(
            -x$best_npeak,
            -x$best_score,
            x$rtdiff,
            x$best_deviation_mz), ]
        best_peak <- x[1, ]
        # eject the annotations where the same compound is
        # not at the same rT
        fwhm <- (abs(best_peak$rtmax - best_peak$rtmin) /
                     sigma * 2.35 * perfwhm)
        x <- x[x$rt >= best_peak$rt - fwhm &
                   x$rt <= best_peak$rt + fwhm, , drop = FALSE]
        # merge rows where the peak picking or the alignment fail
        if (any(duplicated(x$adduct))) {
            do.call(
                rbind,
                lapply(split(x, x$adduct), function(y) {
                    if (nrow(y) == 1) {
                        return(y)
                    }
                    new_y <- y[1, , drop = FALSE]
                    new_y[, c("best_score", "best_npeak")] <- apply(
                        y[, c("best_score", "best_npeak"),
                              drop = FALSE], 2, max
                    )
                    new_y$best_deviation_mz <- y[which.min(
                        abs(y$best_deviation_mz)), "best_deviation_mz"]
                    # for each sample select the spectra which is not
                    # missing (if there is multiple it will select the
                    # one with the best score)
                    new_y[, 14:ncol(y)] <- apply(y[, 14:ncol(y),
                           drop = FALSE], 2, function(z) {
                        if (length(which(!is.na(z))) == 1) {
                            z[!is.na(z)]
                        } else {
                            # select the spectra with the best score
                            z[!is.na(z)][which.max(spectra_infos[
                                spectra_infos$spectra_id %in%
                                    z[!is.na(z)], "score"])]
                        }
                    })
                    new_y$nsamples <- sum(!is.na(new_y[, 14:ncol(new_y)]))
                    new_y
                })
            )
        } else {
            x
        }
    }))
    rownames(ann) <- NULL
    ann
}

#' @title Get conflicts
#'
#' @description
#' Split the annotation `DataFrame` to get the conflicts
#' Conflicts are when for a group of peaks multiple annotations are possible
#' (it happens often when for an ion formula refers to multiple compounds)
#'
#' @param ann `DataFrame` each line correspond to a compound found
#' with the columns:
#' \itemize{
#'     \item group_id `integer` group ID
#'     \item name `character` name
#'     \item formula `character` chemical formula
#'     \item adduct `character` adduct form
#'     \item ion_formula `character` ion chemical formula
#'     \item rtdiff `numeric` retention time difference between the measured &
#'     the expected
#'     \item rt `numeric` retention time measured meanned accross the samples
#'     \item rtmin `numeric` born min of retention time measured accross the
#'     samples
#'     \item rtmax `numeric` born max of the retention time measured accross the
#'      samples
#'     \item nsamples `integer` number of samples where the compound was found
#'     \item best_score `numeric` best isotopic score seen
#'     \item best_deviation_mz `numeric` best m/z deviation seen
#'     \item best_npeak `integer` best number of isotopologues found
#'     \item ... `integer` a column for each sample which contain the spectra ID
#' }
split_conflicts <- function(ann) {
    conflicts <- split(ann, ann$group_id)
    names(conflicts) <- NULL
    conflicts_nrow <- sapply(conflicts, nrow, USE.NAMES = FALSE)
    list(
        no_conflicts = if (any(conflicts_nrow == 1)) do.call(
            rbind,
            conflicts[which(conflicts_nrow == 1)]
        ) else ann[0, ],
        conflicts = conflicts[which(conflicts_nrow > 1)]
    )
}

#' @title Summarise annotations
#'
#' @description
#' Summarise the annotations `DataFrame` by compound instead by ion
#' it will return in the column samples the sum of intensity of ALL basepeaks
#'
#' @param ann `DataFrame` each line correspond to a compound found
#' with the columns:
#' \itemize{
#'     \item group_id `integer` group ID
#'     \item name `character` name
#'     \item formula `character` chemical formula
#'     \item adduct `character` adduct form
#'     \item ion_formula `character` ion chemical formula
#'     \item rtdiff `numeric` retention time difference between the measured &
#'     the expected
#'     \item rt `numeric` retention time measured meanned accross the samples
#'     \item rtmin `numeric` born min of retention time measured accross the
#'     samples
#'     \item rtmax `numeric` born max of the retention time measured accross the
#'      samples
#'     \item nsamples `integer` number of samples where the compound was found
#'     \item best_score `numeric` best isotopic score seen
#'     \item best_deviation_mz `numeric` best m/z deviation seen
#'     \item best_npeak `integer` best number of isotopologues found
#'     \item ... `integer` a column for each sample which contain the spectra ID
#' }
#' @param spectra_infos `DataFrame`, each line correspond to a spectra
#' annotated, with the columns :
#' \itemize{
#'     \item spectra_id `integer` spectra ID
#'     \item score `numeric` isotopic score observed
#'     \item deviation_mz `numeric` m/z deviation observed
#'     \item npeak `integer` number of isotopologue annotated
#'     \item basepeak_int `numeric` area of the basepeak annotated
#'     \item sum_int `numeric` cumulative sum off all the area of the
#'     isotopologues annotated
#'     \item rt `numeric` retention time
#' }
#'
#' @return `DataFrame` each line represent a compound with the columns
#' \itemize{
#'     \item name `character` name of the compound
#'     \item rt (min) `numeric` meanned rT
#'     \item Diff rT (sec) `numeric` rT difference between observed &
#'     theoretical
#'     \item Adducts `character` all adducts detected separated by a space
#'     \item nSamples `integer` number of samples where the compound was
#'     detected
#'     \item Most intense ion `factor` name of the adduct where the intensity
#'     measured is the highest
#'     \item best score (%) `numeric` the highest isotopic score
#'     \item best m/z dev (mDa) `numeric` the minimal m/z deviation observed
#'     in mDa
#'     \item max iso `integer` the highest number of isotopologue for the ions
#'     reported
#'     \item ... `integer` a column for each sample which contain the summed
#'     intensity of ALL basepeaks
#' }
summarise_ann <- function(ann, spectra_infos) {
    int_ann <- get_int_ann(ann, spectra_infos)
    if (nrow(int_ann) == 0) {
        return(data.frame(matrix(, nrow = 0, ncol = 10,
            dimnames = list(c(),
                c("name", "rT (min)", "Diff rT (sec)", "Adducts",
                  "nSamples", "Most intense ion", "Best score (%)",
                  "Best m/z dev (mDa)", "Max iso", "X"))), check.names = FALSE))
    }
    int_ann <- split(int_ann, int_ann$name)
    names(int_ann) <- NULL
    do.call(rbind, lapply(int_ann, function(x) {
        data.frame(
            name = x[1, "name"],
            `rT (min)` = round(mean(x[, "rT (min)"]), 2),
            `Diff rT (sec)` = min(x[, "Diff rT (sec)"]),
            Adducts = paste(x$Adduct, collapse = " "),
            nSamples = sum(
                sapply(
                    x[, 9:ncol(x), drop = FALSE],
                    function(y)
                        any(!is.na(y))
                )
            ),
            `Most intense ion` = as.factor(x[which.max(
                apply(
                    x[, 9:ncol(x), drop = FALSE],
                    1,
                    max,
                    na.rm = TRUE
                )), "Adduct"]
            ),
            `Best score (%)` = max(x[, "Best score (%)"]),
            `Best m/z dev (mDa)` = min(x[, "Best m/z dev (mDa)"]),
            `Max iso` = max(x[, "Max iso"]),
            lapply(
                x[, 9:ncol(x), drop = FALSE],
                sum,
                na.rm = TRUE
            ),
            check.names = FALSE
        )
    }))
}

#' @title Get annotations with intensity
#'
#' @description
#' Replace the feature ID in the annotation `DataFrame` with the intensity of
#' the basepeak
#'
#' @param ann `DataFrame` each line correspond to a compound found
#' with the columns:
#' \itemize{
#'     \item group_id `integer` group ID
#'     \item name `character` name
#'     \item formula `character` chemical formula
#'     \item adduct `character` adduct form
#'     \item ion_formula `character` ion chemical formula
#'     \item rtdiff `numeric` retention time difference between the measured &
#'     the expected
#'     \item rt `numeric` retention time measured meanned accross the samples
#'     \item rtmin `numeric` born min of retention time measured accross the
#'     samples
#'     \item rtmax `numeric` born max of the retention time measured accross the
#'      samples
#'     \item nsamples `integer` number of samples where the compound was found
#'     \item best_score `numeric` best isotopic score seen
#'     \item best_deviation_mz `numeric` best m/z deviation seen
#'     \item best_npeak `integer` best number of isotopologues found
#'     \item ... `integer` a column for each sample which contain the spectra ID
#' }
#' @param spectra_infos `DataFrame`, each line correspond to a spectra
#' annotated, with the columns :
#' \itemize{
#'     \item spectra_id `integer` spectra ID
#'     \item score `numeric` isotopic score observed
#'     \item deviation_mz `numeric` m/z deviation observed
#'     \item npeak `integer` number of isotopologue annotated
#'     \item basepeak_int `numeric` area of the basepeak annotated
#'     \item sum_int `numeric` cumulative sum off all the area of the
#'     isotopologues annotated
#'     \item rt `numeric` retention time
#' }
#'
#' @return `DataFrame` each line represent an ion with the columns
#' \itemize{
#'     \item name `character` name of the compound
#'     \item rt (min) `numeric` meanned rT
#'     \item Diff rT (sec) `numeric` rT difference between observed &
#'     theoretical
#'     \item Adduct `character` adduct name
#'     \item nSamples `integer` number of samples where the ions was
#'     detected
#'     \item best score (%) `numeric` the highest isotopic score
#'     \item best m/z dev (mDa) `numeric` the minimal m/z deviation observed
#'     in mDa
#'     \item max iso `integer` the highest number of isotopologue for the ions
#'     reported
#'     \item ... `integer` a column for each sample which contain the
#'     intensity the basepeak
#' }
get_int_ann <- function(ann, spectra_infos) {
    if (nrow(ann) == 0) {
        return(data.frame(matrix(, nrow = 0, ncol = 8,
            dimnames = list(c(),
                c("name", "rT (min)", "Diff rT (sec)", "Adduct",
                  "nSamples", "Best score (%)", "Best m/z dev (mDa)", "Max iso")
            )
        ), check.names = FALSE))
    }
    # extract intensity of basepeaks
    data.frame(
        name = ann$name,
        `rT (min)` = round(ann$rt / 60, 2),
        `Diff rT (sec)` = round(ann$rtdiff),
        Adduct = ann$adduct,
        nSamples = ann$nsamples,
        `Best score (%)` = round(ann$best_score),
        `Best m/z dev (mDa)` = round(ann$best_deviation_mz),
        `Max iso` = ann$best_npeak,
        apply(ann[, 14:ncol(ann), drop = FALSE], c(1, 2), function(x) {
            if (is.na(x)) NA else spectra_infos[as.numeric(x), "basepeak_int"]
        }),
        check.names = FALSE
    )
}
