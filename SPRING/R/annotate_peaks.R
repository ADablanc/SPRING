#' @title Annotate peaklists
#'
#' @description
#' Annotate peaklists from a `xsAnnotate` with grouping information
#' it loop through the pcgroups
#' if one of the peak match with one of the theoretical monoisotopic from
#' database it will compare the pseudo spectra obtained by CAMERA against the
#' theoretical spectra & compute an isotopic score
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
#' @param xsa `xsAnnotate`
#' @param ann_params `AnnotationParameter`
#' @param pb_fct `function` used to update the progress bar
#'
#' @return `xsAnnotate` with three additional slots :
#' \itemize{
#'     \item ann `DataFrame` each line represent an hypothesis annotation
#'     it contains the columns :
#'     \itemize{
#'         \item group_id `integer` group ID
#'         \item eic_id `ìnteger` EIC ID
#'         \item class `character` cpd class
#'         \item name `character` name
#'         \item referent_adduct `character` referent adduct for the compound
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
#'         \item mzmin `numeric` m/z born min
#'         \item mzmax `numeric` m/z born max
#'         \item rt `numeric` rT
#'         \item rtmin `numeric` rT born min
#'         \item rtmax `numeric` rT born max
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
#'         \item basepeak_mz `numeric` m/z of the basepeak annotated
#'         \item basepeak_int `numeric` area of the basepeak annotated
#'         \item sum_int `numeric` cumulative sum off all the area of the
#'         isotopologues annotated
#'         \item rt `numeric` retention time
#'     }
#' }
annotate_pcgroups <- function(xsa, ann_params, pb_fct = NULL) {
    if (!is.null(pb_fct)) {
        pb_fct(n = 0, total = 1, title = "Annotate")
    }
    chem_db <- load_ion_db(
        ann_params@database,
        ann_params@instrument,
        ann_params@polarity,
        cpd_classes = ann_params@cpd_classes
    )
    l_spectras <- unique(chem_db[, c("ion_id", "formula", "adduct",
                                     "ion_formula", "mz", "abd", "iso")])
    l_spectras <- split(l_spectras, l_spectras$ion_id)
    colnames(chem_db)[colnames(chem_db) == "adduct"] <- "referent_adduct"
    chem_db <- unique(chem_db[chem_db$abd == 100,
                    c("class", "name",
                      "formula", "referent_adduct",
                      "ion_formula", "mz", "rt", "ion_id")])

    xset <- xsa@xcmsSet
    samples <- rownames(xset@phenoData)
    peaks <- data.frame(xset@peaks)
    peaks <- cbind(feature_id = seq(nrow(peaks)), peaks)
    colnames(peaks)[which(colnames(peaks) == "into")] <- "int"
    # replace the value in peak_groups by the feature id
    # instead of the intensity
    peak_groups <- data.frame(xset@groups)
    peak_groups[, 8:ncol(peak_groups)] <- xcms::groupval(xset)
    colnames(peak_groups)[8:ncol(peak_groups)] <- samples
    peak_groups <- cbind(
        group_id = seq(nrow(peak_groups)),
        cluster_id = seq(nrow(peak_groups)),
        iso = "M",
        peak_groups
    )
    peak_groups[xsa@isoID[, "isopeak"], "cluster_id"] <- xsa@isoID[, "mpeak"]
    peak_groups[xsa@isoID[, "isopeak"], "iso"] <- "M+*"

    spectra_id <- 1
    samples <- rownames(xset@phenoData)
    spectras <- data.frame(matrix(, nrow = 0, ncol = 14, dimnames = list(
        c(), c("spectra_id", "feature_id", "mz", "mzmin", "mzmax", "rt",
               "rtmin", "rtmax", "int", "abd", "ion_id_theo", "mz_theo",
               "abd_theo", "iso_theo")
    )))
    spectra_infos <- data.frame(matrix(, nrow = 0, ncol = 9, dimnames = list(
        c(), c("spectra_id", "score", "deviation_mz", "npeak", "basepeak_mz",
               "basepeak_int", "sum_int", "sample", "rt")
    )))
    ann <- data.frame(
        matrix(, nrow = 0, ncol = 16 + length(samples), dimnames = list(
            c(), c("group_id", "eic_id", "class", "name", "referent_adduct",
                   "formula", "adduct", "ion_formula", "rtdiff", "rt", "rtmin",
                   "rtmax", "nsamples", "best_score", "best_deviation_mz",
                   "best_npeak", samples))
        ),
        check.names = FALSE
    )
    sample_matrix <- matrix(, nrow = 1, ncol = length(samples),
                            dimnames = list(c(), samples))

    # for each pcgroup
    for (i in seq(length(xsa@pspectra))) {
        if (!is.null(pb_fct)) {
            # update the progress bar only every 1%
            if (i %% ceiling(length(xsa@pspectra) / 100) == 0) {
                pb_fct(i, length(xsa@pspectra), "Annotate")
            }
        }

        pcgroup <- peak_groups[xsa@pspectra[[i]], , drop = FALSE]
        idx <- lapply(seq(nrow(pcgroup)), function(i) {
            which(
                chem_db$mz >= pcgroup[i, "mzmed"] - ann_params@da_tol &
                    chem_db$mz <= pcgroup[i, "mzmed"] + ann_params@da_tol &
                    chem_db$rt >= pcgroup[i, "rtmed"] - ann_params@rt_tol &
                    chem_db$rt <= pcgroup[i, "rtmed"] + ann_params@rt_tol
            )
        })
        if (any(lengths(idx) > 0)) {
            pc_group_basepeak <- pcgroup[which(
                lengths(idx) > 0)[1], , drop = FALSE]
            chem_db_match <- chem_db[unlist(idx), , drop = FALSE]
            da_tol_iso <- max(abs(pc_group_basepeak$mzmed - chem_db_match$mz)) *
                10
        } else {
            pc_group_basepeak <- pcgroup[which.max(
                apply(pcgroup[, 11:ncol(pcgroup)], 1, function(x) {
                    max(peaks[x, "int"], na.rm = TRUE)
                })), ]
            chem_db_match <- chem_db[0, ]
            da_tol_iso <- ann_params@da_tol
        }

        # for cluster
        for (cluster in split(pcgroup, pcgroup$cluster)) {
            # create the same base for the annotation DataFrame for all
            # basepeaks
            tmp_ann <- cbind(
                rtdiff = NA,
                rt = pc_group_basepeak$rtmed,
                rtmin = pc_group_basepeak$rtmin,
                rtmax = pc_group_basepeak$rtmax,
                nsamples = sum(!is.na(pcgroup[i, 11:ncol(pcgroup)])),
                best_score = 0,
                best_deviation_mz = NA,
                best_npeak = 0,
                sample_matrix
            )

            # any adduct identified by CAMERA ?
            hypo_adducts <- which(xsa@annoID[, "id"] ==
                                      cluster[cluster$iso == "M", "group_id"])
            if (nrow(chem_db_match) > 0 && length(hypo_adducts) > 0) {
                l_spectras2 <- do.call(c, lapply(hypo_adducts,
                                                     function(hypo_adduct) {
                    get_ions(
                        unique(chem_db_match$formula),
                        adducts[xsa@annoID[hypo_adduct, "ruleID"], ],
                        ann_params@instrument
                    )
                }))

            } else if (nrow(chem_db_match) > 0) {
                # in here we will certainly search in non target later
                    # like decomposition of the mass difference against the
                        # chem_db_match mass neutral
                l_spectras2 <- l_spectras[unique(chem_db_match$ion_id)]
            } else {
                l_spectras2 <- list(data.frame(matrix(
                    , nrow = 0, ncol = 7, dimnames = list(c(), c(
                        "ion_id", "formula", "adduct", "ion_formula", "mz",
                        "abd", "iso"
                    )))))
            }
            tmp_ann <- cbind(
                do.call(rbind, lapply(l_spectras2, function(x) {
                    x[1, c("formula", "adduct", "ion_formula")]
                })),
                tmp_ann
            )

            # for each sample
            for (j in 11:ncol(cluster)) {
                if (is.na(cluster[cluster$iso == "M", j])) {
                    next
                }
                basepeak <- peaks[cluster[cluster$iso == "M", j], ]
                q_spectra <- peaks[cluster[, j],
                                   c("feature_id", "mz", "mzmin", "mzmax",
                                     "rt", "rtmin", "rtmax", "int"),
                                   drop = FALSE]
                q_spectra$abd <- q_spectra$int / basepeak$int * 100
                tmp <- compare_spectras(
                    q_spectra,
                    l_spectras2,
                    da_tol_iso,
                    ann_params@abd_tol
                )
                for (l in seq(length(tmp))) {
                    if (tmp[[l]]$score == 0 && l > 1) {
                        # register at least one time the spectra of the pcgroup
                        next
                    }
                    spectras <- rbind(
                        spectras,
                        cbind(
                            spectra_id = spectra_id,
                            tmp[[l]]$spectra[, c("feature_id", "mz", "mzmin",
                                                 "mzmax", "rt", "rtmin",
                                                 "rtmax", "int", "abd",
                                                 "mz_theo", "abd_theo",
                                                 "iso_theo")]
                        )
                    )
                    spectra_infos <- rbind(
                        spectra_infos,
                        data.frame(
                            spectra_id = spectra_id,
                            score = tmp[[l]]$score,
                            deviation_mz = tmp[[l]]$deviation_mz,
                            npeak = tmp[[l]]$npeak,
                            basepeak_mz = peaks[cluster[cluster$iso == "M", j],
                                                "mz"],
                            basepeak_int = peaks[cluster[cluster$iso == "M", j],
                                                 "int"],
                            sum_int = sum(tmp[[l]]$spectra[
                                which(!is.na(tmp[[l]]$spectra$mz_theo) &
                                          !is.na(tmp[[l]]$spectra$mz)),
                                "int"]
                            ),
                            sample = samples[j - 10],
                            rt = q_spectra[q_spectra$abd == 100, "rt"]
                        )
                    )
                    tmp_ann[l, ncol(tmp_ann) - length(samples) + j - 10] <-
                        spectra_id
                    if (tmp[[l]]$score > tmp_ann[l, "best_score"]) {
                        tmp_ann[l, "best_score"] <- tmp[[l]]$score
                        tmp_ann[l, "best_deviation_mz"] <-
                            tmp[[l]]$deviation_mz
                        tmp_ann[l, "best_npeak"] <- tmp[[l]]$npeak
                    }
                    tmp_ann$rtmin <- min(tmp_ann$rtmin, basepeak$rtmin)
                    tmp_ann$rtmax <- max(tmp_ann$rtmax, basepeak$rtmax)
                    spectra_id <- spectra_id + 1
                }
            }
            tmp_ann <- merge(
                chem_db_match[, c("class", "name", "referent_adduct",
                                  "formula")],
                tmp_ann,
                by = "formula",
                all = TRUE,
                sort = FALSE
            )
            if (nrow(chem_db_match) > 0) {
                tmp_ann$rtdiff <- abs(chem_db_match$rt - tmp_ann$rt)
            }
            tmp_ann <- cbind(
                group_id = i,
                eic_id = cluster[1, "cluster_id"],
                tmp_ann
            )
            ann <- rbind(ann, tmp_ann)
        }
    }
    if (!is.null(pb_fct)) {
        pb_fct(n = 1, total = 1, title = "Annotate")
    }

    rownames(ann) <- NULL
    rownames(spectras) <- NULL
    attributes(xsa)$ann <- ann
    attributes(xsa)$spectra_infos <- spectra_infos
    attributes(xsa)$spectras <- spectras
    xsa
}

#' @title Get conflicts
#'
#' @description
#' Split the annotation `DataFrame` to get the conflicts
#' Conflicts are when for a group of peaks multiple annotations are possible
#' (it happens often when a chemical formula refers to multiple compounds)
#'
#' @param ann `DataFrame` each line correspond to a compound found
#' with the columns:
#' \itemize{
#'     \item group_id `integer` group ID
#'     \item eic_id `integer` EIC ID
#'     \item class `character` cpd class
#'     \item name `character` name
#'     \item referent_adduct `character` referent adduct for the compound
#'     \item formula `character` chemical formula
#'     \item adduct `character` adduct form
#'     \item ion_formula `character` ion chemical formula
#'     \item rtdiff `numeric` retention time difference between the measured
#'     & the expected
#'     \item rt `numeric` retention time measured meanned accross the
#'     samples
#'     \item rtmin `numeric` born min of retention time measured accross the
#'     samples
#'     \item rtmax `numeric` born max of the retention time measured accross
#'     the samples
#'     \item nsamples `integer` number of samples where the compound was
#'     found
#'     \item best_score `numeric` best isotopic score seen
#'     \item best_deviation_mz `numeric` best m/z deviation seen
#'     \item best_npeak `integer` best number of isotopologues found
#'     \item ... `integer` a column for each sample which contain the
#'     spectra ID
#' }
#'
#' @return `list` of two items :
#' \itemize{
#'     \item no_conflicts : `DataFrame` each line correspond to a compound found
#'     with the columns:
#'     \itemize{
#'         \item group_id `integer` group ID
#'         \item eic_id `integer` EIC ID
#'         \item class `character` cpd class
#'         \item name `character` name
#'         \item referent_adduct `character` referent adduct for the compound
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
#'     \item conflicts : `DataFrame list` each item correspond to the same group
#'      of peaks where multiple annotations is possible. each dataframe has the
#'     columns :
#'     \itemize{
#'         \item group_id `integer` group ID
#'         \item eic_id `integer` EIC ID
#'         \item class `character` cpd class
#'         \item name `character` name
#'         \item referent_adduct `character` referent adduct for the compound
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
#' }
split_conflicts <- function(ann) {
    splitted_ann <- lapply(split(ann, ann$group_id), function(x) {
        if (length(unique(x$name)) == 1) x else split(x, x$name)
    })
    names(splitted_ann) <- NULL
    is_conflict <- sapply(splitted_ann, class) == "list"
    list(
        no_conflicts = if (any(!is_conflict)) do.call(
            rbind,
            splitted_ann[!is_conflict]
        ) else ann[0, ],
        conflicts = splitted_ann[is_conflict]
    )
}

#' @title Summarise annotations
#'
#' @description
#' Summarise the annotations `DataFrame` by compound instead by ion
#' it will return in the column samples the intensity of the basepeak with the
#' referent adduct
#'
#' @param ann `DataFrame` each line correspond to a compound found
#' with the columns:
#' \itemize{
#'     \item group_id `integer` group ID
#'     \item eic_id `integer` EIC ID
#'     \item class `character` cpd class
#'     \item name `character` name
#'     \item referent_adduct `character` referent adduct for the compound
#'     \item formula `character` chemical formula
#'     \item adduct `character` adduct form
#'     \item ion_formula `character` ion chemical formula
#'     \item rtdiff `numeric` retention time difference between the measured
#'     & the expected
#'     \item rt `numeric` retention time measured meanned accross the
#'     samples
#'     \item rtmin `numeric` born min of retention time measured accross the
#'     samples
#'     \item rtmax `numeric` born max of the retention time measured accross
#'     the samples
#'     \item nsamples `integer` number of samples where the compound was
#'     found
#'     \item best_score `numeric` best isotopic score seen
#'     \item best_deviation_mz `numeric` best m/z deviation seen
#'     \item best_npeak `integer` best number of isotopologues found
#'     \item ... `integer` a column for each sample which contain the
#'     spectra ID
#' }
#' @param spectra_infos `DataFrame`, each line correspond to a spectra
#' annotated, with the columns :
#' \itemize{
#'     \item spectra_id `integer` spectra ID
#'     \item score `numeric` isotopic score observed
#'     \item deviation_mz `numeric` m/z deviation observed
#'     \item npeak `integer` number of isotopologue annotated
#'     \item basepeak_mz `numeric` m/z of the basepeak annotated
#'     \item basepeak_int `numeric` area of the basepeak annotated
#'     \item sum_int `numeric` cumulative sum off all the area of the
#'     isotopologues annotated
#'     \item rt `numeric` retention time
#' }
#' @param nsamples `integer` the number of samples processed in total, it is use
#'  as an offset on the DataFrame
#' @param by `character(1)` should be `referent` to report only the intensity of
#' the referent ion or `all` to sum all the intensity for the compound
#'
#' @return list of two items :
#' \itemize{
#'     \item resume: `DataFrame` each line represent a compound with the columns
#'     \itemize{
#'         \item Group ID `numeric` group ID
#'         \item class `character` cpd class
#'         \item name `character` name of the compound
#'         \item rt (min) `numeric` meanned rT
#'         \item Diff rT (sec) `numeric` rT difference between observed &
#'         theoretical
#'         \item Adducts `character` all adducts detected separated by a space
#'         \item nSamples `integer` number of samples where the compound was
#'         detected
#'         \item best score (%) `numeric` the highest isotopic score
#'         \item best m/z dev (mDa) `numeric` the minimal m/z deviation observed
#'         in mDa
#'         \item max iso `integer` the highest number of isotopologue for the
#'          ions reported
#'         \item ... `integer` a column for each sample which contain the summed
#'         intensity of ALL basepeaks
#'     }
#'     \item details: `DataFrame` each line represent an ion with the columns
#'     \itemize{
#'         \item Group ID `numeric` group ID
#'         \item Class `character` cpd class
#'         \item Name `character` name of the compound
#'         \item rt (min) `numeric` meanned rT
#'         \item Diff rT (sec) `numeric` rT difference between observed &
#'         theoretical
#'         \item Adduct `character` adduct name
#'         \item nSamples `integer` number of samples where the ions was
#'         detected
#'         \item best score (%) `numeric` the highest isotopic score
#'         \item best m/z dev (mDa) `numeric` the minimal m/z deviation observed
#'         in mDa
#'         \item max iso `integer` the highest number of isotopologue for the
#'          ions reported
#'         \item ... `integer` a column for each sample which contain the
#'         intensity the basepeak
#'     }
#' }
summarise_ann <- function(ann, spectra_infos, nsamples, by = "referent") {
    int_ann <- get_int_ann(ann, spectra_infos, nsamples)
    if (nrow(int_ann) == 0) {
        return(list(
           resume = data.frame(matrix(, nrow = 0, ncol = 10, dimnames = list(
               c(), c("Group ID", "Class", "Name", "rT (min)",
                      "Diff rT (sec)", "Adducts", "nSamples",
                      "Best score (%)", "Best m/z dev (mDa)", "Max iso"))),
               check.names = FALSE),
           details = int_ann
       ))
    }
    list(
        resume = do.call(
            rbind,
            c(lapply(
                split(
                    int_ann,
                    paste(int_ann[, "Group ID"], int_ann$Name, sep = "/")
                ), function(x) {
                data.frame(
                    `Group ID` = x[1, "Group ID"],
                    Class = x[1, "Class"],
                    Name = x[1, "Name"],
                    `rT (min)` = round(mean(x[, "rT (min)"]), 2),
                    `Diff rT (sec)` = min(x[, "Diff rT (sec)"]),
                    Adducts = if (all(is.na(x$Adduct))) NA
                        else paste(x$Adduct, collapse = " "),
                    nSamples = sum(
                        sapply(
                            x[, (ncol(x) - nsamples + 1):ncol(x), drop = FALSE],
                            function(y) any(!is.na(y))
                        )
                    ),
                    `Best score (%)` = max(x[, "Best score (%)"]),
                    `Best m/z dev (mDa)` = min(x[, "Best m/z dev (mDa)"]),
                    `Max iso` = max(x[, "Max iso"]),
                    if (is.na(x[1, "Name"]) & by == "referent") do.call(
                        cbind,
                        lapply(x[, (ncol(x) - nsamples + 1):ncol(x)],
                               function(y) {
                                   if (all(is.na(y))) NA
                                   else max(y, na.rm = TRUE)
                    })) else if (!is.na(x[1, "Name"]) & by == "referent") x[
                        x$Adduct == x[, "Referent adduct"],
                        (ncol(x) - nsamples + 1):ncol(x),
                          drop = FALSE
                    ] else do.call(
                        cbind,
                        lapply(x[, (ncol(x) - nsamples + 1):ncol(x)],
                               function(y) {
                                   if (all(is.na(y))) NA
                                   else sum(y, na.rm = TRUE)
                    })),
                    check.names = FALSE
                )
            }), make.row.names = FALSE)
        ),
        details = int_ann[, -which(colnames(int_ann) %in% c("EIC ID",
                                                            "Referent adduct"))]
    )
}

#' @title Get annotations with intensity
#'
#' @description
#' Replace the feature ID in the annotation `DataFrame` with the intensity of
#' the basepeak or the m/z
#'
#' @param ann `DataFrame` each line correspond to a compound found
#' with the columns:
#' \itemize{
#'     \item group_id `integer` group ID
#'     \item eic_id `integer` eic ID
#'     \item class `character` cpd class
#'     \item name `character` name
#'     \item referent_adduct `character` referent adduct for the compound
#'     \item formula `character` chemical formula
#'     \item adduct `character` adduct form
#'     \item ion_formula `character` ion chemical formula
#'     \item rtdiff `numeric` retention time difference between the measured
#'     & the expected
#'     \item rt `numeric` retention time measured meanned accross the
#'     samples
#'     \item rtmin `numeric` born min of retention time measured accross the
#'     samples
#'     \item rtmax `numeric` born max of the retention time measured accross
#'     the samples
#'     \item nsamples `integer` number of samples where the compound was
#'     found
#'     \item best_score `numeric` best isotopic score seen
#'     \item best_deviation_mz `numeric` best m/z deviation seen
#'     \item best_npeak `integer` best number of isotopologues found
#'     \item ... `integer` a column for each sample which contain the
#'     spectra ID
#' }
#' @param spectra_infos `DataFrame`, each line correspond to a spectra
#' annotated, with the columns :
#' \itemize{
#'     \item spectra_id `integer` spectra ID
#'     \item score `numeric` isotopic score observed
#'     \item deviation_mz `numeric` m/z deviation observed
#'     \item npeak `integer` number of isotopologue annotated
#'     \item basepeak_mz `numeric` m/z of the basepeak annotated
#'     \item basepeak_int `numeric` area of the basepeak annotated
#'     \item sum_int `numeric` cumulative sum off all the area of the
#'     isotopologues annotated
#'     \item rt `numeric` retention time
#' }
#' @param nsamples `integer` the number of samples processed in total, it is use
#'  as an offset on the "ann" DataFrame
#' @param val `character(1)` "int" or "mz" to specify which type of value is
#' desired
#'
#' @return `DataFrame` each line represent an ion with the columns
#' \itemize{
#'     \item Group ID `numeric` group ID
#'     \item EIC ID `numeric` EIC ID
#'     \item Class `character` cpd class
#'     \item Name `character` name of the compound
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
get_int_ann <- function(ann, spectra_infos, nsamples, val = "int") {
    if (nrow(ann) == 0) {
        return(data.frame(matrix(, nrow = 0, ncol = 12,
            dimnames = list(c(),
                c("Group ID", "EIC ID", "Class", "Name", "rT (min)",
                  "Diff rT (sec)", "Referent adduct", "Adduct", "nSamples",
                  "Best score (%)", "Best m/z dev (mDa)", "Max iso")
            )
        ), check.names = FALSE))
    }
    # extract intensity of basepeaks
    data.frame(
        `Group ID` = as.factor(ann$group_id),
        `EIC ID` = ann$eic_id,
        Class = as.factor(ann$class),
        Name = ann$name,
        `rT (min)` = round(ann$rt / 60, 2),
        `Diff rT (sec)` = round(ann$rtdiff),
        `Referent adduct` = ann$referent_adduct,
        Adduct = ann$adduct,
        nSamples = ann$nsamples,
        `Best score (%)` = round(ann$best_score),
        `Best m/z dev (mDa)` = round(ann$best_deviation_mz),
        `Max iso` = ann$best_npeak,
        apply(ann[, (ncol(ann) - nsamples + 1):ncol(ann), drop = FALSE],
              c(1, 2), function(x) {
            if (is.na(x)) {
                NA
            } else if (val == "int") {
                spectra_infos[spectra_infos$spectra_id == as.numeric(x),
                              "basepeak_int"]
            } else if (val == "mz") {
                spectra_infos[spectra_infos$spectra_id == as.numeric(x),
                              "basepeak_mz"]
            }
        }),
        check.names = FALSE, row.names = NULL
    )
}
