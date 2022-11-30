#' @title Annotate peaklists
#'
#' @description
#' Annotate peaklists from a `xsAnnotate` with grouping information
#' it loop through the pcgroups
#' if one of the peak match with one of the theoretical monoisotopic from
#' database it will compare the pseudo spectra obtained by CAMERA against the
#' theoretical spectra & compute an isotopic score
#' The scoring algorithm will search each corresponding observed peak with
#' theoreticals. Therefore it contains some important rules :
#' \itemize{
#'      \item an observed peak can only correspond to ONE theoretical peak
#'       and vice versa
#'      \item the relative abundance peak must not be under a tolerance
#'      compared to the theoretical (but it can be higher since a peak can hide
#'      another)
#'      \item the A+x is not searched if the A+x-1 is not found (the loop search
#'       is stopped)
#' }
#' The pcgroup_id correspond to the pcgroup ID given by CAMERA (one pcgroup =
#' one compound), the cluster_id correspond to the same ion (compound + adduct)
#'  identified by CAMERA with the 12C/13C delta mass shift. The group_id is the
#'  group ID given by XCMS.
#'
#' @param xsa `xsAnnotate`
#' @param ann_params `AnnotationParameter`
#'
#' @return `list` with items :
#' \itemize{
#'     \item ann `DataFrame` each line represent an hypothesis annotation
#'     it contains the columns :
#'     \itemize{
#'         \item pcgroup_id `integer` pcgroup ID
#'         \item basepeak_group_id `ìnteger` group ID of the basepeak
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
#'         \item group_id `integer` group ID
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
#'     \item peakgroups `DataFrame`, each line correspond to a group annotated
#'      by XCMS and CAMERA
#'     \itemize{
#'         \item group_id `integer` group ID
#'         \item pcgroup_id `integer` pcgroup ID
#'         \item adduct `character` adduct annotation by CAMERA (NULL if absent)
#'         \item cluster_id `integer` cluster ID
#'         \item iso `character` could be "M" or "M+*"
#'         \item mzmed `float` m/z median computed by XCMS
#'         \item mzmin `float` m/z median born min (not the m/z born min !!)
#'         \item mzmax `float` m/z median born max (not the m/z born max !!)
#'         \item rtmed `float` rT median computed by XCMS
#'         \item rtmin `float` rT median born min (not the rT born min !!)
#'         \item rtmax `float` rT median born max (not the rT born max !!)
#'         \item npeaks `integer` number of peaks grouped accross samples
#'         \item ... `integer` a column for each sample which contain the
#'         feature ID (row ID from the peaktable)
#'     }
#'     \item peaks `DataFrame`, peaktable from XCMS
#'     \itemize{
#'         \item mz `float` m/z
#'         \item mzmin `float` m/z born min
#'         \item mzmax `float` m/z born max
#'         \item rt `float` rT
#'         \item rtmin `float` rT born min
#'         \item rtmax `float` rT born max
#'         \item into `float` area of the peak
#'         \item intb `float` area of the peak above baseline
#'         \item maxo `float` maximum intensity
#'         \item sn `float` signal/noise
#'         \item egauss `float` ignore
#'         \item mu `float` ignore
#'         \item sigma `float` ignore
#'         \item h `float` ignore
#'         \item f `integer` ID of the ROI
#'         \item dppm `float` ppm deviation
#'         \item scale `integer` width of the wave used for the peak detection
#'         \item scpos `integer` scan ID
#'         \item scmin `integer` scan ID born min of the wave detection
#'         \item scmax `integer` scan ID born max of the wave detection
#'         \item lmin `integer` scan ID after extension of the scmin
#'         \item lmax `integer` scan ID after extension of the scmax
#'     }
#' }
annotate_pcgroups <- function(xsa, ann_params) {
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
    peakgroups <- data.frame(xset@groups)
    if (nrow(peakgroups) == 0) {
        peakgroups <- data.frame(
            mzmed = peaks[, "mz"],
            mzmin = peaks[, "mz"],
            mzmax  = peaks[, "mz"],
            rtmed = peaks[, "rt"],
            rtmin = peaks[, "rt"],
            rtmax = peaks[, "rt"],
            npeaks = 1,
            `1` = peaks[, "feature_id"]
        )
    } else {
        # replace the value in peak_groups by the feature id
        # instead of the intensity
        peakgroups[, 8:ncol(peakgroups)] <- xcms::groupval(xset)
    }
    colnames(peakgroups)[8:ncol(peakgroups)] <- samples
    peakgroups <- cbind(
        group_id = seq(nrow(peakgroups)),
        cluster_id = seq(nrow(peakgroups)),
        iso = "M",
        peakgroups
    )
    peakgroups[xsa@isoID[, "isopeak"], "cluster_id"] <- xsa@isoID[, "mpeak"]
    peakgroups[xsa@isoID[, "isopeak"], "iso"] <- "M+*"

    spectra_id <- 1
    samples <- rownames(xset@phenoData)
    spectras <- data.frame(matrix(, nrow = 0, ncol = 15, dimnames = list(
        c(), c("spectra_id", "group_id", "feature_id", "mz", "mzmin", "mzmax",
               "rt", "rtmin", "rtmax", "int", "abd", "ion_id_theo", "mz_theo",
               "abd_theo", "iso_theo")
    )))
    spectra_infos <- data.frame(matrix(, nrow = 0, ncol = 9, dimnames = list(
        c(), c("spectra_id", "score", "deviation_mz", "npeak", "basepeak_mz",
               "basepeak_int", "sum_int", "sample", "rt")
    )))
    ann <- data.frame(
        matrix(, nrow = 0, ncol = 16 + length(samples), dimnames = list(
            c(), c("pcgroup_id", "basepeak_group_id", "class", "name",
                   "referent_adduct", "formula", "adduct", "ion_formula",
                   "rtdiff", "rt", "rtmin", "rtmax", "nsamples", "best_score",
                   "best_deviation_mz", "best_npeak", samples))
        ),
        check.names = FALSE
    )
    sample_matrix <- matrix(, nrow = 1, ncol = length(samples),
                            dimnames = list(c(), samples))

    # for each pcgroup
    for (i in seq(length(xsa@pspectra))) {

        pcgroup <- peakgroups[xsa@pspectra[[i]], , drop = FALSE]
        idx <- lapply(seq(nrow(pcgroup)), function(i) {
            which(
                chem_db$mz >= pcgroup[i, "mzmed"] - ann_params@da_tol &
                    chem_db$mz <= pcgroup[i, "mzmed"] + ann_params@da_tol &
                    chem_db$rt >= pcgroup[i, "rtmed"] - ann_params@rt_tol &
                    chem_db$rt <= pcgroup[i, "rtmed"] + ann_params@rt_tol
            )
        })
        if (any(lengths(idx) > 0)) {
            chem_db_match <- chem_db[unlist(idx), , drop = FALSE]
            da_tol_iso <- max(unlist(sapply(seq(length(idx)), function(j) {
                abs(chem_db[idx[[j]], "mz"] - pcgroup[j, "mzmed"])
            }))) * 10
        } else {
            chem_db_match <- chem_db[0, ]
            da_tol_iso <- ann_params@da_tol
        }

        # for cluster
        for (cluster in split(pcgroup, pcgroup$cluster)) {
            # create the same base for the annotation DataFrame for all
            # basepeaks
            if (!any(cluster$iso == "M")) {
                # something weird happen by CAMERA
                # reattribute iso ?
                cluster[which.min(cluster$mzmed), "iso"] <- "M"
            }
            basepeak <- cluster[cluster$iso == "M", ]
            tmp_ann <- cbind(
                rtdiff = NA,
                rt = basepeak$rtmed,
                rtmin = basepeak$rtmin,
                rtmax = basepeak$rtmax,
                nsamples = sum(!is.na(basepeak[, 11:ncol(pcgroup)])),
                best_score = 0,
                best_deviation_mz = NA,
                best_npeak = 0,
                sample_matrix
            )

            # any adduct identified by CAMERA ?
            hypo_adducts <- which(xsa@annoID[, "id"] == basepeak$group_id)
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
                q_spectra <- merge(
                    cluster[, c(1, j)],
                    peaks[cluster[, j],
                          c("feature_id", "mz", "mzmin", "mzmax", "rt", "rtmin",
                            "rtmax", "int"), drop = FALSE],
                    by.x = colnames(cluster)[j],
                    by.y = "feature_id"
                )
                colnames(q_spectra)[1] <- "feature_id"
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
                            tmp[[l]]$spectra[, c("group_id", "feature_id", "mz",
                                                 "mzmin", "mzmax", "rt",
                                                 "rtmin", "rtmax", "int", "abd",
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
                            basepeak_mz = basepeak$mz,
                            basepeak_int = basepeak$int,
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
            if (any(tmp_ann$best_score > 0)) {
                tmp_ann <- merge(
                    chem_db_match[, c("class", "name", "referent_adduct",
                                      "formula")],
                    tmp_ann,
                    by = "formula",
                    all = TRUE,
                    sort = FALSE
                )
                # tmp_ann is a full join so we need to retrieve foreach row
                    #  the corresponding cpd name in the chem_db_match
                tmp_ann$rtdiff <- abs(
                    chem_db_match[
                        match(tmp_ann$name, chem_db_match$name),
                        "rt"
                    ] - tmp_ann$rt
                )
            } else {
                tmp_ann <- cbind(
                    class = NA,
                    name = NA,
                    referent_adduct = NA,
                    tmp_ann
                )
            }
            tmp_ann <- cbind(
                pcgroup_id = i,
                basepeak_group_id = cluster[cluster$iso == "M", "group_id"],
                tmp_ann
            )
            ann <- rbind(ann, tmp_ann)
        }
    }
    ann[which(ann$best_npeak == 0),
        c("formula", "class", "name", "referent_adduct", "adduct",
          "ion_formula", "rtdiff")] <- NA

    # create the peak groups table which combine XCMS info & CAMERA
    peakgroups <- merge(
        merge(
            cbind(
                pcgroup_id = rep(
                    seq(length(xsa@pspectra)),
                    lengths(xsa@pspectra)
                ),
                group_id = unlist(xsa@pspectra)
            ),
            cbind(
                group_id = xsa@annoID[, "id"],
                adduct = adducts[xsa@annoID[, "ruleID"], "name"]
            ),
            by = "group_id",
            all = TRUE
        ),
        peakgroups,
        by = "group_id"
    )

    rownames(ann) <- NULL
    rownames(spectras) <- NULL

    list(
        ann = ann,
        spectra_infos = spectra_infos,
        spectras = spectras,
        peakgroups = peakgroups,
        peaks = as.data.frame(xset@peaks)
    )
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
#'     \item pcgroup_id `integer` pcgroup ID
#'     \item basepeak_group_id `ìnteger` group ID of the basepeak
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
#'         \item pcgroup_id `integer` pcgroup ID
#'         \item basepeak_group_id `ìnteger` group ID of the basepeak
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
#'         \item pcgroup_id `integer` pcgroup ID
#'         \item basepeak_group_id `ìnteger` group ID of the basepeak
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
    splitted_ann <- lapply(split(ann, ann$pcgroup_id), function(x) {
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
#'     \item pcgroup_id `integer` pcgroup ID
#'     \item basepeak_group_id `ìnteger` group ID of the basepeak
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
#'         \item PCGroup ID `numeric` pcgroup ID
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
#'         \item PCGroup ID `numeric` pcgroup ID
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
               c(), c("PCGroup ID", "Class", "Name", "rT (min)",
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
                    paste(int_ann[, "PCGroup ID"], int_ann$Name, sep = "/")
                ), function(x) {
                data.frame(
                    `PCGroup ID` = x[1, "PCGroup ID"],
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
        details = int_ann[, -which(colnames(int_ann) %in% c("Group ID",
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
#'     \item pcgroup_id `integer` pcgroup ID
#'     \item basepeak_group_id `ìnteger` group ID of the basepeak
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
#'     \item PCGroup ID `numeric` pcgroup ID
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
                c("PCGroup ID", "Group ID", "Class", "Name", "rT (min)",
                  "Diff rT (sec)", "Referent adduct", "Adduct", "nSamples",
                  "Best score (%)", "Best m/z dev (mDa)", "Max iso")
            )
        ), check.names = FALSE))
    }
    # extract intensity of basepeaks
    data.frame(
        `PCGroup ID` = as.factor(ann$pcgroup_id),
        `Group ID` = ann$basepeak_group_id,
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

#' @title Reintegrate compound
#'
#' @description
#' Reintegrate compound by giving a new rT range
#' It will reintegrate all isotopologues for all files for all ions for this
#'  compound. It will recreate spectra, peakgroups & all new entries.
#'
#' @param db `SQLiteConnection`
#' @param cpd_name `character(1)` compound name
#' @param rtmin `numeric(1)` rT born min (in sec)
#' @param rtmax `numeric(1)` rT born max (in sec)
#'
#' @return `DataFrame list` with items :
#' \itemize{
#'     \item ann `DataFrame` each line correspond to a compound found
#'     with the columns:
#'     \itemize{
#'         \item pcgroup_id `integer` group ID
#'         \item basepeak_group_id `integer` EIC ID
#'         \item class `character` cpd class
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
#'     its corresponding theoretical peak or the theoretical peak missed,
#'     with the columns :
#'         \itemize{
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
#'         \item basepeak_mz `numeric` m/z of the basepeak annotated
#'         \item basepeak_int `numeric` area of the basepeak annotated
#'         \item sum_int `numeric` cumulative sum off all the area of the
#'         isotopologues annotated
#'         \item rt `numeric` retention time
#'     }
#'     \item peakgroups `DataFrame`, each line correspond to a group annotated
#'     by XCMS and CAMERA
#'     \itemize{
#'         \item group_id `integer` group ID
#'         \item pcgroup_id `integer` pcgroup ID
#'         \item adduct `character` adduct annotation by CAMERA (NULL if absent)
#'         \item cluster_id `integer` cluster ID
#'         \item iso `character` could be "M" or "M+*"
#'         \item mzmed `float` m/z median computed by XCMS
#'         \item mzmin `float` m/z median born min (not the m/z born min !!)
#'         \item mzmax `float` m/z median born max (not the m/z born max !!)
#'         \item rtmed `float` rT median computed by XCMS
#'         \item rtmin `float` rT median born min (not the rT born min !!)
#'         \item rtmax `float` rT median born max (not the rT born max !!)
#'         \item npeaks `integer` number of peaks grouped accross samples
#'         \item ... `integer` a column for each sample which contain the
#'         feature ID (row ID from the peaktable)
#'     }
#'     \item peaks `DataFrame`, peaktable from XCMS
#'     \itemize{
#'         \item mz `float` m/z
#'         \item mzmin `float` m/z born min
#'         \item mzmax `float` m/z born max
#'         \item rt `float` rT
#'         \item rtmin `float` rT born min
#'         \item rtmax `float` rT born max
#'         \item into `float` area of the peak
#'         \item intb `float` area of the peak above baseline
#'         \item maxo `float` maximum intensity
#'         \item sn `float` signal/noise
#'         \item egauss `float` ignore
#'         \item mu `float` ignore
#'         \item sigma `float` ignore
#'         \item h `float` ignore
#'         \item f `integer` ID of the ROI
#'         \item dppm `float` ppm deviation
#'         \item scale `integer` width of the wave used for the peak detection
#'         \item scpos `integer` scan ID
#'         \item scmin `integer` scan ID born min of the wave detection
#'         \item scmax `integer` scan ID born max of the wave detection
#'         \item lmin `integer` scan ID after extension of the scmin
#'         \item lmax `integer` scan ID after extension of the scmax
#'     }
#' }
#'
#' @export
reintegrate_ann <- function(db, cpd_name, rtmin, rtmax) {

    if (class(db) != "SQLiteConnection") {
        stop("db must be a connection to the sqlite database")
    } else if (length(cpd_name) > 1) {
        stop("only one compound per reintegration !")
    } else if (!is.character(cpd_name)) {
        stop("cpd_name must be represent a compound name")
    } else if (length(rtmin) > 1) {
        stop("only one rtmin is required")
    } else if (!is.numeric(rtmin)) {
        stop("rtmin must be a numeric")
    } else if (length(rtmax) > 1) {
        stop("only one rtmax is required")
    } else if (!is.numeric(rtmax)) {
        stop("rtmax must be a numeric")
    } else if (rtmin >= rtmax) {
        stop("rtmax must be upper than rtmin")
    }

    # get the corresponding annotation line
    ann <- db_get_annotations(db, names = cpd_name)
    if (nrow(ann) == 0) {
        stop("no annotation with this name was recorded in the database")
    }
    nsamples <- db_get_nsamples(db)
    ann$pcgroup_id <- NA
    ann$nsamples <- 0

    process_params <- db_get_params(db)
    theoretical_rt <- load_chem_db(
        process_params$ann$database,
        process_params$ann$polarity,
        cpd_names = ann[, "name"]
    )$rt
    max_ids <- db_get_max_ids(db)
    peaks <- spectras <- spectra_infos <- peakgroups <- data.frame()

    ### For each annotation
    for (i in seq(nrow(ann))) {
        new_peakgroups <- db_get_peakgroups(
            db,
            group_ids = db_get_group_ids(
                db,
                spectra_ids = na.omit(unlist(
                    ann[i, (ncol(ann) - nsamples + 1):ncol(ann)]))
            )
        )
        new_peakgroups$npeaks <- 0
        peakgroups_mz <- rep(list(c()), nrow(new_peakgroups))
        peakgroups_rt <- rep(list(c()), nrow(new_peakgroups))
        new_spectra_infos <- new_spectras <- data.frame()

        # recompute the theoretical ion
        l_spectra <- get_ions(
            ann[i, "formula"],
            adducts[adducts$name == ann[i, "adduct"], ],
            process_params$ann$instrument
        )[[1]]
        l_spectra <- cbind(
            l_spectra,
            data.frame(matrix(
                get_mz_range(l_spectra$mz, process_params$cwt$ppm),
                nrow = nrow(l_spectra), ncol = 2, dimnames = list(
                    c(), c("mzmin", "mzmax"))))
        )

        #### For each spectra
        for (j in (ncol(ann) - nsamples + 1):ncol(ann)) {
            # check if any spectra was recorded for this sample
            if (!is.na(ann[i, j])) {
                q_spectra <- db_get_spectras(db, ann[i, j])
                q_spectra <- q_spectra[!is.na(q_spectra$feature_id),
                                       c("group_id", "feature_id", "mz",
                                         "mzmin", "mzmax", "rt", "rtmin",
                                         "rtmax", "int")]
            } else {
                q_spectra <- NULL
            }
            new_peaks <- new_q_spectra <- data.frame()

            # for each peak in peakgroups
            for (k in seq(nrow(new_peakgroups))) {
                # first: peak does exist already ?
                feature_row <- which(q_spectra$feature_id ==
                                         new_peakgroups[k, j - ncol(ann) +
                                                            nsamples + 12])
                if (length(feature_row) == 1) {
                    # if this is a isotopologue & the precedent range rT was
                    # already smallest than the one selected we will no enlarge
                    # the integration area (only the basepeak integration area
                    # is considered)
                    if (
                        new_peakgroups[k, "iso"] != "M" &&
                        q_spectra[feature_row, "rtmin"] >= rtmin &&
                        q_spectra[feature_row, "rtmax"] <= rtmax
                    ) {
                        new_q_spectra <- rbind(
                            new_q_spectra,
                            q_spectra[feature_row, ]
                        )
                        new_peakgroups[k, "npeaks"] <- new_peakgroups[k,
                                                                  "npeaks"] + 1
                        peakgroups_mz[[k]] <- c(
                            peakgroups_mz[[k]],
                            q_spectra[feature_row, "mz"]
                        )
                        peakgroups_rt[[k]] <- c(
                            peakgroups_rt[[k]],
                            q_spectra[feature_row, "rt"]
                        )
                        next
                    }
                }

                # reintegrate & create a new peak
                eic <- db_get_eic(db, group_id = new_peakgroups[k, "group_id"])
                mzmat <- db_get_mzmat(
                    db,
                    group_id = new_peakgroups[k, "group_id"]
                )
                # select the eic between the range selected & for the sample
                # more accurate to integrate with scanrange at -1/+1
                scmin <- max(1, which(eic$rt >= rtmin)[1] - 1)
                scmax <- min(nrow(eic), rev(which(eic$rt <= rtmax))[1] + 1)
                eic <- eic[scmin:scmax, c(1, j - ncol(ann) + nsamples + 1)]
                mzmat <- mzmat[scmin:scmax, c(1, j - ncol(ann) + nsamples + 1)]
                if (all(eic[, 2] == 0)) {
                    new_peakgroups[k, j - ncol(ann) + nsamples + 12] <- NA
                    next
                }

                scan_pos <- which.max(eic[, 2])
                fwhm <- (rtmax - rtmin) / 6 * 2.35 * .6 / 2
                scans <- c(
                    max(1, floor(scan_pos - fwhm)),
                    min(nrow(eic), ceiling(scan_pos + fwhm))
                )
                new_peak <- data.frame(
                    mz = xcms:::mzCenter.wMean(
                        mzmat[scans[1]:scans[2], 2],
                        eic[scans[1]:scans[2], 2]
                    ),
                    mzmin = min(
                        mzmat[scans[1]:scans[2], 2],
                        na.rm = TRUE
                    ),
                    mzmax = max(
                        mzmat[scans[1]:scans[2], 2],
                        na.rm = TRUE
                    ),
                    rt = eic[scan_pos, "rt"],
                    rtmin = rtmin,
                    rtmax = rtmax,
                    into = diff(range(eic$rt)) / nrow(eic) * sum(eic[, 2]),
                    intb = NA,
                    maxo = max(eic[, 2]),
                    sn = NA,
                    egauss = NA,
                    mu = NA,
                    sigma = NA,
                    h = NA,
                    f = NA,
                    dppm = NA,
                    scale = NA,
                    scpos = NA,
                    scmin = NA,
                    scmax = NA,
                    lmin = NA,
                    lmax = NA,
                    sample = j - ncol(ann) + nsamples
                )
                new_peaks <- rbind(new_peaks, new_peak)

                # add the newly peak in q_spectra
                new_q_spectra <- rbind(
                    new_q_spectra,
                    data.frame(
                        group_id = max_ids["group"] + k,
                        feature_id = max_ids["feature"] + k,
                        new_peak[, c("mz", "mzmin", "mzmax", "rt", "rtmin",
                                     "rtmax")],
                        int = new_peak[["into"]]
                    )
                )

                # dont forget to update peakgroup with the new feature ID
                new_peakgroups[k, j - ncol(ann) + nsamples + 12] <-
                    max_ids["feature"] + k
                new_peakgroups[k, "npeaks"] <- new_peakgroups[k, "npeaks"] + 1
                peakgroups_mz[[k]] <- c(peakgroups_mz[[k]], new_peak$mz)
                peakgroups_rt[[k]] <- c(peakgroups_rt[[k]], new_peak$rt)

            }
            if (nrow(new_q_spectra) == 0) {
                ann[i, j] <- NA
            } else {
                peaks <- rbind(peaks, new_peaks)
                new_q_spectra$abd <- new_q_spectra$int /
                    new_q_spectra[1, "int"] * 100
                # recompute score ...
                tmp <- compare_spectras(
                    new_q_spectra,
                    list(l_spectra[, c("mz", "abd", "iso")]),
                    process_params$ann$da_tol,
                    process_params$ann$abd_tol
                )[[1]]
                if (tmp$score == 0) {
                    ann[i, j] <- NA
                } else {
                   # record new spectra & peaks
                    new_spectras <- rbind(
                        new_spectras,
                        suppressWarnings(cbind(
                            spectra_id = max_ids["spectra"] + 1,
                            tmp$spectra
                        ))
                    )
                    new_spectra_infos <- rbind(
                        new_spectra_infos,
                        data.frame(
                            spectra_id = max_ids["spectra"] + 1,
                            score = tmp$score,
                            deviation_mz = tmp$deviation_mz,
                            npeak = tmp$npeak,
                            basepeak_mz = tmp$spectra[1, "mz"],
                            basepeak_int = tmp$spectra[1, "int"],
                            sum_int = sum(tmp$spectra[
                                which(!is.na(tmp$spectra$mz) &
                                          !is.na(tmp$spectra$mz_theo)),
                                "int"]),
                            sample = colnames(ann)[j],
                            rt = tmp$spectra[1, "rt"]
                        )
                    )
                    ann[i, j] <- max_ids[["spectra"]] + 1
                    ann[i, "nsamples"] <- ann[i, "nsamples"] + 1

                    # dont forget to update ids
                    max_ids["feature"] <- max_ids[["feature"]] +
                        nrow(new_peakgroups)
                    max_ids["spectra"] <- max_ids["spectra"] + 1
                }
            }
        }

        # if no integration at all for all peaks for the annotation row !
        if (all(new_peakgroups$npeaks == 0)) {
            next
        }

        # modify peakgroups
        new_peakgroups <- new_peakgroups[lengths(peakgroups_mz) > 0, ]
        peakgroups_mz <- peakgroups_mz[lengths(peakgroups_mz) > 0]
        peakgroups_rt <- peakgroups_rt[lengths(peakgroups_rt) > 0]
        # we need to copy eic & mzmat for the new group id formed
        db_copy_eic_mzmat(
            db,
            samples = colnames(ann)[(ncol(ann) - nsamples + 1):ncol(ann)],
            old_group_ids = new_peakgroups$group_id,
            new_group_ids = max_ids["group"] + seq(nrow(new_peakgroups))
        )
        new_peakgroups$group_id <- max_ids["group"] + seq(nrow(new_peakgroups))
        new_peakgroups$pcgroup_id <- max_ids["pcgroup"] + 1
        new_peakgroups$adduct <- NA
        new_peakgroups$cluster_id <- max_ids["cluster"] + 1
        new_peakgroups$mzmed <- sapply(peakgroups_mz, median)
        new_peakgroups$rtmed <- sapply(peakgroups_rt, median)
        new_peakgroups$mzmin <- sapply(peakgroups_mz, min)
        new_peakgroups$mzmax <- sapply(peakgroups_mz, max)
        new_peakgroups$rtmin <- sapply(peakgroups_rt, min)
        new_peakgroups$rtmax <- sapply(peakgroups_rt, max)
        peakgroups <- rbind(peakgroups, new_peakgroups)

        # modify ann
        ann[i, "pcgroup_id"] <- max_ids["pcgroup"] + 1
        ann[i, "basepeak_group_id"] <- new_peakgroups[1, "group_id"]
        if (max(new_spectra_infos$score) > 0) {
            ann[i, "rt"] <- median(new_spectras[which(new_spectras$abd == 100),
                                                "rt"])
            ann[i, "rtmin"] <- min(new_spectras[which(new_spectras$abd == 100),
                                                "rtmin"])
            ann[i, "rtmax"] <- max(new_spectras[which(new_spectras$abd == 100),
                                                "rtmax"])
            spectra_infos <- rbind(spectra_infos, new_spectra_infos)
            spectras <- rbind(spectras, new_spectras)
            best_spectra <- which.max(new_spectra_infos$score)
            ann[i, "rtdiff"] <- min(abs(theoretical_rt -
                    new_spectra_infos[best_spectra, "rt"]))
            ann[i, c("best_score", "best_deviation_mz", "best_npeak")] <-
                new_spectra_infos[best_spectra,
                                  c("score", "deviation_mz", "npeak")]
        } else {
            ann[i, "rt"] <- (rtmax - rtmin) / 2 + rtmin
            ann[i, "rtmin"] <- rtmin
            ann[i, "rtmax"] <- rtmax
            ann[i, c("rtdiff", "best_score", "best_deviation_mz",
                     "best_npeak")] <- NA
        }

        # dont forget to update ids
        max_ids["cluster"] <- max_ids[["cluster"]] + 1
        max_ids["group"] <- max_ids["group"] + nrow(new_peakgroups)
    }

    db_replace_ann(
        db,
        cpd_name,
        ann = ann,
        spectra_infos = spectra_infos,
        spectras = spectras,
        peaks = peaks,
        peakgroups = peakgroups
    )
    list(
        ann = ann,
        spectra_infos = spectra_infos,
        spectras = spectras,
        peaks = peaks,
        peakgroups = peakgroups
    )
}
