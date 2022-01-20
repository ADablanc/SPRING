#' @title Get m/z of database
#'
#' @description
#' Get all m/z & relative abundance for the internal database
#'
#' @param adduct_names vector of adduct names present in the enviPat list
#' @param instrument instrument name for enviPat
#'
#' @return DataFrame with 
#' \itemize{
#'      \item id integer ID
#'      \item formula chemical formula 
#'      \item name of the compound
#'      \item rt retention time of the compound
#'      \item adduct adduct name 
#'      \item ion_formula ion chemical formula 
#'      \item charge charge of the ion 
#'      \item mz m/z 
#'      \item abd relative abundance 
#'      \item iso isotopologue annotation 
#' }
load_db_ions <- function(adduct_names,  instrument) {
    db <- utils::read.csv(system.file("extdata", "database.csv", 
        package = "workflow.lipido"))
    ions <- do.call(rbind, lapply(adduct_names, function(adduct_name) 
        get_ions(
            unique(db$formula), 
            adducts[which(adducts$Name == adduct_name), ], 
            instrument)))
    db_ions <- merge(db, ions, all = TRUE)
    db_ions <- db_ions[!is.na(db_ions$mz), , drop = FALSE]
    if (nrow(db_ions) == 0) return(db_ions)
    db_ions <- cbind(id = as.numeric(as.factor(
        paste(db_ions$name, db_ions$adduct, sep = "_"))), db_ions)
    return(db_ions[order(db_ions$id), ])
}

#' @title Get m/z
#'
#' @description
#' Get all m/z & relative abundance for a set of formulas
#'
#' @param forms vector with chemical formulas
#' @param adduct subset of the adducts dataframe from enviPat
#'      it needs the columns : 
#'      \itemize{
#'          item Name name of the adduct
#'          item Mult the multiplicator to apply
#'          item Charge charge of the adduct
#'          item Formula_add the chemical formula to add
#'          item Formula_ded the chemical formula to deduct
#'      }
#' @param instrument instrument name for enviPat
#'
#' @return DataFrame with 
#' \itemize{
#'      \item formula chemical formula 
#'      \item adduct adduct name 
#'      \item ion_formula ion chemical formula 
#'      \item charge charge of the ion 
#'      \item mz m/z 
#'      \item abd relative abundance 
#'      \item iso isotopologue annotation 
#' }
get_ions <- function(forms, adduct, instrument) {
    ion_forms <- forms
    if (adduct$Mult > 1) ion_forms <- enviPat::multiform(ion_forms, 
        adduct$Mult)
    if (adduct$Formula_add != "FALSE") ion_forms <- enviPat::mergeform(
        ion_forms, adduct$Formula_add)
    if (adduct$Formula_ded != "FALSE") {
        test <- enviPat::check_ded(ion_forms, adduct$Formula_ded)
        if (any(test == FALSE)) {
            forms <- forms[test == FALSE]
            ion_forms <- enviPat::subform(ion_forms[test == FALSE], 
                adduct$Formula_ded)
        } else return(data.frame(matrix(, nrow = 0, ncol = 6, 
            dimnames = list(c(), c("formula", "adduct", "ion_formula", 
            "charge", "mz", "abd")))))
    }
    ion_forms <- enviPat::check_chemform(isotopes, ion_forms)
    resmass <- resolution_list[[which(names(resolution_list) == instrument)]]
    out_resmass <- which(ion_forms$monoisotopic_mass < min(resmass[, "m/z"]) | 
        ion_forms$monoisotopic_mass > max(resmass[, "m/z"]))
    if (length(out_resmass) == length(forms)) return(data.frame(
        matrix(, nrow = 0, ncol = 6, dimnames = list(c(), 
        c("formula", "adduct", "ion_formula", "charge", "mz", "abd")))))
    else if (length(out_resmass) > 0) {
        forms <- forms[-out_resmass]
        ion_forms <- ion_forms[-out_resmass, ]
    }	
    invisible(utils::capture.output(isotopic_profiles <- enviPat::isowrap(
        isotopes, ion_forms, resmass = resmass, charge = adduct$Charge)))
    ions <- do.call(rbind, lapply(seq(isotopic_profiles), function(i) 
        data.frame(
            formula = forms[i], 
            adduct = adduct$Name, 
            ion_formula = ion_forms[i, "new_formula"], 
            charge = adduct$Charge, 
            mz = round(isotopic_profiles[[i]][, "m/z"], 5), 
            abd = round(isotopic_profiles[[i]][, "abundance"], 2), 
            iso = paste0("M+", seq(nrow(isotopic_profiles[[i]])) - 1)
        )
    ))
    ions[ions$iso == "M+0", "iso"] <- "M"
    return(ions)
}

#' @title Compare spectras
#' 
#' @description
#' Compare a query spectra against library spectras
#'
#' @param q_spectra dataframe with columns : 
#'      \itemize{
#'          \item mz m/z
#'          \item int or abd for intensity or relative abundance tolerance
#'      }
#' @param l_spectras list of dataframe with columns :  
#'      \itemize{
#'          \item mz m/z
#'          \item int or abd for intensity or relative abundance tolerance
#'      }
#' @param da_tol Da tolerance
#' @param abd_tol abundance tolerance, at which \% tolerance a peak need to be
#' @param suffix which suffix to apply to 
#'      the column names of the library spectras
#'
#' @return list with : 
#'      \itemize{
#'          \item score the isotopic score between query & library spectra
#'          \item deviation_mz the meanned m/z deviation
#'          \item npeak number of peaks matched in query spectra
#'          \item spectras list of dataframe consisting of 
#'              the merge of the query spectra with the corresponding lines 
#'              of the library spectra
#'      }
#'      each item is repeated x times, 
#'          corresponding to the number of library spectras
compare_spectras <- function(q_spectra, l_spectras, 
        da_tol = 0.05, abd_tol = 25, suffix = "theo") {
    res <- align_spectras(q_spectra, l_spectras, da_tol, abd_tol)
    res$spectras <- lapply(seq(res$idx), function(i) {
        colnames(l_spectras[[i]]) <- paste(colnames(l_spectras[[i]]), 
            suffix, sep = "_")
        suppressWarnings(cbind(
            q_spectra[res$idx[[i]]$q_id, , drop = FALSE], 
            l_spectras[[i]][res$idx[[i]]$l_id, ]
        ))
    })
    res[-1]
}

#' @title Annotate peaklists
#' 
#' @description 
#' Annotate peaklists from an XCMSnExp with grouping information
#' 
#' @param ms_files XCMSnExp
#' @param ann_params AnnotationParameter object
#' @param show_pb boolean print progression bar
#' @param sigma ignore
#' @param perfwhm ignore
#' 
#' @return a XCMSnExp
annotate_peaklists <- function(ms_files, ann_params, show_pb, 
        sigma = 6, perfwhm = .6) {
    db_ions <- load_db_ions(ann_params@adduct_names, ann_params@instrument)
    # the rt in database is in min !!
    db_ions$rt <- db_ions$rt * 60
    l_spectras <- split(db_ions, db_ions$id)
    l_monoisotopic <- db_ions[db_ions$abd == 100, ]
    
    peaks <- xcms::chromPeaks(ms_files)
    peaks <- cbind(feature_id = seq(nrow(peaks)), peaks)
    peaks_grouped <- xcms::featureDefinitions(ms_files)
    if (nrow(peaks_grouped) > 0) peaks_grouped <- peaks_grouped[order(
        peaks_grouped$mzmed, peaks_grouped$rtmed), ] 
    else peaks_grouped <- data.frame(mzmed = peaks[, "mz"], 
        rtmed = peaks[, "rt"], peakidx = seq(nrow(peaks)))
    
    matched_peaks <- cbind(peaks[0, ], db_ions[0, ])
    cluster <- 0
    if (show_pb) pb <- utils::txtProgressBar(min = 0, 
        max = nrow(peaks_grouped), style = 3)
    for (i in seq(nrow(peaks_grouped))) {
        if (show_pb) utils::setTxtProgressBar(pb, i)
        # dont know why there is duplication here ???
        l_monoisotopic_id <- unique(l_monoisotopic[which(
            l_monoisotopic$mz >= peaks_grouped[i, "mzmed"] - 
                ann_params@da_tol & 
            l_monoisotopic$mz <= peaks_grouped[i, "mzmed"] + 
                ann_params@da_tol & 
            l_monoisotopic$rt >= peaks_grouped[i, "rtmed"] - 
                ann_params@rt_tol & 
            l_monoisotopic$rt <= peaks_grouped[i, "rtmed"] + 
                ann_params@rt_tol), 
            "id"])
        if (length(l_monoisotopic_id) == 0) next
        
        for (j in peaks_grouped[i, "peakidx"][[1]]) {
            # get all isotopologues
            fwhm <- abs(peaks[j, "rtmax"] - peaks[j, "rtmin"]) / 
                sigma * 2.35 * perfwhm
            q_spectra <- peaks[
                peaks[, "mz"] >= peaks[j, "mz"] - 6 &  
                peaks[, "mz"] <= peaks[j, "mz"] + 6 & 
                peaks[, "rt"] >= peaks[j, "rt"] - fwhm & 
                peaks[, "rt"] <= peaks[j, "rt"] + fwhm & 
                peaks[, "sample"] == peaks[j, "sample"], , drop = FALSE]
            q_spectra <- cbind(q_spectra, 
                abd = q_spectra[, "into"] / peaks[j, "into"] * 100)
            q_spectra <- q_spectra[order(
                abs(q_spectra[, "rt"] - peaks[j, "rt"])), ]
                
            tmp <- compare_spectras(
                q_spectra, 
                l_spectras[l_monoisotopic_id], 
                ann_params@da_tol, 
                ann_params@abd_tol)
            k <- which(tmp$score > 0)
            if (length(k) == 0) next
            tmp <- lapply(tmp, function(x) x[k])
            spectras <- do.call(rbind, 
                lapply(seq(length(tmp$score)), function(k) 
                    cbind(
                        cluster = cluster + k, 
                        tmp$spectras[[k]],
                        score = tmp$score[k], 
                        npeaks = tmp$npeak[k]
                    )
                ))
            spectras <- spectras[!is.na(spectras$mz) & 
                !is.na(spectras$mz_theo), , drop = FALSE]
            spectras <- cbind(spectras, 
                mz_dev = abs(spectras$mz - spectras$mz_theo), 
                rt_dev = abs(spectras$rt - spectras$rt_theo)
            )
            matched_peaks <- rbind(matched_peaks, spectras)
            cluster <- cluster + length(k)
        }
    }
    
    # eject the annotations where the same compound is not at the same rT
    # if (nrow(matched_peaks) > 0) {
        # matched_monoisotopics <- matched_peaks[
            # matched_peaks$abd_theo == 100, , drop = FALSE]
        # matched_monoisotopics <- matched_monoisotopics[order(
            # -matched_monoisotopics$npeaks, 
            # -matched_monoisotopics$score, 
            # matched_monoisotopics$rt_dev, 
            # matched_monoisotopics$mz_dev), ]
        # cluster_ids <- unlist(lapply(
            # split(matched_monoisotopics, 
                # matched_monoisotopics$name_theo), function(x) {
            # best_peak <- x[1, , drop = FALSE]
            # fwhm <- (abs(best_peak$rtmax - best_peak$rtmin) / 
                # sigma * 2.35 * perfwhm)
            # x <- x[x$rt >= best_peak$rt - fwhm & 
                # x$rt <= best_peak$rt + fwhm, , drop = FALSE]
            # x$cluster
        # }))
        # matched_peaks <- matched_peaks[
            # matched_peaks$cluster %in% cluster_ids, , drop = FALSE]
    # }
    peaks <- peaks[which(
        !peaks[, "feature_id"] %in% matched_peaks$feature_id), , drop = FALSE]
    if (nrow(peaks) == 0) attributes(ms_files)$ann <- rbind(
        matched_peaks, peaks)
    else {
        peaks <- cbind(cluster = NA, peaks, abd = NA, id_theo = NA, 
            formula_theo = NA, name_theo = NA, rt_theo = NA, adduct_theo = NA, 
            ion_formula_theo = NA, charge_theo = NA, mz_theo = NA, 
            abd_theo = NA, iso_theo = NA, score = NA, npeaks = NA, 
            mz_dev = NA, rt_dev = NA)
        attributes(ms_files)$ann <- rbind(matched_peaks, peaks)
    }
    return(ms_files)
}
