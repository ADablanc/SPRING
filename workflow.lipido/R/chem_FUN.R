#' @title Load cpd database
#'
#' @description
#' Load chemical database
#' @param database `character(1)` database name
#'
#' @return `DataFrame` with columns:
#' \itemize{
#'    \item class `character` compound class
#'    \item name `character` compound name
#'    \item formula `character` chemical formula
#'    \item rt `numeric` retention time
#' }
load_chem_db <- function(database) {
    if (!database %in% get_available_database()) {
        stop(sprintf(
            "%s doesn't exist in software",
            database
        ))
    }
    database <- utils::read.csv(system.file(
        "extdata",
        "database",
        paste(database, "csv", sep = "."),
        package = "workflow.lipido"
    ))
    if (nrow(database) == 0) {
        stop("no compound in database")
    }
    database
}

#' @title Get all ions of chemical database
#'
#' @description
#' Get all m/z & relative abundance for the internal database
#'
#' @param adduct_names `character vector`vector of adduct names present in the
#' enviPat list
#' @param instrument `character(1)` instrument name for enviPat
#' @param database `character(1)` name of the database to load
#' @param cpd_classes `character vector` compound classes to restrict
#' @param cpd_names name of compounds to restrict
#'
#' @return DataFrame with
#' \itemize{
#'      \item formula chemical formula
#'      \item class class of the compound
#'      \item name of the compound
#'      \item rt retention time of the compound
#'      \item ion_id id unique for an entry of the database + adduct associated
#'      \item adduct adduct name
#'      \item ion_formula ion chemical formula
#'      \item charge charge of the ion
#'      \item mz m/z
#'      \item abd relative abundance
#'      \item iso isotopologue annotation
#' }
load_ion_db <- function(adduct_names,
                    instrument,
                    database,
                    cpd_classes = NULL,
                    cpd_names = NULL) {
    chem_db <- load_chem_db(database)
    if (nrow(chem_db) == 0) {
        stop("no compound available in database !")
    }
    # the rt in the database is in minutes !!
    chem_db$rt <- chem_db$rt * 60
    if (!is.null(cpd_classes)) {
        chem_db <- chem_db[chem_db$class %in% cpd_classes, , drop = FALSE]
    }
    if (!is.null(cpd_names)) {
        chem_db <- chem_db[chem_db$name %in% cpd_names, , drop = FALSE]
    }
    if (nrow(chem_db) == 0 || length(adduct_names) == 0) {
        return(data.frame(matrix(, nrow = 0, ncol = 11, dimnames = list(
            c(), c("class", "formula", "name", "rt", "ion_id", "adduct",
                   "ion_formula", "charge", "mz", "abd", "iso")
        ))))
    }
    ions <- do.call(
        rbind,
        lapply(adduct_names, function(adduct_name)
            get_ions(
                unique(chem_db$formula),
                adducts[which(adducts$name == adduct_name), ],
                instrument
            )
        )
    )
    ions <- cbind(
        ion_id = as.numeric(as.factor(ions$ion_formula)),
        ions,
        order_id = seq(nrow(ions))
    )
    chem_db <- merge(chem_db, ions, by = "formula", all = TRUE)
    chem_db <- chem_db[order(chem_db$name, chem_db$order_id), ]
    chem_db[!is.na(chem_db$mz), -ncol(chem_db), drop = FALSE]
}

#' @title Get ions
#'
#' @description
#' Get all m/z & relative abundance for a set of formulas
#'
#' @param forms vector with chemical formulas
#' @param adduct subset of the adduct dataframe from enviPat
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
get_ions <- function(forms,
                     adduct,
                     instrument) {
    default_df <- data.frame(matrix(, nrow = 0, ncol = 6, dimnames = list(c(),
        c("formula", "adduct", "ion_formula", "charge", "mz", "abd"))))
    ion_forms <- forms
    if (adduct$nmol > 1) {
        ion_forms <- enviPat::multiform(ion_forms, adduct$nmol)
    }
    if (adduct$formula_add != "FALSE") {
        ion_forms <- enviPat::mergeform(ion_forms, adduct$formula_add)
    }
    if (adduct$formula_ded != "FALSE") {
        test <- enviPat::check_ded(ion_forms, adduct$formula_ded)
        if (any(test == FALSE)) {
            forms <- forms[test == FALSE]
            ion_forms <- enviPat::subform(
                ion_forms[test == FALSE],
                adduct$formula_ded
            )
        } else {
            return(default_df)
        }
    }
    ion_forms <- enviPat::check_chemform(isotopes, ion_forms)
    resmass <- resolution_list[[which(names(resolution_list) == instrument)]]
    out_resmass <- which(
        ion_forms$monoisotopic_mass < min(resmass[, "m/z"]) |
        ion_forms$monoisotopic_mass > max(resmass[, "m/z"])
    )
    if (length(out_resmass) == length(forms)) {
        return(default_df)
    }
    else if (length(out_resmass) > 0) {
        forms <- forms[-out_resmass]
        ion_forms <- ion_forms[-out_resmass, ]
    }
    invisible(utils::capture.output(
        isotopic_profiles <- enviPat::isowrap(
            isotopes,
            ion_forms,
            resmass = resmass,
            charge = adduct$charge
        )
    ))
    ions <- do.call(
        rbind,
        lapply(seq(isotopic_profiles), function(i)
            data.frame(
                formula = forms[i],
                adduct = adduct$name,
                ion_formula = ion_forms[i, "new_formula"],
                charge = adduct$charge,
                mz = round(isotopic_profiles[[i]][, "m/z"], 5),
                abd = round(isotopic_profiles[[i]][, "abundance"], 2),
                iso = paste0("M+", seq(nrow(isotopic_profiles[[i]])) - 1)
            )
        )
    )
    ions[ions$iso == "M+0", "iso"] <- "M"
    ions
}

#' @title Compare spectras
#'
#' @description
#' Compare query spectras against library spectras
#' The scoring algorithm will search each corresponding observed peak
#'      with theoreticals
#' Therefore it contains some important rules :
#'      \itemize{
#'          \item an observed peak can only correspond to ONE theoretical peak
#'              and vice versa
#'          \item the relative abundance peak must not be under a tolerance
#'              compared to the theoretical
#'              but it can be higher since a peak can hide another
#'          \item the A+x is not searched if the A+x-1 is not found
#'              (the loop search is stopped)
#'      }
#'
#' @param q_spectra dataframe with columns (at least) :
#'      \itemize{
#'          \item mz m/z
#'          \item int or abd for intensity or relative abundance tolerance
#'      }
#' @param l_spectras list of dataframe with columns (at least):
#'      \itemize{
#'          \item mz m/z
#'          \item int or abd for intensity or relative abundance tolerance
#'      }
#' @param da_tol Da tolerance
#' @param abd_tol abundance tolerance, at which \% tolerance a peak need to be
#' @param suffix which suffix to apply to
#'      the column names of the library spectras
#'
#' @return list (for each library spectra) wich contains a list with :
#'      \itemize{
#'          \item score the isotopic score between query & library spectra
#'          \item deviation_mz the meanned m/z deviation
#'          \item npeak number of peaks matched in query spectra
#'          \item spectras list of dataframe consisting of
#'              the merge of the query spectra with the corresponding lines
#'              of the library spectra
#'      }
compare_spectras <- function(q_spectra,
                             l_spectras,
                             da_tol = 0.05,
                             abd_tol = 25,
                             suffix = "theo") {
    tmp <- align_spectras(q_spectra, l_spectras, da_tol, abd_tol)
    lapply(seq(l_spectras), function(i) {
        l_spectra <- l_spectras[[i]]
        colnames(l_spectra) <- paste(colnames(l_spectra), suffix, sep = "_")
        list(
            spectra = suppressWarnings(cbind(
                q_spectra[tmp$idx[[i]]$q_id, , drop = FALSE],
                l_spectra[tmp$idx[[i]]$l_id, , drop = FALSE]
            )),
            score = tmp$score[i],
            deviation_mz = tmp$deviation_mz[i],
            npeak = tmp$npeak[i]
        )
    })
}

#' @title Get EIC
#'
#' @description
#' Get EIC data for an xcmsRaw object
#' It override the rawEIC function from XCMS in order to return rt instead of
#' scans & to use the slot `scantime_corrected` obtain from the custom function
#' `obiwarp`
#' If the m/z range or rT range is outside from the one from the file it will
#' return a dataframe with intensities at 0
#'
#' @param ms_file xcmsRaw object
#' @param mz_range numeric(2) containing the m/z range to look for
#' @param rt_range numeric(2) containing the rt range in sec to look for
#'
#' @return dataframe with the columns:
#' \itemize{
#'     \item rt retention time in seconds
#'     \item int intensity measured
#' }
get_eic <- function(ms_file, mz_range, rt_range) {
    if (is.null(ms_file)) {
        return(data.frame(rt = 0, int = 0))
    }
    if (
            mz_range[1] >= ms_file@mzrange[2] ||
            mz_range[2] <= ms_file@mzrange[1] ||
            rt_range[1] >= range(ms_file@scantime)[2] ||
            rt_range[2] <= ms_file@scantime[1]
        ) {
        return(data.frame(rt = seq(rt_range[1], rt_range[2]), int = 0))
    }
    eic <- xcms::rawEIC(ms_file, mzrange = mz_range, rtrange = rt_range)
    if (any(names(attributes(ms_file)) == "scantime_corrected")) {
        data.frame(
            rt = ms_file@scantime_corrected[eic$scan],
            int = eic$intensity
        )
    } else {
        data.frame(
            rt = ms_file@scantime[eic$scan],
            int = eic$intensity
        )
    }
}

#' @title Get m/z deviation
#'
#' @description
#' Get m/z deviations for an xcmsRaw object
#' It override the rawMat function from XCMS in order to return rt corrected by
#' the function `obiwarp`
#' If the m/z range or rT range is outside from the one from the file it will
#' return a dataframe with intensities at 0
#'
#' @param ms_file xcmsRaw object
#' @param mz_range numeric(2) containing the m/z range to look for
#' @param rt_range numeric(2) containing the rt range in sec to look for
#'
#' @return dataframe with the columns:
#' \itemize{
#'     \item rt retention time in seconds
#'     \item int intensity measured
#' }
get_mzdev <- function(ms_file, mz_range, rt_range) {
    if (is.null(ms_file)) {
        return(data.frame(rt = 0, mz = NA))
    }
    if (
        mz_range[1] >= ms_file@mzrange[2] ||
        mz_range[2] <= ms_file@mzrange[1] ||
        rt_range[1] >= range(ms_file@scantime)[2] ||
        rt_range[2] <= ms_file@scantime[1]
    ) {
        return(data.frame(rt = seq(rt_range[1], rt_range[2]), mz = NA))
    }
    rawmat <- xcms::rawMat(ms_file, mzrange = mz_range, rtrange = rt_range)
    if (any(names(attributes(ms_file)) == "scantime_corrected")) {
        scans <- sapply(rawmat[, "time"], function(x)
            which.min(abs(ms_file@scantime - x)))
        data.frame(
            rt = ms_file@scantime_corrected[scans],
            mz = rawmat[, "mz"]
        )
    } else {
        data.frame(
            rt = rawmat[, "time"],
            mz = rawmat[, "mz"]
        )
    }
}

#' @title Convert ppm to Da
#'
#' @description
#' Convert ppm to Dalton according a mass
#'
#' @param ppm `numeric(1)` ppm tolerance
#' @param mass `numeric(1)` mass
#'
#' @return `numeric(1)` ppm tolerance converted in Dalton
convert_ppm_da <- function(ppm, mass) {
    mass * ppm * 10**-6
}

#' @title Get m/z range
#'
#' @description
#' Get m/z range with a ppm tolerance
#'
#' @param mz `numeric(1)` m/z
#' @param ppm `numeric(1)` ppm tolerance
#'
#' @return `numeric(2)` m/z range
get_mz_range <- function(mz, ppm) {
    da <- convert_ppm_da(ppm, mz)
    mz + c(-da, da)
}
