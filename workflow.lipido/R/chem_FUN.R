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
#'      \item spectra_id integer spectra ID
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
load_db <- function(adduct_names,  instrument) {
    db <- utils::read.csv(system.file("extdata", "database.csv", 
        package = "workflow.lipido"))
    ions <- do.call(rbind, lapply(adduct_names, function(adduct_name) 
        get_ions(
            unique(db$formula), 
            adducts[which(adducts$Name == adduct_name), ], 
            instrument)))
    ions <- cbind(spectra_id = as.numeric(as.factor(ions$ion_formula)), ions)
    db <- merge(db, ions, by = "formula", all = TRUE)
    return(db[!is.na(db$mz), , drop = FALSE]) 
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
#' @param q_spectras list of dataframe with columns (at least) : 
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
#' @return list (1st level for each library spectra, 
#'   2nd for each query spectra) 
#'      with a dataframe with columns :
#'      \itemize{
#'          \item score the isotopic score between query & library spectra
#'          \item deviation_mz the meanned m/z deviation
#'          \item npeak number of peaks matched in query spectra
#'          \item spectras list of dataframe consisting of 
#'              the merge of the query spectra with the corresponding lines 
#'              of the library spectra
#'      }
compare_spectras <- function(q_spectras, l_spectras, 
        da_tol = 0.05, abd_tol = 25, suffix = "theo") {
    lapply(l_spectras, function(l_spectra) {
        tmp <- align_spectras(l_spectra, q_spectras, da_tol, abd_tol)
        colnames(l_spectra) <- paste(colnames(l_spectra), suffix, sep = "_")
        lapply(seq(q_spectras), function(i) 
            list(
                spectra = suppressWarnings(cbind(
                    q_spectras[[i]][tmp$idx[[i]]$q_id, , drop = FALSE], 
                    l_spectra[tmp$idx[[i]]$l_id, , drop = FALSE]
                )), 
                score = tmp$score[i], 
                deviation_mz = tmp$deviation_mz[i] * 10**3, 
                npeak = tmp$npeak[i]
            )
        )
    })
}
