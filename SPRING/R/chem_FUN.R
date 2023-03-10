#' @title Load cpd database
#'
#' @description
#' Load chemical database
#' @param database `character(1)` database path
#' @param polarity `character(1)` can be only "positive" or "negative"
#' @param cpd_classes `character vector` compound classes to restrict
#' @param cpd_names `character vector` compound names to restrict
#'
#' @return `DataFrame` with columns:
#' \itemize{
#'    \item class `character` compound class
#'    \item adduct `character` referent adduct
#'    \item name `character` compound name
#'    \item formula `character` chemical formula
#'    \item rt `numeric` retention time
#' }
load_chem_db <- function(database, polarity = NULL, cpd_classes = NULL,
                         cpd_names = NULL) {
    database <- utils::read.csv(database)
    # the rt in the database is in minutes !!
    database$rt <- as.numeric(database$rt) * 60
    if (!is.null(polarity)) {
        if (polarity == "positive") {
            database <- database[which(grepl("\\+$", database$adduct)), ,
                                 drop = FALSE]
        } else if (polarity == "negative") {
            database <- database[which(grepl("\\-$", database$adduct)), ,
                                 drop = FALSE]
        }
    }
    if (!is.null(cpd_classes)) {
        database <- database[database$class %in% cpd_classes, , drop = FALSE]
    }
    if (!is.null(cpd_names)) {
        database <- database[database$name %in% cpd_names, , drop = FALSE]
    }
    database
}

#' @title Get all ions of chemical database
#'
#' @description
#' Get all m/z & relative abundance for the internal database
#' It will compute each m/z by taking the adduct given in the database for each
#' compound
#'
#' @param database `character(1)` path to the database to load
#' @param polarity `character(1)` polarity to restrict database
#' @param instrument `character(1)` instrument name for enviPat
#' @param cpd_classes `character vector` compound classes to restrict
#' @param cpd_names name of compounds to restrict
#'
#' @return DataFrame with
#' \itemize{
#'      \item formula chemical formula
#'      \item adduct referent adduct used for the compound
#'      \item class class of the compound
#'      \item name of the compound
#'      \item rt retention time of the compound
#'      \item ion_id id unique for an entry of the database + adduct associated
#'      \item ion_formula ion chemical formula
#'      \item charge charge of the ion
#'      \item mz m/z
#'      \item abd relative abundance
#'      \item iso isotopologue annotation
#' }
load_ion_db <- function(database,
                    instrument,
                    polarity,
                    cpd_classes = NULL,
                    cpd_names = NULL) {
    chem_db <- load_chem_db(database, polarity, cpd_classes, cpd_names)
    if (nrow(chem_db) == 0) {
        return(data.frame(matrix(, nrow = 0, ncol = 11, dimnames = list(
            c(), c("class", "formula", "name", "rt", "ion_id", "adduct",
                   "ion_formula", "charge", "mz", "abd", "iso")
        ))))
    }

    ions <- do.call(
        rbind,
        lapply(split(chem_db, chem_db$adduct), function(x) {
            do.call(rbind, get_ions(
                unique(x$formula),
                adducts[adducts$name == x[1, "adduct"], ],
                instrument
            ))
        })
    )
    ions <- cbind(
        ion_id = as.numeric(as.factor(paste(ions$formula, ions$adduct))),
        ions,
        order_id = seq(nrow(ions))
    )
    chem_db <- merge(
        chem_db,
        ions,
        by = c("formula", "adduct"),
        all = TRUE
    )
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
#' @return list of DataFrames (one item per isotopic profile) with
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
            return(list(data.frame(matrix(
                , nrow = 0, ncol = 7, dimnames = list(c(), c(
                    "ion_id", "formula", "adduct", "ion_formula", "mz",
                    "abd", "iso"
                ))))))
        }
    }
    ion_forms <- enviPat::check_chemform(isotopes, ion_forms)
    resmass <- resolution_list[[which(names(resolution_list) == instrument)]]
    out_resmass <- which(
        ion_forms$monoisotopic_mass < min(resmass[, "m/z"]) |
        ion_forms$monoisotopic_mass > max(resmass[, "m/z"])
    )
    if (length(out_resmass) == length(forms)) {
        return(list(data.frame(matrix(
            , nrow = 0, ncol = 7, dimnames = list(c(), c(
                "ion_id", "formula", "adduct", "ion_formula", "mz",
                "abd", "iso"
            ))))))
    } else if (length(out_resmass) > 0) {
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
    lapply(seq(isotopic_profiles), function(i) {
        data.frame(
            formula = forms[i],
            adduct = adduct$name,
            ion_formula = ion_forms[i, "new_formula"],
            charge = adduct$charge,
            mz = round(isotopic_profiles[[i]][, "m/z"], 5),
            abd = round(isotopic_profiles[[i]][, "abundance"], 2),
            iso = c("M",
                    paste0("M+", seq(nrow(isotopic_profiles[[i]]))[-1] - 1))
        )
    })
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

#' @title Compute Kendrick Mass
#'
#' @description
#' Compute Kendrick mass according the base "CH2"
#' KM : Kendrick Mass
#' KMD : Kendrick Mass Defect
#'
#' @param mzs `numeric` m/z vector
#'
#' @return `list` with items:
#' \itemize{
#'     \item x `numeric` x coordinates (KM)
#'     \item y `numeric` y coordinates (KMD)
#' }
#'
#' @source \url{https://doi.org/10.1007/s13361-018-2040-9}
get_kendrick_mass <- function(mzs) {
    k <- 14 / 14.01565
    km <- mzs * k
    list(
        x = km,
        y = km %% 1
    )
}

#' @title Get TIC
#'
#' @description
#' Get TIC from raw files using the msaccess executable file from ProteoWizard
#' It filters on the polarity of the scans & the ms level (by default 1)
#'
#' @param raw_files `character vector` filepaths to the raw files
#' @param msaccess `character(1)` filepath to the msaccess executable file
#' @param polarity `character(1)` should be "positive" or "negative" only
#'
#' @return `list of DataFrames` each `DataFrame` represent a file and has
#'  columns :
#' \itemize{
#'    \item index
#'    \item id
#'    \item event
#'    \item analyzer
#'    \item msLevel
#'    \item rt
#'    \item int
#' }
get_raw_tic <- function(raw_files, msaccess, polarity = "positive") {
    tmp_dir <- file.path(tempdir(), runif(1))
    file_list <- tempfile(pattern = ".txt")
    writeLines(raw_files, file_list)
    query <- sprintf(
        paste(
            "\"%s\" -f \"%s\"",
            "-o \"%s\"",
            "-x \"tic delimiter=tab\"",
            "--filter=\"msLevel 1\"",
            "--filter=\"polarity \"%s\""
        ),
        msaccess, file_list, tmp_dir, polarity
    )

    output <- system(query, intern = TRUE, wait = TRUE)
    output_files <- list.files(tmp_dir, full.names = TRUE)
    if (length(output) > 0 || length(output_files) == 0) {
        print(output)
        stop("no files were generated by msaccess !")
    } else {
        data <- lapply(output_files, read.table, sep = "\t",
                   col.names = c("index", "id", "event", "analyzer", "msLevel",
                                 "rt", "int"))
        names(data) <- gsub(
            ".tic.0.00-10000.00.tsv$",
            "",
            basename(output_files)
        )
        data
    }
}
