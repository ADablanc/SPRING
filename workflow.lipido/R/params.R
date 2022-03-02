#' @title Check all args of the processing workflow
#'
#' @description
#' Check all arguments of the processing workflow.
#'      Will raise an error if one argument is incorrect
#'
#' @param raw_files vector of filepaths to the raw files
#' @param sqlite_path filepath to the sqlite database used to save the process
#' results
#' @param converter filepath to the msconvert application
#' @param filter_params FilterParam object
#' @param cwt_params CentWaveParam object created by xcms,
#' @param obw_params ObiwarpParam object created by xcms,
#' @param pd_params PeakDensityParam object created by xcms,
#' @param ann_params AnnotationParam object
#' @param cores numeric number of cores to use for the processing workflow
#' (generaly one core = one file peakpicked)
check_ms_process_args <- function(raw_files,
                                  sqlite_path,
                                  converter,
                                  filter_params,
                                  cwt_params,
                                  obw_params,
                                  pd_params,
                                  ann_params,
                                  cores) {
    # check if the system is Windows
    if (Sys.info()[["sysname"]] != "Windows")
        stop("conversion only works on Windows")
    else if (length(raw_files) == 0)
        stop("you must give at least one raw file to convert")
    else if (class(raw_files) != "character")
        stop("raw_files argument must be a vector of filepaths")

    file_ext <- c("\\.mzML$", "\\.mzXML$", "\\.RAW$", "\\.d$",
                  "\\.YEP$", "\\.BAF$", "\\.FID$", "\\.WIFF$", "\\.MGF$")
    test_ext <- sapply(raw_files, function(x)
        any(sapply(file_ext, grepl, x, ignore.case = TRUE))
    )
    if (any(!test_ext))
        stop(sprintf(
            "file extension of %s are not supported",
             paste(
                 basename(raw_files[!test_ext]),
                 collapse = " and "
             )
         ))
    test_exist <- file.exists(raw_files)
    if (any(!test_exist))
        stop(sprintf(
            "file(s) %s doesn't exist",
            paste(
                raw_files[!test_exist],
                collapse = " and "
            )
        ))
    raw_files <- normalizePath(raw_files)

    if (class(sqlite_path) != "character")
        stop("sqlite path must be a filepath")
    else if (length(sqlite_path) > 1)
        stop("sqlite path must contain only one filepath")
    else if (!dir.exists(dirname(sqlite_path)))
        stop("directory where to save sqlite database doesn't exist")

    if (class(converter) != "character")
        stop("converter argument must be a filepath to the msconvert exe")
    else if (length(converter) > 1)
        stop("converter argument must contain only one filepath")
    else if (!file.exists(converter))
        stop(sprintf("converter is not found at %s", converter))
    converter <- normalizePath(converter)

    if (class(filter_params) != "FilterParam")
        stop("filter_params argument must be a FilterParam object")
    else if (class(cwt_params) != "CentWaveParam")
        stop("cwt_params argument must be a CentWaveParam object")
    else if (class(obw_params) != "ObiwarpParam")
        stop("obw_params argument must be a ObiwarpParam object")
    else if (class(pd_params) != "PeakDensityParam")
        stop("pd_params argument must be a PeakDensityParam object")
    else if (class(ann_params) != "AnnotationParam")
        stop("ann_params argument must be an AnnotationParam object")
    else if (class(cores) != "numeric" & class(cores) != "integer")
        stop("cores argument must be numerical")
    else if (length(cores) > 1)
        stop("cores argument must contain only ONE number !")
    else if (cores < 1)
        stop("cores cannot be a number under 1")
    else if (cores %% 1 != 0)
        stop("cores must not contain any digits")
    else if (cores > parallel::detectCores())
        stop(sprintf(
            "system have a maximum of %s cores",
            parallel::detectCores()
        ))
    1
}

setClass(
    "FilterParam",
    slots = c(
        mz_range = "numeric",
        rt_range = "numeric"
    ),
    prototype = prototype(
        mz_range = c(300, 1000),
        rt_range = c(.7 * 60, 6.3 * 60)
    ),
    validity = function(object) {
        msg <- character()
        if (length(object@mz_range) < 2 | any(object@mz_range < 0))
            msg <- c(msg, "mz_range must contain two positive number")
        else if (diff(object@mz_range) <= 0)
            msg <- c(msg, "mz_range born min must be lower than the born max")
        if (length(object@rt_range) < 2 | any(object@rt_range < 0))
            msg <- c(msg, "rt_range must contain two positive number")
        else if (diff(object@rt_range) <= 0)
            msg <- c(msg, "rt_range born min must be lower than the born max")
        if (length(msg))
            paste(msg, collapse = "\n  ")
        else
            TRUE
    }
)
FilterParam <- function(mz_range = c(300, 1000),
                        rt_range = c(.7 * 60, 6.3 * 60))
    methods::new("FilterParam", mz_range = mz_range, rt_range = rt_range)

setClass(
    "AnnotationParam",
    slots = c(
        da_tol = "numeric",
        rt_tol = "numeric",
        abd_tol = "numeric",
        adduct_names = "character",
        instrument = "character"
    ),
    prototype = prototype(
        da_tol = .015,
        rt_tol = 10,
        abd_tol = 25,
        adduct_names = c(
            "[M+Na]+",
            "[M+NH4]+",
            "[M+H-H2O]+",
            "[M+H]+",
            "[M-H]-"
        ),
        instrument = "QTOF_XevoG2-S_R25000@200"
    ),
    validity = function(object) {
        msg <- character()
        if (length(object@da_tol) != 1 | any(object@da_tol < 0))
            msg <- c(msg, "da_tol need to be a positive number")
        if (length(object@rt_tol) != 1 | any(object@rt_tol < 0))
            msg <- c(msg, "rt_tol need to be a positive number")
        if (length(object@abd_tol) != 1 | any(object@abd_tol < 0) |
            any(object@abd_tol > 100))
            msg <- c(
                msg,
                "abd_tol need to be a positive number between 0 & 100"
            )
        if (length(object@adduct_names) < 1)
            msg <- c(msg, "adduct_names is required")
        test <- which(!object@adduct_names %in% adducts$Name)
        if (length(test) > 0)
            msg <- c(msg, sprintf(
                "%s doesn't exists in the adduct list",
                paste(
                    object@adduct_names[test],
                    collapse = " and "
                )
            ))
        if (length(object@instrument) != 1)
            msg <- c(msg, "an instrument is required")
        if (!object@instrument %in% names(resolution_list))
            msg <- c(msg, sprintf(
                "%s doesn't exists in the instrument list",
                object@instrument
            ))
        if (length(msg))
            paste(msg, collapse = "\n  ")
        else
            TRUE
    }
)
AnnotationParam <- function(da_tol = 0.015,
                            rt_tol = 10,
                            abd_tol = 25,
                            adduct_names = c(
                                "[M+Na]+",
                                "[M+NH4]+",
                                "[M+H-H2O]+",
                                "[M+H]+",
                                "[M-H]-"
                            ),
                            instrument = "QTOF_XevoG2-S_R25000@200")
    methods::new(
        "AnnotationParam",
        da_tol = da_tol,
        rt_tol = rt_tol,
        abd_tol = abd_tol,
        adduct_names = adduct_names,
        instrument = instrument
    )
setGeneric("restrict_adducts_polarity", function(object, polarity)
    standardGeneric("restrict_adducts_polarity"))
setMethod(
    "restrict_adducts_polarity",
    "AnnotationParam",
    function(object, polarity) {
        if (polarity != "positive" & polarity != "negative")
            stop("polarity must be set to \"positive\" or \"negative\"")
        adducts_restricted <- adducts[adducts$Name %in% object@adduct_names,
                                      , drop = FALSE]
        if (polarity == "positive")
            adducts_restricted <- adducts_restricted[
                adducts_restricted$Charge >= 1, , drop = FALSE]
        else
            adducts_restricted <- adducts_restricted[
                adducts_restricted$Charge <= -1, , drop = FALSE]
        object@adduct_names <- adducts_restricted$Name
        return(object)
    }
)

setGeneric("params_to_dataframe", function(object)
    standardGeneric("params_to_dataframe"))
setMethod(
    "params_to_dataframe",
    "FilterParam",
    function(object) {
        data.frame(
            mz_range_min = object@mz_range[1],
            mz_range_max = object@mz_range[2],
            rt_range_min = object@rt_range[1],
            rt_range_max = object@rt_range[2]
        )
    }
)
setMethod(
    "params_to_dataframe",
    "CentWaveParam",
    function(object) {
        data.frame(
            ppm = object@ppm,
            peakwidth_min = object@peakwidth[1],
            peakwidth_max = object@peakwidth[2],
            snthresh = object@snthresh,
            prefilter_step = object@prefilter[1],
            prefilter_level = object@prefilter[2],
            mzCenterFun = object@mzCenterFun,
            integrate = object@integrate,
            mzdiff = object@mzdiff,
            fitgauss = object@fitgauss,
            noise = object@noise,
            verboseColumns = object@verboseColumns,
            firstBaselineCheck = object@firstBaselineCheck
        )
    }
)
setMethod(
    "params_to_dataframe",
    "ObiwarpParam",
    function(object) {
        data.frame(
            binSize = object@binSize,
            response = object@response,
            distFun = object@distFun,
            gapInit = object@gapInit,
            gapExtend = object@gapExtend,
            factorDiag = object@factorDiag,
            factorGap = object@factorGap,
            localAlignment = object@localAlignment,
            initPenalty = object@initPenalty
        )
    }
)

setMethod(
    "params_to_dataframe",
    "PeakDensityParam",
    function(object) {
        data.frame(
            bw = object@bw,
            minFraction = object@minFraction,
            minSamples = object@minSamples,
            binSize = object@binSize,
            maxFeatures = object@maxFeatures
        )
    }
)
setMethod(
    "params_to_dataframe",
    "AnnotationParam",
    function(object) {
        data.frame(
            da_tol = object@da_tol,
            rt_tol = object@rt_tol,
            abd_tol = object@abd_tol,
            adduct_names = paste(object@adduct_names, collapse = ";"),
            instrument = object@instrument
        )
    }
)
