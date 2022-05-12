#' @title Check all args of the processing workflow
#'
#' @description
#' Check all arguments of the processing workflow.
#'      Will raise an error if one argument is incorrect
#'
#' @param raw_files `character vector` filepaths to the raw files
#' @param sqlite_path `character(1)` filepath to the sqlite database used to
#' save the process results
#' @param converter `character(1)` filepath to the msconvert application
#' @param cwt_params `CentWaveParam`
#' @param obw_params `ObiwarpParam`
#' @param pd_params `PeakDensityParam`
#' @param ann_params `AnnotationParam`
#' @param cores `integer(1)` number of cores to use for the processing workflow
#' (generaly one core = one file peakpicked)
check_ms_process_args <- function(raw_files,
                                  sqlite_path,
                                  converter,
                                  cwt_params,
                                  obw_params,
                                  pd_params,
                                  ann_params,
                                  cores) {
    # check if the system is Windows
    if (Sys.info()[["sysname"]] != "Windows") {
        stop("conversion only works on Windows")
    } else if (length(raw_files) == 0) {
        stop("you must give at least one raw file to convert")
    } else if (class(raw_files) != "character") {
        stop("raw_files argument must be a vector of filepaths")
    }

    file_ext <- c("\\.mzML$", "\\.mzXML$", "\\.RAW$", "\\.d$",
                  "\\.YEP$", "\\.BAF$", "\\.FID$", "\\.WIFF$", "\\.MGF$")
    test_ext <- sapply(raw_files, function(x)
        any(sapply(file_ext, grepl, x, ignore.case = TRUE))
    )
    if (any(!test_ext)) {
        stop(sprintf(
            "file extension of %s are not supported",
             paste(
                 basename(raw_files[!test_ext]),
                 collapse = " and "
             )
        ))
    }

    test_exist <- file.exists(raw_files)
    if (any(!test_exist)) {
        stop(sprintf(
            "file(s) %s doesn't exist",
            paste(
                raw_files[!test_exist],
                collapse = " and "
            )
        ))
    }

    if (class(sqlite_path) != "character") {
        stop("sqlite path must be a filepath")
    } else if (length(sqlite_path) > 1) {
        stop("sqlite path must contain only one filepath")
    } else if (!dir.exists(dirname(sqlite_path))) {
        stop("directory where to save sqlite database doesn't exist")
    }

    if (class(converter) != "character") {
        stop("converter argument must be a filepath to the msconvert exe")
    } else if (length(converter) > 1) {
        stop("converter argument must contain only one filepath")
    } else if (!file.exists(converter)) {
        stop(sprintf("converter is not found at %s", converter))
    }

    if (class(cwt_params) != "CentWaveParam") {
        stop("cwt_params argument must be a CentWaveParam object")
    } else if (class(obw_params) != "ObiwarpParam") {
        stop("obw_params argument must be a ObiwarpParam object")
    } else if (class(pd_params) != "PeakDensityParam") {
        stop("pd_params argument must be a PeakDensityParam object")
    } else if (class(ann_params) != "AnnotationParam") {
        stop("ann_params argument must be an AnnotationParam object")
    } else if (class(cores) != "numeric" && class(cores) != "integer") {
        stop("cores argument must be numerical")
    } else if (length(cores) > 1) {
        stop("cores argument must contain only ONE number !")
    } else if (cores < 1) {
        stop("cores cannot be a number under 1")
    } else if (cores %% 1 != 0) {
        stop("cores must not contain any digits")
    } else if (cores > parallel::detectCores()) {
        stop(sprintf(
            "system have a maximum of %s cores",
            parallel::detectCores()
        ))
    }
    1
}

#' A class containing the filter parameters
#'
#' @slot mz_range `numeric(2)` the m/z range to trim all ms files
#' @slot rt_range `numeric(2)` the rT range to trim all ms files (in sec)
setClass(
    "FilterParam",
    slots = c(
        mz_range = "numeric",
        rt_range = "numeric"
    )
)

#' @title FilterParam
#'
#' @description
#' Construct a FilterParam object
#'
#' @param cwt_params `CentWaveParam`
#' @param ann_params `AnnotationParam`
#'
#' @return `FilterParam` object
FilterParam <- function(cwt_params, ann_params) {
    chem_db <- load_chem_db(
        ann_params@adduct_names,
        ann_params@instrument,
        cpd_classes = ann_params@cpd_classes
    )

    # xcms define noiserange as peakwidth * 3 !
    rt_range <- range(chem_db$rt)
    rt_tol <- max(cwt_params@peakwidth[2] * 3, ann_params@rt_tol)
    rt_range[1] <- rt_range[1] - rt_tol
    rt_range[2] <- rt_range[2] + rt_tol

    mz_range <- range(chem_db$mz)
    mz_range <- c(
        mz_range[1] - max(
            convert_ppm_da(cwt_params@ppm, mz_range[1]),
            ann_params@da_tol
        ),
        mz_range[2] + max(
            convert_ppm_da(cwt_params@ppm, mz_range[2]),
            ann_params@da_tol
        )
    )

    methods::new("FilterParam", mz_range = mz_range, rt_range = rt_range)
}

#' A class containing the annotation parameters
#'
#' @slot da_tol `numeric(1)` m/z tolerance in Dalton
#' @slot rt_tol `numeric(1)` rT tolerance in sec
#' @slot abd_tol `numeric(1)` relative abundance tolerance, each peak which
#' have an higher difference of relative abundance with its corresponding
#' theoretical peak will be discarded
#' @slot adduct_names `character vector` adduct names from the enviPat package
#' @slot instrument `character(1)` instrument names from the enviPat package
#' @slot cpd_classes `character vector` compound classes in database to
#' restrict for annotation
setClass(
    "AnnotationParam",
    slots = c(
        da_tol = "numeric",
        rt_tol = "numeric",
        abd_tol = "numeric",
        adduct_names = "character",
        instrument = "character",
        cpd_classes = "character"
    ),
    prototype = prototype(
        da_tol = .015,
        rt_tol = 10,
        abd_tol = 25,
        adduct_names = "[M+H]+",
        instrument = "QTOF_XevoG2-S_R25000@200",
        cpd_classes = "LPC"
    ),
    validity = function(object) {
        msg <- character()
        if (length(object@da_tol) != 1 || any(object@da_tol < 0)) {
            msg <- c(msg, "da_tol need to be a positive number")
        }
        if (length(object@rt_tol) != 1 || any(object@rt_tol < 0)) {
            msg <- c(msg, "rt_tol need to be a positive number")
        }
        if (length(object@abd_tol) != 1 || any(object@abd_tol < 0) ||
            any(object@abd_tol > 100)
        ) {
            msg <- c(msg,
                     "abd_tol need to be a positive number between 0 & 100")
        }
        test <- which(!object@adduct_names %in% adducts$Name)
        if (length(test) > 0) {
            msg <- c(msg, sprintf(
                "%s doesn't exists in the adduct list",
                paste(
                    object@adduct_names[test],
                    collapse = " and "
                )
            ))
        }
        if (length(object@instrument) != 1) {
            msg <- c(msg, "an instrument is required")
        }
        if (!object@instrument %in% names(resolution_list)) {
            msg <- c(msg, sprintf(
                "%s doesn't exists in the instrument list",
                object@instrument
            ))
        }
        test <- which(!object@cpd_classes %in% get_cpd_classes())
        if (length(test) > 0) {
            msg <- c(msg, sprintf(
                "%s doesn't exists in database",
                paste(
                    object@cpd_classes[test],
                    collapse = " and "
                )
            ))
        }
        if (length(msg) > 0) {
            paste(msg, collapse = "\n  ")
        } else {
            TRUE
        }
    }
)

#' @title AnnotationParam
#'
#' @description
#' Create an AnnotationParam object
#'
#' @param da_tol `numeric(1)` m/z tolerance in Dalton
#' @param rt_tol `numeric(1)` rT tolerance in sec
#' @param abd_tol `numeric(1)` relative abundance tolerance, each peak which
#' have an higher difference of relative abundance with its corresponding
#' theoretical peak will be discarded
#' @param adduct_names `character vector` adduct names from the enviPat package
#' (optional)
#' @param instrument `character(1)` instrument names from the enviPat package
#' @param cpd_classes `character vector` compound classes in database to
#' restrict for annotation
#'
#' @return `AnnotationParam` object
AnnotationParam <- function(da_tol = 0.015,
                            rt_tol = 10,
                            abd_tol = 25,
                            adduct_names = NULL,
                            instrument = "QTOF_XevoG2-S_R25000@200",
                            cpd_classes = NULL) {
    if (length(adduct_names) == 0) {
        adduct_names <- adducts$Name
    }
    if (length(cpd_classes) == 0) {
        cpd_classes <- get_cpd_classes()
    }
    methods::new(
        "AnnotationParam",
        da_tol = da_tol,
        rt_tol = rt_tol,
        abd_tol = abd_tol,
        adduct_names = adduct_names,
        instrument = instrument,
        cpd_classes = cpd_classes
    )
}

#' @title Restrict Annotation parameters
#'
#' @description
#' will recreate a new `AnnotationParam` with a set of adduct according a
#' polarity
#'
#' @param object `AnnotationParam`
#' @param polarity `character(1)` "positive" or "negative"
#'
#' @return `AnnotationParam` object
setGeneric("restrict_adducts_polarity", function(object, polarity)
    standardGeneric("restrict_adducts_polarity")
)
setMethod(
    "restrict_adducts_polarity",
    "AnnotationParam",
    function(object, polarity) {
        if (polarity != "positive" && polarity != "negative") {
            stop("polarity must be set to \"positive\" or \"negative\"")
        }
        adducts_restricted <- adducts[adducts$Name %in% object@adduct_names,
                                      , drop = FALSE]
        if (polarity == "positive") {
            adducts_restricted <- adducts_restricted[
                adducts_restricted$Charge >= 1, , drop = FALSE]
        } else {
            adducts_restricted <- adducts_restricted[
                adducts_restricted$Charge <= -1, , drop = FALSE]
        }
        object@adduct_names <- adducts_restricted$Name
        object
    }
)

setGeneric("params_to_dataframe", function(object)
    standardGeneric("params_to_dataframe")
)

#' @title Convert `FilterParam` to `DataFrame`
#'
#' @description
#' Convert a `FilterParam` object to a `DataFrame` with one line
#'
#' @param object `FilterParam`
#'
#' @return `DataFrame` with one line & the columns:
#' \itemize{
#'     \item mz_range_min `numeric` m/z range min
#'     \item mz_range_max `numeric` m/z range max
#'     \item rt_range_min `numeric` rT range min
#'     \item rt_range_max `numeric` rT range max
#' }
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

#' @title Convert `CentWaveParam` to `DataFrame`
#'
#' @description
#' Convert a `CentWaveParam` object to a `DataFrame` with one line
#'
#' @param object `CentWaveParam`
#'
#' @return `DataFrame` with one line & the columns:
#' \itemize{
#'     \item ppm `numeric` Maximal tolerated m/z deviation in consecutive scans
#'     in parts sper million (ppm)
#'     \item peakwidth_min `numeric` Expected approximate peak width min in
#'     chromatographic space
#'     \item peakwidth_max `numeric` Expected approximate peak width max in
#'     chromatographic space
#'     \item snthresh `numeric` Signal to noise ratio cutoff
#'     \item prefilter_step `numeric` Mass traces are only retained if they
#'     contain at least k peaks with intensity >= I
#'     \item prefilter_level `numeric` Mass traces are only retained if they
#'     contain at least k peaks with intensity >= I
#'     \item mzCenterFun `character` Name of the function to calculate the m/z
#'     center of the chromatographic peak
#'     \item integrate `integer` Integration method. If unchecked the descent
#'     is done on the real data, if checked peak limits are found through
#'     descent on the mexican hat filtered data. Method 1 is very accurate but
#'     prone to noise, while method 2 is more robust to noise but less exact
#'     \item mzdiff `numeric` Minimum difference in m/z for peaks with
#'     overlapping retention times, can be negative to allow overlap
#'     \item fitgauss `integer` whether or not a Gaussian should be fitted to
#'     each peak. This affects mostly the retention time position of the peak
#'     \item noise `numeric` Optional argument which is useful for data that was
#'     centroided without any intensity threshold, centroids with intensity <
#'     noise are omitted from ROI detection
#'     \item verboseColumns `integer` whether additional peak meta data columns
#'     should be returned, ignore
#'     \item firstBaselineCheck `integer` Continuous data within regions of
#'     interest is checked to be above the first baseline
#' }
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
            fitgauss = as.numeric(object@fitgauss),
            noise = object@noise,
            verboseColumns = as.numeric(object@verboseColumns),
            firstBaselineCheck = as.numeric(object@firstBaselineCheck)
        )
    }
)

#' @title Convert `ObiwarpParam` to `DataFrame`
#'
#' @description
#' Convert a `ObiwarpParam` object to a `DataFrame` with one line
#'
#' @param object `ObiwarpParam` object
#'
#' @return `DataFrame` with one line & the columns:
#' \itemize{
#'     \item binSize `numeric` slice of overlapping m/z groups
#'     \item response `numeric` Defining the responsiveness of warping with
#'     response = 0 giving linear warping on start and end points and
#'     response = 100 warping using all bijective anchors
#'     \item distFun `character` Distance function to be used.
#'     Allowed values are :
#'     \itemize{
#'         \item cor : Pearson's correlation
#'         \item cor_opt : calculate only 10% diagonal band of distance matrix;
#'         better runtime)
#'         \item cov : covariance
#'         \item prd : product
#'         \item euc : Euclidian distance
#'     }
#'     \item gapInit `numeric` Defining the penalty for gap opening
#'     \item gapExtend `numeric` Defining the penalty for gap enlargement
#'     \item factorDiag `numeric` Defining the local weight applied to diagonal
#'     moves in the alignment
#'     \item factorGap `numeric` Defining the local weight for gap moves in the
#'     alignment
#'     \item localAlignment `integer` Whether a local alignment should be
#'     performed instead of the default global alignment
#'     \item initPenalty `numeric` Defining the penalty for initiating an
#'     alignment (for local alignment only)
#' }
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
            localAlignment = as.numeric(object@localAlignment),
            initPenalty = object@initPenalty
        )
    }
)

#' @title Convert `PeakDensityParam` to `DataFrame`
#'
#' @description
#' Convert a `PeakDensityParam` object to a `DataFrame` with one line
#'
#' @param object a `PeakDensityParam` object
#'
#' @return `DataFrame` with one line & the columns:
#' \itemize{
#'     \item bw `numeric` retention time standard deviation (s) allowed
#'     \item minFraction `numeric` defining the minimum fraction of samples in
#'     at least one sample group in which the peaks have to be present to be
#'     considered as a peak group (feature)
#'     \item minSamples `integer` with the minimum number of samples in at
#'     least one sample group in which the peaks have to be detected to be
#'     considered a peak group (feature)
#'     \item binSize `numeric` slice of overlapping m/z groups
#'     \item maxFeatures `integer` with the maximum number of peak groups to be
#'     identified in a single mz slice
#' }
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

#' @title Convert `AnnotationParam` to `DataFrame`
#'
#' @description
#' Convert a `AnnotationParam` object to a `DataFrame` with one line
#'
#' @param object `AnnotationParam`
#'
#' @return `DataFrame` with one line & the columns:
#' \itemize{
#'     \item da_tol `numeric` m/z tolerance in Dalton
#'     \item rt_tol `numeric` rT tolerance in sec
#'     \item abd_tol `numeric` relative abundance tolerance, each peak which
#'     have an higher difference of relative abundance with its corresponding
#'     theoretical peak will be discarded
#'     \item adduct_names `character` adduct names from the enviPat package
#'     collapsed with the character ";"
#'     \item instrument `character` instrument names from the enviPat package
#'     \item cpd_classes `character` compound classes in database to restrict
#'     for annotation collapsed with the character ";"
#' }
setMethod(
    "params_to_dataframe",
    "AnnotationParam",
    function(object) {
        data.frame(
            da_tol = object@da_tol,
            rt_tol = object@rt_tol,
            abd_tol = object@abd_tol,
            adduct_names = paste(
                object@adduct_names,
                collapse = ";"
            ),
            instrument = object@instrument,
            cpd_classes = paste(
                object@cpd_classes,
                collapse = ";"
            )
        )
    }
)
