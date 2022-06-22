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
#' @param camera_params `CameraParam`
#' @param ann_params `AnnotationParam`
#' @param cores `integer(1)` number of cores to use for the processing workflow
#' (generaly one core = one file peakpicked)
check_ms_process_args <- function(raw_files,
                                  sqlite_path,
                                  converter,
                                  cwt_params,
                                  obw_params,
                                  pd_params,
                                  camera_params,
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
    } else if (class(camera_params) != "CameraParam") {
        stop("camera_params argument must be a CameraParam object")
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
#' @slot polarity `character(1)` polarity to filter files
#' @slot mz_range `numeric(2)` the m/z range to trim all ms files
#' @slot rt_range `numeric(2)` the rT range to trim all ms files (in sec)
setClass(
    "FilterParam",
    slots = c(
        polarity = "character",
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
        ann_params@database,
        ann_params@polarity,
        ann_params@cpd_classes
    )
    if (ann_params@polarity == "positive") {
        adducts_restricted <- adducts[adducts$charge >= 1, , drop = FALSE]
    } else if (ann_params@polarity == "negative") {
        adducts_restricted <- adducts[adducts$charge <= -1, , drop = FALSE]
    }
    ions <- do.call(rbind, get_ions(
        unique(chem_db$formula),
        list(
            name = "[M]",
            nmol = 1,
            formula_add = FALSE,
            formula_ded = FALSE,
            charge = 0
        ),
        ann_params@instrument
    ))

    # xcms define noiserange as peakwidth * 3 !
    rt_range <- range(chem_db$rt)
    rt_tol <- max(cwt_params@peakwidth[2] * 3, ann_params@rt_tol)
    rt_range[1] <- max(rt_range[1] - rt_tol, 0)
    rt_range[2] <- rt_range[2] + rt_tol

    mz_range <- range(ions$mz)
    mz_range <- c(
        max(0, mz_range[1] + min(adducts_restricted$massdiff) - max(
            convert_ppm_da(cwt_params@ppm, mz_range[1]),
            ann_params@da_tol
        )),
        mz_range[2] + max(adducts_restricted$massdiff) + max(
            convert_ppm_da(cwt_params@ppm, mz_range[2]),
            ann_params@da_tol
        )
    )

    methods::new(
        "FilterParam",
        polarity = ann_params@polarity,
        mz_range = mz_range,
        rt_range = rt_range
    )
}

#' A class containing the filter parameters
#'
#' @slot cores `numeric(1)` number of cores for parallelization
#' @slot polarity `character(1)` "positive" or "negative"
#' @slot sigma `numeric(1)` multiplier of the standard deviation
#' @slot perfwhm `numeric(1)` percentage of the FWHM
#' @slot intval `character(1)` "into", "maxo" or "intb"
#' @slot cor_eic_th `numeric(1)` correlation threshold
#' @slot pval `numeric(1)` significant correlation threshold
#' @slot graphMethod `character(1)` method selection for grouping peaks after
#' correlation analysis into pseudospectra, could be "hcs" or "lpc"
#' @slot calcIso `logical(1)` use isotopic relationship for peak grouping
#' @slot calcCiS `logical(1)` use correlation inside samples for peak grouping
#' @slot calcCaS `logical(1)` use correlation across samples for peak grouping
#' @slot maxcharge `numeric(1)` max ion charge
#' @slot maxiso `numeric(1)` max isotopologues
#' @slot ppm `numeric(1)` ppm tolerance
#' @slot mzabs `numeric(1)` mDa tolerance
#' @slot minfrac `numeric(1)` percentage number of samples which must satisfy
#' 12C/13C rule
#' @slot multiplier `numeric(1)` max number n in [nM+H]+
#' @slot max_peaks `numeric(1)` max how much peaks per thread
#' @slot rules `dataframe` with columns:
#' \itemize{
#'     \item name `character` name of the adduct
#'     \item nmol `numeric` number n in [nM+H]+
#'     \item charge `numeric` charge of the adduct
#'     \item massdiff `numeric` mass difference with the adduct
#'     \item oidscore `numeric` adduct with same kation have same score
#'     (ex: [M+H]+ and [2M+H]+)
#'     \item quasi `numeric` valid a pc group if one flagged with at least one
#'     adduct at 1
#'     \item ips `numeric` rule score, if one peak can be explained,
#'     only its pc group can be kept
#' }
setClass(
    "CameraParam",
    slots = c(
        cores = "numeric",
        polarity = "character",
        sigma = "numeric",
        perfwhm = "numeric",
        intval = "character",
        cor_eic_th = "numeric",
        pval = "numeric",
        graphMethod = "character",
        calcIso = "logical",
        calcCiS = "logical",
        calcCaS = "logical",
        maxcharge = "numeric",
        maxiso = "numeric",
        ppm = "numeric",
        mzabs = "numeric",
        minfrac = "numeric",
        multiplier = "numeric",
        rules = "data.frame",
        max_peaks = "numeric"
    ),
    validity = function(object) {
        msg <- character()
        if (length(object@cores) != 1 | any(object@cores <= 0)) {
            msg <- c(msg, "cores need to be a positive number")
        }
        if (length(object@sigma) != 1 | any(object@sigma <= 0)) {
            msg <- c(msg, "sigma need to be a positive number")
        }
        if (length(object@perfwhm) != 1 | any(object@perfwhm <= 0) |
                any(object@perfwhm >= 1)) {
            msg <- c(msg, "perfwhm need to be a number between 0 and 1")
        }
        if (length(object@cor_eic_th) != 1 | any(object@cor_eic_th <= 0) |
                any(object@cor_eic_th >= 1)) {
            msg <- c(msg, "cor_eic_th must be a number between 0 and 1")
        }
        if (length(object@pval) != 1 | any(object@pval <= 0) |
                any(object@pval >= 1)) {
            msg <- c(msg, "pval must be a number between 0 and 1")
        }
        if (length(object@graphMethod) != 1 |
                !any(object@graphMethod %in% c("lpc", "hcs"))) {
            msg <- c(msg, "graphMethod must be \"lpc\" or \"hcs\"")
        }
        if (length(msg) > 0) {
            paste(msg, collapse = "\n  ")
        } else {
            TRUE
        }
    }
)

#' @title Create CameraParam object
#'
#' @description
#' Create CameraParam object, some parameters like `max_iso` will be computed by
#'  loading the chemical database
#' This object will store CAMERA parameters
#'
#' @param ann_params `AnnotationParam` object
#' @param cores `numeric(1)` number of cores for parallelization
#' @param sigma `numeric(1)` multiplier of the standard deviation
#' @param perfwhm `numeric(1)` percentage of the FWHM
#' @param cor_eic_th `numeric(1)` correlation threshold
#' @param pval `numeric(1)` significant correlation threshold
#' @param graphMethod `character(1)` method selection for grouping peaks after
#' correlation analysis into pseudospectra, could be "hcs" or "lpc"

#' @return `CameraParam` object
#' @export
#' @examples
#' \dontrun{
#' ann_params <- AnnotationParam(
#'    da_tol = 0.015,
#'    rt_tol = 10,
#'    abd_tol = 25,
#'    adduct_names = c(
#'        "[M+Na]+",
#'        "[M+NH4]+",
#'        "[M+H-H2O]+",
#'        "[M+H]+",
#'        "[M-H]-"
#'    ),
#'    instrument = "QTOF_XevoG2-S_R25000@200",
#'    database = "test",
#'    cpd_classes = c("LPC", "Cer", "FA")
#' )
#' camera_params <- CameraParam(
#'    ann_param = ann_param,
#'    cores = 1,
#'    sigma = 6,
#'    perfwhm = .6,
#'    cor_eic_th = .75,
#'    pval = .05,
#'    graphMethod = "hcs"
#' )
#' }
CameraParam <- function(ann_params, cores = 1, sigma = 6, perfwhm = .6,
                        cor_eic_th = .75, pval = .05, graphMethod = "hcs") {
    if (class(ann_params) != "AnnotationParam") {
        stop("ann_params must be an AnnotationParam object")
    }
    chem_db <- load_ion_db(
        ann_params@database,
        ann_params@instrument,
        ann_params@polarity,
        cpd_classes = ann_params@cpd_classes
    )

    max_iso <- max(
        suppressWarnings(as.numeric(gsub("M\\+", "", chem_db$iso))),
        na.rm = TRUE
    ) + 1
    if (ann_params@polarity == "positive") {
        adducts_restricted <- adducts[adducts$charge >= 1, , drop = FALSE]
    } else if (ann_params@polarity == "negative") {
        adducts_restricted <- adducts[adducts$charge <= -1, , drop = FALSE]
    }

    methods::new(
        "CameraParam",
        cores = cores,
        polarity = ann_params@polarity,
        sigma = sigma,
        perfwhm = perfwhm,
        intval = "into",
        cor_eic_th = cor_eic_th,
        pval = pval,
        graphMethod = graphMethod,
        calcIso = TRUE,
        calcCiS = TRUE,
        calcCaS = TRUE,
        maxcharge = max(abs(adducts_restricted$charge)),
        maxiso = max_iso,
        ppm = 0,
        mzabs = ann_params@da_tol,
        minfrac = 0,
        rules = adducts_restricted[, c("name", "nmol", "charge", "massdiff",
                                       "oidscore", "quasi", "ips")],
        multiplier = max(adducts_restricted$nmol),
        max_peaks = 100
    )
}

#' A class containing the annotation parameters
#'
#' @slot da_tol `numeric(1)` m/z tolerance in Dalton
#' @slot rt_tol `numeric(1)` rT tolerance in sec
#' @slot abd_tol `numeric(1)` relative abundance tolerance, each peak which
#' have an higher difference of relative abundance with its corresponding
#' theoretical peak will be discarded
#' @slot instrument `character(1)` instrument names from the enviPat package
#' @slot polarity `character(1)` "positive" or "negative"
#' @slot database `character(1)` name of the database to load
#' @slot cpd_classes `character vector` compound classes in database to
#' restrict for annotation
setClass(
    "AnnotationParam",
    slots = c(
        da_tol = "numeric",
        rt_tol = "numeric",
        abd_tol = "numeric",
        instrument = "character",
        polarity = "character",
        database = "character",
        cpd_classes = "character"
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
        if (length(object@instrument) != 1) {
            msg <- c(msg, "an instrument is required")
        }
        if (!object@instrument %in% names(resolution_list)) {
            msg <- c(msg, sprintf(
                "%s doesn't exists in the instrument list",
                object@instrument
            ))
        }
        if (length(msg) > 0) {
            paste(msg, collapse = "\n  ")
        } else {
            chem_db <- load_chem_db(
                object@database,
                object@polarity,
                object@cpd_classes
            )
            if (nrow(chem_db) == 0) {
                "No chemicals can be loaded with the given parameters"
            } else {
                TRUE
            }
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
#' @param instrument `character(1)` instrument names from the enviPat package
#' @param database `character(1)` name of the database to load
#' @param polarity `character(1)` "positive" or "negative"
#' @param cpd_classes `character vector` compound classes in database to
#' restrict for annotation
#'
#' @return `AnnotationParam` object
#' @export
#' @examples
#' \dontrun{
#' AnnotationParam(
#'      da_tol = .015,
#'      rt_tol = 10,
#'      abd_tol = 25,
#'      instrument = "QTOF_XevoG2-S_R25000@200",
#'      database = "test"
#' )}
AnnotationParam <- function(da_tol = 0.015,
                            rt_tol = 10,
                            abd_tol = 25,
                            instrument = "QTOF_XevoG2-S_R25000@200",
                            database = "test",
                            polarity = "positive",
                            cpd_classes = NULL) {
    if (!database %in% get_available_database()) {
        stop(sprintf("database %s doesn't exist in software", database))
    }
    if (polarity != "positive" && polarity != "negative") {
        stop("polarity needs to be \"positive\" or \"negative\"")
    }
    if (length(cpd_classes) == 0) {
        cpd_classes <- unique(load_chem_db(database, polarity)$class)
    }
    methods::new(
        "AnnotationParam",
        da_tol = da_tol,
        rt_tol = rt_tol,
        abd_tol = abd_tol,
        instrument = instrument,
        database = database,
        polarity = polarity,
        cpd_classes = cpd_classes
    )
}

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
#'     \item polarity `character` "positive" or "negative"
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
            polarity = object@polarity,
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

#' @title Convert `CameraParam` to `DataFrame`
#'
#' @description
#' Convert a `CameraParam` object to a `DataFrame` with one line
#'
#' @param object `CameraParam`
#'
#' @return `DataFrame` with one line & the columns:
#' \itemize{
#'     \item cores `numeric` number of cores for parallelization
#'     \item sigma `numeric` multiplier of the standard deviation
#'     \item perfwhm `numeric` percentage of the FWHM
#'     \item intval `character` "into", "maxo" or "intb"
#'     \item cor_eic_th `numeric` correlation threshold
#'     \item pval `numeric` significant correlation threshold
#'     \item graphMethod `character` method selection for grouping peaks after
#' correlation analysis into pseudospectra, could be "hcs" or "lpc"
#'     \item calcIso `logical` use isotopic relationship for peak grouping
#'     \item calcCiS `logical` use correlation inside samples for peak grouping
#'     \item calcCaS `logical` use correlation across samples for peak grouping
#'     \item maxiso `numeric(1)` max isotopologues
#'     \item ppm `numeric` ppm tolerance
#'     \item mzabs `numeric` mDa tolerance
#'     \item minfrac `numeric` percentage number of samples which must satisfy
#'      12C/13C rule
#'     \item max_peaks `numeric` max how much peaks per thread
#' }
setMethod(
    "params_to_dataframe",
    "CameraParam",
    function(object) {
        data.frame(
            cores = object@cores,
            sigma = object@sigma,
            perfwhm = object@perfwhm,
            intval = object@intval,
            cor_eic_th = object@cor_eic_th,
            pval = object@pval,
            graphMethod = object@graphMethod,
            calcIso = as.numeric(object@calcIso),
            calcCiS = as.numeric(object@calcCiS),
            calcCaS = as.numeric(object@calcCaS),
            maxcharge = object@maxcharge,
            maxiso = object@maxiso,
            ppm = object@ppm,
            mzabs = object@mzabs,
            minfrac = object@minfrac,
            max_peaks = object@max_peaks
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
#'     collapsed with the character ";"
#'     \item instrument `character` instrument names from the enviPat package
#'     \item database `character` name of the database to load
#'     \item polarity `character` "positive" or "negative"
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
            instrument = object@instrument,
            database = object@database,
            polarity = object@polarity,
            cpd_classes = paste(
                object@cpd_classes,
                collapse = ";"
            )
        )
    }
)
