#' @title Execute The step "Process"
#'
#' @description
#' Execute the step "Process" of the workflow.
#' it launch the substeps: 
#' \itemize{
#'      \item peakpicking
#'      \item rT correction
#'      \item alignment
#'      \item annotation
#' }
#' Some parameters have a default parameter. This is the case for : 
#' \itemize{
#'      \item verboseColumns is TRUE cause we want all the infos 
#'          exported by XCMS
#'      \item binSize is 1 cause i find it sufficient & 
#'          1 is too high to perform a well alignment of the spectras
#'      \item sampleGroups is equal to the length of the provided files
#'          I prefer to apply some rules to filter later & that will allow
#'          the possibility to change this anytime instead of re-run 
#'          the entire process
#'      \item minFraction is the minimum integer possible for the same 
#'          reason than `sampleGroups`
#'      \item minSamples is at 1, same reason than `sampleGroups`
#'      \item maxFeatures is at 500 cause the workflow will probably 
#'          need to deal with some huge datasets
#' }
#'
#' @param mzxml_files vector of filepaths to the files
#' @param filter_params FilterParam object 
#' @param cwt_params CentWaveParam object created by xcms
#' @param obw_params ObiwarpParam object created by xcms 
#' @param pd_params PeakDensityParam object created by xcms
#' @param ann_params AnnotationParam object 
#' @param cores numeric number of cores to use for the peakpicking 
#'      (one core = one file peakpicked)
#' @param show_pb boolean print a progress bar or not
#' 
#' @return XCMSnExp object
#' 
#' @export
#' @examples
#' \dontrun{
#'     mzxml_files <- c(
#'         system.file("testdata", "200204PLF_QC01_pos_filtered.mzML", 
#'              package = "workflow.lipido"), 
#'         system.file("testdata", "200204PLF_QC02_pos_filtered.mzML", 
#'              package = "workflow.lipido"))
#'     cwt_params <- xcms::CentWaveParam(
#'         ppm = 30, 
#'         peakwidth = c(4, 39), 
#'         snthresh = 6.5, 
#'         prefilter = c(2, 815), 
#'         mzdiff = .041, 
#'         noise = 0, 
#'         firstBaselineCheck = FALSE)
#'     obw_params <- xcms::ObiwarpParam() 
#'     pd_params <- xcms::PeakDensityParam(
#'          bw = 5, 
#'          binSize = .010
#'     )
#'     ms_process(mzxml_files, cwt_params, obw_params, pd_params)
#' }
ms_process <- function(mzxml_files, filter_params, cwt_params, 
        obw_params, pd_params, ann_params, 
        cores = parallel::detectCores(), show_pb = TRUE) {
    check_ms_process_args(mzxml_files, filter_params, cwt_params, obw_params, 
        pd_params, ann_params, cores)
    xcms::verboseColumns(cwt_params) <- TRUE
    xcms::binSize(obw_params) <- 1
    xcms::sampleGroups(pd_params) <- seq(length(mzxml_files))
    xcms::minFraction(pd_params) <- 10**-9
    xcms::minSamples(pd_params) <- 1
    xcms::maxFeatures(pd_params) <- 500
    
    if (cores > 1) {
        if (cores > length(mzxml_files)) cores <- length(mzxml_files)
        cl <- parallel::makeCluster(cores)
        doSNOW::registerDoSNOW(cl)
        parallel::clusterEvalQ(cl, require("MSnbase"))
        operator <- foreach::"%dopar%"
    } else operator <- foreach::"%do%"
    
    # only to delete the note : no visible binding for global variable
    mzxml_file <- NULL
    
    ms_files <- tryCatch({
        if (show_pb) pb <- utils::txtProgressBar(min = 0, 
            max = length(mzxml_files), style = 3)
        ms_files <- operator(foreach::foreach(
            mzxml_file = iterators::iter(mzxml_files), 
            .combine = append, 
            .options.snow = if (!show_pb) NULL 
                else list(progress = function(n) 
                    utils::setTxtProgressBar(pb, n))), 
            list(xcms::findChromPeaks(
                MSnbase::filterMz(
                    MSnbase::filterRt(
                        MSnbase::readMSData(mzxml_file, msLevel = 1, 
                            centroided = TRUE, mode = "onDisk"), 
                        rt = filter_params@rt_range), 
                    mz = filter_params@mz_range), 
                param = cwt_params, 
                BPPARAM = BiocParallel::SerialParam())))
        if (show_pb) close(pb)
    
        annotate_peaklists(
            group_peaks(
                obiwarp(ms_files, obw_params, operator, show_pb), 
                    pd_params, operator, show_pb), 
            ann_params, show_pb)
    }, error = function(e) e$message)
    
    if (exists("cl")) parallel::stopCluster(cl)
    if (class(ms_files) != "XCMSnExp") {
        print(ms_files)
        stop("cannot execute ms_process")
    }
    
    return(ms_files)
}

#' @title Check all args of "Process"
#'
#' @description
#' Check all arguments of the "Process" step. 
#'      Will raise an error if one argument is incorrect
#'
#' @param mzxml_files vector of filepaths to the files
#' @param filter_params FilterParam object 
#' @param cwt_params CentWaveParam object created by xcms, 
#' @param obw_params ObiwarpParam object created by xcms, 
#' @param pd_params PeakDensityParam object created by xcms, 
#' @param ann_params AnnotationParam object 
#' @param cores numeric number of cores to use for the peakpicking 
#'      (one core = one file peakpicked)
check_ms_process_args <- function(mzxml_files, filter_params, cwt_params, 
        obw_params, pd_params, ann_params, cores) {
    if (length(mzxml_files) <= 1) stop("you must give at least two mzxml files")
    else if (class(mzxml_files) != "character") stop(
        "mzxml_files argument must contain only characters")
    file_ext <- c("\\.mzXML$", "\\.mzML$", "\\.CDF$")
    test_ext <- sapply(mzxml_files, function(x) 
        any(sapply(file_ext, grepl, x, ignore.case = TRUE)))
    if (any(!test_ext)) stop(sprintf("file extension of %s are not supported",
        paste(basename(mzxml_files[!test_ext]), collapse = " and ")))
    test_exist <- file.exists(mzxml_files)
    if (any(!test_exist)) stop(sprintf("file(s) %s doesn't exist", 
        paste(mzxml_files[!test_exist], collapse = " and ")))
    mzxml_files <- normalizePath(mzxml_files)
    if (class(filter_params) != "FilterParam") stop(
        "filter_params argument must be a FilterParam object")
    if (class(cwt_params) != "CentWaveParam") stop(
        "cwt_params argument must be a CentWaveParam object")
    if (class(obw_params) != "ObiwarpParam") stop(
        "obw_params argument must be a ObiwarpParam object")
    if (class(pd_params) != "PeakDensityParam") stop(
        "pd_params argument must be a PeakDensityParam object")
    if (class(ann_params) != "AnnotationParam") stop(
        "ann_params argument must be an AnnotationParam object")
    if (class(cores) != "numeric" & class(cores) != "integer") stop(
        "cores argument must be numerical")
    else if (length(cores) > 1) stop(
        "cores argument must contain only ONE number !")
    else if (cores < 1) stop("cores cannot be a number under 1")
    else if (cores %% 1 != 0) stop("cores must not contain any digits")
    else if (cores > parallel::detectCores()) stop(sprintf(
        "system have a maximum of %s cores", parallel::detectCores()))
    return(0)
}

setClass("FilterParam", 
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
        if (length(object@mz_range) < 2 | 
            any(object@mz_range < 0)) msg <- c(msg, 
                "mz_range must contain two positive number")
        if (length(object@rt_range) < 2 | 
            any(object@rt_range < 0)) msg <- c(msg, 
                "rt_range must contain two positive number")
        if (length(msg)) msg else TRUE
    }
)
FilterParam <- function(mz_range = c(300, 1000), 
        rt_range = c(.7 * 60, 6.3 * 60)) 
    methods::new("FilterParam", mz_range = mz_range, rt_range = rt_range)

setClass("AnnotationParam", 
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
        adduct_names = c("M+Na", "M+NH4", "M+H-H2O", "M+H"), 
        instrument = "QTOF_XevoG2-S_R25000@200"
    ), 
    validity = function(object) {
        msg <- character()
        if (length(object@da_tol) != 1 | any(object@da_tol < 0)) msg <- c(msg, 
           "da_tol need to be a positive number")
        if (length(object@rt_tol) != 1 | any(object@rt_tol < 0)) msg <- c(msg, 
            "rt_tol need to be a positive number")
        if (length(object@abd_tol) != 1 | any(object@abd_tol < 0) | 
            any(object@abd_tol > 100)) msg <- c(msg, 
            "abd_tol need to be a positive number between 0 & 100")
        if (length(object@adduct_names) < 1) msg <- c(msg, 
            "adduct_names is required")
        test <- which(!object@adduct_names %in% adducts$Name)
        if (length(test) > 0) msg <- c(msg, sprintf(
            "%s doesn't exists in the adduct list", 
            paste(object@adduct_names[test], collapse = " and ")))
        if (length(object@instrument) != 1) msg <- c(msg, 
            "instrument is required")
        if (!object@instrument %in% names(resolution_list)) msg <- 
            c(msg, sprintf("%s doesn't exists in the instrument list", 
                object@instrument))        
        if (length(msg)) msg else TRUE
    }
)
AnnotationParam <- function(da_tol = 0.015, rt_tol = 10, abd_tol = 25, 
        adduct_names = c("M+Na", "M+NH4", "M+H-H2O", "M+H"), 
        instrument = "QTOF_XevoG2-S_R25000@200") 
    methods::new("AnnotationParam", da_tol = da_tol, rt_tol = rt_tol, 
        abd_tol = abd_tol, adduct_names = adduct_names, instrument = instrument)
