#' @title Execute The step "Process"
#'
#' @description
#' Execute the step "Process" of the workflow.
#' it launch the substeps: 
#' \itemize{
#'      \item peakpicking
#'      \item rT correction
#'      \item alignment
#'      \item fill missing peaks
#' }
#'
#' @param mzxml_files vector of filepaths to the files
#' @param cwt_params CentWaveParam object created by xcms, 
#'      if not given it will launch the parameters by default
#' @param obw_params ObiwarpParam object created by xcms, 
#'      if not given it will launch the parameters by default
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
#'         mzCenterFun = "wMean", 
#'         integrate = 1, 
#'         mzdiff = .041, 
#'         fitgauss = FALSE, 
#'         noise = 0, 
#'         verboseColumns = TRUE, 
#'         firstBaselineCheck = FALSE)
#'     obw_params <- xcms::ObiwarpParam(
#'         binSize = .1,
#'         centerSample = integer(),
#'         response = 1L,
#'         distFun = "cor_opt",
#'         gapInit = .3,
#'         gapExtend = 2.4,
#'         factorDiag = 2,
#'         factorGap = 1,
#'         localAlignment = FALSE,
#'         initPenalty = 0
#'     ) 
#'     ms_process(mzxml_files, cwt_params)
#' }
ms_process <- function(mzxml_files, cwt_params, 
        obw_params, cores = parallel::detectCores(), show_pb = TRUE) {
    check_ms_process_args(mzxml_files, cwt_params, obw_params, cores)
    
    if (show_pb) pb <- utils::txtProgressBar(min = 0, 
        max = length(mzxml_files), style = 3)
    if (cores > 1) {
        if (cores > length(mzxml_files)) cores <- length(mzxml_files)
        cl <- parallel::makeCluster(cores)
        doSNOW::registerDoSNOW(cl)
        operator <- foreach::"%dopar%"
    } else operator <- foreach::"%do%"
    
    # only to delete the note : no visible binding for global variable
    mzxml_file <- NULL
    ms_file <- NULL
    
    ms_files <- tryCatch({
        ms_files <- operator(foreach::foreach(
            mzxml_file = iterators::iter(mzxml_files), 
            .combine = append, 
            .options.snow = if (!show_pb) NULL 
                else list(progress = function(n) 
                    utils::setTxtProgressBar(pb, n))), 
            list(xcms::findChromPeaks(
                MSnbase::readMSData(mzxml_file, msLevel = 1, 
                    centroided = TRUE, mode = "onDisk"), 
                param = cwt_params, BPPARAM = BiocParallel::SerialParam())))
    
        if (length(ms_files) > 1) {
            if (cores > 1) parallel::clusterExport(cl, "obiwarp", 
                envir = environment())
            xcms::centerSample(obw_params) <- which.max(
                sapply(ms_files, function(x) nrow(xcms::chromPeaks(x))))
            rtraw <- lapply(ms_files, xcms::rtime)
            suppressMessages(
                bin_center_ms_file <- xcms::profMat(
                    ms_files[[xcms::centerSample(obw_params)]], 
                    method = "bin",
                    step = xcms::binSize(obw_params),
                    returnBreaks = TRUE)[[1]])
            tmp_adjusted_rtimes <- tryCatch(
                operator(foreach::foreach(
                    ms_file = iterators::iter(
                        ms_files[-xcms::centerSample(obw_params)]), 
                    .combine = append, 
                    .options.snow = if (!show_pb) NULL 
                        else list(progress = function(n) 
                            utils::setTxtProgressBar(pb, n))), 
                    list(obiwarp(ms_file, 
                        ms_files[[xcms::centerSample(obw_params)]], 
                        bin_center_ms_file, 
                        obw_params))), 
                error = function(e) e$message)
            adjusted_rtime <- vector(mode = "list", length = length(ms_files))
            adjusted_rtime[[xcms::centerSample(obw_params)]] <- 
                unname(xcms::rtime(ms_files[[xcms::centerSample(obw_params)]]))
            adjusted_rtime[-xcms::centerSample(obw_params)] <- 
                tmp_adjusted_rtimes
            adjusted_rtime <- xcms:::adjustRtimeSubset(rtraw, adjusted_rtime)
            ms_files <- do.call(c, ms_files)
            xcms::adjustedRtime(ms_files) <- adjusted_rtime
        }
        ms_files
    }, error = function(e) e$message)
    
    if (cores > 1) parallel::stopCluster(cl)
    if (show_pb) close(pb)
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
#' @param cwt_params CentWaveParam object created by xcms, 
#'      if not given it will launch the parameters by default
#' @param obw_params ObiwarpParam object created by xcms, 
#'      if not given it will launch the parameters by default
#' @param cores numeric number of cores to use for the peakpicking 
#'      (one core = one file peakpicked)
check_ms_process_args <- function(mzxml_files, cwt_params, obw_params, cores) {
    if (length(mzxml_files) == 0) stop("you must give at least one mzxml file")
    else if (class(mzxml_files) != "character") stop(
        "mzxml_files argument must contain only characters")
    file_ext <- c("\\.mzXML$", "\\.mzML$", "\\.CDF$")
    test_ext <- sapply(mzxml_files, function(x) 
        any(sapply(file_ext, grepl, x, ignore.case = TRUE)))
    if (any(!test_ext)) stop(sprintf("file extension of %s are not supported",
        paste(basename(mzxml_files[!test_ext]), collapse = " ")))
    test_exist <- file.exists(mzxml_files)
    if (any(!test_exist)) stop(sprintf("file(s) %s doesn't exist", 
        paste(mzxml_files[!test_exist], collapse = " ")))
    mzxml_files <- normalizePath(mzxml_files)
    if (length(cwt_params) == 0) cwt_params <- xcms::CentWaveParam()
    else if (class(cwt_params) != "CentWaveParam") stop(
        "cwt_params argument must be a CentWaveParam object")
    if (length(obw_params) == 0) obw_params <- xcms::ObiwarpParam()
    else if (class(obw_params) != "ObiwarpParam") stop(
        "obw_params argument must be a ObiwarpParam object")
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
