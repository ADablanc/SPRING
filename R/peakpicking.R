#' @title Execute peakpicking
#'
#' @description
#' Launch xcms peakpicking
#'
#' @param mzxml_files vector of filepaths to the files
#' @param cwt_params CentWaveParam object created by xcms, 
#'      if not given it will launch the parameters by default
#' @param cores numeric number of cores to use for the peakpicking 
#'      (one core = one file peakpicked)
#' @param show_pb boolean print a progress bar or not
#' 
#' @return XCMSnExp object with all peakpicked files combined
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
#'     xcms::chromPeaks(peakpicking(mzxml_files, cwt_params)
#' }
peakpicking <- function(mzxml_files, cwt_params, 
        cores = parallel::detectCores(), show_pb = TRUE) {
    check_peakpicking_args(mzxml_files, cwt_params, cores)
    
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
    
    ms_files <- tryCatch(
        operator(foreach::foreach(
            mzxml_file = iterators::iter(mzxml_files), 
            .combine = c, 
            .options.snow = if (!show_pb) NULL 
                else list(progress = function(n) 
                    utils::setTxtProgressBar(pb, n))), 
            xcms::findChromPeaks(
                MSnbase::readMSData(mzxml_file, msLevel = 1, 
                    centroided = TRUE, mode = "onDisk"), 
                param = cwt_params, BPPARAM = BiocParallel::SerialParam())), 
        error = function(e) e$message)
    if (cores > 1) parallel::stopCluster(cl)
    if (show_pb) close(pb)
    
    if (class(ms_files) != "XCMSnExp") {
        print(ms_files)
        stop("cannot execute peakpicking")
    }
    return(ms_files)
}

#' @title Check peakpicking args
#'
#' @description
#' Check peakpicking args. Will raise an error if one argument is incorrect
#'
#' @param mzxml_files vector of filepaths to the files
#' @param cwt_params CentWaveParam object created by xcms, 
#'      if not given it will launch the parameters by default
#' @param cores numeric number of cores to use for the peakpicking 
#'      (one core = one file peakpicked)
check_peakpicking_args <- function(mzxml_files, cwt_params, cores) {
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
