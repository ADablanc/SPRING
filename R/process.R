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
#' @param cwt_params CentWaveParam object created by xcms, 
#' @param obw_params ObiwarpParam object created by xcms, 
#' @param pd_params PeakDensityParam object created by xcms, 
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
ms_process <- function(mzxml_files, cwt_params, obw_params = NULL, 
        pd_params = NULL, cores = parallel::detectCores(), show_pb = TRUE) {
    check_ms_process_args(mzxml_files, cwt_params, obw_params, pd_params, cores)
    xcms::verboseColumns(cwt_params) <- TRUE
    if (length(mzxml_files) > 1) {
        xcms::binSize(obw_params) <- 1
        xcms::sampleGroups(pd_params) <- seq(length(mzxml_files))
        xcms::minFraction(pd_params) <- 10**-9
        xcms::minSamples(pd_params) <- 1
        xcms::maxFeatures(pd_params) <- 500
    }
    
    if (cores > 1 & length(mzxml_files) > 1) {
        if (cores > length(mzxml_files)) cores <- length(mzxml_files)
        cl <- parallel::makeCluster(cores)
        doSNOW::registerDoSNOW(cl)
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
                MSnbase::readMSData(mzxml_file, msLevel = 1, 
                    centroided = TRUE, mode = "onDisk"), 
                param = cwt_params, BPPARAM = BiocParallel::SerialParam())))
        if (show_pb) close(pb)
    
        if (length(ms_files) > 1) group_peaks(
            obiwarp(ms_files, obw_params, operator, show_pb), 
                pd_params, operator, show_pb)
        else ms_files[[1]]
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
#' @param cwt_params CentWaveParam object created by xcms, 
#'      if not given it will launch the parameters by default
#' @param obw_params ObiwarpParam object created by xcms, 
#'      if not given it will launch the parameters by default
#' @param pd_params PeakDensityParam object created by xcms, 
#'      if not given it will launch the parameters by default
#' @param cores numeric number of cores to use for the peakpicking 
#'      (one core = one file peakpicked)
check_ms_process_args <- function(mzxml_files, cwt_params, obw_params, 
        pd_params, cores) {
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
    if (class(cwt_params) != "CentWaveParam") stop(
        "cwt_params argument must be a CentWaveParam object")
    if (class(obw_params) != "ObiwarpParam" & length(mzxml_files) > 1) stop(
        "obw_params argument must be a ObiwarpParam object")
    if (class(pd_params) != "PeakDensityParam" & length(mzxml_files) > 1) stop(
        "pd_params argument must be a PeakDensityParam object")
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
