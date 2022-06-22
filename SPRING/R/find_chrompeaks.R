#' @title Peak picking
#'
#' @description
#' Execute peak peacking process. It differs from the one from XCMS cause it
#' take as input an `xcmsRaw` & reconstruct an `xcmsSet` with it
#'
#' @param ms_file `xcmsRaw` object
#' @param cwt_params `CentwaveParam` object
#' @param sample_name `character(1)` sample name
#'
#' @return `xcmsSet` object
find_chrompeaks <- function(ms_file, cwt_params, sample_name) {
    empty_peaklist <- matrix(, nrow = 0, ncol = 23, dimnames = list(
        c(), c("mz", "mzmin", "mzmax", "rt", "rtmin", "rtmax", "into",
               "intb", "maxo", "sn", "egauss", "mu", "sigma", "h", "f",
               "dppm", "scale", "scpos", "scmin", "scmax", "lmin", "lmax",
               "sample")
    ))

    object <- methods::new("xcmsSet")
    file <- ms_file@filepath[1]
    object@filepaths <- file

    ## determine experimental design
    from_paths <- xcms::phenoDataFromPaths(file)
    object@phenoData <- from_paths
    rownames(object@phenoData) <- sample_name
    object@profinfo <- xcms::profinfo(ms_file)

    suppressMessages(suppressWarnings(peaks <- xcms::do_findChromPeaks_centWave(
        mz = as.double(ms_file@env$mz),
        int = as.double(ms_file@env$intensity),
        scantime = ms_file@scantime,
        valsPerSpect = diff(c(ms_file@scanindex, length(ms_file@env$mz))),
        ppm = cwt_params@ppm,
        peakwidth = cwt_params@peakwidth,
        snthresh = cwt_params@snthresh,
        prefilter = cwt_params@prefilter,
        mzCenterFun = cwt_params@mzCenterFun,
        integrate = cwt_params@integrate,
        mzdiff = cwt_params@mzdiff,
        fitgauss = cwt_params@fitgauss,
        noise = cwt_params@noise,
        verboseColumns = cwt_params@verboseColumns,
        roiList = cwt_params@roiList,
        firstBaselineCheck = cwt_params@firstBaselineCheck,
        roiScales = cwt_params@roiScales,
        sleep = 0,
        extendLengthMSW = cwt_params@extendLengthMSW
    )))

    if (is.null(peaks)) {
        peaks <- empty_peaklist
    } else if (nrow(peaks) == 0) {
        peaks <- empty_peaklist
    } else {
        peaks <- cbind(peaks, sample = 1)
    }

    proclist <- xcms:::ProcessHistory(
        info. = sprintf(
            "Peak detection in %s : %s peaks identified.",
            basename(file),
            nrow(peaks)
        ),
        date. = date(),
        type. = "Peak detection",
        fileIndex. = 1
    )

    object@peaks <- peaks
    object@rt <- list(ms_file@scantime)
    attributes(object)$mzrange <- ms_file@mzrange
    object@.processHistory <- list(proclist)
    object@mslevel <- ms_file@mslevel

    rm(ms_file)
    gc()
    object
}
