#' @title Alignment: Retention time correction methods.
#'
#' @description
#' perform retention time correction (alignment) between chromatograms of 
#'      different samples. 
#' the alignment is based on the complete mz-rt data. This method does not 
#'      require any identified peaks or defined features. 
#' This function comes from the XCMS package and only the function `.obiwarp` 
#'      was imported in order to use & export the parallelisation infos
#' It force to use the sample with the most peak as the center sample
#' 
#' @param ms_files list of XCMSnExp
#' @param obw_params ObiwarpParam object created by xcms
#' @param operator function to use for parallelization (`\%dopar\%`) 
#'      or not (`\%do\%`)
#' @param show_pb boolean indicate if we print a progress bar or not
#' 
#' @return XCMSnExp which regroup all the files in one object 
#'      with the rT corrected
#' 
#' @seealso xcms::adjustRtime
obiwarp <- function(ms_files, obw_params, operator, show_pb) {
    xcms::centerSample(obw_params) <- which.max(
        sapply(ms_files, function(x) nrow(xcms::chromPeaks(x))))
    rtraw <- lapply(ms_files, xcms::rtime)
    suppressMessages(
        binned_ms_file_center <- xcms::profMat(
            ms_files[[xcms::centerSample(obw_params)]], 
            method = "bin",
            step = xcms::binSize(obw_params),
            returnBreaks = TRUE)[[1]])
    ms_file_center <- ms_files[[xcms::centerSample(obw_params)]]
    
    if (show_pb) pb <- utils::txtProgressBar(min = 0, 
        max = length(ms_files) - 1, style = 3)
    # only to delete the note : no visible binding for global variable
    ms_file <- NULL
    tmp_adjusted_rtimes <- operator(
        foreach::foreach(
            ms_file = iterators::iter(
                ms_files[-xcms::centerSample(obw_params)]), 
            .combine = append, 
            .options.snow = if (!show_pb) NULL 
                else list(progress = function(n) 
                    utils::setTxtProgressBar(pb, n))
        ), tryCatch({
            message("Aligning ", basename(MSnbase::fileNames(ms_file)), 
                " against ", basename(MSnbase::fileNames(ms_file_center)), 
                " ... ", appendLF = FALSE)
            ## Get the profile matrix for the current file.
            suppressMessages(cur_p <- xcms::profMat(ms_file, method = "bin", 
                step = xcms::binSize(obw_params), returnBreaks = TRUE)[[1]])
            ## ---------------------------------------
            ## 1)Check the scan times of both objects:
            scantime1 <- unname(xcms::rtime(ms_file_center))
            scantime2 <- unname(xcms::rtime(ms_file))
            scantime1_diff <- diff(scantime1)
            scantime2_diff <- diff(scantime2)
            ## median difference between spectras' scan time.
            mstdiff <- stats::median(c(scantime1_diff, scantime2_diff), 
                na.rm = TRUE)

            mst1 <- which(scantime1_diff > 5 * mstdiff)[1]
            if (!is.na(mst1)) {
                message("Found gaps in scan times of the center sample: cut ", 
                    "scantime-vector at ", scantime1[mst1], " seconds.")
                scantime1 <- scantime1[seq_len(max(2, (mst1 - 1)))]
            }
            mst2 <- which(scantime2_diff > 5 * mstdiff)[1]
            if (!is.na(mst2)) {
                message("Found gaps in scan time of file ", 
                    basename(MSnbase::fileNames(ms_file)), 
                    ": cut scantime-vector at ", 
                    scantime2[mst2], " seconds.")
                scantime2 <- scantime2[seq_len(max(2, (mst2 - 1)))]
            }
            ## Drift of measured scan times - expected to be largest at the end.
            rtmaxdiff <- abs(diff(c(scantime1[length(scantime1)], 
                scantime2[length(scantime2)])))
            ## If the drift is larger than the threshold, 
                ## cut the matrix up to the max allowed difference.
            if (rtmaxdiff > (5 * mstdiff)) {
                rtmax <- min(scantime1[length(scantime1)], 
                    scantime2[length(scantime2)])
                scantime1 <- scantime1[scantime1 <= rtmax]
                scantime2 <- scantime2[scantime2 <= rtmax]
            }
            valscantime1 <- length(scantime1)
            valscantime2 <- length(scantime2)
            ## Ensure we have the same number of scans.
            if (valscantime1 != valscantime2) {
                min_number <- min(valscantime1, valscantime2)
                diffs <- abs(range(scantime1) - range(scantime2))
                ## Cut at the start or at the end, 
                    ## depending on where we have the larger difference
                if (diffs[2] > diffs[1]) {
                    scantime1 <- scantime1[1:min_number]
                    scantime2 <- scantime2[1:min_number]
                } else {
                    scantime1 <- rev(rev(scantime1)[1:min_number])
                    scantime2 <- rev(rev(scantime2)[1:min_number])
                }
                valscantime1 <- length(scantime1)
                valscantime2 <- length(scantime2)
            }
            ## Finally, restrict the profile matrix to the restricted data
            if (ncol(binned_ms_file_center[["profMat"]]) != valscantime1) {
                ## Find out whether we were cutting at the start or end.
                start_idx <- which(scantime1[1] == xcms::rtime(ms_file_center))
                end_idx <- which(scantime1[length(scantime1)] == 
                    xcms::rtime(ms_file_center))
                binned_ms_file_center[["profMat"]] <- 
                    binned_ms_file_center[["profMat"]][, start_idx:end_idx]
            }
            if (ncol(cur_p[["profMat"]]) != valscantime2) {
                start_idx <- which(scantime2[1] == xcms::rtime(ms_file))
                end_idx <- which(scantime2[length(scantime2)] == 
                    xcms::rtime(ms_file))
                cur_p[["profMat"]] <- cur_p[["profMat"]][, start_idx:end_idx]
            }
            ## ---------------------------------
            ## 2) Now match the breaks/mz range.
            ##    The -1 below is because the breaks define the upper and lower
            ##    boundary. Have to do it that way to be in line with the 
            ##    orignal code... would be better to use the breaks as is.
            mzr1 <- c(binned_ms_file_center$breaks[1], 
                binned_ms_file_center$breaks[
                    length(binned_ms_file_center$breaks) - 1])
            mzr2 <- c(cur_p$breaks[1], cur_p$breaks[length(cur_p$breaks) - 1])
            mzmin <- min(c(mzr1[1], mzr2[1]))
            mzmax <- max(c(mzr1[2], mzr2[2]))
            mzs <- seq(mzmin, mzmax, by = xcms::binSize(obw_params))
            ## Eventually add empty rows at the beginning
            if (mzmin < mzr1[1]) {
                tmp <- matrix(0, (length(seq(mzmin, mzr1[1], 
                    xcms::binSize(obw_params))) - 1), 
                    ncol = ncol(binned_ms_file_center[["profMat"]]))
                binned_ms_file_center[["profMat"]] <- rbind(tmp, 
                    binned_ms_file_center[["profMat"]])
            }
            ## Eventually add empty rows at the end
            if (mzmax > mzr1[2]) {
                tmp <- matrix(0, (length(seq(mzr1[2], mzmax, 
                    xcms::binSize(obw_params))) - 1), 
                    ncol = ncol(binned_ms_file_center[["profMat"]]))
                binned_ms_file_center[["profMat"]] <- rbind(
                    binned_ms_file_center[["profMat"]], tmp)
            }
            ## Eventually add empty rows at the beginning
            if (mzmin < mzr2[1]) {
                tmp <- matrix(0, (length(seq(mzmin, mzr2[1], 
                    xcms::binSize(obw_params))) - 1), 
                    ncol = ncol(cur_p[["profMat"]]))
                cur_p[["profMat"]] <- rbind(tmp, cur_p[["profMat"]])
            }
            ## Eventually add empty rows at the end
            if (mzmax > mzr2[2]) {
                tmp <- matrix(0, (length(seq(mzr2[2], mzmax, 
                    xcms::binSize(obw_params))) - 1), 
                    ncol = ncol(cur_p[["profMat"]]))
                cur_p[["profMat"]] <- rbind(cur_p[["profMat"]], tmp)
            }
            ## A final check of the data.
            mzvals <- length(mzs)
            cntr_vals <- length(binned_ms_file_center[["profMat"]])
            cur_vals <- length(cur_p[["profMat"]])
            if ((mzvals * valscantime1) != cntr_vals | 
                (mzvals * valscantime2) != cur_vals) stop(
                    "Dimensions of profile matrices of files ", 
                    basename(MSnbase::fileNames(ms_file_center)), " and ", 
                    basename(MSnbase::fileNames(ms_file)), " do not match!")
            ## Done with preparatory stuff - now I can perform the alignment.
            rtadj <- .Call("R_set_from_xcms", valscantime1, scantime1, mzvals, 
                mzs, binned_ms_file_center[["profMat"]], valscantime2, 
                scantime2, mzvals, mzs, cur_p[["profMat"]], 
                xcms::response(obw_params), xcms::distFun(obw_params), 
                xcms::gapInit(obw_params), xcms::gapExtend(obw_params), 
                xcms::factorDiag(obw_params), xcms::factorGap(obw_params), 
                as.numeric(xcms::localAlignment(obw_params)), 
                xcms::initPenalty(obw_params), PACKAGE = "xcms")
            if (length(xcms::rtime(ms_file)) != valscantime2) {
                nrt <- length(xcms::rtime(ms_file))
                adj_starts_at <- which(xcms::rtime(ms_file) == scantime2[1])
                adj_ends_at <- which(xcms::rtime(ms_file) == 
                    scantime2[length(scantime2)])
                if (adj_ends_at < nrt) rtadj <- c(rtadj, rtadj[length(rtadj)] + 
                    cumsum(diff(xcms::rtime(ms_file)[adj_ends_at:nrt])))
                if (adj_starts_at > 1) rtadj <- c(rtadj[1] + 
                    rev(cumsum(diff(xcms::rtime(ms_file)[
                        adj_starts_at:1]))), rtadj)
            }
            message("OK")
            list(unname(rtadj))
        }, error = function(e) list(unname(xcms::rtime(ms_file))))
    )
    if (show_pb) close(pb)
    adjusted_rtime <- vector(mode = "list", length = length(ms_files))
    adjusted_rtime[[xcms::centerSample(obw_params)]] <- 
        unname(xcms::rtime(ms_files[[xcms::centerSample(obw_params)]]))
    adjusted_rtime[-xcms::centerSample(obw_params)] <- 
        tmp_adjusted_rtimes
    adjusted_rtime <- adjustRtimeSubset(rtraw, adjusted_rtime)
    ms_files <- do.call(c, ms_files)
    xcms::adjustedRtime(ms_files) <- adjusted_rtime
    return(ms_files)
}
