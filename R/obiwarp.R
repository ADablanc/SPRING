#' @title Alignment: Retention time correction methods.
#'
#' @description
#' perform retention time correction (alignment) between chromatograms of 
#'      different samples. 
#' the alignment is based on the complete mz-rt data. This method does not 
#'      require any identified peaks or defined features. 
#' This function comes from the XCMS package and only the function `.obiwarp` 
#'      was imported in order to use & export the parallelisation infos
#' 
#' @param z XCMSnExp object of the file to adjust time retentions
#' @param cntr XCMSnExp object of the file used as reference
#' @param cntr_pr a profile matrix of the file used as reference (rows 
#'      representing scans, columns equally spaced m/z values). 
#' @param parms ObiwarpParam object created by xcms
#' 
#' @return vector with the rT adjusted of the file according the rT of the 
#'      file used as reference
#' 
#' @author Johannes Rainer
#' @seealso xcms:::adjustRtime
obiwarp <- function(z, cntr, cntr_pr, parms) {
    message("Aligning ", basename(MSnbase::fileNames(z)), " against ", 
        basename(MSnbase::fileNames(cntr)), " ... ", appendLF = FALSE)
    ## Get the profile matrix for the current file.
    suppressMessages(cur_p <- xcms::profMat(z, method = "bin", 
        step = xcms::binSize(parms), returnBreaks = TRUE)[[1]])
    ## ---------------------------------------
    ## 1)Check the scan times of both objects:
    scantime1 <- unname(xcms::rtime(cntr))
    scantime2 <- unname(xcms::rtime(z))
    scantime1_diff <- diff(scantime1)
    scantime2_diff <- diff(scantime2)
    ## median difference between spectras' scan time.
    mstdiff <- stats::median(c(scantime1_diff, scantime2_diff), na.rm = TRUE)

    mst1 <- which(scantime1_diff > 5 * mstdiff)[1]
    if (!is.na(mst1)) {
        message("Found gaps in scan times of the center sample: cut ", 
            "scantime-vector at ", scantime1[mst1], " seconds.")
        scantime1 <- scantime1[seq_len(max(2, (mst1 - 1)))]
    }
    mst2 <- which(scantime2_diff > 5 * mstdiff)[1]
    if (!is.na(mst2)) {
        message("Found gaps in scan time of file ", 
            basename(MSnbase::fileNames(z)), ": cut scantime-vector at ", 
            scantime2[mst2], " seconds.")
        scantime2 <- scantime2[seq_len(max(2, (mst2 - 1)))]
    }
    ## Drift of measured scan times - expected to be largest at the end.
    rtmaxdiff <- abs(diff(c(scantime1[length(scantime1)], 
        scantime2[length(scantime2)])))
    ## If the drift is larger than the threshold, cut the matrix up to the
    ## max allowed difference.
    if (rtmaxdiff > (5 * mstdiff)) {
        rtmax <- min(scantime1[length(scantime1)], scantime2[length(scantime2)])
        scantime1 <- scantime1[scantime1 <= rtmax]
        scantime2 <- scantime2[scantime2 <= rtmax]
    }
    valscantime1 <- length(scantime1)
    valscantime2 <- length(scantime2)
    ## Ensure we have the same number of scans.
    if (valscantime1 != valscantime2) {
        min_number <- min(valscantime1, valscantime2)
        diffs <- abs(range(scantime1) - range(scantime2))
        ## Cut at the start or at the end, depending on where we have the
        ## larger difference
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
    if (ncol(cntr_pr[["profMat"]]) != valscantime1) {
        ## Find out whether we were cutting at the start or end.
        start_idx <- which(scantime1[1] == xcms::rtime(cntr))
        end_idx <- which(scantime1[length(scantime1)] == xcms::rtime(cntr))
        cntr_pr[["profMat"]] <- cntr_pr[["profMat"]][, start_idx:end_idx]
    }
    if (ncol(cur_p[["profMat"]]) != valscantime2) {
        start_idx <- which(scantime2[1] == xcms::rtime(z))
        end_idx <- which(scantime2[length(scantime2)] == xcms::rtime(z))
        cur_p[["profMat"]] <- cur_p[["profMat"]][, start_idx:end_idx]
    }
    ## ---------------------------------
    ## 2) Now match the breaks/mz range.
    ##    The -1 below is because the breaks define the upper and lower
    ##    boundary. Have to do it that way to be in line with the orignal
    ##    code... would be better to use the breaks as is.
    mzr1 <- c(cntr_pr$breaks[1], cntr_pr$breaks[length(cntr_pr$breaks) - 1])
    mzr2 <- c(cur_p$breaks[1], cur_p$breaks[length(cur_p$breaks) - 1])
    mzmin <- min(c(mzr1[1], mzr2[1]))
    mzmax <- max(c(mzr1[2], mzr2[2]))
    mzs <- seq(mzmin, mzmax, by = xcms::binSize(parms))
    ## Eventually add empty rows at the beginning
    if (mzmin < mzr1[1]) {
        tmp <- matrix(0, (length(seq(mzmin, mzr1[1], 
            xcms::binSize(parms))) - 1), ncol = ncol(cntr_pr[["profMat"]]))
        cntr_pr[["profMat"]] <- rbind(tmp, cntr_pr[["profMat"]])
    }
    ## Eventually add empty rows at the end
    if (mzmax > mzr1[2]) {
        tmp <- matrix(0, (length(seq(mzr1[2], mzmax, 
            xcms::binSize(parms))) - 1), ncol = ncol(cntr_pr[["profMat"]]))
        cntr_pr[["profMat"]] <- rbind(cntr_pr[["profMat"]], tmp)
    }
    ## Eventually add empty rows at the beginning
    if (mzmin < mzr2[1]) {
        tmp <- matrix(0, (length(seq(mzmin, mzr2[1], 
            xcms::binSize(parms))) - 1), ncol = ncol(cur_p[["profMat"]]))
        cur_p[["profMat"]] <- rbind(tmp, cur_p[["profMat"]])
    }
    ## Eventually add empty rows at the end
    if (mzmax > mzr2[2]) {
        tmp <- matrix(0, (length(seq(mzr2[2], mzmax, 
            xcms::binSize(parms))) - 1), ncol = ncol(cur_p[["profMat"]]))
        cur_p[["profMat"]] <- rbind(cur_p[["profMat"]], tmp)
    }
    ## A final check of the data.
    mzvals <- length(mzs)
    cntr_vals <- length(cntr_pr[["profMat"]])
    cur_vals <- length(cur_p[["profMat"]])
    if ((mzvals * valscantime1) != cntr_vals | 
        (mzvals * valscantime2) != cur_vals) stop(
            "Dimensions of profile matrices of files ", 
            basename(MSnbase::fileNames(cntr)), " and ", 
            basename(MSnbase::fileNames(z)), " do not match!")
    ## Done with preparatory stuff - now I can perform the alignment.
    rtadj <- .Call("R_set_from_xcms", valscantime1, scantime1, mzvals, mzs, 
        cntr_pr[["profMat"]], valscantime2, scantime2, mzvals, mzs, 
        cur_p[["profMat"]], xcms::response(parms), xcms::distFun(parms), 
        xcms::gapInit(parms), xcms::gapExtend(parms), xcms::factorDiag(parms), 
        xcms::factorGap(parms), as.numeric(xcms::localAlignment(parms)), 
        xcms::initPenalty(parms), PACKAGE = "xcms")
    if (length(xcms::rtime(z)) != valscantime2) {
        nrt <- length(xcms::rtime(z))
        adj_starts_at <- which(xcms::rtime(z) == scantime2[1])
        adj_ends_at <- which(xcms::rtime(z) == scantime2[length(scantime2)])
        if (adj_ends_at < nrt) rtadj <- c(rtadj, rtadj[length(rtadj)] + 
            cumsum(diff(xcms::rtime(z)[adj_ends_at:nrt])))
        if (adj_starts_at > 1) rtadj <- c(rtadj[1] + 
            rev(cumsum(diff(xcms::rtime(z)[adj_starts_at:1]))), rtadj)
    }
    message("OK")
    return(unname(rtadj))
}
