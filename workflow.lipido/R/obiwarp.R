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
#' @param pb_fct function used to update the progress bar
#'
#' @return XCMSnExp which regroup all the files in one object
#'      with the rT corrected
#'
#' @seealso xcms::adjustRtime
obiwarp <- function(sqlite_path, samples, polarity, xsets, obw_params,
                    operator, pb_fct = NULL) {
    rtcor <- lapply(xsets, function(xset) xset@rt[[1]])
    mzranges <- lapply(xsets, function(xset) xset@mzrange)
    xsets <- do.call(c, xsets)
    xsets@rt <- list(raw = rtcor, corrected = rtcor)

    peakmat <- xsets@peaks

    center <- which.max(table(peakmat[, "sample"]))
    center_scantime <- rtcor[[center]]
    center_mzrange <- mzranges[[center]]

    cat("center sample: ", samples[center], "\nProcessing: ")

    rtimecor <- operator(
        foreach::foreach(
            s = iterators::iter(seq(1,length(samples))[-center]),
            .combine = append,
            .options.snow = if (is.null(pb_fct))
                                NULL
                            else list(progress = function(n)
                                pb_fct(n, length(samples) - 1, "Correct rT"))),
        tryCatch({
            cat(samples[s], " ")

            db <- db_connect(sqlite_path)
            center_profile <- db_get_profile(db, samples[center], polarity)
            profile <- db_get_profile(db, samples[s], polarity)
            RSQLite::dbDisconnect(db)
            scantime <- xsets@rt$raw[[s]]

            mzmin <-  min(center_mzrange[1], mzranges[[s]][1])
            mzmax <-  max(center_mzrange[2], mzranges[[s]][2])

            mz <- seq(mzmin, mzmax, by = obw_params@binSize)
            mz <- as.double(mz)
            mzval <- length(mz)

            ## median difference between spectras' scan times.
            mstdiff <- median(c(diff(center_scantime), diff(scantime)))

            center_rtup <- c(1:length(center_scantime))
            rtup <- c(1:length(scantime))

            mst <- which(diff(center_scantime) > 5 * mstdiff)[1]
            rtup <- seq(length(center_scantime))
            if (!is.na(mst)) {
                cat("Found gaps: cut scantime-vector at ",
                    center_scantime[mst],"seconds", "\n")
                center_scantime <- center_scantime[which(rtup <= mst)]
            }

            mst <- which(diff(scantime) > 5 * mstdiff)[1]
            rtup <- seq(length(scantime))
            if (!is.na(mst)) {
                cat("Found gaps: cut scantime-vector at ",
                    scantime[mst],"seconds", "\n")
                rtup <- which(rtup <= mst) # we keep rtup for later
                scantime <- scantime[rtup]
            }

            ## Drift of measured scan times - expected to be largest at the end.
            rtmaxdiff <- abs(diff(c(center_scantime[length(center_scantime)],
                                    scantime[length(scantime)])))
            ## If the drift is larger than the threshold, cut the matrix up to the
            ## max allowed difference.
            if (rtmaxdiff > (5 * mstdiff)) {
                rtmax <- min(center_scantime[length(center_scantime)],
                             scantime[length(scantime)])
                center_scantime <- center_scantime[
                    which(center_scantime <= rtmax)]
                scantime <- scantime[which(scantime <= rtmax)]
            }

            center_valscantime <- length(center_scantime)
            valscantime <- length(scantime)

            ## Restrict the profile matrix to columns 1:valscantime
            if (length(center_scantime) > center_valscantime)
                center_profile <- center_profile[, -c(
                    (center_valscantime + 1):length(center_scantime))]
            if (length(scantime) > valscantime)
                profile <- profile[, -c((valscantime + 1):length(scantime))]

            ## Now ensure that the nrow of the profile matrix matches.
            ## Add empty rows at the beginning
            if (mzmin < center_mzrange[1]) {
                ## The profile matrices should not get larger than mz!
                max_missing_rows <- mzval - nrow(center_profile)
                low_mz <- seq(mzmin, center_mzrange[1], obw_params@binSize)
                ## keep all mz bins that are smaller than mzrange,
                ## but ensure that we're not adding more rows than needed.
                seqlen <- min(sum(low_mz < center_mzrange[1]), max_missing_rows)
                x <- matrix(0, seqlen, dim(center_profile)[2])
                center_profile <- rbind(x, center_profile)
            }
            ## Add emtpy rows at the end.
            if (mzmax > mzranges[[s]][2]) {
                max_missing_rows <- mzval - nrow(center_profile)
                high_mz <- seq(center_mzrange[2], mzmax, obw_params@binSize)
                seqlen <- min(sum(high_mz > center_mzrange[2]),
                              max_missing_rows)
                x <- matrix(0, seqlen, dim(center_profile)[2])
                center_profile <- rbind(center_profile, x)
            }
            if (mzmin < mzranges[[s]][1]) {
                max_missing_rows <- mzval - nrow(profile)
                low_mz <- seq(mzmin, mzranges[[s]][1], obw_params@binSize)
                seqlen <- min(sum(low_mz < mzranges[[s]][1]), max_missing_rows)
                x <- matrix(0, seqlen, dim(profile)[2])
                profile <- rbind(x, profile)
            }
            if (mzmax > mzranges[[s]][2]) {
                max_missing_rows <- mzval - nrow(profile)
                high_mz <-  seq(mzranges[[s]][2], mzmax, obw_params@binSize)
                seqlen <- min(sum(high_mz > mzranges[[s]][2]), max_missing_rows)
                x <- matrix(0, seqlen, dim(profile)[2])
                profile <- rbind(profile, x)
            }

            ## Final check to ensure that our expansion of profile matrix rows was
            ## correct.
            if ((mzval * center_valscantime != length(center_profile)) ||
                    (mzval * valscantime != length(profile)))
                stop("Dimensions of profile matrices do not match !\n")

            ## Would it be possible to supply non-binned data too???
            tmp <- .Call("R_set_from_xcms",
                center_valscantime,
                center_scantime,
                mzval,
                mz,
                center_profile,
                valscantime,
                scantime,
                mzval,
                mz,
                profile,
                obw_params@response,
                obw_params@distFun,
                obw_params@gapInit,
                obw_params@gapExtend,
                obw_params@factorDiag,
                obw_params@factorGap,
                obw_params@localAlignment,
                obw_params@initPenalty,
                PACKAGE = "xcms")

            ## Hm, silently add the raw retention times if we cut the retention time
            ## vector above - would merit at least a warning I believe.
            if (length(scantime) > valscantime)
                tmp <- c(tmp, scantime[(max(rtup) + 1):length(scantime)])

            rm(center_profile)
            rm(profile)
            gc()
            list(tmp)
        }, error = function(e) rtcor[s])
    )

    # don't forget to add the rtcor of the center sample
    rtimecor <- append(rtimecor, rtcor[center], after = center - 1)
    xsets@rt$corrected <- rtimecor
    ## Why are we rounding here, but NOT in the retcor.peakgroups?
    ## -> issue #122
    ## The point is we're using the un-rounded adjusted rt for the rt, BUT
    ## use the rounded values for the adjustment of the peak rts.
    rtdevsmo <- lapply(seq(rtcor), function(i)
        round(rtcor[[i]] - rtimecor[[i]], 2))

    cat("\n")
    ## Why are we rounding here, but NOT in the retcor.peakgroups?
    ## -> issue #122
    rtdevsmo[[center]] <- round(rtcor[[center]] - xsets@rt$corrected[[center]], 2)

    for (i in seq(length(samples))) {
        cfun <- stepfun(rtcor[[i]][-1] - diff(rtcor[[i]]) / 2,
        rtcor[[i]] - rtdevsmo[[i]])
        rtcor[[i]] <- rtcor[[i]] - rtdevsmo[[i]]

        sidx <- which(peakmat[,"sample"] == i)
        peakmat[sidx, c("rt", "rtmin", "rtmax")] <- cfun(
        peakmat[sidx, c("rt", "rtmin", "rtmax")])
    }

    xsets@peaks <- peakmat
    xsets
}
