#' @title Correct retention times
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
#' @param sqlite_path `character(1)` path to the sqlite file
#' @param samples `character vector` name of the samples in the database, needed
#' to load the `xcmsRaw` objects
#' @param polarity `character(1)` "negative" or "positive", needed to load the
#' good `xcmsRaw` objects
#' @param xsets `xcmsSet list`
#' @param obw_params `ObiwarpParam` object
#' @param operator function to use for parallelization (`\%dopar\%`)
#'      or not (`\%do\%`)
#' @param pb_fct function used to update the progress bar
#'
#' @return `xcmsSet` which regroup all the files in one object with the rT
#' corrected
#'
#' @seealso xcms::adjustRtime
obiwarp <- function(sqlite_path,
                    samples,
                    polarity,
                    xsets,
                    obw_params,
                    operator = foreach::"%do%",
                    pb_fct = NULL) {
    rtcor <- lapply(xsets, function(xset) xset@rt[[1]])
    mzranges <- lapply(xsets, function(xset) xset@mzrange)
    peakmat <- do.call(rbind, lapply(seq(xsets), function(i)
        if (nrow(xsets[[i]]@peaks) > 0) {
            cbind(xsets[[i]]@peaks[, -23], sample = i)
        } else {
            xsets[[i]]@peaks
        }
    ))
    xset <- do.call(c, xsets)
    xset@rt <- list(raw = rtcor, corrected = rtcor)

    if (length(xsets) == 1) {
        return(xset)
    }
    if (nrow(peakmat) == 0) {
        center <- 1
    } else {
        center <- as.integer(names(which.max(table(peakmat[, "sample"]))))
    }

    s <- NULL # just to get rid of the NOTE when checking package
    rtimecor <- operator(
        foreach::foreach(
            s = iterators::iter(seq(1, length(samples))[-center]),
            .combine = append,
            .options.snow = list(
                progress = if (is.null(pb_fct)) {
                    NULL
                } else {
                    function(n) {
                        pb_fct(n, length(samples) - 1, "Correct rT")
                    }
                }
            )
        ), {
            db <- db_connect(sqlite_path)
            center_profile <- db_get_profile(db, samples[center], polarity)
            profile <- db_get_profile(db, samples[s], polarity)
            RSQLite::dbDisconnect(db)

            if (is.null(profile)) {
                return(list(rtcor[[s]]))
            }

            center_scantime <- rtcor[[center]]
            center_mzrange <- mzranges[[center]]

            scantime <- rtcor[[s]]
            mzrange <- mzranges[[s]]

            mzmin <- min(center_mzrange[1], mzrange[1])
            mzmax <- max(center_mzrange[2], mzrange[2])
            ########## this is not true if we use the profMat function
            mzmin <- floor(mzmin / obw_params@binSize) * obw_params@binSize
            mzmax <- ceiling(mzmax / obw_params@binSize) * obw_params@binSize
            mz <- seq(mzmin, mzmax, by = obw_params@binSize)
            mz <- as.double(mz)
            mzval <- length(mz)

            ## median difference between spectras' scan times.
            diff_scantime <- stats::median(
                c(diff(center_scantime),
                  diff(scantime))
            )

            # check if there is some gaps in the scantime of the center profile
            center_scantime_gap <- which(
                diff(center_scantime) > 5 * diff_scantime)[1]
            if (!is.na(center_scantime_gap)) {
                center_profile <- center_profile[, 1:center_scantime_gap]
                center_scantime <- center_scantime[1:center_scantime_gap]

            }

            # check if there is some gaps in the scantime of the profile
            scantime_gap <- which(diff(scantime) > 5 * diff_scantime)[1]
            if (!is.na(scantime_gap)) {
                profile <- profile[, 1:scantime_gap]
                scantime <- scantime[1:scantime_gap]
            }

            # check if there is a gap of rtmax between profile & center
            diff_rtmax <- abs(center_scantime[length(center_scantime)] -
                                 scantime[length(scantime)])
            # readjust according the rtmax possible
            if (diff_rtmax > 5 * diff_scantime) {
                rtmax <- min(
                    center_scantime[length(center_scantime)],
                    scantime[length(scantime)]
                )
                center_profile <- center_profile[,
                    which(center_scantime <= rtmax)]
                center_scantime <- center_scantime[
                    which(center_scantime <= rtmax)]
                profile <- profile[, which(scantime <= rtmax)]
                scantime <- scantime[which(scantime <= rtmax)]
            }

            # Now ensure that the nrow of the center profile matrix matches.
            # Add empty rows at the beginning of the profile matrix
            if (mzmin < center_mzrange[1]) {
                max_missing_rows <- mzval - nrow(center_profile)
                low_mz <- seq(mzmin, center_mzrange[1], obw_params@binSize)
                # keep all mz bins that are smaller than mzrange,
                # but ensure that we're not adding more rows than needed.
                seqlen <- min(sum(low_mz < center_mzrange[1]), max_missing_rows)
                x <- matrix(0, seqlen, dim(center_profile)[2])
                center_profile <- rbind(x, center_profile)
            }
            # Add emtpy rows at the end.
            if (mzmax > center_mzrange[2]) {
                max_missing_rows <- mzval - nrow(center_profile)
                high_mz <- seq(center_mzrange[2], mzmax, obw_params@binSize)
                seqlen <- min(sum(high_mz > center_mzrange[2]),
                              max_missing_rows)
                x <- matrix(0, seqlen, dim(center_profile)[2])
                center_profile <- rbind(center_profile, x)
            }

            # Now ensure that the nrow of the profile matrix matches.
            # Add empty rows at the beginning of the profile matrix
            if (mzmin < mzrange[1]) {
                max_missing_rows <- mzval - nrow(profile)
                low_mz <- seq(mzmin, mzrange[1], obw_params@binSize)
                seqlen <- min(sum(low_mz < mzrange[1]), max_missing_rows)
                x <- matrix(0, seqlen, dim(profile)[2])
                profile <- rbind(x, profile)
            }
            if (mzmax > mzrange[2]) {
                max_missing_rows <- mzval - nrow(profile)
                high_mz <-  seq(mzrange[2], mzmax, obw_params@binSize)
                seqlen <- min(sum(high_mz > mzrange[2]), max_missing_rows)
                x <- matrix(0, seqlen, dim(profile)[2])
                profile <- rbind(profile, x)
            }

            # Final check to ensure that our expansion of profile matrix rows
            # was correct.
            center_valscantime <- length(center_scantime)
            valscantime <- length(scantime)
            if (
                (mzval * center_valscantime != length(center_profile)) ||
                (mzval * valscantime != length(profile))
            ) {
                stop("Dimensions of profile matrices do not match !\n")
            }

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
                PACKAGE = "xcms"
            )

            # Hm, silently add the raw retention times if we cut the retention
            # time vector above - would merit at least a warning I believe.
            if (length(rtcor[[s]]) > valscantime) {
                tmp <- c(tmp, rtcor[[s]][
                    (valscantime + 1) : length(rtcor[[s]])])
            }

            rm(center_profile)
            rm(profile)
            gc()
            list(tmp)
        }
    )

    # don't forget to add the rtcor of the center sample
    rtimecor <- append(rtimecor, rtcor[center], after = center - 1)
    xset@rt$corrected <- rtimecor
    # Why are we rounding here, but NOT in the retcor.peakgroups?
    # -> issue #122
    # The point is we're using the un-rounded adjusted rt for the rt, BUT
    # use the rounded values for the adjustment of the peak rts.
    rtdevsmo <- lapply(seq(rtcor), function(i)
        round(rtcor[[i]] - rtimecor[[i]], 2))

    for (i in seq(length(samples))) {
        if (length(rtcor[[i]]) == 0) {
            next
        }
        cfun <- stats::stepfun(
            rtcor[[i]][-1] - diff(rtcor[[i]]) / 2,
            rtcor[[i]] - rtdevsmo[[i]]
        )
        rtcor[[i]] <- rtcor[[i]] - rtdevsmo[[i]]

        sidx <- which(peakmat[, "sample"] == i)
        if (length(sidx) > 0) {
            peakmat[sidx, c("rt", "rtmin", "rtmax")] <- cfun(
                peakmat[sidx, c("rt", "rtmin", "rtmax")]
            )
        }
    }

    xset@peaks <- peakmat
    xset
}
