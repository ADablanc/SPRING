#' @title Group peaklists
#'
#' @description
#' Group multiple peaklists from a `xcmsSet` object
#' Use the method group.density from XCMS
#' The function was modified to parallelize &
#'      output the progression on a progress bar
#'
#' @param xset `xcmsSet` object with peaklists
#' @param pd_params `PeakDensityParam` object created by XCMS
#' @param operator `function` to use for parallelization (`\%dopar\%`)
#'      or not (`\%do\%`)
#' @param pb_fct `function` used to update the progress bar
#'
#' @return `xcmsSet` object with the grouped peaks
#'
#' @seealso xcms::group.density
group_peaks <- function(xset,
                        pd_params,
                        operator = foreach::"%do%",
                        pb_fct = NULL) {
    peaks <- xset@peaks

    sample_groups <- as.character(pd_params@sampleGroups)
    sample_group_table <- table(sample_groups)

    peaks <- cbind(
        peaks[, c("mz", "rt", "sample"), drop = FALSE],
        index = seq_len(nrow(peaks))
    )

    # Order peaks matrix by mz
    peaks <- peaks[order(peaks[, "mz"]), , drop = FALSE]
    rownames(peaks) <- NULL
    rt_range <- range(peaks[, "rt"])

    # Define the mass slices and the index in the peaks matrix with an mz
    # value >= mass[i].
    mass <- seq(
        peaks[1, "mz"],
        peaks[nrow(peaks), "mz"] + pd_params@binSize,
        by = pd_params@binSize / 2
    )
    masspos <- xcms:::findEqualGreaterM(peaks[, "mz"], mass)

    dens_from <- rt_range[1] - 3 * pd_params@bw
    dens_to <- rt_range[2] + 3 * pd_params@bw
    ## Increase the number of sampling points for the density distribution.
    dens_n <- max(
        512,
        2 * 2** (ceiling(log2(
            diff(rt_range) / (xcms::bw(pd_params) / 2)
        )))
    )

    # only to delete the note : no visible binding for global variable
    i <- NULL
    groups <- operator(
        foreach::foreach(
            i = seq_len(length(mass) - 2),
            .combine = rbind,
            .options.snow = if (is.null(pb_fct)) {
                NULL
            } else {
                list(progress = function(n) {
                    pb_fct(n, length(mass) - 2, "Group")
                }
            )}
        ), {
            start_idx <- masspos[i]
            end_idx <- masspos[i + 2] - 1
            if (end_idx - start_idx < 0) {
                NULL
            } else {
                xcms:::.group_peaks_density(
                    peaks[start_idx:end_idx, , drop = FALSE],
                    bw = pd_params@bw,
                    densFrom = dens_from,
                    densTo = dens_to,
                    densN = dens_n,
                    sampleGroups = sample_groups,
                    sampleGroupTable = sample_group_table,
                    minFraction = pd_params@minFraction,
                    minSamples = pd_params@minSamples,
                    maxFeatures = pd_params@maxFeatures,
                    sleep = 0
                )
            }
        }
    )
    if (nrow(groups) > 0) {
        # Remove groups that overlap with more "well-behaved" groups
        numsamp <- rowSums(as.matrix(
            groups[,
                   (match("npeaks", colnames(groups)) + 1):
                    (ncol(groups) - 1), drop = FALSE])
        )
        uorder <- order(-numsamp, groups[, "npeaks"])

        uindex <- xcms:::rectUnique(
            as.matrix(groups[, c("mzmin", "mzmax", "rtmin", "rtmax"),
                             drop = FALSE]),
            uorder
        )
        groups <- groups[uindex, , drop = FALSE]
        rownames(groups) <- NULL
    }

    xset@groups <- as.matrix(groups[, -match("peakidx", colnames(groups))])
    xset@groupidx <- groups$peakidx
    xset
}
