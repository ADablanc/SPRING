#' @title Group peaklists
#'
#' @description
#' Group multiple peaklists from a XCMSnExp object
#' Use the method group.density from XCMS
#' The function was modified to parallelize & 
#'      output the progression on a progress bar
#'
#' @param ms_files XCMSnExp object with peaklists (not checked)
#' @param pd_params PeakDensityParam object created by XCMS
#' @param operator function to use for parallelization (`\%dopar\%`) 
#'      or not (`\%do\%`)
#' @param show_pb boolean indicate if we print a progress bar or not
#'
#' @return XCMSnExp object with the grouped info
#' 
#' @seealso xcms::group.density
group_peaks <- function(ms_files, pd_params, operator, show_pb) {
    peaks <- xcms::chromPeaks(ms_files)
    
    sample_groups <- as.character(xcms::sampleGroups(pd_params))
    sample_group_table <- table(sample_groups)
    
    peaks <- cbind(peaks[, c("mz", "rt", "sample"), drop = FALSE],
        index = seq_len(nrow(peaks)))

    ## Order peaks matrix by mz
    peaks <- peaks[order(peaks[, "mz"]), , drop = FALSE]
    rownames(peaks) <- NULL
    rt_range <- range(peaks[, "rt"])

    ## Define the mass slices and the index in the peaks matrix with an mz
    ## value >= mass[i].
    mass <- seq(peaks[1, "mz"], 
        peaks[nrow(peaks), "mz"] + xcms::binSize(pd_params), 
        by = xcms::binSize(pd_params) / 2)
    masspos <- xcms:::findEqualGreaterM(peaks[, "mz"], mass)

    dens_from <- rt_range[1] - 3 * xcms::bw(pd_params)
    dens_to <- rt_range[2] + 3 * xcms::bw(pd_params)
    ## Increase the number of sampling points for the density distribution.
    dens_n <- max(512, 2 * 2** (ceiling(log2(diff(rt_range) / 
        (xcms::bw(pd_params) / 2)))))
    
    if (show_pb) pb <- utils::txtProgressBar(min = 0, 
            max = length(mass) - 2, style = 3)
    # only to delete the note : no visible binding for global variable
    i <- NULL
    groups <- operator(
        foreach::foreach(
            i = seq_len(length(mass) - 2), 
            .combine = rbind, 
            .options.snow = if (!show_pb) NULL 
                else list(progress = function(n) 
                    utils::setTxtProgressBar(pb, n))
        ), {
            start_idx <- masspos[i]
            end_idx <- masspos[i + 2] - 1
            if (end_idx - start_idx < 0) NULL
            else xcms:::.group_peaks_density(
                peaks[start_idx:end_idx, , drop = FALSE],
                bw = xcms::bw(pd_params), densFrom = dens_from,
                densTo = dens_to, densN = dens_n,
                sampleGroups = sample_groups,
                sampleGroupTable = sample_group_table,
                minFraction = xcms::minFraction(pd_params),
                minSamples = xcms::minSamples(pd_params),
                maxFeatures = xcms::maxFeatures(pd_params),
                sleep = 0
            )
        }
    )
    if (show_pb) close(pb)
    if (nrow(groups) > 0) {
        ## Remove groups that overlap with more "well-behaved" groups
        numsamp <- rowSums(
            as.matrix(groups[, 
                (match("npeaks", colnames(groups)) + 1):(ncol(groups) - 1),
                    drop = FALSE]))
        uorder <- order(-numsamp, groups[, "npeaks"])

        uindex <- xcms:::rectUnique(
            as.matrix(groups[, c("mzmin", "mzmax", "rtmin", "rtmax"),
                drop = FALSE]), uorder)
        groups <- groups[uindex, , drop = FALSE]
        rownames(groups) <- NULL
    }
    
    ## Add the results.
    df <- S4Vectors::DataFrame(groups)
    if (!nrow(df)) stop("Unable to group any chromatographic peaks. ",
        "You might have to adapt your settings.")
    df$ms_level <- as.integer(1)
    rownames(df) <- xcms:::.featureIDs(nrow(df))
    xcms::featureDefinitions(ms_files) <- df
    return(ms_files)
}
