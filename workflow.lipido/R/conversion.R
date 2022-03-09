#' @title Convert RAW file
#'
#' @description
#' Convert one RAW file to a mzXML file
#' It will try to convert the file and extract only the scans in positive or
#' negative
#' It also trim the file according the rt range and m/z range specified in
#' the filter_params with msconvert
#' Check with XCMS package if the file can be converted
#' If the file is a CDF it will copy the file instead
#' If the conversion failed & the file is a mzML or mzXML
#' it will copy the file instead and try to trim only the rt range
#'
#' @param raw_file `character(1)` filepath
#' @param converter `character(1)` filepath to msconvert executable
#' @param polarity `character(1)` "positive" or "negative" only !
#' @param filter_params `FilterParam` object
#'
#' @return `xcmsRaw` object
convert_file <- function(raw_file, converter, polarity, filter_params) {
    filepath <- tempfile(fileext = ".mzXML")

    # if it is a wiff file it needs the corresponding wiff.scan file
    if (grepl("\\.WIFF$", raw_file, ignore.case = TRUE)) {
        if (!file.exists(paste(raw_file, "SCAN", sep = "."))) {
            stop("missing corresponding wiff.scan in same directory")
        }
    }
    # if it is a Water repertory, don't use the vendor algorithm
    if ((grepl("\\.raw$", raw_file) & dir.exists(raw_file)) ||
        grepl("\\.mzXML$", raw_file) | grepl("\\.mzML$", raw_file)
    ) algorithm <- "cwt" else algorithm <- "vendor"

    query <- sprintf(
        paste(
            "\"%s\" \"%s\" -o \"%s\"",
            "--outfile \"%s\" --mzXML -z -n",
            "--filter \"peakPicking %s msLevel=1-\"",
            "--filter \"zeroSamples removeExtra\"",
            "--filter \"polarity %s\"",
            "--filter \"scanTime [%s,%s]\"",
            "--filter \"mzWindow [%s,%s]\""
        ),
        converter, raw_file, dirname(filepath),
        basename(filepath),
        algorithm,
        polarity,
        filter_params@rt_range[1], filter_params@rt_range[2],
        filter_params@mz_range[1], filter_params@mz_range[2]
    )
    msconvert_blabla <- suppressWarnings(
        system(query, intern = TRUE, wait = TRUE)
    )

    if (!file.exists(filepath)) {
        if (grepl("\\.mzML$", raw_file)) {
            filepath <- tempfile(fileext = ".mzML")
            file.copy(raw_file, filepath, overwrite = TRUE)
        } else if (grepl("\\.mzXML$", raw_file)) {
            file.copy(raw_file, filepath, overwrite = TRUE)
        } else {
            stop("msconvert error")
        }
    }
    check_ms_file(filepath, polarity)
}

#' @title Check mass spectrometry file
#'
#' @description
#' check if a mass spectrometry file can be read by xcms
#' & if the polarity expected is the good one
#'
#' @param filepath `character(1)`
#' @param exp_polarity `character(1)` "positive" or "negative" only !
#'
#' @return `xcmsRaw` object
check_ms_file <- function(filepath, exp_polarity) {
    # check if file can be read
    ms_file <- tryCatch(
        xcms::xcmsRaw(filepath, profstep = 0),
        error = function(e)
            e$message)
    if (class(ms_file) != "xcmsRaw") {
        stop("file converted cannot be read")
    }
    obs_polarity <- ms_file@polarity
    if (!any(obs_polarity == exp_polarity)) {
        stop("no scans detected in desired polarity")
    } else if (length(unique(obs_polarity)) > 1) {
        scan_idx <- which(obs_polarity == exp_polarity)
        ms_file[scan_idx]
    } else ms_file
}

#' @title Filter MS file
#'
#' @description
#' Filter MS file according a rT range in seconds
#' don't filter on the m/z dimension (too long & too heavy in flash memory)
#'
#' @param ms_file `xcmsRaw` object
#' @param filter_params `FilterParam` object
#'
#' @return `xcmsRaw` object filtered
filter_ms_file <- function(ms_file, filter_params) {
    obs_rt_range <- ms_file@scantime
    if (range(obs_rt_range)[1] <= filter_params@rt_range[1] |
        range(obs_rt_range)[2] >= filter_params@rt_range[2]
    ) {
        scan_idx <- which(
            ms_file@scantime >= filter_params@rt_range[1] &
            ms_file@scantime <= filter_params@rt_range[2]
        )
        if (length(scan_idx) == 0) {
            stop("no spectras between rt filters")
        }
        ms_file[scan_idx]
    } else {
        ms_file
    }
}
