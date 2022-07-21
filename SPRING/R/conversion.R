#' @title Convert RAW file
#'
#' @description
#' Convert one RAW file to a mzXML file
#' It will try to convert the file and extract only the scans in positive or
#' negative
#' It also trim the file according the rt range and m/z range specified in
#' the filter_params with msconvert
#' Check with MSnbase package if the file can be converted
#' If the file is a CDF it will copy the file instead
#' If the conversion failed & the file is a mzML or mzXML
#' it will copy the file instead and try to trim only the rt range
#'
#' @param raw_file `character(1)` filepath
#' @param converter `character(1)` filepath to msconvert executable
#' @param filter_params `FilterParam` object
#'
#' @return `character(1)` filepath of the converted file
convert_file <- function(raw_file, converter, filter_params) {
    filepath <- gsub(
        "\\\\",
        "/",
        file.path(
            tempdir(),
            paste(
                tools::file_path_sans_ext(basename(raw_file)),
                "mzXML",
                sep = "."
            )
        )
    )

    # if it is a wiff file it needs the corresponding wiff.scan file
    if (grepl("\\.WIFF$", raw_file, ignore.case = TRUE)) {
        if (!file.exists(paste(raw_file, "SCAN", sep = "."))) {
            stop("missing corresponding wiff.scan in same directory")
        }
    }
    if (grepl("\\.mzXML$", raw_file) || grepl("\\.mzML$", raw_file)
    ) algorithm <- "cwt" else algorithm <- "vendor"

    query <- sprintf(
        paste(
            "\"%s\" \"%s\" -o \"%s\"",
            "--outfile \"%s\" --mzXML",
            # "-z -n",
            "--filter \"peakPicking %s msLevel=1-\"",
            # "--filter \"zeroSamples removeExtra\"",
            "--filter \"polarity %s\"",
            "--filter \"scanTime [%s,%s]\""#,
            # "--filter \"mzWindow [%s,%s]\""
        ),
        converter, raw_file, dirname(filepath),
        basename(filepath),
        algorithm,
        filter_params@polarity,
        filter_params@rt_range[1], filter_params@rt_range[2]#,
        # filter_params@mz_range[1], filter_params@mz_range[2]
    )
    suppressWarnings(
        msg <- system(query, intern = TRUE, wait = TRUE)
    )

    if (!file.exists(filepath)) {
        if (grepl("\\.mzML$", raw_file) || grepl("\\.mzXML$", raw_file)) {
            file.copy(raw_file, filepath)
        } else {
            print(msg)
            stop("msconvert error")
        }
    }
    check_ms_file(filepath, filter_params)
}

#' @title Check mass spectrometry file
#'
#' @description
#' check if a mass spectrometry file can be read by MSnbase
#' & if the polarity expected is the good one
#' If not it will split the file & keep only scans with the desired polarity &
#' rewrite it in a new file (see function (split_ms_file))
#'
#' @param filepath `character(1)`
#' @param filter_params `FilterParam` object
#'
#' @return `character(1)` filepath to the file checked
check_ms_file <- function(filepath, filter_params) {
    exp_polarity <- filter_params@polarity
    # check if file can be read
    ms_file <- tryCatch(
        MSnbase::readMSData(filepath, mode = "onDisk"),
        error = function(e) {
            e$message
        })
    if (class(ms_file) != "OnDiskMSnExp") {
        stop(ms_file)
    }

    # cannot extract polarity from CDF files
    if (!grepl("cdf$", filepath, ignore.case = TRUE)) {
        obs_polarity <- unique(MSnbase::polarity(ms_file))

        # if there is more than one polarity, split the file &
            # keep only the desired polarity
        if (length(obs_polarity) > 1) {
            filepath <- split_ms_file(ms_file, exp_polarity)
        } else if (
                (exp_polarity == "positive" && obs_polarity < 1) ||
                (exp_polarity == "negative" && obs_polarity >= 1)
            ) {
            stop("no scans detected in desired polarity")
        }
    }

    filter_ms_file(ms_file, filter_params@rt_range)
}

#' @title Split MS file
#'
#' @description
#' Split MS file to keep only the scans in the desired polarity & rewrite it
#' in a new file
#'
#' @param ms_file `OnDiskMSnExp` MSnbase object
#' @param exp_polarity `character(1)` expected polarity ("positive" or
#' "negative")
#' @param overwrite `logical(1)` should overwrite the file if needed ?
#'
#' @return `character(1)` filepath
split_ms_file <- function(ms_file, exp_polarity, overwrite = TRUE) {
    filepath <- MSnbase::fileNames(ms_file)
    if (exp_polarity == "positive") {
        spectras <- which(MSnbase::polarity(ms_file) >= 1)
    } else {
        spectras <- which(MSnbase::polarity(ms_file) < 1)
    }
    out_filepath <- file.path(tempdir(), paste0(runif(1), ".mzML"))
    MSnbase::writeMSData(ms_file[spectras], out_filepath, copy = TRUE)
    if (overwrite) {
        file.rename(out_filepath, filepath)
        filepath
    } else {
        out_filepath
    }
}

#' @title Filter MS file
#'
#' @description
#' Filter MS file according a rT range in seconds
#' don't filter on the m/z dimension (too long & too heavy in flash memory)
#'
#' @param ms_file `OnDiskMSnExp` MSnbase object
#' @param exp_rt_range `numeric(2)` expected rT range (in seconds !!!)
#' @param overwrite `logical(1)` should overwrite the file if needed ?
#'
#' @return `character(1)` filepath
filter_ms_file <- function(ms_file, exp_rt_range, overwrite = TRUE) {
    filepath <- MSnbase::fileNames(ms_file)
    obs_rt_range <- MSnbase::rtime(ms_file)
    if (
        range(obs_rt_range)[1] <= exp_rt_range[1] ||
        range(obs_rt_range)[2] >= exp_rt_range[2]
    ) {
        spectras <- which(obs_rt_range >= exp_rt_range[1] &
                obs_rt_range <= exp_rt_range[2])
        if (length(spectras) == 0) {
            stop("no spectras between rt filters")
        }
        out_filepath <- file.path(tempdir(), paste0(runif(1), ".mzML"))
        MSnbase::writeMSData(ms_file[spectras], out_filepath, copy = TRUE)
        if (overwrite) {
            file.rename(out_filepath, filepath)
        } else {
            return(out_filepath)
        }
    }
    filepath
}
