#' @title Split Scans in a Mass Spectrometry file
#'
#' @description
#' split mass spectrometry file according polarity of scans &
#'     the polarity desired
#' record a new mzXML file with scans of the polarity desired
#'
#' @param ms_file OnDiskMSnExp, ms file object read by MSnbase pkg
#' @param polarity integer, can only be 0 (negative) or 1(positive)
#'
#' @return message "SUCCESS" if no error occur or the reason of the failure
split_ms_file <- function(ms_file,
                          polarity) {
    spectras <- if (polarity == 1)
        which(MSnbase::polarity(ms_file) >= polarity)
    else
        which(MSnbase::polarity(ms_file) <= polarity)
    if (length(spectras) == 0)
        stop("no scans in the desired polarity")
    tmp_filepath <- file.path(tempdir(),
                              paste0(stats::runif(1), ".mzXML"))
    MSnbase::writeMSData(ms_file[spectras],
                         tmp_filepath,
                         copy = TRUE)
    # replace the old file by the new one
    filepath <- MSnbase::fileNames(ms_file)
    file.copy(tmp_filepath,
              filepath)
    return("SUCCESS")
}

#' @title Check mass spectrometry file
#'
#' @description
#' check if a mass spectrometry file can be read by MSnbase
#'      & if the polarity expected is the good one
#'
#' @param filepath
#' @param polarity 1 if positive, 0 if negative
#' @param verbose boolean disable prints, only useful for testing
#'
#' @return message "SUCCESS" if no error occur or the reason of the failure
check_ms_file <- function(filepath,
                          polarity,
                          verbose) {
    # check if file can be read
    ms_file <- tryCatch(
        MSnbase::readMSData(filepath, mode = "onDisk"),
        error = function(e)
            e$message
    )
    if (class(ms_file) != "OnDiskMSnExp") {
        print(ms_file)
        file.remove(filepath)
        stop("file converted cannot be read")
    }
    msg <- tryCatch({
        # check if there is some scans
        if (length(ms_file) == 0)
            stop("no scans detected")

        # chek if polarity is good
        if (grepl("\\.CDF$", filepath, ignore.case = TRUE))
            stop("cannot get polarity, guess it is the good one")
        real_polarity <- unique(MSnbase::polarity(ms_file))
        if (is.na(real_polarity))
            stop("cannot get polarity, guess it is the good one")
        else if (length(real_polarity) > 1)
            split_ms_file(ms_file, polarity)
        else if ((polarity == 1 & real_polarity < 1) |
                 (polarity == 0 & real_polarity > 0))
            "no scans detected in desired polarity"
        else
            "SUCCESS"
    }, error = function(e) {
        file.remove(filepath)
        e$message
    })
    if (verbose)
        print(msg)
    return(msg)
}

#' @title Convert RAW file
#'
#' @description
#' Convert one RAW file to a mzXML file
#' It will try to convert the file in pos or neg
#' Check with MSnbase package if the file can be converted
#' If the file is a CDF it will copy the file instead
#' If the conversion failed & the file is a mzML or mzXML
#'      it will copy the file instead
#'
#' @param raw_file filepath
#' @param outdir dirpath to the directory of the converted file
#' @param converter filepath to msconvert executable
#' @param polarity "positive" or "negative"
#' @param verbose boolean disable prints, only useful for testing
#'
#' @return message "SUCCESS" if no error occur or the reason of the failure
convert_file <- function(raw_file,
                         outdir,
                         converter,
                         polarity,
                         verbose = FALSE) {
    # if it is a wiff file it needs the corresponding wiff.scan file
    if (grepl("\\.WIFF$", raw_file, ignore.case = TRUE))
        if (!file.exists(paste(raw_file, "scan", sep = ".")) |
            !file.exists(paste(raw_file, "SCAN", sep = ".")))
            stop("missing corresponding wiff.scan in same directory")
    # if it is a CDF file : cannot centroid it
    if (grepl("\\.CDF$", raw_file, ignore.case = TRUE)) {
        new_filepath <- file.path(outdir, basename(raw_file))
        file.copy(raw_file, new_filepath, overwrite = TRUE)
    } else {
        if (verbose) {
            print("#######################")
            print(sprintf(
                "conversion of %s in %s",
                basename(raw_file),
                polarity
            ))
        }
        new_filename <-
            paste(tools::file_path_sans_ext(basename(raw_file)),
                  "mzXML", sep = ".")
        new_filepath <- file.path(outdir, new_filename)
        # if it is a Water repertory, don"t use the vendor algorithm
        algorithm <- if ((grepl("\\.raw$", raw_file) &
                          !dir.exists(raw_file)) |
                         grepl("\\.mzXML$", raw_file) |
                         grepl("\\.mzML$", raw_file))
            "cwt"
        else
            "vendor"

        query <- sprintf(
            paste(
                "\"%s\" \"%s\" -o \"%s\"",
                "--outfile \"%s\" --mzXML -z -n",
                "--filter \"peakPicking %s msLevel=1-\"",
                "--filter \"zeroSamples removeExtra\"",
                "--filter \"polarity %s\""
            ),
            converter,
            raw_file,
            outdir,
            new_filename,
            algorithm,
            polarity
        )
        if (verbose)
            print(query)
        msconvert_blabla <-
            system(query, intern = TRUE, wait = TRUE)
        if (verbose)
            print(msconvert_blabla)

        if (!file.exists(new_filepath)) {
            if (grepl("\\.mzXML$", raw_file) | grepl("\\.mzML$", raw_file)) {
                new_filepath <- file.path(outdir, basename(raw_file))
                file.copy(raw_file, new_filepath, overwrite = TRUE)
            } else
                stop("msconvert error")
        }
    }
    return(check_ms_file(new_filepath,
                         if (polarity == "positive")
                             1
                         else
                             0,
                         verbose))
}

#' @title Convert RAW files
#'
#' @description
#' Convert RAW files with msconvert to mzXML files
#' It will create in the `outdir` directory two dirs :
#' one for the positive converted file, the other for the negatives
#' For each file converted with success it will run some test to see
#'      if the file can be read, contains not empty scans, ...
#'
#' @param raw_files vector of filepaths
#' @param converter filepath to the msconvert executable
#' @param outdir dirpath to directory where to save the mzXML files
#' @param cores numeric number of cores to use for the peakpicking
#'      (one core = one file peakpicked)
#'      if cores == 1 it will print for each file a commands &
#'          result of the conversion (useful for tracking)
#' @param show_txt_pb boolean print a progress bar or not on the console
#' @param pb_fct function used to update the progress bar
#'
#' @return dataframe (each row represent a file) with a column foreach polarity
#'      Each cell contains a message about the conversion process
#'          ("SUCCESS" if no errors occur for example)
#'
#' @export
#' @examples
#' \dontrun{convert_files("testdata/small.RAW",
#'      converter = "pwiz/msconvert.exe", "outdir = "test")}
convert_files <- function(raw_files,
                          converter,
                          outdir,
                          cores = parallel::detectCores(),
                          show_txt_pb = TRUE,
                          pb_fct = NULL) {
    # check if the system is Windows
    if (Sys.info()[["sysname"]] != "Windows")
        stop("conversion only works on Windows")
    if (length(raw_files) == 0)
        stop("you must give at least one raw file to convert")
    if (class(raw_files) != "character")
        stop("raw_files argument must be a vector of filepaths")
    file_ext <-
        c(
            "\\.mzML$",
            "\\.mzXML$",
            "\\.CDF$",
            "\\.RAW$",
            "\\.d$",
            "\\.YEP$",
            "\\.BAF$",
            "\\.FID$",
            "\\.WIFF$",
            "\\.MGF$"
        )
    test_ext <- sapply(raw_files, function(x)
        any(sapply(file_ext, grepl, x, ignore.case = TRUE)))
    if (any(!test_ext))
        stop(sprintf(
            "file extension of %s are not supported",
            paste(basename(raw_files[!test_ext]), collapse = " ")
        ))
    test_exist <- file.exists(raw_files)
    if (any(!test_exist))
        stop(sprintf("file(s) %s doesn't exist",
                     paste(raw_files[!test_exist], collapse = " ")))
    raw_files <- normalizePath(raw_files)

    if (class(converter) != "character")
        stop("converter argument must be a filepath to the msconvert exe")
    if (length(converter) > 1)
        stop("converter argument must contain only one filepath")
    if (!file.exists(converter))
        stop(sprintf("converter is not found at %s",
                     converter))
    converter <- normalizePath(converter)

    if (class(outdir) != "character")
        stop("outdir argument must be a filepath")
    else if (length(outdir) > 1)
        stop("outdir argument must contain only one filepath")
    outdir <- normalizePath(outdir)
    if (!dir.exists(outdir))
        dir.create(outdir)
    pos_outdir <- normalizePath(file.path(outdir, "pos"))
    neg_outdir <- normalizePath(file.path(outdir, "neg"))
    if (!dir.exists(pos_outdir))
        dir.create(pos_outdir)
    if (!dir.exists(neg_outdir))
        dir.create(neg_outdir)

    if (show_txt_pb) {
        pb <-
            utils::txtProgressBar(min = 0,
                                  max = length(raw_files),
                                  style = 3)
        if (is.null(pb_fct))
            pb_fct <- function(n, total, title)
                utils::setTxtProgressBar(pb, value = n, title = title)
    }

    if (cores > 1) {
        if (cores > length(raw_files))
            cores <- length(raw_files)
        cl <- parallel::makeCluster(cores)
        doSNOW::registerDoSNOW(cl)
        operator <- foreach::"%dopar%"
        # don't knwo why but covr doesn't need this line &
        # don't find the internal functions...
        try(parallel::clusterExport(cl,
                                    list("convert_file",
                                         "check_ms_file",
                                         "split_ms_file"))
        )
    } else
        operator <- foreach::"%do%"

    raw_file <- NULL
    conversion_res <- operator(
        foreach::foreach(
            raw_file = iterators::iter(raw_files),
            .combine = rbind,
            .options.snow = if (is.null(pb_fct))
                NULL
            else
                list(
                    progress = function(n)
                        pb_fct(n, length(raw_files), "")
                )
        ),
        cbind(
            sample = tools::file_path_sans_ext(basename(raw_file)),
            pos = tryCatch(
                convert_file(raw_file, pos_outdir,
                             converter, "positive", cores == 1),
                error = function(e)
                    e$message
            ),
            neg = tryCatch(
                convert_file(raw_file, neg_outdir,
                             converter, "negative", cores == 1),
                error = function(e)
                    e$message
            )
        )
    )
    if (show_txt_pb)
        close(pb)
    if (exists("cl"))
        parallel::stopCluster(cl)
    return(conversion_res)
}
