#' @title Convert RAW files
#'
#' @description
#' Convert RAW files with msconvert to mzXML files
#'
#' @param raw_files vector of filepaths
#' @param converter filepath to the msonvert executable
#' @param verbose boolean disable prints
#'
#' @return vector of filepaths to the mzXML files converted
#' @export
#' @examples
#' \dontrun{convert_files("testdata/small.RAW", 
#'      converter = "pwiz/msconvert.exe")}
convert_files <- function(raw_files, converter, verbose = TRUE) {
    # check if the system is Windows
    if (Sys.info()[["sysname"]] != "Windows") stop(
        "conversion only works on Windows")
    if (length(raw_files) == 0) stop(
        "you must give at least one raw file to convert")
    if (class(raw_files) != "character") stop(
        "raw_files argument must be a vector of filepaths")
    file_ext <- c("\\.mzML$", "\\.mzXML$", "\\.CDF$", "\\.RAW$", "\\.d$", 
        "\\.YEP$", "\\.BAF$", "\\.FID$", "\\.WIFF$", "\\.MGF$")
    test_ext <- sapply(raw_files, function(x) 
        any(sapply(file_ext, grepl, x, ignore.case = TRUE)))
    if (any(!test_ext)) stop(sprintf("file extension of %s are not supported",
        paste(basename(raw_files[!test_ext]), collapse = " ")))
    test_exist <- file.exists(raw_files)
    if (any(!test_exist)) stop(sprintf("file(s) %s doesn't exist", 
        paste(raw_files[!test_exist], collapse = " ")))
    raw_files <- normalizePath(raw_files)
    if (class(converter) != "character") stop(
        "converter argument must be a filepath to the msconvert exe")
    if (length(converter) > 1) stop(
        "converter argument must contain only one filepath")
    if (!file.exists(converter)) stop(sprintf("converter is not found at %s", 
        converter))
    
    outdir <- tempdir()
    new_filepaths <- sapply(seq(raw_files), function(i) 
        convert_file(raw_files[i], outdir, converter, verbose))
    return(new_filepaths)
}

#' @title Convert RAW file
#' 
#' @description
#' Convert one RAW file to a mzML file
#' Check with MSnbase package if the file can be converted
#' If the file is a CDF it will copy the file instead
#' If the conversion failed & the file is a mzML or mzXML 
#'      it will copy the file instead
#' 
#' @param raw_file filepath
#' @param outdir dirpath to the directory of the converted file
#' @param converter filepath to msconvert executable
#' @param verbose boolean disable prints
#' 
#' @return filepath to the converted file
convert_file <- function(raw_file, outdir, converter, verbose = TRUE) {
    # if it is a wiff file it needs the corresponding wiff.scan file
    if (grepl("\\.WIFF$", raw_file, ignore.case = TRUE)) if (
        !file.exists(paste(raw_file, "scan", sep = ".")) | 
        !file.exists(paste(raw_file, "SCAN", sep = "."))) stop(sprintf(
            "missing %s", paste(basename(raw_file), "scan", sep = ".")))
    # if it is a CDF file : cannot centroid it
    if (grepl("\\.CDF$", raw_file, ignore.case = TRUE)) {
        new_filepath <- file.path(outdir, basename(raw_file))
        file.copy(raw_file, new_filepath, overwrite = TRUE)
    } else {
        if (verbose) print(sprintf("conversion of %s", basename(raw_file)))
        new_filename <- paste(tools::file_path_sans_ext(basename(raw_file)), 
            "mzXML", sep = ".")
        new_filepath <- file.path(outdir, new_filename)
        # if it is a Water repertory, don"t use the vendor algorithm
        algorithm <- if (
            (grepl("\\.raw$", raw_file) & !dir.exists(raw_file)) | 
            grepl("\\.mzXML$", raw_file) | 
            grepl("\\.mzML$", raw_file)) "cwt" else "vendor"
        
        query <- sprintf("\"%s\" \"%s\" -o \"%s\" 
            --outfile \"%s\" --mzXML -z -n 
            --filter \"peakPicking %s msLevel=1-\" 
            --filter \"zeroSamples removeExtra\"", 
            converter, raw_file, outdir, new_filename, algorithm)
        if (verbose) print(query)
        msconvert_blabla <- system(query, intern = TRUE, wait = TRUE)
        if (verbose) print(msconvert_blabla)
                
        if (!file.exists(new_filepath)) {
            if (grepl("\\.mzXML$", raw_file) | grepl("\\.mzML$", raw_file)) {
                new_filepath <- file.path(outdir, basename(raw_file))
                file.copy(raw_file, new_filepath, overwrite = TRUE)
            } else stop(msconvert_blabla[length(msconvert_blabla)])
        }
    }
    # check if file can be read
    ms_file <- tryCatch(MSnbase::readMSData(new_filepath, mode = "onDisk"), 
        error = function(e) e$message)
    if (class(ms_file) != "OnDiskMSnExp") {
        print(ms_file)
        stop(sprintf("file converted %s cannot be read", basename(raw_file)))
    }
    return(new_filepath)
}
