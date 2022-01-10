testthat::test_that("conversion", {
    converter <- "~/../pwiz/msconvert.exe"
    if (!file.exists(converter)) testthat::skip("no msconvert.exe founded")
    else converter <- normalizePath(converter)
    
    testthat::expect_error(
        convert_files(c(), converter = converter, verbose = FALSE), 
        "you must give at least one raw file to convert")
    
    testthat::expect_error(
        convert_files(12723, converter = converter, verbose = FALSE), 
        "raw_files argument must be a vector of filepaths")
    
    testthat::expect_error(
        convert_files("C:/small.txt", converter = converter, verbose = FALSE), 
        "file extension of small.txt are not supported")
    
    testthat::expect_error(
        convert_files("C:/small.mzXML", converter = converter, 
            verbose = FALSE), 
        escape_regex("file(s) C:/small.mzXML doesn't exist"))
    
    testthat::expect_error(
        convert_files(
            system.file("testdata", "small.RAW", package = "workflow.lipido"), 
            converter = 154354534, verbose = FALSE), 
        "converter argument must be a filepath to the msconvert exe")
    
    testthat::expect_error(
        convert_files(
            system.file("testdata", "small.RAW", package = "workflow.lipido"), 
            converter = c("a", "b"), verbose = FALSE), 
        "converter argument must contain only one filepath")
    
    testthat::expect_error(
        convert_files(
            system.file("testdata", "small.RAW", package = "workflow.lipido"), 
            converter = "C:/msconvert.exe", verbose = FALSE), 
        "converter is not found at C:/msconvert.exe")
    
    wiff_file <- tempfile(fileext = ".WIFF")
    if (!file.exists(wiff_file)) invisible(capture.output(
        file.create(wiff_file)))
    testthat::expect_error(
        convert_files(wiff_file, converter = converter, verbose = FALSE), 
        sprintf("missing %s", paste(basename(wiff_file), "scan", sep = ".")))
    
    cdf_file <- tempfile(fileext = ".CDF")
    if (!file.exists(cdf_file)) invisible(capture.output(file.create(cdf_file)))
    testthat::expect_error(invisible(capture.output(
        convert_files(cdf_file, converter = converter, verbose = FALSE), 
        sprintf("file converted %s cannot be read", basename(cdf_file)))))
    
    mzxml_file <- file.path(tempdir(), "a", "test.mzXML")
    if (!dir.exists(dirname(mzxml_file))) invisible(capture.output(
        dir.create(dirname(mzxml_file))))
    if (!file.exists(mzxml_file)) invisible(capture.output(
        file.create(mzxml_file)))
    testthat::expect_error(suppressWarnings(
        convert_files(mzxml_file, converter = converter, verbose = FALSE)), 
        sprintf("file converted %s cannot be read", basename(mzxml_file)))
    
    raw_file <- tempfile(fileext = ".raw")
    if (!file.exists(raw_file)) invisible(capture.output(file.create(raw_file)))
    testthat::expect_error(suppressWarnings(
        convert_files(raw_file, converter = converter, verbose = FALSE)), 
        sprintf(""))
    
    invisible(capture.output(converted_file <- 
        convert_files(
            system.file("testdata", "small.RAW", package = "workflow.lipido"), 
            converter = converter, verbose = TRUE)))
    testthat::expect_true(file.exists(converted_file))
})
