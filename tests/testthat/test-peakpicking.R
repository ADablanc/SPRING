testthat::test_that("peakpicking", {
    testthat::expect_error(
        peakpicking(c(), NULL),
        "you must give at least one mzxml file")
    
    testthat::expect_error(
       peakpicking(1, NULL), 
       "mzxml_files argument must contain only characters")

     testthat::expect_error(
       peakpicking("C:/small.txt", NULL), 
       "file extension of small.txt are not supported")

    testthat::expect_error(
       peakpicking("C:/small.mzXML", NULL), 
        escape_regex("file(s) C:/small.mzXML doesn't exist"))
    
    mzxml_files <- c(
        system.file("testdata", "200204PLF_QC01_pos_filtered.mzML", 
            package = "workflow.lipido"), 
        system.file("testdata", "200204PLF_QC02_pos_filtered.mzML", 
            package = "workflow.lipido"))
    testthat::expect_error(
        peakpicking(mzxml_files, "a"), 
        "cwt_params argument must be a CentWaveParam object")
        
    cwt_params <- xcms::CentWaveParam(
        ppm = 30, 
        peakwidth = c(4, 39), 
        snthresh = 6.5, 
        prefilter = c(2, 815), 
        mzCenterFun = "wMean", 
        integrate = 1, 
        mzdiff = .041, 
        fitgauss = FALSE, 
        noise = 0, 
        verboseColumns = TRUE, 
        firstBaselineCheck = FALSE
    )
    testthat::expect_error(
        peakpicking(mzxml_files, NULL, cores = "a"), 
        "cores argument must be numerical")
    
    testthat::expect_error(
        peakpicking(mzxml_files, cwt_params, cores = c(1, 2)), 
        "cores argument must contain only ONE number !")
    
    testthat::expect_error(
        peakpicking(mzxml_files, cwt_params, cores = 0), 
        "cores cannot be a number under 1")
    
    testthat::expect_error(
        peakpicking(mzxml_files, cwt_params, cores = 1.2), 
        "cores must not contain any digits")
    
    testthat::expect_error(
        peakpicking(mzxml_files, cwt_params, cores = 99), 
        sprintf("system have a maximum of %s cores", parallel::detectCores()))
    
    tmp_file <- tempfile(fileext = ".mzXML")
    if (!file.exists(tmp_file)) invisible(capture.output(file.create(tmp_file)))
    expect_error(
        invisible(capture.output(
            peakpicking(tmp_file, cwt_params, show_pb = FALSE))), 
        "cannot execute peakpicking")
    
    expect_identical(
        xcms::chromPeaks(peakpicking(mzxml_files, cwt_params, 
            show_pb = FALSE)), 
        readRDS(system.file("testdata", "peakpicking.rds", 
            package = "workflow.lipido"))
    )
    
    expect_identical(
        suppressMessages(xcms::chromPeaks(
            peakpicking(mzxml_files, cwt_params, cores = 1, show_pb = FALSE))), 
        readRDS(system.file("testdata", "peakpicking.rds", 
            package = "workflow.lipido"))
    )
})
