testthat::test_that("process", {
    testthat::expect_error(
        ms_process(c(), NULL, NULL, NULL),
        "you must give at least one mzxml file")
    
    testthat::expect_error(
       ms_process(1, NULL, NULL, NULL), 
       "mzxml_files argument must contain only characters")

    testthat::expect_error(
       ms_process("C:/small.txt", NULL, NULL, NULL), 
       "file extension of small.txt are not supported")

    testthat::expect_error(
       ms_process("C:/small.mzXML", NULL, NULL, NULL), 
        escape_regex("file(s) C:/small.mzXML doesn't exist"))
    
    mzxml_files <- c(
        system.file("testdata", "200204PLF_QC01_pos_filtered.mzML", 
            package = "workflow.lipido"), 
        system.file("testdata", "200204PLF_QC02_pos_filtered.mzML", 
            package = "workflow.lipido"))
    testthat::expect_error(
        ms_process(mzxml_files, "a", NULL, NULL), 
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
        ms_process(mzxml_files, cwt_params, "a", NULL), 
        "obw_params argument must be a ObiwarpParam object")
    
    obw_params <- xcms::ObiwarpParam(
        binSize = 1,
        centerSample = integer(),
        response = 1L,
        distFun = "cor_opt",
        gapInit = .3,
        gapExtend = 2.4,
        factorDiag = 2,
        factorGap = 1,
        localAlignment = FALSE,
        initPenalty = 0
    )
    testthat::expect_error(
        ms_process(mzxml_files, cwt_params, obw_params, "a"), 
        "pd_params argument must be a PeakDensityParam object")
    
    pd_params <- xcms::PeakDensityParam(
        sampleGroups = seq(length(mzxml_files)),
        bw = 5,
        minFraction = 10**-9,
        minSamples = 1,
        binSize = 0.01,
        maxFeatures = 500
    )
    
    testthat::expect_error(
        ms_process(mzxml_files, cwt_params, obw_params, pd_params, 
            cores = "a"), 
        "cores argument must be numerical")
    
    testthat::expect_error(
        ms_process(mzxml_files, cwt_params, obw_params, pd_params, 
            cores = c(1, 2)), 
        "cores argument must contain only ONE number !")
    
    testthat::expect_error(
        ms_process(mzxml_files, cwt_params, obw_params, pd_params, 
            cores = 0), 
        "cores cannot be a number under 1")
    
    testthat::expect_error(
        ms_process(mzxml_files, cwt_params, obw_params, pd_params, 
            cores = 1.2), 
        "cores must not contain any digits")
    
    testthat::expect_error(
        ms_process(mzxml_files, cwt_params, obw_params, pd_params, 
            cores = 99), 
        sprintf("system have a maximum of %s cores", parallel::detectCores()))
    
    tmp_file <- tempfile(fileext = ".mzXML")
    if (!file.exists(tmp_file)) invisible(capture.output(file.create(tmp_file)))
    expect_error(
        invisible(capture.output(
            ms_process(tmp_file, cwt_params, obw_params, pd_params, 
                show_pb = FALSE))), 
        "cannot execute ms_process")
    
    observed <- suppressMessages(ms_process(mzxml_files[1], cwt_params, 
        show_pb = FALSE))
    expected <- readRDS(system.file("testdata", "process_1.rds", 
            package = "workflow.lipido"))
    expect_identical(
        xcms::chromPeaks(observed), 
        xcms::chromPeaks(expected)
    )
    
    observed <- suppressMessages(ms_process(mzxml_files, cwt_params, 
        obw_params, pd_params, show_pb = FALSE))
    expected <- readRDS(system.file("testdata", "process_2.rds", 
            package = "workflow.lipido"))
    expect_identical(
        xcms::chromPeaks(observed), 
        xcms::chromPeaks(expected)
    )
    expect_identical(
        xcms::adjustedRtime(observed), 
        xcms::adjustedRtime(expected)
    )
    expect_identical(
        xcms::featureDefinitions(observed), 
        xcms::featureDefinitions(expected)
    )
})
