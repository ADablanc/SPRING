testthat::test_that("process", {
    testthat::expect_error(
        ms_process(NULL),
        "you must give at least two mzxml files")
    
    testthat::expect_error(
       ms_process(c(1, 2)), 
       "mzxml_files argument must contain only characters")

    testthat::expect_error(
       ms_process(c("C:/small.txt", "C:/small.nfo")), 
       "file extension of small.txt and small.nfo are not supported")

    testthat::expect_error(
        ms_process(c("C:/small.mzXML", "C:/small.mzML")), 
        escape_regex("file(s) C:/small.mzXML and C:/small.mzML doesn't exist"))
    
    mzxml_files <- c(
        system.file("testdata", "200204PLF_QC01_pos_filtered.mzML", 
            package = "workflow.lipido"), 
        system.file("testdata", "200204PLF_QC02_pos_filtered.mzML", 
            package = "workflow.lipido"))
    
    testthat::expect_error(
        ms_process(mzxml_files, "a"), 
        "filter_params argument must be a FilterParam object")
    
    filter_params <- FilterParam(
        mz_range = c(300, 1000), 
        rt_range = c(.7 * 60, 6.3 * 60)
    )
    testthat::expect_error(
        ms_process(mzxml_files, filter_params, "a"), 
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
        ms_process(mzxml_files, filter_params, cwt_params, "a"), 
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
        ms_process(mzxml_files, filter_params, cwt_params, obw_params, "a"), 
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
        ms_process(mzxml_files, filter_params, cwt_params, obw_params, 
            pd_params, "a"), 
        "ann_params argument must be an AnnotationParam object")
    
    ann_params <- AnnotationParam(
         da_tol = .015, 
        rt_tol = 10, 
        abd_tol = 25, 
        adduct_names = c("M+Na", "M+NH4", "M+H-H2O", "M+H"), 
        instrument = "QTOF_XevoG2-S_R25000@200"
    )
    
    testthat::expect_error(
        ms_process(mzxml_files, filter_params, cwt_params, obw_params, 
            pd_params, ann_params, cores = "a"), 
        "cores argument must be numerical")
    
    testthat::expect_error(
        ms_process(mzxml_files, filter_params, cwt_params, obw_params, 
            pd_params, ann_params, cores = c(1, 2)), 
        "cores argument must contain only ONE number !")
    
    testthat::expect_error(
        ms_process(mzxml_files, filter_params, cwt_params, obw_params, 
            pd_params, ann_params, cores = 0), 
        "cores cannot be a number under 1")
    
    testthat::expect_error(
        ms_process(mzxml_files, filter_params, cwt_params, obw_params, 
            pd_params, ann_params, cores = 1.2), 
        "cores must not contain any digits")
    
    testthat::expect_error(
        ms_process(mzxml_files, filter_params, cwt_params, obw_params, 
            pd_params, ann_params, cores = 99), 
        sprintf("system have a maximum of %s cores", parallel::detectCores()))
    
    tmp_files <- tempfile(c("file1", "file2"), fileext = ".mzXML")
    a <- sapply(tmp_files, function(tmp_file) 
        if (!file.exists(tmp_file)) invisible(capture.output(
            file.create(tmp_file))))
    expect_error(
        invisible(capture.output(
            ms_process(tmp_files, filter_params, cwt_params, obw_params, 
                pd_params, ann_params, show_pb = FALSE))), 
        "cannot execute ms_process")
    
    observed <- suppressMessages(ms_process(mzxml_files, filter_params, 
        cwt_params, obw_params, pd_params, ann_params, show_pb = FALSE))
    expected <- readRDS(system.file("testdata", "process.rds", 
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
    expect_identical(
        observed@ann, 
        expected@ann
    )
})
