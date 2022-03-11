testthat::test_that("group peaks", {
    raw_files <- c(
        system.file(
            "testdata",
            "220221CCM_global_POS_01_ssleu_filtered.mzML",
            package = "workflow.lipido"
        ),
        system.file(
            "testdata",
            "220221CCM_global_POS_02_ssleu_filtered.mzML",
            package = "workflow.lipido"
        )
    )
    sqlite_path <- tempfile(fileext = ".sqlite")
    converter <- tools::file_path_as_absolute(
        "~/GitHub/workflow.lipido/pwiz/msconvert.exe"
    )
    filter_params <- FilterParam(
        mz_range = c(300, 1000),
        rt_range = c(.7 * 60, 6.3 * 60)
    )
    cwt_params <- xcms::CentWaveParam(
        ppm = 30,
        peakwidth = c(4, 39),
        snthresh = 1,
        prefilter = c(2, 815),
        mzCenterFun = "wMean",
        integrate = 1,
        mzdiff = .041,
        fitgauss = FALSE,
        noise = 0,
        verboseColumns = TRUE,
        firstBaselineCheck = FALSE
    )
    obw_params <- xcms::ObiwarpParam(
        binSize = .1,
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
    pd_params <- xcms::PeakDensityParam(
        sampleGroups = seq(length(raw_files)),
        bw = 5,
        minFraction = 10**-9,
        minSamples = 1,
        binSize = 0.01,
        maxFeatures = 500
    )

    db <- db_connect(sqlite_path)
    sample_names <- tools::file_path_sans_ext(basename(raw_files))
    db_record_samples(db, sample_names)
    a <- lapply(raw_files, function(raw_file)
        import_ms_file(
            db,
            tools::file_path_sans_ext(basename(raw_file)),
            raw_file,
            converter,
            "positive",
            filter_params
        )
    )
    xsets <- lapply(sample_names, function(sample_name)
        find_chrompeaks(
            db_read_ms_file(db, sample_name, "positive"),
            cwt_params
        )
    )
    xset <- obiwarp(
        sqlite_path,
        sample_names,
        "positive",
        xsets,
        obw_params
    )
    xset <- group_peaks(xset, pd_params)
    testthat::expect_equal(
        data.frame(xset@groups),
        data.frame(
            mzmed = c(408.251325886321, 426.262123243348, 427.265526700411,
                      428.267904280982, 428.268229167555, 429.27060132947,
                      429.270256788594, 429.270493526998, 448.244170162955,
                      464.447379285654, 504.440256583886, 505.443684675365),
            mzmin = c(408.251325886321, 426.261913381751, 427.265348519813,
                      428.267835743959, 428.267896400125, 429.270341280206,
                      429.270175958258, 429.270493526998, 448.244170162955,
                      464.447304014051, 504.440032161331, 505.443534603),
            mzmax = c(408.251325886321, 426.262333104945, 427.265704881008,
                      428.268466489791, 428.268561934985, 429.270861378734,
                      429.270782294993, 429.270493526998, 448.244170162955,
                      464.447454557257, 504.44048100644, 505.44383474773),
            rtmed = c(286.278, 286.8085, 286.8085, 279.407, 259.3105, 259.8395,
                      301.616, 279.407, 286.807, 201.573, 201.3085, 201.044),
            rtmin = c(286.278, 286.807, 286.807, 278.875, 258.253, 258.253,
                      291.569, 279.407, 286.807, 197.973, 197.444, 197.444),
            rtmax = c(286.278, 286.81, 286.81, 291.569, 260.368, 261.426,
                      306.914, 279.407, 286.807, 205.173, 205.173, 204.644),
            npeaks = c(1, 2, 2, 3, 2, 2, 3, 1, 1, 2, 2, 2),
            X1 = c(0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1),
            X2 = c(1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1)
        )
    )
    RSQLite::dbDisconnect(db)
})
