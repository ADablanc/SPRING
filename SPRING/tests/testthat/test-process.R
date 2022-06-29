testthat::test_that("workflow", {
    # initialize parameters
    raw_files <- c(
        system.file(
            "testdata",
            "220221CCM_global_POS_01_ssleu_filtered.mzML",
            package = "SPRING"
        ),
        system.file(
            "testdata",
            "220221CCM_global_POS_02_ssleu_filtered.mzML",
            package = "SPRING"
        )
    )
    sqlite_path <- tempfile(fileext = ".sqlite")
    sqlite_path_test <- system.file(
        "testdata",
        "220221CCM_global.sqlite",
        package = "SPRING"
    )
    converter <- tools::file_path_as_absolute(
        "~/GitHub/SPRING/pwiz/msconvert.exe"
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
        sampleGroups = seq(2),
        bw = 5,
        minFraction = 10**-9,
        minSamples = 1,
        binSize = 0.01,
        maxFeatures = 500
    )
    ann_params <- AnnotationParam(
        da_tol = .015,
        rt_tol = 10,
        abd_tol = 25,
        instrument = "QTOF_XevoG2-S_R25000@200",
        database = "test",
        polarity = "positive"
    )
    camera_params <- CameraParam(
        ann_params = ann_params,
        cores = 1,
        sigma = 6,
        perfwhm = .6,
        cor_eic_th = .75,
        pval = .05,
        graphMethod = "hcs"
    )

    # 1st test: test with an empty file (should stop all the process)
    raw_file_error <- tempfile(fileext = ".mzXML")
    if (!file.exists(raw_file_error)) {
        invisible(file.create(raw_file_error))
    }
    invisible(capture.output(testthat::expect_error(
        ms_process(
            raw_file_error,
            sqlite_path,
            converter,
            cwt_params,
            obw_params,
            pd_params,
            camera_params,
            ann_params
        ),
        "some of the file was not imported correctly"
    )))

    # 2nd test: no parallelization
    invisible(capture.output(ms_process(
        raw_files,
        sqlite_path,
        converter,
        cwt_params,
        obw_params,
        pd_params,
        camera_params,
        ann_params,
        cores = 1
    )))
    db <- db_connect(sqlite_path)
    db_test <- db_connect(sqlite_path_test)
    testthat::expect_equal(
        dbReadTable(db, "ann"),
        dbReadTable(db_test, "ann")
    )
    testthat::expect_equal(
        dbReadTable(db, "spectra_infos"),
        dbReadTable(db_test, "spectra_infos")
    )
    testthat::expect_equal(
        dbReadTable(db, "spectras"),
        dbReadTable(db_test, "spectras")
    )
    testthat::expect_equal(
        dbReadTable(db, "eic"),
        dbReadTable(db_test, "eic")
    )
    RSQLite::dbDisconnect(db)

    # 3rd test: parallelization
    capture.output(ms_process(
        raw_files,
        sqlite_path,
        converter,
        cwt_params,
        obw_params,
        pd_params,
        camera_params,
        ann_params
    ))
    db <- db_connect(sqlite_path)
    testthat::expect_equal(
        dbReadTable(db, "ann"),
        dbReadTable(db_test, "ann")
    )
    testthat::expect_equal(
        dbReadTable(db, "spectra_infos"),
        dbReadTable(db_test, "spectra_infos")
    )
    testthat::expect_equal(
        dbReadTable(db, "spectras"),
        dbReadTable(db_test, "spectras")
    )
    testthat::expect_equal(
        dbReadTable(db, "eic"),
        dbReadTable(db_test, "eic")
    )
    RSQLite::dbDisconnect(db)
    RSQLite::dbDisconnect(db_test)
})

testthat::test_that("export annotations", {
    excel_file <- tempfile(fileext = ".xlsx")
    sqlite_file <- system.file(
        "testdata",
        "220221CCM_global.sqlite",
        package = "SPRING"
    )

    # 1st : test with a wrong sqlite path
    testthat::expect_error(
        export_annotations(2, NULL),
        "sqlite file arg must be a filepath to a database file"
    )

    # 2nd : test with a missing excel file
    testthat::expect_error(
        export_annotations("test.sqlite", NULL),
        "the path in the sqlite file arg doesn't exist"
    )

    # 3rd : test with a wrong excel file
    testthat::expect_error(
        export_annotations(sqlite_file, 2),
        "excel file arg must be a filepath to a database file"
    )

    # 4th : test with a missing direcory for the excel file
    testthat::expect_error(
        export_annotations(sqlite_file, "a/a.xlsx"),
        "the directory path to the excel file arg doesn't exist"
    )

    # 5th: test with no annotations
    sqlite_file_error <- tempfile(fileext = ".sqlite")
    if (!file.exists(sqlite_file_error)) {
        invisible(file.create(sqlite_file_error))
    }
    testthat::expect_error(
        export_annotations(sqlite_file_error, excel_file),
        "no annotations in database"
    )

    # 5th : normal
    nsamples <- 2
    db <- db_connect(sqlite_file)
    ann <- db_get_annotations(db)
    spectra_infos <- db_get_spectra_infos(db)
    summarised_ann <- summarise_ann(ann, spectra_infos, 2)
    summarised_ann$resume[, "Group ID"] <- as.character(
        summarised_ann$resume[, "Group ID"])
    summarised_ann$resume$Class <- as.character(summarised_ann$resume$Class)

    export_annotations(sqlite_file, excel_file)
    testthat::expect_equal(
        openxlsx::read.xlsx(excel_file, 1, sep.names = " "),
        summarised_ann$resume
    )
    summarised_ann$details[, "Group ID"] <- as.character(
        summarised_ann$details[, "Group ID"])
    summarised_ann$details$Class <- as.character(summarised_ann$details$Class)
    testthat::expect_equal(
        openxlsx::read.xlsx(excel_file, 2, sep.names = " "),
        summarised_ann$details
    )
    RSQLite::dbDisconnect(db)
})
