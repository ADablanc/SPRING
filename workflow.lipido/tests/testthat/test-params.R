testthat::test_that("filter_param", {
    testthat::expect_error(
        FilterParam(mz_range = "a"),
        "got class \"character\", should be or extend class \"numeric\""
    )
    testthat::expect_error(
        FilterParam(mz_range = 1),
        "mz_range must contain two positive number"
    )
    testthat::expect_error(
        FilterParam(mz_range = c(-1, 1)),
        "mz_range must contain two positive number"
    )
    testthat::expect_error(
        FilterParam(mz_range = c(2, 1)),
        "mz_range born min must be lower than the born max"
    )
    testthat::expect_error(
        FilterParam(mz_range = c(300, 1000), rt_range = "a"),
        "got class \"character\", should be or extend class \"numeric\""
    )
    testthat::expect_error(
        FilterParam(mz_range = c(300, 1000), rt_range = 1),
        "rt_range must contain two positive number"
    )
    testthat::expect_error(
        FilterParam(mz_range = c(300, 1000), rt_range = c(-1, 1)),
        "rt_range must contain two positive number"
    )
    testthat::expect_error(
        FilterParam(mz_range = c(300, 1000), rt_range = c(2, 1)),
        "rt_range born min must be lower than the born max"
    )
    obj <- FilterParam(
        mz_range = c(300, 1000),
        rt_range = c(.7 * 60, 6.3 * 60)
    )
    testthat::expect_equal(obj, FilterParam())
    testthat::expect_equal(obj@mz_range, c(300, 1000))
    testthat::expect_equal(obj@rt_range, c(.7 * 60, 6.3 * 60))
    testthat::expect_equal(
        params_to_dataframe(obj),
        data.frame(
            mz_range_min = 300,
            mz_range_max = 1000,
            rt_range_min = 42,
            rt_range_max = 378
        )
    )
})

testthat::test_that("annotation_param", {
    testthat::expect_error(
        AnnotationParam(da_tol = "a"),
        "got class \"character\", should be or extend class \"numeric\""
    )
    testthat::expect_error(
        AnnotationParam(da_tol = c(1, 2)),
        "da_tol need to be a positive number"
    )
    testthat::expect_error(
        AnnotationParam(da_tol = -1),
        "da_tol need to be a positive number"
    )
    testthat::expect_error(
        AnnotationParam(da_tol = .015, rt_tol = "a"),
        "got class \"character\", should be or extend class \"numeric\""
    )
    testthat::expect_error(
        AnnotationParam(da_tol = .015, rt_tol = c(1, 2)),
        "rt_tol need to be a positive number"
    )
    testthat::expect_error(
        AnnotationParam(da_tol = .015, rt_tol = -1),
        "rt_tol need to be a positive number"
    )
    testthat::expect_error(
        AnnotationParam(da_tol = .015, rt_tol = 10, abd_tol = "a"),
        "got class \"character\", should be or extend class \"numeric\""
    )
    testthat::expect_error(
        AnnotationParam(da_tol = .015, rt_tol = 10, abd_tol = c(1, 2)),
        "abd_tol need to be a positive number between 0 & 100"
    )
    testthat::expect_error(
        AnnotationParam(da_tol = .015, rt_tol = 10, abd_tol = -1),
        "abd_tol need to be a positive number between 0 & 100"
    )
    testthat::expect_error(
        AnnotationParam(da_tol = .015, rt_tol = 10, abd_tol = -1),
        "abd_tol need to be a positive number between 0 & 100"
    )
    testthat::expect_error(
        AnnotationParam(
            da_tol = .015,
            rt_tol = 10,
            abd_tol = 25,
            adduct_names = 1
        ),
        "got class \"numeric\", should be or extend class \"character\""
    )
    testthat::expect_error(
        AnnotationParam(
            da_tol = .015,
            rt_tol = 10,
            abd_tol = 25,
            adduct_names = c("[M+H]+", "[M+CH3COOH]-")
        ),
        escape_regex("[M+CH3COOH]- doesn't exists in the adduct list")
    )
    testthat::expect_error(
        AnnotationParam(
            da_tol = .015,
            rt_tol = 10,
            abd_tol = 25,
            adduct_names = "[M+H]+",
            instrument = 1
        ),
        "got class \"numeric\", should be or extend class \"character\""
    )
    testthat::expect_error(
        AnnotationParam(
            da_tol = .015,
            rt_tol = 10,
            abd_tol = 25,
            adduct_names = "[M+H]+",
            instrument = "orbitrap"
        ),
        "orbitrap doesn't exists in the instrument list"
    )
    obj <- AnnotationParam(
        da_tol = 0.015,
        rt_tol = 10,
        abd_tol = 25,
        adduct_names = c(
            "[M+Na]+",
            "[M+NH4]+",
            "[M+H-H2O]+",
            "[M+H]+",
            "[M-H]-"
        ),
        instrument = "QTOF_XevoG2-S_R25000@200"
    )
    testthat::expect_equal(obj, AnnotationParam())
    testthat::expect_equal(obj@da_tol, .015)
    testthat::expect_equal(obj@rt_tol, 10)
    testthat::expect_equal(obj@abd_tol, 25)
    testthat::expect_equal(
        obj@adduct_names,
        c("[M+Na]+", "[M+NH4]+", "[M+H-H2O]+", "[M+H]+", "[M-H]-")
    )
    testthat::expect_equal(obj@instrument, "QTOF_XevoG2-S_R25000@200")
    testthat::expect_error(
        restrict_adducts_polarity(obj, "maybe"),
        "polarity must be set to \"positive\" or \"negative\""
    )
    obj_pos <- AnnotationParam(
        da_tol = 0.015,
        rt_tol = 10,
        abd_tol = 25,
        adduct_names = c("[M+H]+", "[M+NH4]+", "[M+Na]+", "[M+H-H2O]+"),
        instrument = "QTOF_XevoG2-S_R25000@200"
    )
    testthat::expect_equal(
        restrict_adducts_polarity(obj, "positive"),
        obj_pos
    )
    obj_neg <- AnnotationParam(
        da_tol = 0.015,
        rt_tol = 10,
        abd_tol = 25,
        adduct_names = "[M-H]-",
        instrument = "QTOF_XevoG2-S_R25000@200"
    )
    testthat::expect_equal(
        restrict_adducts_polarity(obj, "negative"),
        obj_neg
    )
    testthat::expect_equal(
        params_to_dataframe(obj),
        data.frame(
            da_tol = 0.015,
            rt_tol = 10,
            abd_tol = 25,
            adduct_names = paste(
                c("[M+Na]+", "[M+NH4]+", "[M+H-H2O]+", "[M+H]+", "[M-H]-"),
                collapse = ";"
            ),
            instrument = "QTOF_XevoG2-S_R25000@200"
        )
    )
})

testthat::test_that("cwt_param", {
    obj <- xcms::CentWaveParam(
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
    testthat::expect_equal(
        params_to_dataframe(obj),
        data.frame(
            ppm = 30,
            peakwidth_min = 4,
            peakwidth_max = 39,
            snthresh = 6.5,
            prefilter_step = 2,
            prefilter_level = 815,
            mzCenterFun = "wMean",
            integrate = 1,
            mzdiff = .041,
            fitgauss = as.numeric(FALSE),
            noise = 0,
            verboseColumns = as.numeric(TRUE),
            firstBaselineCheck = as.numeric(FALSE)
        )
    )
})

testthat::test_that("obw_param", {
    obj <- xcms::ObiwarpParam(
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
    testthat::expect_equal(
        params_to_dataframe(obj),
        data.frame(
            binSize = .1,
            response = 1,
            distFun = "cor_opt",
            gapInit = .3,
            gapExtend = 2.4,
            factorDiag = 2,
            factorGap = 1,
            localAlignment = as.numeric(FALSE),
            initPenalty = 0
        )
    )
})

testthat::test_that("pd_param", {
    obj <- xcms::PeakDensityParam(
        sampleGroups = 1:2,
        bw = 5,
        minFraction = 10**-9,
        minSamples = 1,
        binSize = 0.01,
        maxFeatures = 500
    )
    testthat::expect_equal(
        params_to_dataframe(obj),
        data.frame(
            bw = 5,
            minFraction = 10**-9,
            minSamples = 1,
            binSize = 0.01,
            maxFeatures = 500
        )
    )
})

testthat::test_that("check_ms_process_args", {
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
    converter <- "~/GitHub/workflow.lipido/pwiz/msconvert.exe"
    filter_params <- FilterParam(
        mz_range = c(200, 1000),
        rt_range = c(.7 * 60, 6.3 * 60)
    )
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
    ann_params <- AnnotationParam(
        da_tol = .015,
        rt_tol = 10,
        abd_tol = 25,
        adduct_names = c(
            "[M+Na]+",
            "[M+NH4]+",
            "[M+H-H2O]+",
            "[M+H]+",
            "[M-H]-"
        ),
        instrument = "QTOF_XevoG2-S_R25000@200"
    )

    testthat::expect_error(
        check_ms_process_args(NULL),
        "you must give at least one raw file to convert"
    )

    testthat::expect_error(
        check_ms_process_args(c(1, 2)),
        "raw_files argument must be a vector of filepaths"
    )

    testthat::expect_error(
        check_ms_process_args(c("C:/small.txt", "C:/small.nfo")),
        "file extension of small.txt and small.nfo are not supported"
    )

    testthat::expect_error(
        check_ms_process_args(c("C:/small.mzXML", "C:/small.mzML")),
        escape_regex("file(s) C:/small.mzXML and C:/small.mzML doesn't exist")
    )
    testthat::expect_error(
        check_ms_process_args(raw_files, 1),
        "sqlite path must be a filepath"
    )
    testthat::expect_error(
        check_ms_process_args(raw_files, c("a", "b")),
        "sqlite path must contain only one filepath"
    )
    testthat::expect_error(
        check_ms_process_args(raw_files, "~/test/test.sqlite"),
        "directory where to save sqlite database doesn't exist"
    )
    testthat::expect_error(
        check_ms_process_args(raw_files, sqlite_path, 1),
        "converter argument must be a filepath to the msconvert exe"
    )
    testthat::expect_error(
        check_ms_process_args(raw_files, sqlite_path, c("a", "b")),
        "converter argument must contain only one filepath"
    )
    testthat::expect_error(
        check_ms_process_args(raw_files, sqlite_path, "C:/pwiz/msconvert.exe"),
        escape_regex("converter is not found at C:/pwiz/msconvert.exe")
    )
    testthat::expect_error(
        check_ms_process_args(
            raw_files,
            sqlite_path,
            converter,
            NULL
        ),
        "filter_params argument must be a FilterParam object"
    )
    testthat::expect_error(
        check_ms_process_args(
            raw_files,
            sqlite_path,
            converter,
            filter_params,
            NULL
        ),
        "cwt_params argument must be a CentWaveParam object"
    )
    testthat::expect_error(
        check_ms_process_args(
            raw_files,
            sqlite_path,
            converter,
            filter_params,
            cwt_params,
            NULL
        ),
        "obw_params argument must be a ObiwarpParam object"
    )
    testthat::expect_error(
        check_ms_process_args(
            raw_files,
            sqlite_path,
            converter,
            filter_params,
            cwt_params,
            obw_params,
            NULL
        ),
        "pd_params argument must be a PeakDensityParam object"
    )
    testthat::expect_error(
        check_ms_process_args(
            raw_files,
            sqlite_path,
            converter,
            filter_params,
            cwt_params,
            obw_params,
            pd_params,
            NULL
        ),
        "ann_params argument must be an AnnotationParam object"
    )
    testthat::expect_error(
        check_ms_process_args(
            raw_files,
            sqlite_path,
            converter,
            filter_params,
            cwt_params,
            obw_params,
            pd_params,
            ann_params,
            "a"
        ),
        "cores argument must be numerical"
    )
    testthat::expect_error(
        check_ms_process_args(
            raw_files,
            sqlite_path,
            converter,
            filter_params,
            cwt_params,
            obw_params,
            pd_params,
            ann_params,
            c(1, 2)
        ),
        "cores argument must contain only ONE number !"
    )
    testthat::expect_error(
        check_ms_process_args(
            raw_files,
            sqlite_path,
            converter,
            filter_params,
            cwt_params,
            obw_params,
            pd_params,
            ann_params,
            0
        ),
        "cores cannot be a number under 1"
    )
    testthat::expect_error(
        check_ms_process_args(
            raw_files,
            sqlite_path,
            converter,
            filter_params,
            cwt_params,
            obw_params,
            pd_params,
            ann_params,
            1.2
        ),
        "cores must not contain any digits"
    )
    testthat::expect_error(
        check_ms_process_args(
            raw_files,
            sqlite_path,
            converter,
            filter_params,
            cwt_params,
            obw_params,
            pd_params,
            ann_params,
            99
        ),
        sprintf("system have a maximum of %s cores", parallel::detectCores())
    )
    expect_equal(
        check_ms_process_args(
            raw_files,
            sqlite_path,
            converter,
            filter_params,
            cwt_params,
            obw_params,
            pd_params,
            ann_params,
            1
        ),
        1
    )
})
