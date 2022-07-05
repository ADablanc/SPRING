testthat::test_that("filter_param", {
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
    ann_params <- AnnotationParam(
        da_tol = .015,
        rt_tol = 10,
        abd_tol = 25,
        database = "test",
        instrument = "QTOF_XevoG2-S_R25000@200",
        polarity = "positive"
    )

    # test in positive
    obj <- FilterParam(cwt_params, ann_params)
    testthat::expect_identical(
        obj@polarity,
        "positive"
    )
    testthat::expect_identical(
        obj@mz_range,
        c(408.2365, 657.430028)
    )
    testthat::expect_equal(
        obj@rt_range,
        c(78.6, 412.8)
    )

    testthat::expect_equal(
        params_to_dataframe(obj),
        data.frame(
            polarity = "positive",
            mz_range_min = 408.2365,
            mz_range_max = 657.430028,
            rt_range_min = 78.6,
            rt_range_max = 412.8
        )
    )

    # test in negative
    ann_params <- AnnotationParam(
        da_tol = .015,
        rt_tol = 10,
        abd_tol = 25,
        database = "test-neg",
        instrument = "QTOF_XevoG2-S_R25000@200",
        polarity = "negative"
    )
    obj <- FilterParam(cwt_params, ann_params)
    testthat::expect_identical(
        obj@polarity,
        "negative"
    )
    testthat::expect_identical(
        obj@mz_range,
        c(406.22085, 653.436272)
    )
    testthat::expect_equal(
        obj@rt_range,
        c(78.6, 412.8)
    )

    testthat::expect_equal(
        params_to_dataframe(obj),
        data.frame(
            polarity = "negative",
            mz_range_min = 406.22085,
            mz_range_max = 653.436272,
            rt_range_min = 78.6,
            rt_range_max = 412.8
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

testthat::test_that("camera_param", {
    da_tol <- .015
    polarity <- "positive"
    ann_params <- AnnotationParam(
        da_tol = da_tol,
        rt_tol = 10,
        abd_tol = 25,
        database = "test",
        instrument = "QTOF_XevoG2-S_R25000@200",
        polarity = polarity
    )
    cores <- 1
    sigma <- 6
    perfwhm <- .6
    cor_eic_th <- .75
    pval <- .05
    graph_method <- "hcs"

    testthat::expect_error(
        CameraParam(ann_params = "a"),
        "ann_params must be an AnnotationParam object"
    )
    testthat::expect_error(
        CameraParam(ann_params = ann_params, cores = "a"),
        "got class \"character\", should be or extend class \"numeric\""
    )
    testthat::expect_error(
        CameraParam(ann_params = ann_params, cores = c(1, 2)),
        "cores need to be a positive number"
    )
    testthat::expect_error(
        CameraParam(ann_params = ann_params, cores = 0),
        "cores need to be a positive number"
    )
    testthat::expect_error(
        CameraParam(ann_params = ann_params, cores = cores, sigma = "a"),
        "got class \"character\", should be or extend class \"numeric\""
    )
    testthat::expect_error(
        CameraParam(ann_params = ann_params, cores = cores, sigma = c(3, 6)),
        "sigma need to be a positive number"
    )
    testthat::expect_error(
        CameraParam(ann_params = ann_params, cores = cores, sigma = -1),
        "sigma need to be a positive number"
    )
    testthat::expect_error(
        CameraParam(
            ann_params = ann_params,
            cores = cores,
            sigma = sigma,
            perfwhm = "a"
        ),
        "got class \"character\", should be or extend class \"numeric\""
    )
    testthat::expect_error(
        CameraParam(
            ann_params = ann_params,
            cores = cores,
            sigma = sigma,
            perfwhm = c(.4, .5)
        ),
        "perfwhm need to be a number between 0 and 1"
    )
    testthat::expect_error(
        CameraParam(
            ann_params = ann_params,
            cores = cores,
            sigma = sigma,
            perfwhm = -2
        ),
        "perfwhm need to be a number between 0 and 1"
    )
    testthat::expect_error(
        CameraParam(
            ann_params = ann_params,
            cores = cores,
            sigma = sigma,
            perfwhm = 1.2
        ),
        "perfwhm need to be a number between 0 and 1"
    )
    testthat::expect_error(
        CameraParam(
            ann_params = ann_params,
            cores = cores,
            sigma = sigma,
            perfwhm = perfwhm,
            cor_eic_th = "a"
        ),
        "got class \"character\", should be or extend class \"numeric\""
    )
    testthat::expect_error(
        CameraParam(
            ann_params = ann_params,
            cores = cores,
            sigma = sigma,
            perfwhm = perfwhm,
            cor_eic_th = c(.4, .6)
        ),
        "cor_eic_th must be a number between 0 and 1"
    )
    testthat::expect_error(
        CameraParam(
            ann_params = ann_params,
            cores = cores,
            sigma = sigma,
            perfwhm = perfwhm,
            cor_eic_th = -2
        ),
        "cor_eic_th must be a number between 0 and 1"
    )
    testthat::expect_error(
        CameraParam(
            ann_params = ann_params,
            cores = cores,
            sigma = sigma,
            perfwhm = perfwhm,
            cor_eic_th = 1.2
        ),
        "cor_eic_th must be a number between 0 and 1"
    )
    testthat::expect_error(
        CameraParam(
            ann_params = ann_params,
            cores = cores,
            sigma = sigma,
            perfwhm = perfwhm,
            cor_eic_th = cor_eic_th,
            pval = "a"
        ),
        "got class \"character\", should be or extend class \"numeric\""
    )
    testthat::expect_error(
        CameraParam(
            ann_params = ann_params,
            cores = cores,
            sigma = sigma,
            perfwhm = perfwhm,
            cor_eic_th = cor_eic_th,
            pval = c(.4, .6)
        ),
        "pval must be a number between 0 and 1"
    )
    testthat::expect_error(
        CameraParam(
            ann_params = ann_params,
            cores = cores,
            sigma = sigma,
            perfwhm = perfwhm,
            cor_eic_th = cor_eic_th,
            pval = -2
        ),
        "pval must be a number between 0 and 1"
    )
    testthat::expect_error(
        CameraParam(
            ann_params = ann_params,
            cores = cores,
            sigma = sigma,
            perfwhm = perfwhm,
            cor_eic_th = cor_eic_th,
            pval = 1.2
        ),
        "pval must be a number between 0 and 1"
    )
    testthat::expect_error(
        CameraParam(
            ann_params = ann_params,
            cores = cores,
            sigma = sigma,
            perfwhm = perfwhm,
            cor_eic_th = cor_eic_th,
            pval = pval,
            graph_method = 1
        ),
        "got class \"numeric\", should be or extend class \"character\""
    )
    testthat::expect_error(
        CameraParam(
            ann_params = ann_params,
            cores = cores,
            sigma = sigma,
            perfwhm = perfwhm,
            cor_eic_th = cor_eic_th,
            pval = pval,
            graph_method = "a"
        ),
        "graph_method must be \"lpc\" or \"hcs\""
    )

    # test in positive
    obj <- CameraParam(
        ann_params = ann_params,
        cores = cores,
        sigma = sigma,
        perfwhm = perfwhm,
        cor_eic_th = cor_eic_th,
        pval = pval,
        graph_method = graph_method
    )
    testthat::expect_equal(obj@cores, cores)
    testthat::expect_equal(obj@polarity, polarity)
    testthat::expect_equal(obj@sigma, sigma)
    testthat::expect_equal(obj@perfwhm, perfwhm)
    testthat::expect_equal(obj@cor_eic_th, cor_eic_th)
    testthat::expect_equal(obj@pval, pval)
    testthat::expect_equal(obj@graph_method, graph_method)
    testthat::expect_equal(obj@calcIso, TRUE)
    testthat::expect_equal(obj@calcCiS, TRUE)
    testthat::expect_equal(obj@calcCaS, TRUE)
    testthat::expect_equal(obj@maxcharge, 3)
    testthat::expect_equal(obj@maxiso, 4)
    testthat::expect_equal(obj@ppm, 0)
    testthat::expect_equal(obj@mzabs, da_tol)
    testthat::expect_equal(obj@minfrac, 0)
    testthat::expect_equal(obj@multiplier, 3)
    testthat::expect_equal(
        obj@rules,
        adducts[
            adducts$charge >= 1,
            c("name", "nmol", "charge", "massdiff", "oidscore", "quasi", "ips"),
            drop = FALSE]
    )
    testthat::expect_equal(obj@max_peaks, 100)

    # test in negative
    ann_params@database <- "test-neg"
    ann_params@polarity <- "negative"
    obj_neg <- CameraParam(
        ann_params = ann_params,
        cores = cores,
        sigma = sigma,
        perfwhm = perfwhm,
        cor_eic_th = cor_eic_th,
        pval = pval,
        graph_method = graph_method
    )
    testthat::expect_equal(obj_neg@cores, cores)
    testthat::expect_equal(obj_neg@polarity, "negative")
    testthat::expect_equal(obj_neg@sigma, sigma)
    testthat::expect_equal(obj_neg@perfwhm, perfwhm)
    testthat::expect_equal(obj_neg@cor_eic_th, cor_eic_th)
    testthat::expect_equal(obj_neg@pval, pval)
    testthat::expect_equal(obj_neg@graph_method, graph_method)
    testthat::expect_equal(obj_neg@calcIso, TRUE)
    testthat::expect_equal(obj_neg@calcCiS, TRUE)
    testthat::expect_equal(obj_neg@calcCaS, TRUE)
    testthat::expect_equal(obj_neg@maxcharge, 3)
    testthat::expect_equal(obj_neg@maxiso, 4)
    testthat::expect_equal(obj_neg@ppm, 0)
    testthat::expect_equal(obj_neg@mzabs, da_tol)
    testthat::expect_equal(obj_neg@minfrac, 0)
    testthat::expect_equal(obj_neg@multiplier, 3)
    testthat::expect_equal(
        obj_neg@rules,
        adducts[
            adducts$charge <= -1,
            c("name", "nmol", "charge", "massdiff", "oidscore", "quasi", "ips"),
            drop = FALSE]
    )
    testthat::expect_equal(obj_neg@max_peaks, 100)

    testthat::expect_equal(
        params_to_dataframe(obj),
        data.frame(
            cores = cores,
            sigma = sigma,
            perfwhm = perfwhm,
            intval = "into",
            cor_eic_th = cor_eic_th,
            pval = pval,
            graph_method = graph_method,
            calcIso = 1,
            calcCiS = 1,
            calcCaS = 1,
            maxcharge = 3,
            maxiso = 4,
            ppm = 0,
            mzabs = da_tol,
            minfrac = 0,
            max_peaks = 100
        )
    )
})

testthat::test_that("annotation_param", {
    da_tol <- .015
    rt_tol <- 10
    abd_tol <- 25
    instrument <- "QTOF_XevoG2-S_R25000@200"
    database <- "test"
    polarity <- "positive"
    cpd_classes <- c("LPC", "Cer")

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
        AnnotationParam(da_tol = da_tol, rt_tol = "a"),
        "got class \"character\", should be or extend class \"numeric\""
    )
    testthat::expect_error(
        AnnotationParam(da_tol = da_tol, rt_tol = c(1, 2)),
        "rt_tol need to be a positive number"
    )
    testthat::expect_error(
        AnnotationParam(da_tol = da_tol, rt_tol = -1),
        "rt_tol need to be a positive number"
    )
    testthat::expect_error(
        AnnotationParam(da_tol = da_tol, rt_tol = rt_tol, abd_tol = "a"),
        "got class \"character\", should be or extend class \"numeric\""
    )
    testthat::expect_error(
        AnnotationParam(da_tol = da_tol, rt_tol = rt_tol, abd_tol = c(1, 2)),
        "abd_tol need to be a positive number between 0 & 100"
    )
    testthat::expect_error(
        AnnotationParam(da_tol = da_tol, rt_tol = rt_tol, abd_tol = -1),
        "abd_tol need to be a positive number between 0 & 100"
    )
    testthat::expect_error(
        AnnotationParam(da_tol = da_tol, rt_tol = rt_tol, abd_tol = -1),
        "abd_tol need to be a positive number between 0 & 100"
    )
    testthat::expect_error(
        AnnotationParam(
            da_tol = da_tol,
            rt_tol = rt_tol,
            abd_tol = abd_tol,
            instrument = 1
        ),
        "got class \"numeric\", should be or extend class \"character\""
    )
    testthat::expect_error(
        AnnotationParam(
            da_tol = da_tol,
            rt_tol = rt_tol,
            abd_tol = abd_tol,
            instrument = "orbitrap",
        ),
        "orbitrap doesn't exists in the instrument list"
    )
    testthat::expect_error(
        AnnotationParam(
            da_tol = da_tol,
            rt_tol = rt_tol,
            abd_tol = abd_tol,
            instrument = instrument,
            database = 1
        ),
        "database 1 doesn't exist in software"
    )
    testthat::expect_error(
        AnnotationParam(
            da_tol = da_tol,
            rt_tol = rt_tol,
            abd_tol = abd_tol,
            instrument = instrument,
            database = database,
            polarity = 1
        ),
        "polarity needs to be \"positive\" or \"negative\""
    )
    testthat::expect_error(
        AnnotationParam(
            da_tol = da_tol,
            rt_tol = rt_tol,
            abd_tol = abd_tol,
            instrument = instrument,
            database = database,
            polarity = polarity,
            cpd_classes = 1
        ),
        "got class \"numeric\", should be or extend class \"character\""
    )
    testthat::expect_error(
        AnnotationParam(
            da_tol = da_tol,
            rt_tol = rt_tol,
            abd_tol = abd_tol,
            instrument = instrument,
            database = database,
            polarity = polarity,
            cpd_classes = c("OP", "CC")
        ),
        "No chemicals can be loaded with the given parameters"
    )
    testthat::expect_error(
        AnnotationParam(
            da_tol = da_tol,
            rt_tol = rt_tol,
            abd_tol = abd_tol,
            instrument = instrument,
            database = database,
            polarity = "negative"
        ),
        "No chemicals can be loaded with the given parameters"
    )

    obj <- AnnotationParam(
        da_tol = da_tol,
        rt_tol = rt_tol,
        abd_tol = abd_tol,
        instrument = instrument,
        database = database,
        polarity = polarity
    )
    testthat::expect_equal(obj@da_tol, da_tol)
    testthat::expect_equal(obj@rt_tol, rt_tol)
    testthat::expect_equal(obj@abd_tol, abd_tol)
    testthat::expect_equal(obj@instrument, instrument)
    testthat::expect_equal(obj@database, database)
    testthat::expect_equal(obj@polarity, polarity)
    testthat::expect_equal(
        obj@cpd_classes,
        unique(load_chem_db(obj@database, obj@polarity)$class)
    )

    testthat::expect_equal(
        params_to_dataframe(obj),
        data.frame(
            da_tol = da_tol,
            rt_tol = rt_tol,
            abd_tol = abd_tol,
            instrument = instrument,
            database = database,
            polarity = polarity,
            cpd_classes = paste(
                unique(load_chem_db(obj@database, obj@polarity)$class),
                collapse = ";"
            )
        )
    )
})

testthat::test_that("check_ms_process_args", {
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
    converter <- "~/GitHub/SPRING/pwiz/msconvert.exe"
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
        instrument = "QTOF_XevoG2-S_R25000@200",
        database = "test",
        cpd_classes = c("LPC", "Cer", "FA")
    )
    camera_params <- CameraParam(
        ann_param = ann_params,
        cores = 1,
        sigma = 6,
        perfwhm = .6,
        cor_eic_th = .75,
        pval = .05,
        graph_method = "hcs"
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
        "cwt_params argument must be a CentWaveParam object"
    )
    testthat::expect_error(
        check_ms_process_args(
            raw_files,
            sqlite_path,
            converter,
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
            cwt_params,
            obw_params,
            pd_params,
            NULL
        ),
        "camera_params argument must be a CameraParam object"
    )
    testthat::expect_error(
        check_ms_process_args(
            raw_files,
            sqlite_path,
            converter,
            cwt_params,
            obw_params,
            pd_params,
            camera_params,
            NULL
        ),
        "ann_params argument must be an AnnotationParam object"
    )
    testthat::expect_error(
        check_ms_process_args(
            raw_files,
            sqlite_path,
            converter,
            cwt_params,
            obw_params,
            pd_params,
            camera_params,
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
            cwt_params,
            obw_params,
            pd_params,
            camera_params,
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
            cwt_params,
            obw_params,
            pd_params,
            camera_params,
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
            cwt_params,
            obw_params,
            pd_params,
            camera_params,
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
            cwt_params,
            obw_params,
            pd_params,
            camera_params,
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
            cwt_params,
            obw_params,
            pd_params,
            camera_params,
            ann_params,
            1
        ),
        1
    )
})
