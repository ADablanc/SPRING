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
            adduct_names = c(
                "[M+Na]+",
                "[M+NH4]+",
                "[M+H-H2O]+",
                "[M+H]+",
                "[M-H]-"
            ),
            instrument = "orbitrap",
        ),
        "orbitrap doesn't exists in the instrument list"
    )
    testthat::expect_error(
        AnnotationParam(
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
            instrument = "QTOF_XevoG2-S_R25000@200",
            database = 1
        ),
        "1 doesn't exist in software"
    )
    testthat::expect_error(
        AnnotationParam(
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
            instrument = "QTOF_XevoG2-S_R25000@200",
            database = "test",
            cpd_classes = 1
        ),
        "got class \"numeric\", should be or extend class \"character\""
    )
    testthat::expect_error(
        AnnotationParam(
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
            instrument = "QTOF_XevoG2-S_R25000@200",
            database = "test",
            cpd_classes = c("OP", "CC")
        ),
        "OP and CC doesn't exists in database"
    )
    obj <- AnnotationParam(
        da_tol = 0.015,
        rt_tol = 10,
        abd_tol = 25,
        instrument = "QTOF_XevoG2-S_R25000@200"
    )
    testthat::expect_equal(obj, AnnotationParam())
    testthat::expect_equal(obj@da_tol, .015)
    testthat::expect_equal(obj@rt_tol, 10)
    testthat::expect_equal(obj@abd_tol, 25)
    testthat::expect_equal(obj@adduct_names, adducts$name)
    testthat::expect_equal(obj@instrument, "QTOF_XevoG2-S_R25000@200")
    testthat::expect_equal(obj@database, get_available_database()[1])
    testthat::expect_equal(
        obj@cpd_classes,
        unique(load_chem_db(obj@database)$class)
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
        instrument = "QTOF_XevoG2-S_R25000@200",
        database = "test",
        cpd_classes = c("LPC", "Cer", "FA")
    )
    testthat::expect_equal(obj@da_tol, .015)
    testthat::expect_equal(obj@rt_tol, 10)
    testthat::expect_equal(obj@abd_tol, 25)
    testthat::expect_equal(
        obj@adduct_names,
        c("[M+Na]+", "[M+NH4]+", "[M+H-H2O]+", "[M+H]+", "[M-H]-")
    )
    testthat::expect_equal(obj@instrument, "QTOF_XevoG2-S_R25000@200")
    testthat::expect_equal(obj@database, "test")
    testthat::expect_equal(obj@cpd_classes, c("LPC", "Cer", "FA"))
    testthat::expect_error(
        restrict_ann_param_polarity(obj, "maybe"),
        "polarity must be set to \"positive\" or \"negative\""
    )
    obj_pos <- AnnotationParam(
        da_tol = 0.015,
        rt_tol = 10,
        abd_tol = 25,
        adduct_names = c("[M+H]+", "[M+Na]+", "[M+NH4]+", "[M+H-H2O]+"),
        instrument = "QTOF_XevoG2-S_R25000@200",
        database = "test",
        cpd_classes = c("LPC", "Cer", "FA")
    )
    testthat::expect_equal(
        restrict_ann_param_polarity(obj, "positive"),
        obj_pos
    )
    obj_neg <- AnnotationParam(
        da_tol = 0.015,
        rt_tol = 10,
        abd_tol = 25,
        adduct_names = "[M-H]-",
        instrument = "QTOF_XevoG2-S_R25000@200",
        database = "test",
        cpd_classes = c("LPC", "Cer", "FA")
    )
    testthat::expect_equal(
        restrict_ann_param_polarity(obj, "negative"),
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
            instrument = "QTOF_XevoG2-S_R25000@200",
            database = "test",
            cpd_classes = paste(c("LPC", "Cer", "FA"), collapse = ";")
        )
    )
})

testthat::test_that("camera_param", {
    ann_param <- AnnotationParam(
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
        instrument = "QTOF_XevoG2-S_R25000@200",
        database = "test",
        cpd_classes = c("LPC", "Cer", "FA")
    )

    testthat::expect_error(
        CameraParam(ann_param = "a"),
        "ann_params must be an AnnotationParam object"
    )
    testthat::expect_error(
        CameraParam(ann_param = ann_param, cores = "a"),
        "got class \"character\", should be or extend class \"numeric\""
    )
    testthat::expect_error(
        CameraParam(ann_param = ann_param, cores = c(1, 2)),
        "cores need to be a positive number"
    )
    testthat::expect_error(
        CameraParam(ann_param = ann_param, cores = 0),
        "cores need to be a positive number"
    )
    testthat::expect_error(
        CameraParam(ann_param = ann_param, cores = 1, sigma = "a"),
        "got class \"character\", should be or extend class \"numeric\""
    )
    testthat::expect_error(
        CameraParam(ann_param = ann_param, cores = 1, sigma = c(3, 6)),
        "sigma need to be a positive number"
    )
    testthat::expect_error(
        CameraParam(ann_param = ann_param, cores = 1, sigma = -1),
        "sigma need to be a positive number"
    )
    testthat::expect_error(
        CameraParam(ann_param = ann_param, cores = 1, sigma = 6, perfwhm = "a"),
        "got class \"character\", should be or extend class \"numeric\""
    )
    testthat::expect_error(
        CameraParam(
            ann_param = ann_param,
            cores = 1,
            sigma = 6,
            perfwhm = c(.4, .5)
        ),
        "perfwhm need to be a number between 0 and 1"
    )
    testthat::expect_error(
        CameraParam(ann_param = ann_param, cores = 1, sigma = 6, perfwhm = -2),
        "perfwhm need to be a number between 0 and 1"
    )
    testthat::expect_error(
        CameraParam(ann_param = ann_param, cores = 1, sigma = 6, perfwhm = 1.2),
        "perfwhm need to be a number between 0 and 1"
    )
    testthat::expect_error(
        CameraParam(
            ann_param = ann_param,
            cores = 1,
            sigma = 6,
            perfwhm = .6,
            cor_eic_th = "a"
        ),
        "got class \"character\", should be or extend class \"numeric\""
    )
    testthat::expect_error(
        CameraParam(
            ann_param = ann_param,
            cores = 1,
            sigma = 6,
            perfwhm = .6,
            cor_eic_th = c(.4, .6)
        ),
        "cor_eic_th must be a number between 0 and 1"
    )
    testthat::expect_error(
        CameraParam(
            ann_param = ann_param,
            cores = 1,
            sigma = 6,
            perfwhm = .6,
            cor_eic_th = -2
        ),
        "cor_eic_th must be a number between 0 and 1"
    )
    testthat::expect_error(
        CameraParam(
            ann_param = ann_param,
            cores = 1,
            sigma = 6,
            perfwhm = .6,
            cor_eic_th = 1.2
        ),
        "cor_eic_th must be a number between 0 and 1"
    )
    testthat::expect_error(
        CameraParam(
            ann_param = ann_param,
            cores = 1,
            sigma = 6,
            perfwhm = .6,
            cor_eic_th = .75,
            pval = "a"
        ),
        "got class \"character\", should be or extend class \"numeric\""
    )
    testthat::expect_error(
        CameraParam(
            ann_param = ann_param,
            cores = 1,
            sigma = 6,
            perfwhm = .6,
            cor_eic_th = .75,
            pval = c(.4, .6)
        ),
        "pval must be a number between 0 and 1"
    )
    testthat::expect_error(
        CameraParam(
            ann_param = ann_param,
            cores = 1,
            sigma = 6,
            perfwhm = .6,
            cor_eic_th = .75,
            pval = -2
        ),
        "pval must be a number between 0 and 1"
    )
    testthat::expect_error(
        CameraParam(
            ann_param = ann_param,
            cores = 1,
            sigma = 6,
            perfwhm = .6,
            cor_eic_th = .75,
            pval = 1.2
        ),
        "pval must be a number between 0 and 1"
    )
    testthat::expect_error(
        CameraParam(
            ann_param = ann_param,
            cores = 1,
            sigma = 6,
            perfwhm = .6,
            cor_eic_th = .75,
            pval = .05,
            graphMethod = 1
        ),
        "got class \"numeric\", should be or extend class \"character\""
    )
    testthat::expect_error(
        CameraParam(
            ann_param = ann_param,
            cores = 1,
            sigma = 6,
            perfwhm = .6,
            cor_eic_th = .75,
            pval = .75,
            graphMethod = "a"
        ),
        "graphMethod must be \"lpc\" or \"hcs\""
    )

    obj <- CameraParam(
        ann_param = ann_param,
        cores = 1,
        sigma = 6,
        perfwhm = .6,
        cor_eic_th = .75,
        pval = .05,
        graphMethod = "hcs"
    )
    testthat::expect_equal(obj, CameraParam(ann_param))
    testthat::expect_equal(obj@cores, 1)
    testthat::expect_equal(obj@polarity, NA_character_)
    testthat::expect_equal(obj@sigma, 6)
    testthat::expect_equal(obj@perfwhm, .6)
    testthat::expect_equal(obj@intval, "into")
    testthat::expect_equal(obj@cor_eic_th, .75)
    testthat::expect_equal(obj@pval, .05)
    testthat::expect_equal(obj@graphMethod, "hcs")
    testthat::expect_equal(obj@calcIso, TRUE)
    testthat::expect_equal(obj@calcCiS, TRUE)
    testthat::expect_equal(obj@calcCaS, TRUE)
    testthat::expect_equal(obj@maxcharge, 0)
    testthat::expect_equal(obj@maxiso, 3)
    testthat::expect_equal(obj@ppm, 0)
    testthat::expect_equal(obj@mzabs, .015)
    testthat::expect_equal(obj@minfrac, 0)
    testthat::expect_equal(obj@rules, data.frame())
    testthat::expect_equal(obj@multiplier, 0)
    testthat::expect_equal(obj@max_peaks, 100)

    testthat::expect_error(
        restrict_camera_param_polarity(obj, "maybe"),
        "polarity must be set to \"positive\" or \"negative\""
    )

    obj_pos <- restrict_camera_param_polarity(obj, "positive")
    obj_pos2 <- obj
    obj_pos2@polarity <- "positive"
    obj_pos2@maxcharge <- 3
    obj_pos2@rules <- adducts[
        adducts$charge >= 1,
        c("name", "nmol", "charge", "massdiff", "oidscore", "quasi", "ips")]
    obj_pos2@multiplier <- 3
    testthat::expect_equal(obj_pos, obj_pos2)

    obj_pos <- restrict_camera_param_polarity(obj, "negative")
    obj_pos2 <- obj
    obj_pos2@polarity <- "negative"
    obj_pos2@maxcharge <- 3
    obj_pos2@rules <- adducts[
        adducts$charge <= -1,
        c("name", "nmol", "charge", "massdiff", "oidscore", "quasi", "ips")]
    obj_pos2@multiplier <- 3
    testthat::expect_equal(obj_pos, obj_pos2)

    testthat::expect_equal(
        params_to_dataframe(obj),
        data.frame(
            cores = 1,
            sigma = 6,
            perfwhm = .6,
            intval = "into",
            cor_eic_th = .75,
            pval = .05,
            graphMethod = "hcs",
            calcIso = 1,
            calcCiS = 1,
            calcCaS = 1,
            maxiso = 3,
            ppm = 0,
            mzabs = .015,
            minfrac = 0,
            max_peaks = 100
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
        graphMethod = "hcs"
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
