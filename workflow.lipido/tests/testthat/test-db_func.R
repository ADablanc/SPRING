testthat::test_that("db connect", {
    db <- db_connect(":memory:")
    testthat::expect_s4_class(
        db,
        "SQLiteConnection"
    )
    RSQLite::dbDisconnect(db)
})

testthat::test_that("db write table", {
    db <- db_connect(":memory:")
    dbWriteTable(db, "mtcars", mtcars, row.names = TRUE)
    testthat::expect_identical(
        RSQLite::dbReadTable(db, "mtcars", row.names = TRUE),
        mtcars
    )
    RSQLite::dbDisconnect(db)
})

testthat::test_that("db execute", {
    db <- db_connect(":memory:")
    dbWriteTable(db, "mtcars", mtcars, row.names = TRUE)
    testthat::expect_error(
        dbExecute(
            db,
            "INSERT INTO mtcars
                  (mpg, cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb)
            VALUES (0, 0, 0, 0, 0, 0, 0, 0, 0, 0);"
        ),
        "10 values for 11 columns"
    )
    dbExecute(
        db,
        "INSERT INTO mtcars
                  (mpg, cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb)
            VALUES (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);"
    )
    testthat::expect_equal(
        RSQLite::dbGetQuery(db, "SELECT COUNT(*) FROM mtcars")[1, 1],
        nrow(mtcars) + 1
    )
    RSQLite::dbDisconnect(db)
})

testthat::test_that("db get query", {
    db <- db_connect(":memory:")
    dbWriteTable(db, "mtcars", mtcars, row.names = TRUE)
    testthat::expect_error(
        dbGetQuery(db, "SELECT id FROM mtcars"),
        "no such column: id"
    )
    testthat::expect_identical(
        dbGetQuery(db, "SELECT * FROM peaks"),
        data.frame()
    )
    testthat::expect_identical(
        dbGetQuery(db, "SELECT * FROM mtcars", row.names = TRUE),
        mtcars
    )
    RSQLite::dbDisconnect(db)
})

testthat::test_that("db read table", {
    db <- db_connect(":memory:")
    dbWriteTable(db, "mtcars", mtcars, row.names = TRUE)
    testthat::expect_identical(
        dbReadTable(db, "peaks"),
        data.frame()
    )
    testthat::expect_identical(
        dbReadTable(db, "mtcars", row.names = TRUE),
        mtcars
    )
    RSQLite::dbDisconnect(db)
})

testthat::test_that("compress", {
    expect_identical(
        unserialize(compress(mtcars)[[1]]),
        mtcars
    )
})

testthat::test_that("decompress", {
    expect_identical(
        decompress(compress(mtcars)),
        mtcars
    )
})

testthat::test_that("db record samples", {
    db <- db_connect(":memory:")
    samples <- data.frame(
        sample = "small",
        ms_file_positive = NA,
        ms_file_negative = NA,
        profile_positive = NA,
        profile_negative = NA,
        xset_positive = NA,
        xset_negative = NA
    )
    db_record_samples(db, samples$sample)
    testthat::expect_identical(
        RSQLite::dbReadTable(db, "sample")$sample,
        samples$sample
    )
    RSQLite::dbDisconnect(db)
})

testthat::test_that("record ms file", {
    db <- db_connect(":memory:")
    db_record_samples(db, "small")
    db_record_ms_file(
        db,
        "small",
        "positive",
        xcms::xcmsRaw(
            system.file(
                "testdata",
                "small.mzXML",
                package = "workflow.lipido"
            ),
            profstep = 0
        ),
        .1
    )
    expect_equal(
        decompress(dbGetQuery(
            db,
            "SELECT ms_file_positive FROM sample LIMIT 1"
        )[1, 1])@scanindex,
        c(0, 1810)
    )
    expect_equal(
        length(decompress(dbGetQuery(
            db,
            "SELECT profile_positive FROM sample LIMIT 1"
        )[1, 1])),
        35996
    )
    RSQLite::dbDisconnect(db)
})

testthat::test_that("read ms file", {
    db <- db_connect(":memory:")
    db_record_samples(db, "small")
    expect_equal(
        db_read_ms_file(db, "small", "negative"),
        NULL
    )
    db_record_ms_file(
        db,
        "small",
        "positive",
        xcms::xcmsRaw(
            system.file(
                "testdata",
                "small.mzXML",
                package = "workflow.lipido"
            ),
            profstep = 0
        ),
        .1
    )
    expect_equal(
        db_read_ms_file(db, "small", "positive")@scanindex,
        c(0, 1810)
    )
    RSQLite::dbDisconnect(db)
})

testthat::test_that("get profile matrix", {
    db <- db_connect(":memory:")
    db_record_samples(db, "small")
    expect_equal(
        db_get_profile(db, "small", "negative"),
        NULL
    )
    db_record_ms_file(
        db,
        "small",
        "positive",
        xcms::xcmsRaw(
            system.file(
                "testdata",
                "small.mzXML",
                package = "workflow.lipido"
            ),
            profstep = 0
        ),
        .1
    )
    expect_equal(
        length(db_get_profile(db, "small", "positive")),
        35996
    )
    RSQLite::dbDisconnect(db)
})

testthat::test_that("import ms file", {
    raw_file <- system.file(
        "testdata",
        "small.raw",
        package = "workflow.lipido"
    )
    converter <- tools::file_path_as_absolute(
        "~/GitHub/workflow.lipido/pwiz/msconvert.exe"
    )
    filter_params <- FilterParam(
        mz_range = c(200, 2001),
        rt_range = c(0, 0.5)
    )
    db <- db_connect(":memory:")
    db_record_samples(db, "small")
    testthat::expect_identical(
        import_ms_file(
            db,
            "small",
            raw_file,
            converter,
            "unknown polarity",
            filter_params
        ),
        "msconvert error"
    )
    import_ms_file(db, "small", raw_file, converter, "positive", filter_params)
    testthat::expect_equal(
        db_read_ms_file(db, "small", "positive")@scanindex,
        c(0, 1810)
    )
    testthat::expect_identical(
        length(db_get_profile(db, "small", "positive")),
        as.integer(35996)
    )
    RSQLite::dbDisconnect(db)
})

testthat::test_that("record xset", {
    filepath <- system.file(
        "testdata",
        "small_pos-neg.mzXML",
        package = "workflow.lipido"
    )
    suppressWarnings(suppressMessages(
        xset <- xcms::xcmsSet(filepath)
    ))
    db <- db_connect(":memory:")
    db_record_samples(db, "small")
    db_record_xset(db, xset, xset, "small")
    testthat::expect_identical(
        decompress(dbGetQuery(
            db,
            "SELECT xset_positive FROM sample LIMIT 1"
        )[1, 1]),
        xset
    )
    testthat::expect_identical(
        decompress(dbGetQuery(
            db,
            "SELECT xset_negative FROM sample LIMIT 1"
        )[1, 1]),
        xset
    )
    RSQLite::dbDisconnect(db)
})

testthat::test_that("db record ann", {
    ann <- data.frame(
        group_id = c(6, 7, 1, 2, 5),
        name = c("Cer (d18:1/C12:0)", "Cer (d18:1/C12:0)", "LPC 11:0",
                 "LPC 11:0", "LPC 11:0"),
        formula = c("C30H59N1O3", "C30H59N1O3", "C19H40N1O7P1", "C19H40N1O7P1",
                    "C19H40N1O7P1"),
        adduct = c("[M+H-H2O]+", "[M+Na]+", "[M+H-H2O]+", "[M+H]+", "[M+Na]+"),
        ion_formula = c("C30H58N1O2", "C30H59N1O3Na1", "C19H39N1O6P1",
                        "C19H41N1O7P1", "C19H40N1O7P1Na1"),
        rtdiff = c(2.37300000000002, 2.10850000000002, 9.52199999999993,
                   8.99149999999997, 8.99299999999994),
        rt = c(197.973, 197.7085, 286.278, 286.8085, 286.807),
        rtmin = c(196.122, 184.2245, 284.692, 284.6935, 282.048),
        rtmax = c(200.088, 203.789, 287.864, 292.0965, 292.095),
        nsamples = c(2, 2, 1, 2, 1),
        best_score = c(71.3979721069336, 89.4345550537109, 79.8211975097656,
                       95.1391906738281, 79.6432037353516),
        best_deviation_mz = c(0.0010986328125, 0.00140380859375,
                              0.0003662109375, 0.0008544921875,
                              0.000701904296875),
        best_npeak = c(1, 2, 1, 2, 1),
        `220221CCM_lbl__01_lu_flrd` = c(5, 7, NA, 2, NA),
        `220221CCM_lbl__02_lu_flrd` = c(6, 8, 1, 3, 4)
    )
    spectras <- data.frame(
        spectra_id = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5,
                       5, 6, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8),
        feature_id = c(10, NA, NA, NA, 4, NA, NA, 5, 12, NA, NA, 13, NA, NA,
                       NA, 11, 1, NA, NA, NA, 7, NA, NA, NA, 2, 3, NA, NA, 8,
                       6, NA, NA),
        mz = c(408.251325886321, NA, NA, NA, 427.265348519813, NA, NA,
               426.261913381751, 427.265704881008, NA, NA, 426.262333104945,
               NA, NA, NA, 448.244170162955, 464.447304014051, NA, NA, NA,
               464.447454557257, NA, NA, NA, 504.440032161331, 505.443534603,
               NA, NA, 504.44048100644, 505.44383474773, NA, NA),
        int = c(88824.635233072, NA, NA, NA, 1170639.95871094, NA, NA,
                6214416.44108707, 1186767.56444882, NA, NA, 6201250.27168528,
                NA, NA, NA, 288290.748778874, 4945601.93026269, NA, NA, NA,
                5689144.27927454, NA, NA, NA, 1448292.29379181,
                401071.227087501, NA, NA, 1662831.19267642, 444083.087307128,
                NA, NA),
        abd = c(100, NA, NA, NA, 18.8374881182917, NA, NA, 100,
                19.1375531135642, NA, NA, 100, NA, NA, NA, 100, 100, NA, NA,
                NA, 100, NA, NA, NA, 100, 27.6926991054717, NA, NA, 100,
                26.7064443620612, NA, NA),
        ion_id_theo = c(62, 62, 62, 62, 66, 66, 66, 66, 66, 66, 66, 66, 64, 64,
                        64, 64, 278, 278, 278, 278, 278, 278, 278, 278, 281,
                        281, 281, 281, 281, 281, 281, 281),
        mz_theo = c(408.25095, 409.25427, 410.25672, 411.25935, 427.26484,
                    428.26719, 429.26984, 426.26152, 427.26484, 428.26719,
                    429.26984, 426.26152, 449.24678, 450.24914, 451.25178,
                    448.24346, 464.44621, 465.44955, 466.45272, 467.45576,
                    464.44621, 465.44955, 466.45272, 467.45576, 504.43872,
                    505.44206, 506.44516, 507.44809, 504.43872, 505.44206,
                    506.44516, 507.44809),
        abd_theo = c(100, 21.65, 3.25, 0.38, 21.7, 3.46, 0.42, 100, 21.7, 3.46,
                     0.42, 100, 21.69, 3.45, 0.42, 100, 100, 33.55, 5.86, 0.65,
                     100, 33.55, 5.86, 0.65, 100, 33.67, 6.07, 0.72, 100,
                     33.67, 6.07, 0.72),
        iso_theo = c("M", "M+1", "M+2", "M+3", "M+1", "M+2", "M+3", "M", "M+1",
                     "M+2", "M+3", "M", "M+1", "M+2", "M+3", "M", "M", "M+1",
                     "M+2", "M+3", "M", "M+1", "M+2", "M+3", "M", "M+1", "M+2",
                     "M+3", "M", "M+1", "M+2", "M+3")
    )
    spectra_infos <- data.frame(
        spectra_id = c(1, 2, 3, 4, 5, 6, 7, 8),
        score = c(79.8211975097656, 94.9317855834961, 95.1391906738281,
                  79.6432037353516, 71.3979721069336, 71.3979721069336,
                  89.4345550537109, 88.4888916015625),
        deviation_mz = c(0.0003662109375, 0.000457763671875, 0.0008544921875,
                         0.000701904296875, 0.0010986328125, 0.001251220703125,
                         0.00140380859375, 0.0017852783203125),
        npeak = c(1, 2, 2, 1, 1, 1, 2, 2),
        basepeak_int = c(88824.635233072, 6214416.44108707, 6201250.27168528,
                         288290.748778874, 4945601.93026269, 5689144.27927454,
                         1448292.29379181, 1662831.19267642),
        sum_int = c(88824.635233072, 7385056.39979801, 7388017.8361341,
                    288290.748778874, 4945601.93026269, 5689144.27927454,
                    1849363.52087931, 2106914.27998355),
        sample = c("220221CCM_lbl__02_lu_flrd", "220221CCM_lbl__01_lu_flrd",
                   "220221CCM_lbl__02_lu_flrd", "220221CCM_lbl__02_lu_flrd",
                   "220221CCM_lbl__01_lu_flrd", "220221CCM_lbl__02_lu_flrd",
                   "220221CCM_lbl__01_lu_flrd", "220221CCM_lbl__02_lu_flrd"),
        rt = c(286.278, 286.81, 286.807, 286.807, 197.973, 197.973, 197.444,
               197.973)
    )
    peaks <- data.frame(
        feature_id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13),
        mz = c(464.447304014051, 504.440032161331, 505.443534603,
               427.265348519813, 426.261913381751, 505.44383474773,
               464.447454557257, 504.44048100644, 429.270861378734,
               408.251325886321, 448.244170162955, 427.265704881008,
               426.262333104945),
        mzmin = c(464.447021484375, 504.439697265625, 505.443084716797,
                  427.264587402344, 426.261444091797, 505.440704345703,
                  464.446746826172, 504.439544677734, 429.270660400391,
                  408.251037597656, 448.242279052734, 427.265258789062,
                  426.262023925781),
        mzmax = c(464.447662353516, 504.440490722656, 505.443664550781,
                  427.266052246094, 426.262420654297, 505.444122314453,
                  464.447967529297, 504.441101074219, 429.271148681641,
                  408.25146484375, 448.245422363281, 427.266052246094,
                  426.262786865234),
        rt = c(197.973, 197.444, 197.444, 286.81, 286.81, 197.444, 197.973,
               197.973, 261.426, 286.278, 286.807, 286.807, 286.807),
        rtmin = c(196.386, 194.271, 196.386, 284.695, 284.695, 196.387,
                  195.858, 174.178, 258.253, 284.692, 282.048, 283.105,
                  284.692),
        rtmax = c(200.617, 199.03, 199.03, 291.04, 292.098, 200.617, 199.559,
                  208.548, 264.069, 287.864, 292.095, 292.095, 292.095),
        int = c(4945601.93026269, 1448292.29379181, 401071.227087501,
                1170639.95871094, 6214416.44108707, 444083.087307128,
                5689144.27927454, 1662831.19267642, 801559.236409095,
                88824.635233072, 288290.748778874, 1186767.56444882,
                6201250.27168528),
        intb = c(4945464.15722767, 1448264.72345856, 401069.111887501,
                 1170634.14246094, 6212404.90921921, 444001.578713656,
                 5689141.10698883, 1662613.35060178, 801553.420409095,
                 88821.9918997387, 287904.836179229, 1185632.54587715,
                 6201242.86868528),
        maxo = c(4130684, 1395886, 434808.75, 676902.5, 3501824, 524308,
                 5734716, 1723138, 150635.375, 70041.5625, 186253.875, 673140,
                 3489368),
        sn = c(8329, 22672, 434808, 676902, 3587, 4386, 5734715, 14423, 145000,
               70041, 675, 1134, 3489367),
        egauss = as.numeric(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                              NA)),
        mu = as.numeric(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)),
        sigma = as.numeric(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                             NA)),
        h = as.numeric(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)),
        f = c(1, 2, 3, 4, 6, 1, 2, 3, 4, 5, 6, 7, 8),
        dppm = c(1, 2, 1, 2, 1, 7, 2, 3, 1, 1, 2, 1, 1),
        scale = c(3, 3, 3, 5, 5, 3, 13, 19, 3, 3, 5, 5, 5),
        scpos = c(58, 57, 57, 187, 187, 57, 58, 58, 136, 183, 184, 184, 184),
        scmin = c(55, 54, 54, 182, 182, 54, 45, 39, 133, 180, 179, 179, 179),
        scmax = c(61, 60, 60, 192, 192, 60, 71, 77, 139, 186, 189, 189, 189),
        lmin = c(55, 52, 55, 87, 87, 55, 54, 20, 101, 87, 82, 84, 87),
        lmax = c(63, 60, 60, 99, 101, 63, 61, 77, 112, 93, 101, 101, 101),
        sample = c("220221CCM_lbl__01_lu_flrd", "220221CCM_lbl__01_lu_flrd",
                   "220221CCM_lbl__01_lu_flrd", "220221CCM_lbl__01_lu_flrd",
                   "220221CCM_lbl__01_lu_flrd", "220221CCM_lbl__02_lu_flrd",
                   "220221CCM_lbl__02_lu_flrd", "220221CCM_lbl__02_lu_flrd",
                   "220221CCM_lbl__02_lu_flrd", "220221CCM_lbl__02_lu_flrd",
                   "220221CCM_lbl__02_lu_flrd", "220221CCM_lbl__02_lu_flrd",
                   "220221CCM_lbl__02_lu_flrd"),
        polarity = c("positive", "positive", "positive", "positive",
                     "positive", "positive", "positive", "positive",
                     "positive", "positive", "positive", "positive",
                     "positive")
    )
    peak_groups <- data.frame(
        group_id = c(1, 2, 3, 4, 5, 6, 7, 8),
        polarity = c("positive", "positive", "positive", "positive",
                     "positive", "positive", "positive", "positive"),
        mzmed = c(408.251325886321, 426.262123243348, 427.265526700411,
                  429.270861378734, 448.244170162955, 464.447379285654,
                  504.440256583886, 505.443684675365),
        mzmin = c(408.251325886321, 426.261913381751, 427.265348519813,
                  429.270861378734, 448.244170162955, 464.447304014051,
                  504.440032161331, 505.443534603),
        mzmax = c(408.251325886321, 426.262333104945, 427.265704881008,
                  429.270861378734, 448.244170162955, 464.447454557257,
                  504.44048100644, 505.44383474773),
        rtmed = c(286.278, 286.8085, 286.8085, 261.426, 286.807, 197.973,
                  197.7085, 197.444),
        rtmin = c(286.278, 286.807, 286.807, 261.426, 286.807, 197.973,
                  197.444, 197.444),
        rtmax = c(286.278, 286.81, 286.81, 261.426, 286.807, 197.973, 197.973,
                  197.444),
        npeaks = c(1, 2, 2, 1, 1, 2, 2, 2),
        `220221CCM_lbl__01_lu_flrd` = c(NA, 5, 4, NA, NA, 1, 2, 3),
        `220221CCM_lbl__02_lu_flrd` = c(10, 13, 12, 9, 11, 7, 8, 6)
    )
    db <- db_connect(":memory:")
    db_record_ann(db, ann, spectras, spectra_infos, peaks, peak_groups)
    testthat::expect_identical(
        dbReadTable(db, "ann"),
        ann
    )
    testthat::expect_identical(
        dbReadTable(db, "spectras"),
        spectras
    )
    testthat::expect_identical(
        dbReadTable(db, "spectra_infos"),
        spectra_infos
    )
    testthat::expect_equal(
        dbReadTable(db, "peaks"),
        peaks
    )
    testthat::expect_identical(
        dbReadTable(db, "peak_groups"),
        peak_groups
    )
    RSQLite::dbDisconnect(db)
})

testthat::test_that("record params", {
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
        adduct_names = c(
            "[M+Na]+",
            "[M+NH4]+",
            "[M+H-H2O]+",
            "[M+H]+",
            "[M-H]-"
        ),
        instrument = "QTOF_XevoG2-S_R25000@200"
    )
    db <- db_connect(":memory:")
    db_record_params(
        db,
        filter_params,
        cwt_params,
        obw_params,
        pd_params,
        ann_params
    )
    testthat::expect_identical(
        dbReadTable(db, "filter_params"),
        params_to_dataframe(filter_params)
    )
    testthat::expect_equal(
        dbReadTable(db, "cwt_params"),
        params_to_dataframe(cwt_params)
    )
    testthat::expect_equal(
        dbReadTable(db, "obw_params"),
        params_to_dataframe(obw_params)
    )
    testthat::expect_identical(
        dbReadTable(db, "pd_params"),
        params_to_dataframe(pd_params)
    )
    testthat::expect_identical(
        dbReadTable(db, "ann_params"),
        params_to_dataframe(ann_params)
    )
    RSQLite::dbDisconnect(db)
})

testthat::test_that("get spectras", {
    ann <- data.frame(
        group_id = c(6, 7, 1, 2, 5),
        name = c("Cer (d18:1/C12:0)", "Cer (d18:1/C12:0)", "LPC 11:0",
                 "LPC 11:0", "LPC 11:0"),
        formula = c("C30H59N1O3", "C30H59N1O3", "C19H40N1O7P1", "C19H40N1O7P1",
                    "C19H40N1O7P1"),
        adduct = c("[M+H-H2O]+", "[M+Na]+", "[M+H-H2O]+", "[M+H]+", "[M+Na]+"),
        ion_formula = c("C30H58N1O2", "C30H59N1O3Na1", "C19H39N1O6P1",
                        "C19H41N1O7P1", "C19H40N1O7P1Na1"),
        rtdiff = c(2.37300000000002, 2.10850000000002, 9.52199999999993,
                   8.99149999999997, 8.99299999999994),
        rt = c(197.973, 197.7085, 286.278, 286.8085, 286.807),
        rtmin = c(196.122, 184.2245, 284.692, 284.6935, 282.048),
        rtmax = c(200.088, 203.789, 287.864, 292.0965, 292.095),
        nsamples = c(2, 2, 1, 2, 1),
        best_score = c(71.3979721069336, 89.4345550537109, 79.8211975097656,
                       95.1391906738281, 79.6432037353516),
        best_deviation_mz = c(0.0010986328125, 0.00140380859375,
                              0.0003662109375, 0.0008544921875,
                              0.000701904296875),
        best_npeak = c(1, 2, 1, 2, 1),
        `220221CCM_lbl__01_lu_flrd` = c(5, 7, NA, 2, NA),
        `220221CCM_lbl__02_lu_flrd` = c(6, 8, 1, 3, 4)
    )
    spectras <- data.frame(
        spectra_id = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5,
                       5, 6, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8),
        feature_id = c(10, NA, NA, NA, 4, NA, NA, 5, 12, NA, NA, 13, NA, NA,
                       NA, 11, 1, NA, NA, NA, 7, NA, NA, NA, 2, 3, NA, NA, 8,
                       6, NA, NA),
        mz = c(408.251325886321, NA, NA, NA, 427.265348519813, NA, NA,
               426.261913381751, 427.265704881008, NA, NA, 426.262333104945,
               NA, NA, NA, 448.244170162955, 464.447304014051, NA, NA, NA,
               464.447454557257, NA, NA, NA, 504.440032161331, 505.443534603,
               NA, NA, 504.44048100644, 505.44383474773, NA, NA),
        int = c(88824.635233072, NA, NA, NA, 1170639.95871094, NA, NA,
                6214416.44108707, 1186767.56444882, NA, NA, 6201250.27168528,
                NA, NA, NA, 288290.748778874, 4945601.93026269, NA, NA, NA,
                5689144.27927454, NA, NA, NA, 1448292.29379181,
                401071.227087501, NA, NA, 1662831.19267642, 444083.087307128,
                NA, NA),
        abd = c(100, NA, NA, NA, 18.8374881182917, NA, NA, 100,
                19.1375531135642, NA, NA, 100, NA, NA, NA, 100, 100, NA, NA,
                NA, 100, NA, NA, NA, 100, 27.6926991054717, NA, NA, 100,
                26.7064443620612, NA, NA),
        ion_id_theo = c(62, 62, 62, 62, 66, 66, 66, 66, 66, 66, 66, 66, 64, 64,
                        64, 64, 278, 278, 278, 278, 278, 278, 278, 278, 281,
                        281, 281, 281, 281, 281, 281, 281),
        mz_theo = c(408.25095, 409.25427, 410.25672, 411.25935, 427.26484,
                    428.26719, 429.26984, 426.26152, 427.26484, 428.26719,
                    429.26984, 426.26152, 449.24678, 450.24914, 451.25178,
                    448.24346, 464.44621, 465.44955, 466.45272, 467.45576,
                    464.44621, 465.44955, 466.45272, 467.45576, 504.43872,
                    505.44206, 506.44516, 507.44809, 504.43872, 505.44206,
                    506.44516, 507.44809),
        abd_theo = c(100, 21.65, 3.25, 0.38, 21.7, 3.46, 0.42, 100, 21.7, 3.46,
                     0.42, 100, 21.69, 3.45, 0.42, 100, 100, 33.55, 5.86, 0.65,
                     100, 33.55, 5.86, 0.65, 100, 33.67, 6.07, 0.72, 100,
                     33.67, 6.07, 0.72),
        iso_theo = c("M", "M+1", "M+2", "M+3", "M+1", "M+2", "M+3", "M", "M+1",
                     "M+2", "M+3", "M", "M+1", "M+2", "M+3", "M", "M", "M+1",
                     "M+2", "M+3", "M", "M+1", "M+2", "M+3", "M", "M+1", "M+2",
                     "M+3", "M", "M+1", "M+2", "M+3")
    )
    spectra_infos <- data.frame(
        spectra_id = c(1, 2, 3, 4, 5, 6, 7, 8),
        score = c(79.8211975097656, 94.9317855834961, 95.1391906738281,
                  79.6432037353516, 71.3979721069336, 71.3979721069336,
                  89.4345550537109, 88.4888916015625),
        deviation_mz = c(0.0003662109375, 0.000457763671875, 0.0008544921875,
                         0.000701904296875, 0.0010986328125, 0.001251220703125,
                         0.00140380859375, 0.0017852783203125),
        npeak = c(1, 2, 2, 1, 1, 1, 2, 2),
        basepeak_int = c(88824.635233072, 6214416.44108707, 6201250.27168528,
                         288290.748778874, 4945601.93026269, 5689144.27927454,
                         1448292.29379181, 1662831.19267642),
        sum_int = c(88824.635233072, 7385056.39979801, 7388017.8361341,
                    288290.748778874, 4945601.93026269, 5689144.27927454,
                    1849363.52087931, 2106914.27998355),
        sample = c("220221CCM_lbl__02_lu_flrd", "220221CCM_lbl__01_lu_flrd",
                   "220221CCM_lbl__02_lu_flrd", "220221CCM_lbl__02_lu_flrd",
                   "220221CCM_lbl__01_lu_flrd", "220221CCM_lbl__02_lu_flrd",
                   "220221CCM_lbl__01_lu_flrd", "220221CCM_lbl__02_lu_flrd"),
        rt = c(286.278, 286.81, 286.807, 286.807, 197.973, 197.973, 197.444,
               197.973)
    )
    peaks <- data.frame(
        feature_id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13),
        mz = c(464.447304014051, 504.440032161331, 505.443534603,
               427.265348519813, 426.261913381751, 505.44383474773,
               464.447454557257, 504.44048100644, 429.270861378734,
               408.251325886321, 448.244170162955, 427.265704881008,
               426.262333104945),
        mzmin = c(464.447021484375, 504.439697265625, 505.443084716797,
                  427.264587402344, 426.261444091797, 505.440704345703,
                  464.446746826172, 504.439544677734, 429.270660400391,
                  408.251037597656, 448.242279052734, 427.265258789062,
                  426.262023925781),
        mzmax = c(464.447662353516, 504.440490722656, 505.443664550781,
                  427.266052246094, 426.262420654297, 505.444122314453,
                  464.447967529297, 504.441101074219, 429.271148681641,
                  408.25146484375, 448.245422363281, 427.266052246094,
                  426.262786865234),
        rt = c(197.973, 197.444, 197.444, 286.81, 286.81, 197.444, 197.973,
               197.973, 261.426, 286.278, 286.807, 286.807, 286.807),
        rtmin = c(196.386, 194.271, 196.386, 284.695, 284.695, 196.387,
                  195.858, 174.178, 258.253, 284.692, 282.048, 283.105,
                  284.692),
        rtmax = c(200.617, 199.03, 199.03, 291.04, 292.098, 200.617, 199.559,
                  208.548, 264.069, 287.864, 292.095, 292.095, 292.095),
        int = c(4945601.93026269, 1448292.29379181, 401071.227087501,
                1170639.95871094, 6214416.44108707, 444083.087307128,
                5689144.27927454, 1662831.19267642, 801559.236409095,
                88824.635233072, 288290.748778874, 1186767.56444882,
                6201250.27168528),
        intb = c(4945464.15722767, 1448264.72345856, 401069.111887501,
                 1170634.14246094, 6212404.90921921, 444001.578713656,
                 5689141.10698883, 1662613.35060178, 801553.420409095,
                 88821.9918997387, 287904.836179229, 1185632.54587715,
                 6201242.86868528),
        maxo = c(4130684, 1395886, 434808.75, 676902.5, 3501824, 524308,
                 5734716, 1723138, 150635.375, 70041.5625, 186253.875, 673140,
                 3489368),
        sn = c(8329, 22672, 434808, 676902, 3587, 4386, 5734715, 14423, 145000,
               70041, 675, 1134, 3489367),
        egauss = as.numeric(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                              NA)),
        mu = as.numeric(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)),
        sigma = as.numeric(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                             NA)),
        h = as.numeric(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)),
        f = c(1, 2, 3, 4, 6, 1, 2, 3, 4, 5, 6, 7, 8),
        dppm = c(1, 2, 1, 2, 1, 7, 2, 3, 1, 1, 2, 1, 1),
        scale = c(3, 3, 3, 5, 5, 3, 13, 19, 3, 3, 5, 5, 5),
        scpos = c(58, 57, 57, 187, 187, 57, 58, 58, 136, 183, 184, 184, 184),
        scmin = c(55, 54, 54, 182, 182, 54, 45, 39, 133, 180, 179, 179, 179),
        scmax = c(61, 60, 60, 192, 192, 60, 71, 77, 139, 186, 189, 189, 189),
        lmin = c(55, 52, 55, 87, 87, 55, 54, 20, 101, 87, 82, 84, 87),
        lmax = c(63, 60, 60, 99, 101, 63, 61, 77, 112, 93, 101, 101, 101),
        sample = c("220221CCM_lbl__01_lu_flrd", "220221CCM_lbl__01_lu_flrd",
                   "220221CCM_lbl__01_lu_flrd", "220221CCM_lbl__01_lu_flrd",
                   "220221CCM_lbl__01_lu_flrd", "220221CCM_lbl__02_lu_flrd",
                   "220221CCM_lbl__02_lu_flrd", "220221CCM_lbl__02_lu_flrd",
                   "220221CCM_lbl__02_lu_flrd", "220221CCM_lbl__02_lu_flrd",
                   "220221CCM_lbl__02_lu_flrd", "220221CCM_lbl__02_lu_flrd",
                   "220221CCM_lbl__02_lu_flrd"),
        polarity = c("positive", "positive", "positive", "positive",
                     "positive", "positive", "positive", "positive",
                     "positive", "positive", "positive", "positive",
                     "positive")
    )
    peak_groups <- data.frame(
        group_id = c(1, 2, 3, 4, 5, 6, 7, 8),
        polarity = c("positive", "positive", "positive", "positive",
                     "positive", "positive", "positive", "positive"),
        mzmed = c(408.251325886321, 426.262123243348, 427.265526700411,
                  429.270861378734, 448.244170162955, 464.447379285654,
                  504.440256583886, 505.443684675365),
        mzmin = c(408.251325886321, 426.261913381751, 427.265348519813,
                  429.270861378734, 448.244170162955, 464.447304014051,
                  504.440032161331, 505.443534603),
        mzmax = c(408.251325886321, 426.262333104945, 427.265704881008,
                  429.270861378734, 448.244170162955, 464.447454557257,
                  504.44048100644, 505.44383474773),
        rtmed = c(286.278, 286.8085, 286.8085, 261.426, 286.807, 197.973,
                  197.7085, 197.444),
        rtmin = c(286.278, 286.807, 286.807, 261.426, 286.807, 197.973,
                  197.444, 197.444),
        rtmax = c(286.278, 286.81, 286.81, 261.426, 286.807, 197.973, 197.973,
                  197.444),
        npeaks = c(1, 2, 2, 1, 1, 2, 2, 2),
        `220221CCM_lbl__01_lu_flrd` = c(NA, 5, 4, NA, NA, 1, 2, 3),
        `220221CCM_lbl__02_lu_flrd` = c(10, 13, 12, 9, 11, 7, 8, 6)
    )
    db <- db_connect(":memory:")
    db_record_ann(db, ann, spectras, spectra_infos, peaks, peak_groups)
    testthat::expect_identical(
        db_get_spectras(db),
        spectras
    )
    RSQLite::dbDisconnect(db)
})

testthat::test_that("get spectra infos", {
    ann <- data.frame(
        group_id = c(6, 7, 1, 2, 5),
        name = c("Cer (d18:1/C12:0)", "Cer (d18:1/C12:0)", "LPC 11:0",
                 "LPC 11:0", "LPC 11:0"),
        formula = c("C30H59N1O3", "C30H59N1O3", "C19H40N1O7P1", "C19H40N1O7P1",
                    "C19H40N1O7P1"),
        adduct = c("[M+H-H2O]+", "[M+Na]+", "[M+H-H2O]+", "[M+H]+", "[M+Na]+"),
        ion_formula = c("C30H58N1O2", "C30H59N1O3Na1", "C19H39N1O6P1",
                        "C19H41N1O7P1", "C19H40N1O7P1Na1"),
        rtdiff = c(2.37300000000002, 2.10850000000002, 9.52199999999993,
                   8.99149999999997, 8.99299999999994),
        rt = c(197.973, 197.7085, 286.278, 286.8085, 286.807),
        rtmin = c(196.122, 184.2245, 284.692, 284.6935, 282.048),
        rtmax = c(200.088, 203.789, 287.864, 292.0965, 292.095),
        nsamples = c(2, 2, 1, 2, 1),
        best_score = c(71.3979721069336, 89.4345550537109, 79.8211975097656,
                       95.1391906738281, 79.6432037353516),
        best_deviation_mz = c(0.0010986328125, 0.00140380859375,
                              0.0003662109375, 0.0008544921875,
                              0.000701904296875),
        best_npeak = c(1, 2, 1, 2, 1),
        `220221CCM_lbl__01_lu_flrd` = c(5, 7, NA, 2, NA),
        `220221CCM_lbl__02_lu_flrd` = c(6, 8, 1, 3, 4)
    )
    spectras <- data.frame(
        spectra_id = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5,
                       5, 6, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8),
        feature_id = c(10, NA, NA, NA, 4, NA, NA, 5, 12, NA, NA, 13, NA, NA,
                       NA, 11, 1, NA, NA, NA, 7, NA, NA, NA, 2, 3, NA, NA, 8,
                       6, NA, NA),
        mz = c(408.251325886321, NA, NA, NA, 427.265348519813, NA, NA,
               426.261913381751, 427.265704881008, NA, NA, 426.262333104945,
               NA, NA, NA, 448.244170162955, 464.447304014051, NA, NA, NA,
               464.447454557257, NA, NA, NA, 504.440032161331, 505.443534603,
               NA, NA, 504.44048100644, 505.44383474773, NA, NA),
        int = c(88824.635233072, NA, NA, NA, 1170639.95871094, NA, NA,
                6214416.44108707, 1186767.56444882, NA, NA, 6201250.27168528,
                NA, NA, NA, 288290.748778874, 4945601.93026269, NA, NA, NA,
                5689144.27927454, NA, NA, NA, 1448292.29379181,
                401071.227087501, NA, NA, 1662831.19267642, 444083.087307128,
                NA, NA),
        abd = c(100, NA, NA, NA, 18.8374881182917, NA, NA, 100,
                19.1375531135642, NA, NA, 100, NA, NA, NA, 100, 100, NA, NA,
                NA, 100, NA, NA, NA, 100, 27.6926991054717, NA, NA, 100,
                26.7064443620612, NA, NA),
        ion_id_theo = c(62, 62, 62, 62, 66, 66, 66, 66, 66, 66, 66, 66, 64, 64,
                        64, 64, 278, 278, 278, 278, 278, 278, 278, 278, 281,
                        281, 281, 281, 281, 281, 281, 281),
        mz_theo = c(408.25095, 409.25427, 410.25672, 411.25935, 427.26484,
                    428.26719, 429.26984, 426.26152, 427.26484, 428.26719,
                    429.26984, 426.26152, 449.24678, 450.24914, 451.25178,
                    448.24346, 464.44621, 465.44955, 466.45272, 467.45576,
                    464.44621, 465.44955, 466.45272, 467.45576, 504.43872,
                    505.44206, 506.44516, 507.44809, 504.43872, 505.44206,
                    506.44516, 507.44809),
        abd_theo = c(100, 21.65, 3.25, 0.38, 21.7, 3.46, 0.42, 100, 21.7, 3.46,
                     0.42, 100, 21.69, 3.45, 0.42, 100, 100, 33.55, 5.86, 0.65,
                     100, 33.55, 5.86, 0.65, 100, 33.67, 6.07, 0.72, 100,
                     33.67, 6.07, 0.72),
        iso_theo = c("M", "M+1", "M+2", "M+3", "M+1", "M+2", "M+3", "M", "M+1",
                     "M+2", "M+3", "M", "M+1", "M+2", "M+3", "M", "M", "M+1",
                     "M+2", "M+3", "M", "M+1", "M+2", "M+3", "M", "M+1", "M+2",
                     "M+3", "M", "M+1", "M+2", "M+3")
    )
    spectra_infos <- data.frame(
        spectra_id = c(1, 2, 3, 4, 5, 6, 7, 8),
        score = c(79.8211975097656, 94.9317855834961, 95.1391906738281,
                  79.6432037353516, 71.3979721069336, 71.3979721069336,
                  89.4345550537109, 88.4888916015625),
        deviation_mz = c(0.0003662109375, 0.000457763671875, 0.0008544921875,
                         0.000701904296875, 0.0010986328125, 0.001251220703125,
                         0.00140380859375, 0.0017852783203125),
        npeak = c(1, 2, 2, 1, 1, 1, 2, 2),
        basepeak_int = c(88824.635233072, 6214416.44108707, 6201250.27168528,
                         288290.748778874, 4945601.93026269, 5689144.27927454,
                         1448292.29379181, 1662831.19267642),
        sum_int = c(88824.635233072, 7385056.39979801, 7388017.8361341,
                    288290.748778874, 4945601.93026269, 5689144.27927454,
                    1849363.52087931, 2106914.27998355),
        sample = c("220221CCM_lbl__02_lu_flrd", "220221CCM_lbl__01_lu_flrd",
                   "220221CCM_lbl__02_lu_flrd", "220221CCM_lbl__02_lu_flrd",
                   "220221CCM_lbl__01_lu_flrd", "220221CCM_lbl__02_lu_flrd",
                   "220221CCM_lbl__01_lu_flrd", "220221CCM_lbl__02_lu_flrd"),
        rt = c(286.278, 286.81, 286.807, 286.807, 197.973, 197.973, 197.444,
               197.973)
    )
    peaks <- data.frame(
        feature_id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13),
        mz = c(464.447304014051, 504.440032161331, 505.443534603,
               427.265348519813, 426.261913381751, 505.44383474773,
               464.447454557257, 504.44048100644, 429.270861378734,
               408.251325886321, 448.244170162955, 427.265704881008,
               426.262333104945),
        mzmin = c(464.447021484375, 504.439697265625, 505.443084716797,
                  427.264587402344, 426.261444091797, 505.440704345703,
                  464.446746826172, 504.439544677734, 429.270660400391,
                  408.251037597656, 448.242279052734, 427.265258789062,
                  426.262023925781),
        mzmax = c(464.447662353516, 504.440490722656, 505.443664550781,
                  427.266052246094, 426.262420654297, 505.444122314453,
                  464.447967529297, 504.441101074219, 429.271148681641,
                  408.25146484375, 448.245422363281, 427.266052246094,
                  426.262786865234),
        rt = c(197.973, 197.444, 197.444, 286.81, 286.81, 197.444, 197.973,
               197.973, 261.426, 286.278, 286.807, 286.807, 286.807),
        rtmin = c(196.386, 194.271, 196.386, 284.695, 284.695, 196.387,
                  195.858, 174.178, 258.253, 284.692, 282.048, 283.105,
                  284.692),
        rtmax = c(200.617, 199.03, 199.03, 291.04, 292.098, 200.617, 199.559,
                  208.548, 264.069, 287.864, 292.095, 292.095, 292.095),
        int = c(4945601.93026269, 1448292.29379181, 401071.227087501,
                1170639.95871094, 6214416.44108707, 444083.087307128,
                5689144.27927454, 1662831.19267642, 801559.236409095,
                88824.635233072, 288290.748778874, 1186767.56444882,
                6201250.27168528),
        intb = c(4945464.15722767, 1448264.72345856, 401069.111887501,
                 1170634.14246094, 6212404.90921921, 444001.578713656,
                 5689141.10698883, 1662613.35060178, 801553.420409095,
                 88821.9918997387, 287904.836179229, 1185632.54587715,
                 6201242.86868528),
        maxo = c(4130684, 1395886, 434808.75, 676902.5, 3501824, 524308,
                 5734716, 1723138, 150635.375, 70041.5625, 186253.875, 673140,
                 3489368),
        sn = c(8329, 22672, 434808, 676902, 3587, 4386, 5734715, 14423, 145000,
               70041, 675, 1134, 3489367),
        egauss = as.numeric(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                              NA)),
        mu = as.numeric(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)),
        sigma = as.numeric(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                             NA)),
        h = as.numeric(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)),
        f = c(1, 2, 3, 4, 6, 1, 2, 3, 4, 5, 6, 7, 8),
        dppm = c(1, 2, 1, 2, 1, 7, 2, 3, 1, 1, 2, 1, 1),
        scale = c(3, 3, 3, 5, 5, 3, 13, 19, 3, 3, 5, 5, 5),
        scpos = c(58, 57, 57, 187, 187, 57, 58, 58, 136, 183, 184, 184, 184),
        scmin = c(55, 54, 54, 182, 182, 54, 45, 39, 133, 180, 179, 179, 179),
        scmax = c(61, 60, 60, 192, 192, 60, 71, 77, 139, 186, 189, 189, 189),
        lmin = c(55, 52, 55, 87, 87, 55, 54, 20, 101, 87, 82, 84, 87),
        lmax = c(63, 60, 60, 99, 101, 63, 61, 77, 112, 93, 101, 101, 101),
        sample = c("220221CCM_lbl__01_lu_flrd", "220221CCM_lbl__01_lu_flrd",
                   "220221CCM_lbl__01_lu_flrd", "220221CCM_lbl__01_lu_flrd",
                   "220221CCM_lbl__01_lu_flrd", "220221CCM_lbl__02_lu_flrd",
                   "220221CCM_lbl__02_lu_flrd", "220221CCM_lbl__02_lu_flrd",
                   "220221CCM_lbl__02_lu_flrd", "220221CCM_lbl__02_lu_flrd",
                   "220221CCM_lbl__02_lu_flrd", "220221CCM_lbl__02_lu_flrd",
                   "220221CCM_lbl__02_lu_flrd"),
        polarity = c("positive", "positive", "positive", "positive",
                     "positive", "positive", "positive", "positive",
                     "positive", "positive", "positive", "positive",
                     "positive")
    )
    peak_groups <- data.frame(
        group_id = c(1, 2, 3, 4, 5, 6, 7, 8),
        polarity = c("positive", "positive", "positive", "positive",
                     "positive", "positive", "positive", "positive"),
        mzmed = c(408.251325886321, 426.262123243348, 427.265526700411,
                  429.270861378734, 448.244170162955, 464.447379285654,
                  504.440256583886, 505.443684675365),
        mzmin = c(408.251325886321, 426.261913381751, 427.265348519813,
                  429.270861378734, 448.244170162955, 464.447304014051,
                  504.440032161331, 505.443534603),
        mzmax = c(408.251325886321, 426.262333104945, 427.265704881008,
                  429.270861378734, 448.244170162955, 464.447454557257,
                  504.44048100644, 505.44383474773),
        rtmed = c(286.278, 286.8085, 286.8085, 261.426, 286.807, 197.973,
                  197.7085, 197.444),
        rtmin = c(286.278, 286.807, 286.807, 261.426, 286.807, 197.973,
                  197.444, 197.444),
        rtmax = c(286.278, 286.81, 286.81, 261.426, 286.807, 197.973, 197.973,
                  197.444),
        npeaks = c(1, 2, 2, 1, 1, 2, 2, 2),
        `220221CCM_lbl__01_lu_flrd` = c(NA, 5, 4, NA, NA, 1, 2, 3),
        `220221CCM_lbl__02_lu_flrd` = c(10, 13, 12, 9, 11, 7, 8, 6)
    )
    db <- db_connect(":memory:")
    db_record_ann(db, ann, spectras, spectra_infos, peaks, peak_groups)
    testthat::expect_identical(
        db_get_spectra_infos(db),
        spectra_infos
    )
    RSQLite::dbDisconnect(db)
})

testthat::test_that("get ann", {
    ann <- data.frame(
        group_id = c(6, 7, 1, 2, 5),
        name = c("Cer (d18:1/C12:0)", "Cer (d18:1/C12:0)", "LPC 11:0",
                 "LPC 11:0", "LPC 11:0"),
        formula = c("C30H59N1O3", "C30H59N1O3", "C19H40N1O7P1", "C19H40N1O7P1",
                    "C19H40N1O7P1"),
        adduct = c("[M+H-H2O]+", "[M+Na]+", "[M+H-H2O]+", "[M+H]+", "[M+Na]+"),
        ion_formula = c("C30H58N1O2", "C30H59N1O3Na1", "C19H39N1O6P1",
                        "C19H41N1O7P1", "C19H40N1O7P1Na1"),
        rtdiff = c(2.37300000000002, 2.10850000000002, 9.52199999999993,
                   8.99149999999997, 8.99299999999994),
        rt = c(197.973, 197.7085, 286.278, 286.8085, 286.807),
        rtmin = c(196.122, 184.2245, 284.692, 284.6935, 282.048),
        rtmax = c(200.088, 203.789, 287.864, 292.0965, 292.095),
        nsamples = c(2, 2, 1, 2, 1),
        best_score = c(71.3979721069336, 89.4345550537109, 79.8211975097656,
                       95.1391906738281, 79.6432037353516),
        best_deviation_mz = c(0.0010986328125, 0.00140380859375,
                              0.0003662109375, 0.0008544921875,
                              0.000701904296875),
        best_npeak = c(1, 2, 1, 2, 1),
        `220221CCM_lbl__01_lu_flrd` = c(5, 7, NA, 2, NA),
        `220221CCM_lbl__02_lu_flrd` = c(6, 8, 1, 3, 4)
    )
    spectras <- data.frame(
        spectra_id = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5,
                       5, 6, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8),
        feature_id = c(10, NA, NA, NA, 4, NA, NA, 5, 12, NA, NA, 13, NA, NA,
                       NA, 11, 1, NA, NA, NA, 7, NA, NA, NA, 2, 3, NA, NA, 8,
                       6, NA, NA),
        mz = c(408.251325886321, NA, NA, NA, 427.265348519813, NA, NA,
               426.261913381751, 427.265704881008, NA, NA, 426.262333104945,
               NA, NA, NA, 448.244170162955, 464.447304014051, NA, NA, NA,
               464.447454557257, NA, NA, NA, 504.440032161331, 505.443534603,
               NA, NA, 504.44048100644, 505.44383474773, NA, NA),
        int = c(88824.635233072, NA, NA, NA, 1170639.95871094, NA, NA,
                6214416.44108707, 1186767.56444882, NA, NA, 6201250.27168528,
                NA, NA, NA, 288290.748778874, 4945601.93026269, NA, NA, NA,
                5689144.27927454, NA, NA, NA, 1448292.29379181,
                401071.227087501, NA, NA, 1662831.19267642, 444083.087307128,
                NA, NA),
        abd = c(100, NA, NA, NA, 18.8374881182917, NA, NA, 100,
                19.1375531135642, NA, NA, 100, NA, NA, NA, 100, 100, NA, NA,
                NA, 100, NA, NA, NA, 100, 27.6926991054717, NA, NA, 100,
                26.7064443620612, NA, NA),
        ion_id_theo = c(62, 62, 62, 62, 66, 66, 66, 66, 66, 66, 66, 66, 64, 64,
                        64, 64, 278, 278, 278, 278, 278, 278, 278, 278, 281,
                        281, 281, 281, 281, 281, 281, 281),
        mz_theo = c(408.25095, 409.25427, 410.25672, 411.25935, 427.26484,
                    428.26719, 429.26984, 426.26152, 427.26484, 428.26719,
                    429.26984, 426.26152, 449.24678, 450.24914, 451.25178,
                    448.24346, 464.44621, 465.44955, 466.45272, 467.45576,
                    464.44621, 465.44955, 466.45272, 467.45576, 504.43872,
                    505.44206, 506.44516, 507.44809, 504.43872, 505.44206,
                    506.44516, 507.44809),
        abd_theo = c(100, 21.65, 3.25, 0.38, 21.7, 3.46, 0.42, 100, 21.7, 3.46,
                     0.42, 100, 21.69, 3.45, 0.42, 100, 100, 33.55, 5.86, 0.65,
                     100, 33.55, 5.86, 0.65, 100, 33.67, 6.07, 0.72, 100,
                     33.67, 6.07, 0.72),
        iso_theo = c("M", "M+1", "M+2", "M+3", "M+1", "M+2", "M+3", "M", "M+1",
                     "M+2", "M+3", "M", "M+1", "M+2", "M+3", "M", "M", "M+1",
                     "M+2", "M+3", "M", "M+1", "M+2", "M+3", "M", "M+1", "M+2",
                     "M+3", "M", "M+1", "M+2", "M+3")
    )
    spectra_infos <- data.frame(
        spectra_id = c(1, 2, 3, 4, 5, 6, 7, 8),
        score = c(79.8211975097656, 94.9317855834961, 95.1391906738281,
                  79.6432037353516, 71.3979721069336, 71.3979721069336,
                  89.4345550537109, 88.4888916015625),
        deviation_mz = c(0.0003662109375, 0.000457763671875, 0.0008544921875,
                         0.000701904296875, 0.0010986328125, 0.001251220703125,
                         0.00140380859375, 0.0017852783203125),
        npeak = c(1, 2, 2, 1, 1, 1, 2, 2),
        basepeak_int = c(88824.635233072, 6214416.44108707, 6201250.27168528,
                         288290.748778874, 4945601.93026269, 5689144.27927454,
                         1448292.29379181, 1662831.19267642),
        sum_int = c(88824.635233072, 7385056.39979801, 7388017.8361341,
                    288290.748778874, 4945601.93026269, 5689144.27927454,
                    1849363.52087931, 2106914.27998355),
        sample = c("220221CCM_lbl__02_lu_flrd", "220221CCM_lbl__01_lu_flrd",
                   "220221CCM_lbl__02_lu_flrd", "220221CCM_lbl__02_lu_flrd",
                   "220221CCM_lbl__01_lu_flrd", "220221CCM_lbl__02_lu_flrd",
                   "220221CCM_lbl__01_lu_flrd", "220221CCM_lbl__02_lu_flrd"),
        rt = c(286.278, 286.81, 286.807, 286.807, 197.973, 197.973, 197.444,
               197.973)
    )
    peaks <- data.frame(
        feature_id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13),
        mz = c(464.447304014051, 504.440032161331, 505.443534603,
               427.265348519813, 426.261913381751, 505.44383474773,
               464.447454557257, 504.44048100644, 429.270861378734,
               408.251325886321, 448.244170162955, 427.265704881008,
               426.262333104945),
        mzmin = c(464.447021484375, 504.439697265625, 505.443084716797,
                  427.264587402344, 426.261444091797, 505.440704345703,
                  464.446746826172, 504.439544677734, 429.270660400391,
                  408.251037597656, 448.242279052734, 427.265258789062,
                  426.262023925781),
        mzmax = c(464.447662353516, 504.440490722656, 505.443664550781,
                  427.266052246094, 426.262420654297, 505.444122314453,
                  464.447967529297, 504.441101074219, 429.271148681641,
                  408.25146484375, 448.245422363281, 427.266052246094,
                  426.262786865234),
        rt = c(197.973, 197.444, 197.444, 286.81, 286.81, 197.444, 197.973,
               197.973, 261.426, 286.278, 286.807, 286.807, 286.807),
        rtmin = c(196.386, 194.271, 196.386, 284.695, 284.695, 196.387,
                  195.858, 174.178, 258.253, 284.692, 282.048, 283.105,
                  284.692),
        rtmax = c(200.617, 199.03, 199.03, 291.04, 292.098, 200.617, 199.559,
                  208.548, 264.069, 287.864, 292.095, 292.095, 292.095),
        int = c(4945601.93026269, 1448292.29379181, 401071.227087501,
                1170639.95871094, 6214416.44108707, 444083.087307128,
                5689144.27927454, 1662831.19267642, 801559.236409095,
                88824.635233072, 288290.748778874, 1186767.56444882,
                6201250.27168528),
        intb = c(4945464.15722767, 1448264.72345856, 401069.111887501,
                 1170634.14246094, 6212404.90921921, 444001.578713656,
                 5689141.10698883, 1662613.35060178, 801553.420409095,
                 88821.9918997387, 287904.836179229, 1185632.54587715,
                 6201242.86868528),
        maxo = c(4130684, 1395886, 434808.75, 676902.5, 3501824, 524308,
                 5734716, 1723138, 150635.375, 70041.5625, 186253.875, 673140,
                 3489368),
        sn = c(8329, 22672, 434808, 676902, 3587, 4386, 5734715, 14423, 145000,
               70041, 675, 1134, 3489367),
        egauss = as.numeric(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                              NA)),
        mu = as.numeric(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)),
        sigma = as.numeric(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                             NA)),
        h = as.numeric(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)),
        f = c(1, 2, 3, 4, 6, 1, 2, 3, 4, 5, 6, 7, 8),
        dppm = c(1, 2, 1, 2, 1, 7, 2, 3, 1, 1, 2, 1, 1),
        scale = c(3, 3, 3, 5, 5, 3, 13, 19, 3, 3, 5, 5, 5),
        scpos = c(58, 57, 57, 187, 187, 57, 58, 58, 136, 183, 184, 184, 184),
        scmin = c(55, 54, 54, 182, 182, 54, 45, 39, 133, 180, 179, 179, 179),
        scmax = c(61, 60, 60, 192, 192, 60, 71, 77, 139, 186, 189, 189, 189),
        lmin = c(55, 52, 55, 87, 87, 55, 54, 20, 101, 87, 82, 84, 87),
        lmax = c(63, 60, 60, 99, 101, 63, 61, 77, 112, 93, 101, 101, 101),
        sample = c("220221CCM_lbl__01_lu_flrd", "220221CCM_lbl__01_lu_flrd",
                   "220221CCM_lbl__01_lu_flrd", "220221CCM_lbl__01_lu_flrd",
                   "220221CCM_lbl__01_lu_flrd", "220221CCM_lbl__02_lu_flrd",
                   "220221CCM_lbl__02_lu_flrd", "220221CCM_lbl__02_lu_flrd",
                   "220221CCM_lbl__02_lu_flrd", "220221CCM_lbl__02_lu_flrd",
                   "220221CCM_lbl__02_lu_flrd", "220221CCM_lbl__02_lu_flrd",
                   "220221CCM_lbl__02_lu_flrd"),
        polarity = c("positive", "positive", "positive", "positive",
                     "positive", "positive", "positive", "positive",
                     "positive", "positive", "positive", "positive",
                     "positive")
    )
    peak_groups <- data.frame(
        group_id = c(1, 2, 3, 4, 5, 6, 7, 8),
        polarity = c("positive", "positive", "positive", "positive",
                     "positive", "positive", "positive", "positive"),
        mzmed = c(408.251325886321, 426.262123243348, 427.265526700411,
                  429.270861378734, 448.244170162955, 464.447379285654,
                  504.440256583886, 505.443684675365),
        mzmin = c(408.251325886321, 426.261913381751, 427.265348519813,
                  429.270861378734, 448.244170162955, 464.447304014051,
                  504.440032161331, 505.443534603),
        mzmax = c(408.251325886321, 426.262333104945, 427.265704881008,
                  429.270861378734, 448.244170162955, 464.447454557257,
                  504.44048100644, 505.44383474773),
        rtmed = c(286.278, 286.8085, 286.8085, 261.426, 286.807, 197.973,
                  197.7085, 197.444),
        rtmin = c(286.278, 286.807, 286.807, 261.426, 286.807, 197.973,
                  197.444, 197.444),
        rtmax = c(286.278, 286.81, 286.81, 261.426, 286.807, 197.973, 197.973,
                  197.444),
        npeaks = c(1, 2, 2, 1, 1, 2, 2, 2),
        `220221CCM_lbl__01_lu_flrd` = c(NA, 5, 4, NA, NA, 1, 2, 3),
        `220221CCM_lbl__02_lu_flrd` = c(10, 13, 12, 9, 11, 7, 8, 6)
    )
    db <- db_connect(":memory:")
    db_record_ann(db, ann, spectras, spectra_infos, peaks, peak_groups)
    testthat::expect_identical(
        db_get_ann(db),
        ann
    )
    RSQLite::dbDisconnect(db)
})

testthat::test_that("get spectra", {
    ann <- data.frame(
        group_id = c(6, 7, 1, 2, 5),
        name = c("Cer (d18:1/C12:0)", "Cer (d18:1/C12:0)", "LPC 11:0",
                 "LPC 11:0", "LPC 11:0"),
        formula = c("C30H59N1O3", "C30H59N1O3", "C19H40N1O7P1", "C19H40N1O7P1",
                    "C19H40N1O7P1"),
        adduct = c("[M+H-H2O]+", "[M+Na]+", "[M+H-H2O]+", "[M+H]+", "[M+Na]+"),
        ion_formula = c("C30H58N1O2", "C30H59N1O3Na1", "C19H39N1O6P1",
                        "C19H41N1O7P1", "C19H40N1O7P1Na1"),
        rtdiff = c(2.37300000000002, 2.10850000000002, 9.52199999999993,
                   8.99149999999997, 8.99299999999994),
        rt = c(197.973, 197.7085, 286.278, 286.8085, 286.807),
        rtmin = c(196.122, 184.2245, 284.692, 284.6935, 282.048),
        rtmax = c(200.088, 203.789, 287.864, 292.0965, 292.095),
        nsamples = c(2, 2, 1, 2, 1),
        best_score = c(71.3979721069336, 89.4345550537109, 79.8211975097656,
                       95.1391906738281, 79.6432037353516),
        best_deviation_mz = c(0.0010986328125, 0.00140380859375,
                              0.0003662109375, 0.0008544921875,
                              0.000701904296875),
        best_npeak = c(1, 2, 1, 2, 1),
        `220221CCM_lbl__01_lu_flrd` = c(5, 7, NA, 2, NA),
        `220221CCM_lbl__02_lu_flrd` = c(6, 8, 1, 3, 4)
    )
    spectras <- data.frame(
        spectra_id = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5,
                       5, 6, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8),
        feature_id = c(10, NA, NA, NA, 4, NA, NA, 5, 12, NA, NA, 13, NA, NA,
                       NA, 11, 1, NA, NA, NA, 7, NA, NA, NA, 2, 3, NA, NA, 8,
                       6, NA, NA),
        mz = c(408.251325886321, NA, NA, NA, 427.265348519813, NA, NA,
               426.261913381751, 427.265704881008, NA, NA, 426.262333104945,
               NA, NA, NA, 448.244170162955, 464.447304014051, NA, NA, NA,
               464.447454557257, NA, NA, NA, 504.440032161331, 505.443534603,
               NA, NA, 504.44048100644, 505.44383474773, NA, NA),
        int = c(88824.635233072, NA, NA, NA, 1170639.95871094, NA, NA,
                6214416.44108707, 1186767.56444882, NA, NA, 6201250.27168528,
                NA, NA, NA, 288290.748778874, 4945601.93026269, NA, NA, NA,
                5689144.27927454, NA, NA, NA, 1448292.29379181,
                401071.227087501, NA, NA, 1662831.19267642, 444083.087307128,
                NA, NA),
        abd = c(100, NA, NA, NA, 18.8374881182917, NA, NA, 100,
                19.1375531135642, NA, NA, 100, NA, NA, NA, 100, 100, NA, NA,
                NA, 100, NA, NA, NA, 100, 27.6926991054717, NA, NA, 100,
                26.7064443620612, NA, NA),
        ion_id_theo = c(62, 62, 62, 62, 66, 66, 66, 66, 66, 66, 66, 66, 64, 64,
                        64, 64, 278, 278, 278, 278, 278, 278, 278, 278, 281,
                        281, 281, 281, 281, 281, 281, 281),
        mz_theo = c(408.25095, 409.25427, 410.25672, 411.25935, 427.26484,
                    428.26719, 429.26984, 426.26152, 427.26484, 428.26719,
                    429.26984, 426.26152, 449.24678, 450.24914, 451.25178,
                    448.24346, 464.44621, 465.44955, 466.45272, 467.45576,
                    464.44621, 465.44955, 466.45272, 467.45576, 504.43872,
                    505.44206, 506.44516, 507.44809, 504.43872, 505.44206,
                    506.44516, 507.44809),
        abd_theo = c(100, 21.65, 3.25, 0.38, 21.7, 3.46, 0.42, 100, 21.7, 3.46,
                     0.42, 100, 21.69, 3.45, 0.42, 100, 100, 33.55, 5.86, 0.65,
                     100, 33.55, 5.86, 0.65, 100, 33.67, 6.07, 0.72, 100,
                     33.67, 6.07, 0.72),
        iso_theo = c("M", "M+1", "M+2", "M+3", "M+1", "M+2", "M+3", "M", "M+1",
                     "M+2", "M+3", "M", "M+1", "M+2", "M+3", "M", "M", "M+1",
                     "M+2", "M+3", "M", "M+1", "M+2", "M+3", "M", "M+1", "M+2",
                     "M+3", "M", "M+1", "M+2", "M+3")
    )
    spectra_infos <- data.frame(
        spectra_id = c(1, 2, 3, 4, 5, 6, 7, 8),
        score = c(79.8211975097656, 94.9317855834961, 95.1391906738281,
                  79.6432037353516, 71.3979721069336, 71.3979721069336,
                  89.4345550537109, 88.4888916015625),
        deviation_mz = c(0.0003662109375, 0.000457763671875, 0.0008544921875,
                         0.000701904296875, 0.0010986328125, 0.001251220703125,
                         0.00140380859375, 0.0017852783203125),
        npeak = c(1, 2, 2, 1, 1, 1, 2, 2),
        basepeak_int = c(88824.635233072, 6214416.44108707, 6201250.27168528,
                         288290.748778874, 4945601.93026269, 5689144.27927454,
                         1448292.29379181, 1662831.19267642),
        sum_int = c(88824.635233072, 7385056.39979801, 7388017.8361341,
                    288290.748778874, 4945601.93026269, 5689144.27927454,
                    1849363.52087931, 2106914.27998355),
        sample = c("220221CCM_lbl__02_lu_flrd", "220221CCM_lbl__01_lu_flrd",
                   "220221CCM_lbl__02_lu_flrd", "220221CCM_lbl__02_lu_flrd",
                   "220221CCM_lbl__01_lu_flrd", "220221CCM_lbl__02_lu_flrd",
                   "220221CCM_lbl__01_lu_flrd", "220221CCM_lbl__02_lu_flrd"),
        rt = c(286.278, 286.81, 286.807, 286.807, 197.973, 197.973, 197.444,
               197.973)
    )
    peaks <- data.frame(
        feature_id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13),
        mz = c(464.447304014051, 504.440032161331, 505.443534603,
               427.265348519813, 426.261913381751, 505.44383474773,
               464.447454557257, 504.44048100644, 429.270861378734,
               408.251325886321, 448.244170162955, 427.265704881008,
               426.262333104945),
        mzmin = c(464.447021484375, 504.439697265625, 505.443084716797,
                  427.264587402344, 426.261444091797, 505.440704345703,
                  464.446746826172, 504.439544677734, 429.270660400391,
                  408.251037597656, 448.242279052734, 427.265258789062,
                  426.262023925781),
        mzmax = c(464.447662353516, 504.440490722656, 505.443664550781,
                  427.266052246094, 426.262420654297, 505.444122314453,
                  464.447967529297, 504.441101074219, 429.271148681641,
                  408.25146484375, 448.245422363281, 427.266052246094,
                  426.262786865234),
        rt = c(197.973, 197.444, 197.444, 286.81, 286.81, 197.444, 197.973,
               197.973, 261.426, 286.278, 286.807, 286.807, 286.807),
        rtmin = c(196.386, 194.271, 196.386, 284.695, 284.695, 196.387,
                  195.858, 174.178, 258.253, 284.692, 282.048, 283.105,
                  284.692),
        rtmax = c(200.617, 199.03, 199.03, 291.04, 292.098, 200.617, 199.559,
                  208.548, 264.069, 287.864, 292.095, 292.095, 292.095),
        int = c(4945601.93026269, 1448292.29379181, 401071.227087501,
                1170639.95871094, 6214416.44108707, 444083.087307128,
                5689144.27927454, 1662831.19267642, 801559.236409095,
                88824.635233072, 288290.748778874, 1186767.56444882,
                6201250.27168528),
        intb = c(4945464.15722767, 1448264.72345856, 401069.111887501,
                 1170634.14246094, 6212404.90921921, 444001.578713656,
                 5689141.10698883, 1662613.35060178, 801553.420409095,
                 88821.9918997387, 287904.836179229, 1185632.54587715,
                 6201242.86868528),
        maxo = c(4130684, 1395886, 434808.75, 676902.5, 3501824, 524308,
                 5734716, 1723138, 150635.375, 70041.5625, 186253.875, 673140,
                 3489368),
        sn = c(8329, 22672, 434808, 676902, 3587, 4386, 5734715, 14423, 145000,
               70041, 675, 1134, 3489367),
        egauss = as.numeric(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                              NA)),
        mu = as.numeric(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)),
        sigma = as.numeric(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                             NA)),
        h = as.numeric(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)),
        f = c(1, 2, 3, 4, 6, 1, 2, 3, 4, 5, 6, 7, 8),
        dppm = c(1, 2, 1, 2, 1, 7, 2, 3, 1, 1, 2, 1, 1),
        scale = c(3, 3, 3, 5, 5, 3, 13, 19, 3, 3, 5, 5, 5),
        scpos = c(58, 57, 57, 187, 187, 57, 58, 58, 136, 183, 184, 184, 184),
        scmin = c(55, 54, 54, 182, 182, 54, 45, 39, 133, 180, 179, 179, 179),
        scmax = c(61, 60, 60, 192, 192, 60, 71, 77, 139, 186, 189, 189, 189),
        lmin = c(55, 52, 55, 87, 87, 55, 54, 20, 101, 87, 82, 84, 87),
        lmax = c(63, 60, 60, 99, 101, 63, 61, 77, 112, 93, 101, 101, 101),
        sample = c("220221CCM_lbl__01_lu_flrd", "220221CCM_lbl__01_lu_flrd",
                   "220221CCM_lbl__01_lu_flrd", "220221CCM_lbl__01_lu_flrd",
                   "220221CCM_lbl__01_lu_flrd", "220221CCM_lbl__02_lu_flrd",
                   "220221CCM_lbl__02_lu_flrd", "220221CCM_lbl__02_lu_flrd",
                   "220221CCM_lbl__02_lu_flrd", "220221CCM_lbl__02_lu_flrd",
                   "220221CCM_lbl__02_lu_flrd", "220221CCM_lbl__02_lu_flrd",
                   "220221CCM_lbl__02_lu_flrd"),
        polarity = c("positive", "positive", "positive", "positive",
                     "positive", "positive", "positive", "positive",
                     "positive", "positive", "positive", "positive",
                     "positive")
    )
    peak_groups <- data.frame(
        group_id = c(1, 2, 3, 4, 5, 6, 7, 8),
        polarity = c("positive", "positive", "positive", "positive",
                     "positive", "positive", "positive", "positive"),
        mzmed = c(408.251325886321, 426.262123243348, 427.265526700411,
                  429.270861378734, 448.244170162955, 464.447379285654,
                  504.440256583886, 505.443684675365),
        mzmin = c(408.251325886321, 426.261913381751, 427.265348519813,
                  429.270861378734, 448.244170162955, 464.447304014051,
                  504.440032161331, 505.443534603),
        mzmax = c(408.251325886321, 426.262333104945, 427.265704881008,
                  429.270861378734, 448.244170162955, 464.447454557257,
                  504.44048100644, 505.44383474773),
        rtmed = c(286.278, 286.8085, 286.8085, 261.426, 286.807, 197.973,
                  197.7085, 197.444),
        rtmin = c(286.278, 286.807, 286.807, 261.426, 286.807, 197.973,
                  197.444, 197.444),
        rtmax = c(286.278, 286.81, 286.81, 261.426, 286.807, 197.973, 197.973,
                  197.444),
        npeaks = c(1, 2, 2, 1, 1, 2, 2, 2),
        `220221CCM_lbl__01_lu_flrd` = c(NA, 5, 4, NA, NA, 1, 2, 3),
        `220221CCM_lbl__02_lu_flrd` = c(10, 13, 12, 9, 11, 7, 8, 6)
    )
    db <- db_connect(":memory:")
    db_record_ann(db, ann, spectras, spectra_infos, peaks, peak_groups)
    testthat::expect_identical(
        db_get_spectra(db, 1),
        spectras[spectras$spectra_id == 1, ]
    )
    RSQLite::dbDisconnect(db)
})

testthat::test_that("get params", {
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
        adduct_names = c(
            "[M+Na]+",
            "[M+NH4]+",
            "[M+H-H2O]+",
            "[M+H]+",
            "[M-H]-"
        ),
        instrument = "QTOF_XevoG2-S_R25000@200"
    )
    db <- db_connect(":memory:")
    db_record_params(
        db,
        filter_params,
        cwt_params,
        obw_params,
        pd_params,
        ann_params
    )
    testthat::expect_identical(
        db_get_params(db),
        list(
            filter = params_to_dataframe(filter_params),
            cwt = params_to_dataframe(cwt_params),
            obw = params_to_dataframe(obw_params),
            pd = params_to_dataframe(pd_params),
            ann = params_to_dataframe(ann_params)
        )
    )
    RSQLite::dbDisconnect(db)
})

testthat::test_that("get peaks", {
    ann <- data.frame(
        group_id = c(6, 7, 1, 2, 5),
        name = c("Cer (d18:1/C12:0)", "Cer (d18:1/C12:0)", "LPC 11:0",
                 "LPC 11:0", "LPC 11:0"),
        formula = c("C30H59N1O3", "C30H59N1O3", "C19H40N1O7P1", "C19H40N1O7P1",
                    "C19H40N1O7P1"),
        adduct = c("[M+H-H2O]+", "[M+Na]+", "[M+H-H2O]+", "[M+H]+", "[M+Na]+"),
        ion_formula = c("C30H58N1O2", "C30H59N1O3Na1", "C19H39N1O6P1",
                        "C19H41N1O7P1", "C19H40N1O7P1Na1"),
        rtdiff = c(2.37300000000002, 2.10850000000002, 9.52199999999993,
                   8.99149999999997, 8.99299999999994),
        rt = c(197.973, 197.7085, 286.278, 286.8085, 286.807),
        rtmin = c(196.122, 184.2245, 284.692, 284.6935, 282.048),
        rtmax = c(200.088, 203.789, 287.864, 292.0965, 292.095),
        nsamples = c(2, 2, 1, 2, 1),
        best_score = c(71.3979721069336, 89.4345550537109, 79.8211975097656,
                       95.1391906738281, 79.6432037353516),
        best_deviation_mz = c(0.0010986328125, 0.00140380859375,
                              0.0003662109375, 0.0008544921875,
                              0.000701904296875),
        best_npeak = c(1, 2, 1, 2, 1),
        `220221CCM_lbl__01_lu_flrd` = c(5, 7, NA, 2, NA),
        `220221CCM_lbl__02_lu_flrd` = c(6, 8, 1, 3, 4)
    )
    spectras <- data.frame(
        spectra_id = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5,
                       5, 6, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8),
        feature_id = c(10, NA, NA, NA, 4, NA, NA, 5, 12, NA, NA, 13, NA, NA,
                       NA, 11, 1, NA, NA, NA, 7, NA, NA, NA, 2, 3, NA, NA, 8,
                       6, NA, NA),
        mz = c(408.251325886321, NA, NA, NA, 427.265348519813, NA, NA,
               426.261913381751, 427.265704881008, NA, NA, 426.262333104945,
               NA, NA, NA, 448.244170162955, 464.447304014051, NA, NA, NA,
               464.447454557257, NA, NA, NA, 504.440032161331, 505.443534603,
               NA, NA, 504.44048100644, 505.44383474773, NA, NA),
        int = c(88824.635233072, NA, NA, NA, 1170639.95871094, NA, NA,
                6214416.44108707, 1186767.56444882, NA, NA, 6201250.27168528,
                NA, NA, NA, 288290.748778874, 4945601.93026269, NA, NA, NA,
                5689144.27927454, NA, NA, NA, 1448292.29379181,
                401071.227087501, NA, NA, 1662831.19267642, 444083.087307128,
                NA, NA),
        abd = c(100, NA, NA, NA, 18.8374881182917, NA, NA, 100,
                19.1375531135642, NA, NA, 100, NA, NA, NA, 100, 100, NA, NA,
                NA, 100, NA, NA, NA, 100, 27.6926991054717, NA, NA, 100,
                26.7064443620612, NA, NA),
        ion_id_theo = c(62, 62, 62, 62, 66, 66, 66, 66, 66, 66, 66, 66, 64, 64,
                        64, 64, 278, 278, 278, 278, 278, 278, 278, 278, 281,
                        281, 281, 281, 281, 281, 281, 281),
        mz_theo = c(408.25095, 409.25427, 410.25672, 411.25935, 427.26484,
                    428.26719, 429.26984, 426.26152, 427.26484, 428.26719,
                    429.26984, 426.26152, 449.24678, 450.24914, 451.25178,
                    448.24346, 464.44621, 465.44955, 466.45272, 467.45576,
                    464.44621, 465.44955, 466.45272, 467.45576, 504.43872,
                    505.44206, 506.44516, 507.44809, 504.43872, 505.44206,
                    506.44516, 507.44809),
        abd_theo = c(100, 21.65, 3.25, 0.38, 21.7, 3.46, 0.42, 100, 21.7, 3.46,
                     0.42, 100, 21.69, 3.45, 0.42, 100, 100, 33.55, 5.86, 0.65,
                     100, 33.55, 5.86, 0.65, 100, 33.67, 6.07, 0.72, 100,
                     33.67, 6.07, 0.72),
        iso_theo = c("M", "M+1", "M+2", "M+3", "M+1", "M+2", "M+3", "M", "M+1",
                     "M+2", "M+3", "M", "M+1", "M+2", "M+3", "M", "M", "M+1",
                     "M+2", "M+3", "M", "M+1", "M+2", "M+3", "M", "M+1", "M+2",
                     "M+3", "M", "M+1", "M+2", "M+3")
    )
    spectra_infos <- data.frame(
        spectra_id = c(1, 2, 3, 4, 5, 6, 7, 8),
        score = c(79.8211975097656, 94.9317855834961, 95.1391906738281,
                  79.6432037353516, 71.3979721069336, 71.3979721069336,
                  89.4345550537109, 88.4888916015625),
        deviation_mz = c(0.0003662109375, 0.000457763671875, 0.0008544921875,
                         0.000701904296875, 0.0010986328125, 0.001251220703125,
                         0.00140380859375, 0.0017852783203125),
        npeak = c(1, 2, 2, 1, 1, 1, 2, 2),
        basepeak_int = c(88824.635233072, 6214416.44108707, 6201250.27168528,
                         288290.748778874, 4945601.93026269, 5689144.27927454,
                         1448292.29379181, 1662831.19267642),
        sum_int = c(88824.635233072, 7385056.39979801, 7388017.8361341,
                    288290.748778874, 4945601.93026269, 5689144.27927454,
                    1849363.52087931, 2106914.27998355),
        sample = c("220221CCM_lbl__02_lu_flrd", "220221CCM_lbl__01_lu_flrd",
                   "220221CCM_lbl__02_lu_flrd", "220221CCM_lbl__02_lu_flrd",
                   "220221CCM_lbl__01_lu_flrd", "220221CCM_lbl__02_lu_flrd",
                   "220221CCM_lbl__01_lu_flrd", "220221CCM_lbl__02_lu_flrd"),
        rt = c(286.278, 286.81, 286.807, 286.807, 197.973, 197.973, 197.444,
               197.973)
    )
    peaks <- data.frame(
        feature_id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13),
        mz = c(464.447304014051, 504.440032161331, 505.443534603,
               427.265348519813, 426.261913381751, 505.44383474773,
               464.447454557257, 504.44048100644, 429.270861378734,
               408.251325886321, 448.244170162955, 427.265704881008,
               426.262333104945),
        mzmin = c(464.447021484375, 504.439697265625, 505.443084716797,
                  427.264587402344, 426.261444091797, 505.440704345703,
                  464.446746826172, 504.439544677734, 429.270660400391,
                  408.251037597656, 448.242279052734, 427.265258789062,
                  426.262023925781),
        mzmax = c(464.447662353516, 504.440490722656, 505.443664550781,
                  427.266052246094, 426.262420654297, 505.444122314453,
                  464.447967529297, 504.441101074219, 429.271148681641,
                  408.25146484375, 448.245422363281, 427.266052246094,
                  426.262786865234),
        rt = c(197.973, 197.444, 197.444, 286.81, 286.81, 197.444, 197.973,
               197.973, 261.426, 286.278, 286.807, 286.807, 286.807),
        rtmin = c(196.386, 194.271, 196.386, 284.695, 284.695, 196.387,
                  195.858, 174.178, 258.253, 284.692, 282.048, 283.105,
                  284.692),
        rtmax = c(200.617, 199.03, 199.03, 291.04, 292.098, 200.617, 199.559,
                  208.548, 264.069, 287.864, 292.095, 292.095, 292.095),
        int = c(4945601.93026269, 1448292.29379181, 401071.227087501,
                1170639.95871094, 6214416.44108707, 444083.087307128,
                5689144.27927454, 1662831.19267642, 801559.236409095,
                88824.635233072, 288290.748778874, 1186767.56444882,
                6201250.27168528),
        intb = c(4945464.15722767, 1448264.72345856, 401069.111887501,
                 1170634.14246094, 6212404.90921921, 444001.578713656,
                 5689141.10698883, 1662613.35060178, 801553.420409095,
                 88821.9918997387, 287904.836179229, 1185632.54587715,
                 6201242.86868528),
        maxo = c(4130684, 1395886, 434808.75, 676902.5, 3501824, 524308,
                 5734716, 1723138, 150635.375, 70041.5625, 186253.875, 673140,
                 3489368),
        sn = c(8329, 22672, 434808, 676902, 3587, 4386, 5734715, 14423, 145000,
               70041, 675, 1134, 3489367),
        egauss = as.numeric(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                              NA)),
        mu = as.numeric(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)),
        sigma = as.numeric(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                             NA)),
        h = as.numeric(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)),
        f = c(1, 2, 3, 4, 6, 1, 2, 3, 4, 5, 6, 7, 8),
        dppm = c(1, 2, 1, 2, 1, 7, 2, 3, 1, 1, 2, 1, 1),
        scale = c(3, 3, 3, 5, 5, 3, 13, 19, 3, 3, 5, 5, 5),
        scpos = c(58, 57, 57, 187, 187, 57, 58, 58, 136, 183, 184, 184, 184),
        scmin = c(55, 54, 54, 182, 182, 54, 45, 39, 133, 180, 179, 179, 179),
        scmax = c(61, 60, 60, 192, 192, 60, 71, 77, 139, 186, 189, 189, 189),
        lmin = c(55, 52, 55, 87, 87, 55, 54, 20, 101, 87, 82, 84, 87),
        lmax = c(63, 60, 60, 99, 101, 63, 61, 77, 112, 93, 101, 101, 101),
        sample = c("220221CCM_lbl__01_lu_flrd", "220221CCM_lbl__01_lu_flrd",
                   "220221CCM_lbl__01_lu_flrd", "220221CCM_lbl__01_lu_flrd",
                   "220221CCM_lbl__01_lu_flrd", "220221CCM_lbl__02_lu_flrd",
                   "220221CCM_lbl__02_lu_flrd", "220221CCM_lbl__02_lu_flrd",
                   "220221CCM_lbl__02_lu_flrd", "220221CCM_lbl__02_lu_flrd",
                   "220221CCM_lbl__02_lu_flrd", "220221CCM_lbl__02_lu_flrd",
                   "220221CCM_lbl__02_lu_flrd"),
        polarity = c("positive", "positive", "positive", "positive",
                     "positive", "positive", "positive", "positive",
                     "positive", "positive", "positive", "positive",
                     "positive")
    )
    peak_groups <- data.frame(
        group_id = c(1, 2, 3, 4, 5, 6, 7, 8),
        polarity = c("positive", "positive", "positive", "positive",
                     "positive", "positive", "positive", "positive"),
        mzmed = c(408.251325886321, 426.262123243348, 427.265526700411,
                  429.270861378734, 448.244170162955, 464.447379285654,
                  504.440256583886, 505.443684675365),
        mzmin = c(408.251325886321, 426.261913381751, 427.265348519813,
                  429.270861378734, 448.244170162955, 464.447304014051,
                  504.440032161331, 505.443534603),
        mzmax = c(408.251325886321, 426.262333104945, 427.265704881008,
                  429.270861378734, 448.244170162955, 464.447454557257,
                  504.44048100644, 505.44383474773),
        rtmed = c(286.278, 286.8085, 286.8085, 261.426, 286.807, 197.973,
                  197.7085, 197.444),
        rtmin = c(286.278, 286.807, 286.807, 261.426, 286.807, 197.973,
                  197.444, 197.444),
        rtmax = c(286.278, 286.81, 286.81, 261.426, 286.807, 197.973, 197.973,
                  197.444),
        npeaks = c(1, 2, 2, 1, 1, 2, 2, 2),
        `220221CCM_lbl__01_lu_flrd` = c(NA, 5, 4, NA, NA, 1, 2, 3),
        `220221CCM_lbl__02_lu_flrd` = c(10, 13, 12, 9, 11, 7, 8, 6)
    )
    db <- db_connect(":memory:")
    db_record_ann(db, ann, spectras, spectra_infos, peaks, peak_groups)
    testthat::expect_identical(
        db_get_peaks(db),
        peaks
    )
    testthat::expect_identical(
        db_get_peaks(db, c(1, 2)),
        peaks[1:2, ]
    )
    RSQLite::dbDisconnect(db)
})

testthat::test_that("resolve conflict", {
    ann <- data.frame(
        group_id = c(6, 6, 7, 1, 2, 5),
        name = c("Cer (d18:1/C12:0)", "Cer (d18:2/C12:0)", "Cer (d18:1/C12:0)",
                 "LPC 11:0", "LPC 11:0", "LPC 11:0"),
        formula = c("C30H59N1O3", "C30H59N1O3", "C30H59N1O3", "C19H40N1O7P1",
                    "C19H40N1O7P1", "C19H40N1O7P1"),
        adduct = c("[M+H-H2O]+", "[M+H-H2O]+", "[M+Na]+", "[M+H-H2O]+",
                   "[M+H]+", "[M+Na]+"),
        ion_formula = c("C30H58N1O2", "C30H58N1O2", "C30H59N1O3Na1",
                        "C19H39N1O6P1", "C19H41N1O7P1", "C19H40N1O7P1Na1"),
        rtdiff = c(2.37300000000002, 2.37300000000002, 2.10850000000002,
                   9.52199999999993, 8.99149999999997, 8.99299999999994),
        rt = c(197.973, 197.973, 197.7085, 286.278, 286.8085, 286.807),
        rtmin = c(196.122, 196.122, 184.2245, 284.692, 284.6935, 282.048),
        rtmax = c(200.088, 200.088, 203.789, 287.864, 292.0965, 292.095),
        nsamples = c(2, 2, 2, 1, 2, 1),
        best_score = c(71.3979721069336, 71.3979721069336, 89.4345550537109,
                       79.8211975097656, 95.1391906738281, 79.6432037353516),
        best_deviation_mz = c(0.0010986328125, 0.0010986328125,
                              0.00140380859375, 0.0003662109375,
                              0.0008544921875, 0.000701904296875),
        best_npeak = c(1, 1, 2, 1, 2, 1),
        `220221CCM_lbl__01_lu_flrd` = c(5, 5, 7, NA, 2, NA),
        `220221CCM_lbl__02_lu_flrd` = c(6, 6, 8, 1, 3, 4)
    )
    spectras <- data.frame(
        spectra_id = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5,
                       5, 6, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8),
        feature_id = c(10, NA, NA, NA, 4, NA, NA, 5, 12, NA, NA, 13, NA, NA,
                       NA, 11, 1, NA, NA, NA, 7, NA, NA, NA, 2, 3, NA, NA, 8,
                       6, NA, NA),
        mz = c(408.251325886321, NA, NA, NA, 427.265348519813, NA, NA,
               426.261913381751, 427.265704881008, NA, NA, 426.262333104945,
               NA, NA, NA, 448.244170162955, 464.447304014051, NA, NA, NA,
               464.447454557257, NA, NA, NA, 504.440032161331, 505.443534603,
               NA, NA, 504.44048100644, 505.44383474773, NA, NA),
        int = c(88824.635233072, NA, NA, NA, 1170639.95871094, NA, NA,
                6214416.44108707, 1186767.56444882, NA, NA, 6201250.27168528,
                NA, NA, NA, 288290.748778874, 4945601.93026269, NA, NA, NA,
                5689144.27927454, NA, NA, NA, 1448292.29379181,
                401071.227087501, NA, NA, 1662831.19267642, 444083.087307128,
                NA, NA),
        abd = c(100, NA, NA, NA, 18.8374881182917, NA, NA, 100,
                19.1375531135642, NA, NA, 100, NA, NA, NA, 100, 100, NA, NA,
                NA, 100, NA, NA, NA, 100, 27.6926991054717, NA, NA, 100,
                26.7064443620612, NA, NA),
        ion_id_theo = c(62, 62, 62, 62, 66, 66, 66, 66, 66, 66, 66, 66, 64, 64,
                        64, 64, 278, 278, 278, 278, 278, 278, 278, 278, 281,
                        281, 281, 281, 281, 281, 281, 281),
        mz_theo = c(408.25095, 409.25427, 410.25672, 411.25935, 427.26484,
                    428.26719, 429.26984, 426.26152, 427.26484, 428.26719,
                    429.26984, 426.26152, 449.24678, 450.24914, 451.25178,
                    448.24346, 464.44621, 465.44955, 466.45272, 467.45576,
                    464.44621, 465.44955, 466.45272, 467.45576, 504.43872,
                    505.44206, 506.44516, 507.44809, 504.43872, 505.44206,
                    506.44516, 507.44809),
        abd_theo = c(100, 21.65, 3.25, 0.38, 21.7, 3.46, 0.42, 100, 21.7, 3.46,
                     0.42, 100, 21.69, 3.45, 0.42, 100, 100, 33.55, 5.86, 0.65,
                     100, 33.55, 5.86, 0.65, 100, 33.67, 6.07, 0.72, 100,
                     33.67, 6.07, 0.72),
        iso_theo = c("M", "M+1", "M+2", "M+3", "M+1", "M+2", "M+3", "M", "M+1",
                     "M+2", "M+3", "M", "M+1", "M+2", "M+3", "M", "M", "M+1",
                     "M+2", "M+3", "M", "M+1", "M+2", "M+3", "M", "M+1", "M+2",
                     "M+3", "M", "M+1", "M+2", "M+3")
    )
    spectra_infos <- data.frame(
        spectra_id = c(1, 2, 3, 4, 5, 6, 7, 8),
        score = c(79.8211975097656, 94.9317855834961, 95.1391906738281,
                  79.6432037353516, 71.3979721069336, 71.3979721069336,
                  89.4345550537109, 88.4888916015625),
        deviation_mz = c(0.0003662109375, 0.000457763671875, 0.0008544921875,
                         0.000701904296875, 0.0010986328125, 0.001251220703125,
                         0.00140380859375, 0.0017852783203125),
        npeak = c(1, 2, 2, 1, 1, 1, 2, 2),
        basepeak_int = c(88824.635233072, 6214416.44108707, 6201250.27168528,
                         288290.748778874, 4945601.93026269, 5689144.27927454,
                         1448292.29379181, 1662831.19267642),
        sum_int = c(88824.635233072, 7385056.39979801, 7388017.8361341,
                    288290.748778874, 4945601.93026269, 5689144.27927454,
                    1849363.52087931, 2106914.27998355),
        sample = c("220221CCM_lbl__02_lu_flrd", "220221CCM_lbl__01_lu_flrd",
                   "220221CCM_lbl__02_lu_flrd", "220221CCM_lbl__02_lu_flrd",
                   "220221CCM_lbl__01_lu_flrd", "220221CCM_lbl__02_lu_flrd",
                   "220221CCM_lbl__01_lu_flrd", "220221CCM_lbl__02_lu_flrd"),
        rt = c(286.278, 286.81, 286.807, 286.807, 197.973, 197.973, 197.444,
               197.973)
    )
    peaks <- data.frame(
        feature_id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13),
        mz = c(464.447304014051, 504.440032161331, 505.443534603,
               427.265348519813, 426.261913381751, 505.44383474773,
               464.447454557257, 504.44048100644, 429.270861378734,
               408.251325886321, 448.244170162955, 427.265704881008,
               426.262333104945),
        mzmin = c(464.447021484375, 504.439697265625, 505.443084716797,
                  427.264587402344, 426.261444091797, 505.440704345703,
                  464.446746826172, 504.439544677734, 429.270660400391,
                  408.251037597656, 448.242279052734, 427.265258789062,
                  426.262023925781),
        mzmax = c(464.447662353516, 504.440490722656, 505.443664550781,
                  427.266052246094, 426.262420654297, 505.444122314453,
                  464.447967529297, 504.441101074219, 429.271148681641,
                  408.25146484375, 448.245422363281, 427.266052246094,
                  426.262786865234),
        rt = c(197.973, 197.444, 197.444, 286.81, 286.81, 197.444, 197.973,
               197.973, 261.426, 286.278, 286.807, 286.807, 286.807),
        rtmin = c(196.386, 194.271, 196.386, 284.695, 284.695, 196.387,
                  195.858, 174.178, 258.253, 284.692, 282.048, 283.105,
                  284.692),
        rtmax = c(200.617, 199.03, 199.03, 291.04, 292.098, 200.617, 199.559,
                  208.548, 264.069, 287.864, 292.095, 292.095, 292.095),
        int = c(4945601.93026269, 1448292.29379181, 401071.227087501,
                1170639.95871094, 6214416.44108707, 444083.087307128,
                5689144.27927454, 1662831.19267642, 801559.236409095,
                88824.635233072, 288290.748778874, 1186767.56444882,
                6201250.27168528),
        intb = c(4945464.15722767, 1448264.72345856, 401069.111887501,
                 1170634.14246094, 6212404.90921921, 444001.578713656,
                 5689141.10698883, 1662613.35060178, 801553.420409095,
                 88821.9918997387, 287904.836179229, 1185632.54587715,
                 6201242.86868528),
        maxo = c(4130684, 1395886, 434808.75, 676902.5, 3501824, 524308,
                 5734716, 1723138, 150635.375, 70041.5625, 186253.875, 673140,
                 3489368),
        sn = c(8329, 22672, 434808, 676902, 3587, 4386, 5734715, 14423, 145000,
               70041, 675, 1134, 3489367),
        egauss = as.numeric(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                              NA)),
        mu = as.numeric(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)),
        sigma = as.numeric(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                             NA)),
        h = as.numeric(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)),
        f = c(1, 2, 3, 4, 6, 1, 2, 3, 4, 5, 6, 7, 8),
        dppm = c(1, 2, 1, 2, 1, 7, 2, 3, 1, 1, 2, 1, 1),
        scale = c(3, 3, 3, 5, 5, 3, 13, 19, 3, 3, 5, 5, 5),
        scpos = c(58, 57, 57, 187, 187, 57, 58, 58, 136, 183, 184, 184, 184),
        scmin = c(55, 54, 54, 182, 182, 54, 45, 39, 133, 180, 179, 179, 179),
        scmax = c(61, 60, 60, 192, 192, 60, 71, 77, 139, 186, 189, 189, 189),
        lmin = c(55, 52, 55, 87, 87, 55, 54, 20, 101, 87, 82, 84, 87),
        lmax = c(63, 60, 60, 99, 101, 63, 61, 77, 112, 93, 101, 101, 101),
        sample = c("220221CCM_lbl__01_lu_flrd", "220221CCM_lbl__01_lu_flrd",
                   "220221CCM_lbl__01_lu_flrd", "220221CCM_lbl__01_lu_flrd",
                   "220221CCM_lbl__01_lu_flrd", "220221CCM_lbl__02_lu_flrd",
                   "220221CCM_lbl__02_lu_flrd", "220221CCM_lbl__02_lu_flrd",
                   "220221CCM_lbl__02_lu_flrd", "220221CCM_lbl__02_lu_flrd",
                   "220221CCM_lbl__02_lu_flrd", "220221CCM_lbl__02_lu_flrd",
                   "220221CCM_lbl__02_lu_flrd"),
        polarity = c("positive", "positive", "positive", "positive",
                     "positive", "positive", "positive", "positive",
                     "positive", "positive", "positive", "positive",
                     "positive")
    )
    peak_groups <- data.frame(
        group_id = c(1, 2, 3, 4, 5, 6, 7, 8),
        polarity = c("positive", "positive", "positive", "positive",
                     "positive", "positive", "positive", "positive"),
        mzmed = c(408.251325886321, 426.262123243348, 427.265526700411,
                  429.270861378734, 448.244170162955, 464.447379285654,
                  504.440256583886, 505.443684675365),
        mzmin = c(408.251325886321, 426.261913381751, 427.265348519813,
                  429.270861378734, 448.244170162955, 464.447304014051,
                  504.440032161331, 505.443534603),
        mzmax = c(408.251325886321, 426.262333104945, 427.265704881008,
                  429.270861378734, 448.244170162955, 464.447454557257,
                  504.44048100644, 505.44383474773),
        rtmed = c(286.278, 286.8085, 286.8085, 261.426, 286.807, 197.973,
                  197.7085, 197.444),
        rtmin = c(286.278, 286.807, 286.807, 261.426, 286.807, 197.973,
                  197.444, 197.444),
        rtmax = c(286.278, 286.81, 286.81, 261.426, 286.807, 197.973, 197.973,
                  197.444),
        npeaks = c(1, 2, 2, 1, 1, 2, 2, 2),
        `220221CCM_lbl__01_lu_flrd` = c(NA, 5, 4, NA, NA, 1, 2, 3),
        `220221CCM_lbl__02_lu_flrd` = c(10, 13, 12, 9, 11, 7, 8, 6)
    )
    db <- db_connect(":memory:")
    db_record_ann(db, ann, spectras, spectra_infos, peaks, peak_groups)
    db_resolve_conflict(db, ann[1:2, ], 2)
    ann2 <- rbind(ann[-c(1, 2), ], ann[2, ])
    rownames(ann2) <- seq(nrow(ann2))
    testthat::expect_identical(
        db_get_ann(db),
        ann2
    )
    RSQLite::dbDisconnect(db)
})
