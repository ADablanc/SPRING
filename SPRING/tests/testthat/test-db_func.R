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
    db_write_table(db, "mtcars", mtcars, row.names = TRUE)
    testthat::expect_identical(
        RSQLite::dbReadTable(db, "mtcars", row.names = TRUE),
        mtcars
    )
    RSQLite::dbDisconnect(db)
})

testthat::test_that("db execute", {
    db <- db_connect(":memory:")
    db_write_table(db, "mtcars", mtcars, row.names = TRUE)
    testthat::expect_error(
        db_execute(
            db,
            "INSERT INTO mtcars
                  (mpg, cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb)
            VALUES (0, 0, 0, 0, 0, 0, 0, 0, 0, 0);"
        ),
        "10 values for 11 columns"
    )
    db_execute(
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
    db_write_table(db, "mtcars", mtcars, row.names = TRUE)
    testthat::expect_error(
        db_get_query(db, "SELECT id FROM mtcars"),
        "no such column: id"
    )
    testthat::expect_identical(
        db_get_query(db, "SELECT * FROM peaks"),
        data.frame()
    )
    testthat::expect_identical(
        db_get_query(db, "SELECT * FROM mtcars", row.names = TRUE),
        mtcars
    )
    RSQLite::dbDisconnect(db)
})

testthat::test_that("db read table", {
    db <- db_connect(":memory:")
    db_write_table(db, "mtcars", mtcars, row.names = TRUE)
    testthat::expect_identical(
        db_read_table(db, "peaks"),
        data.frame()
    )
    testthat::expect_identical(
        db_read_table(db, "mtcars", row.names = TRUE),
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
        ms_file = NA,
        xsa = NA
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
        xcms::xcmsRaw(
            system.file(
                "testdata",
                "small.mzXML",
                package = "SPRING"
            ),
            profstep = 0
        )
    )
    ms_file <- decompress(db_get_query(
        db,
        "SELECT ms_file FROM sample LIMIT 1"
    )[1, 1])
    testthat::expect_equal(
        ms_file@scanindex,
        c(0, 1810)
    )
    RSQLite::dbDisconnect(db)
})

testthat::test_that("read ms file", {
    db <- db_connect(":memory:")
    db_record_samples(db, "small")
    expect_equal(
        db_read_ms_file(db, "small"),
        NULL
    )
    db_record_ms_file(
        db,
        "small",
        xcms::xcmsRaw(
            system.file(
                "testdata",
                "small.mzXML",
                package = "SPRING"
            ),
            profstep = 0
        )
    )
    expect_equal(
        db_read_ms_file(db, "test"),
        NULL
    )
    expect_equal(
        db_read_ms_file(db, "small")@scanindex,
        c(0, 1810)
    )
    RSQLite::dbDisconnect(db)
})

testthat::test_that("import ms file", {
    raw_file <- system.file(
        "testdata",
        "small.raw",
        package = "SPRING"
    )
    converter <- tools::file_path_as_absolute(
        "~/GitHub/SPRING/pwiz/msconvert.exe"
    )
    filter_params <- methods::new(
        "FilterParam",
        polarity = "positive",
        mz_range = c(200, 2001),
        rt_range = c(0, 0.5)
    )
    db <- db_connect(":memory:")
    db_record_samples(db, "small")

    # test with an error file
    raw_file_error <- tempfile(fileext = ".raw")
    if (!file.exists(raw_file_error)) {
        invisible(file.create(raw_file_error))
    }
    testthat::expect_identical(
        import_ms_file(db, "small", raw_file_error, converter, filter_params),
        "msconvert error"
    )

    import_ms_file(db, "small", raw_file, converter, filter_params)
    ms_file <- db_read_ms_file(db, "small")
    testthat::expect_equal(
        ms_file@scanindex,
        c(0, 1810)
    )
    testthat::expect_equal(
        ms_file@scantime,
        ms_file@scantime_corrected
    )
    RSQLite::dbDisconnect(db)
})

testthat::test_that("record xsa", {
    filepath <- system.file(
        "testdata",
        "small.mzXML",
        package = "SPRING"
    )
    suppressWarnings(suppressMessages(
        xsa <- CAMERA::xsAnnotate(xcms::xcmsSet(filepath))
    ))
    db <- db_connect(":memory:")
    db_record_samples(db, "small")
    db_record_xsa(db, xsa, "small")
    testthat::expect_identical(
        decompress(db_get_query(
            db,
            "SELECT xsa FROM sample LIMIT 1"
        )[1, 1]),
        xsa
    )
    RSQLite::dbDisconnect(db)
})

testthat::test_that("db record ann", {
    ann <- data.frame(
        group_id = c(1, 2, 3, 3, 3, 3, 3, 3, 4, 4, 5, 6, 7, 8),
        formula = c(NA, NA, "C19H40N1O7P1", "C19H40N1O7P1", "C19H40N1O7P1",
                    "C19H40N1O7P1", "C19H40N1O7P1", "C19H40N1O7P1",
                    "C30H59N1O3", "C30H59N1O3", NA, NA, NA, NA),
        class = c(NA, NA, "LPC", "LPC", "LPC", "LPC", "LPC", "LPC", "Cer",
                  "Cer", NA, NA, NA, NA),
        name = c(NA, NA, "LPC 11:0", "LPC 11a:0", "LPC 11:0", "LPC 11a:0",
                 "LPC 11:0", "LPC 11a:0", "Cer (d18:1/C12:0)",
                 "Cer (d18:1/C12:0)", NA, NA, NA, NA),
        major_adduct = c(NA, NA, "[M+H]+", "[M+H]+", "[M+H]+", "[M+H]+",
                         "[M+H]+", "[M+H]+", "[M+H-H2O]+", "[M+H-H2O]+", NA, NA,
                         NA, NA),
        adduct = c(NA, NA, "[M+H-H2O]+", "[M+H-H2O]+", "[M+H]+", "[M+H]+",
                   "[M+Na]+", "[M+Na]+", "[M+H-H2O]+", "[M+Na]+", NA, NA, NA,
                   NA),
        ion_formula = c(NA, NA, "C19H39N1O6P1", "C19H39N1O6P1", "C19H41N1O7P1",
                        "C19H41N1O7P1", "C19H40N1O7P1Na1", "C19H40N1O7P1Na1",
                        "C30H58N1O2", "C30H59N1O3Na1", NA, NA, NA, NA),
        rtdiff = c(NA, NA, 8.99149999999997, 4.19150000000002, 8.99149999999997,
                   4.19150000000002, 8.99149999999997, 4.19150000000002,
                   2.37300000000002, 2.37300000000002, NA, NA, NA, NA),
        rt = c(279.141, 259.046, 286.8085, 286.8085, 286.8085, 286.8085,
               286.8085, 286.8085, 197.973, 197.973, 297.915, 308.494, 197.444,
               306.904),
        rtmin = c(291.037, 264.598, 287.864, 287.864, 293.152, 293.152, 292.095,
                  292.095, 199.559, 208.548, 303.202, 310.081, 201.145,
                  309.549),
        rtmax = c(279.407, 259.31, 286.81, 286.81, 286.81, 286.81, 286.81,
                  286.81, 197.973, 197.973, 297.915, 308.494, 197.444, 306.904),
        nsamples = c(2, 2, 2, 2, 2, 2, 2, 2, 0, 0, 0, 0, 0, 0),
        best_score = c(0, 0, 79.8211975097656, 79.8211975097656,
                       95.0912628173828, 95.0912628173828, 79.6432037353516,
                       79.6432037353516, 71.3979721069336, 71.1946487426758, 0,
                       0, 0, 0),
        best_deviation_mz = c(Inf, Inf, .0003662109375, .0003662109375,
                              .00048828125, .00048828125, .00018310546875,
                              .00018310546875, .0010986328125, .001312255859375,
                              Inf, Inf, Inf, Inf),
        best_npeak = c(0, 0, 1, 1, 2, 2, 1, 1, 1, 1, 0, 0, 0, 0),
        `220221CCM_global_POS_01_ssleu_filtered` = c(1, 3, NA, NA, 6, 6, 8, 8,
                                                     10, 12, 14, 15, 16, NA),
        `220221CCM_global_POS_02_ssleu_filtered` = c(2, 4, 5, 5, 7, 7, 9, 9, 11,
                                                     13, NA, NA, 17, 18),
        check.names = FALSE
    )
    spectra_infos <- data.frame(
        spectra_id =  c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
                        17, 18),
        score = c(0, 0, 0, 0, 79.8211975097656, 95.0912628173828,
                  95.0683822631836, 79.6432037353516, 79.6432037353516,
                  71.3979721069336, 71.3979721069336, 71.1946487426758,
                  71.1946487426758, 0, 0, 0, 0, 0),
        deviation_mz = c(NaN, NaN, NaN, NaN, .0003662109375, .00048828125,
                         .0008392333984375, .00018310546875, .000701904296875,
                         .0010986328125, .001251220703125, .001312255859375,
                         .00177001953125, NaN, NaN, NaN, NaN, NaN),
        npeak = c(0, 0, 0, 0, 1, 2, 2, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0),
        basepeak_mz = c(428.267772595199, 428.268471709284, 428.267855601901,
                        428.268539638095, 408.251325886322, 426.261908233279,
                        426.262343531217, 448.243644005027, 448.244163142448,
                        464.447304014051, 464.447454557257, 504.440032161331,
                        504.44048100644, 428.2675574767, 428.267840674347,
                        505.443534603, 505.44383474773, 429.270782294993),
        basepeak_int = c(21634957.3317308, 19992518.2568646, 7556081.77126924,
                         7375409.9176154, 88824.635233072, 6139220.0505469,
                         6234084.85605467, 260064.992761365, 288524.169413714,
                         4945601.93026269, 5689144.27927454, 1287181.56877954,
                         1458245.19191226, 2235868.3566111, 753309.518850004,
                         401071.227087501, 444013.097852865, 323001.699462891),
        sum_int = c(0, 0, 0, 0, 88824.635233072, 7309860.00925784,
                    7420749.7462611, 260064.992761365, 288524.169413714,
                    4945601.93026269, 5689144.27927454, 1287181.56877954,
                    1458245.19191226, 0, 0, 0, 0, 0),
        sample = c("220221CCM_global_POS_01_ssleu_filtered",
                   "220221CCM_global_POS_02_ssleu_filtered",
                   "220221CCM_global_POS_01_ssleu_filtered",
                   "220221CCM_global_POS_02_ssleu_filtered",
                   "220221CCM_global_POS_02_ssleu_filtered",
                   "220221CCM_global_POS_01_ssleu_filtered",
                   "220221CCM_global_POS_02_ssleu_filtered",
                   "220221CCM_global_POS_01_ssleu_filtered",
                   "220221CCM_global_POS_02_ssleu_filtered",
                   "220221CCM_global_POS_01_ssleu_filtered",
                   "220221CCM_global_POS_02_ssleu_filtered",
                   "220221CCM_global_POS_01_ssleu_filtered",
                   "220221CCM_global_POS_02_ssleu_filtered",
                   "220221CCM_global_POS_01_ssleu_filtered",
                   "220221CCM_global_POS_01_ssleu_filtered",
                   "220221CCM_global_POS_01_ssleu_filtered",
                   "220221CCM_global_POS_02_ssleu_filtered",
                   "220221CCM_global_POS_02_ssleu_filtered"),
        rt  = c(279.407, 278.875, 258.782, 259.31, 286.278, 286.81, 286.807,
                286.81, 286.807, 197.973, 197.973, 197.444, 197.973, 297.915,
                308.494, 197.444, 197.444, 306.904)
    )
    spectras <- data.frame(
        spectra_id = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7,
                       7, 8, 8, 8, 8, 9, 9, 9, 9, 10, 10, 10, 10, 11, 11, 11,
                       11, 12, 12, 12, 12, 13, 13, 13, 13, 14, 14, 15, 16, 17,
                       18),
        feature_id = c(7, 9, 22, 24, 8, 10, 23, 17, 18, NA, NA, NA, 6, 4, NA,
                       NA, 21, 20, NA, NA, 5, NA, NA, NA, 19, NA, NA, NA, 1, NA,
                       NA, NA, 15, NA, NA, NA, 2, NA, NA, NA, 16, NA, NA, NA,
                       12, 11, 13, 3, 14, 25),
        mz = c(428.267772595199, 429.270423563444, 428.268471709284,
               429.271192885151, 428.267855601901, 429.270339913969,
               428.268539638095, 429.270885645465, 408.251325886321, NA, NA, NA,
               426.261908233279, 427.265397484755, NA, NA, 426.262343531217,
               427.265671264404, NA, NA, 448.243644005027, NA, NA, NA,
               448.244163142448, NA, NA, NA, 464.447304014051, NA, NA, NA,
               464.447454557257, NA, NA, NA, 504.440032161331, NA, NA, NA,
               504.44048100644, NA, NA, NA, 428.2675574767, 429.269607476748,
               428.267840674347, 505.443534603, 505.44383474773,
               429.270782294993),
        mzmin = c(428.267364501953, 429.269836425781, 428.268035888672,
                  429.270721435547, 428.267425537109, 429.269653320312,
                  428.267822265625, 429.270324707031, 408.251037597656, NA, NA,
                  NA, 426.261444091797, 427.264739990234, NA, NA,
                  426.262023925781, 427.264801025391, NA, NA, 448.243011474609,
                  NA, NA, NA, 448.242279052734, NA, NA, NA, 464.447021484375,
                  NA, NA, NA, 464.446746826172, NA, NA, NA, 504.439697265625,
                  NA, NA, NA, 504.439544677734, NA, NA, NA, 428.267242431641,
                  429.269256591797, 428.267364501953, 505.443084716797,
                  505.440704345703, 429.270416259766),
        mzmax = c(428.268310546875, 429.271362304688, 428.269195556641,
                  429.271636962891, 428.268615722656, 429.270904541016,
                  428.269287109375, 429.271575927734, 408.25146484375, NA, NA,
                  NA, 426.262420654297, 427.266052246094, NA, NA,
                  426.262786865234, 427.266052246094, NA, NA, 448.245391845703,
                  NA, NA, NA, 448.245422363281, NA, NA, NA, 464.447662353516,
                  NA, NA, NA, 464.447967529297, NA, NA, NA, 504.440490722656,
                  NA, NA, NA, 504.441101074219, NA, NA, NA, 428.268035888672,
                  429.269958496094, 428.2685546875, 505.443664550781,
                  505.444122314453, 429.271636962891),
        rt = c(279.407, 279.407, 278.875, 278.875, 258.782, 258.782, 259.31,
               258.782, 286.278, NA, NA, NA, 286.81, 286.81, NA, NA, 286.807,
               286.807, NA, NA, 286.81, NA, NA, NA, 286.807, NA, NA, NA,
               197.973, NA, NA, NA, 197.973, NA, NA, NA, 197.444, NA, NA, NA,
               197.973, NA, NA, NA, 297.915, 296.857, 308.494, 197.444, 197.444,
               306.904),
        rtmin = c(265.656, 265.656, 265.656, 265.127, 250.85, 250.85, 250.85,
                  250.85, 284.692, NA, NA, NA, 284.695, 284.695, NA, NA,
                  284.692, 283.105, NA, NA, 285.224, NA, NA, NA, 280.99, NA, NA,
                  NA, 196.386, NA, NA, NA, 195.858, NA, NA, NA, 193.743, NA, NA,
                  NA, 182.11, NA, NA, NA, 293.684, 295.271, 304.789, 196.386,
                  196.387, 301.084),
        rtmax = c(293.156, 294.742, 291.037, 291.037, 264.598, 264.598, 264.598,
                  264.069, 287.864, NA, NA, NA, 291.04, 291.04, NA, NA, 293.152,
                  292.623, NA, NA, 291.04, NA, NA, NA, 292.095, NA, NA, NA,
                  200.617, NA, NA, NA, 199.559, NA, NA, NA, 199.03, NA, NA, NA,
                  208.548, NA, NA, NA, 303.202, 307.966, 310.081, 199.03,
                  201.145, 309.549),
        int = c(21634957.3317308, 5360632.29847273, 19992518.2568646,
                4939357.04715561, 7556081.77126924, 1854836.50349039,
                7375409.9176154, 1621835.871345, 88824.635233072, NA, NA, NA,
                6139220.0505469, 1170639.95871094, NA, NA, 6234084.85605467,
                1186664.89020643, NA, NA, 260064.992761365, NA, NA, NA,
                288524.169413714, NA, NA, NA, 4945601.93026269, NA, NA, NA,
                5689144.27927454, NA, NA, NA, 1287181.56877954, NA, NA, NA,
                1458245.19191226, NA, NA, NA, 2235868.3566111, 450077.636764323,
                753309.518850004, 401071.227087501, 444013.097852865,
                323001.699462891),
        abd = c(100, 24.7776421107639, 100, 24.7060274433394, 100,
                24.5475970170559, 100, 21.9897726290631, 100, NA, NA, NA, 100,
                19.0682195632759, NA, NA, 100, 19.0351096850072, NA, NA, 100,
                NA, NA, NA, 100, NA, NA, NA, 100, NA, NA, NA, 100, NA, NA, NA,
                100, NA, NA, NA, 100, NA, NA, NA, 100, 20.1298808775354, 100,
                100, 100, 100),
        mz_theo = c(NA, NA, NA, NA, NA, NA, NA, NA, 408.25095, 409.25427,
                    410.25672, 411.25935, 426.26152, 427.26484, 428.26719,
                    429.26984, 426.26152, 427.26484, 428.26719, 429.26984,
                    448.24346, 449.24678, 450.24914, 451.25178, 448.24346,
                    449.24678, 450.24914, 451.25178, 464.44621, 465.44955,
                    466.45272, 467.45576, 464.44621, 465.44955, 466.45272,
                    467.45576, 504.43872, 505.44206, 506.44516, 507.44809,
                    504.43872, 505.44206, 506.44516, 507.44809, NA, NA, NA, NA,
                    NA, NA),
        abd_theo = c(NA, NA, NA, NA, NA, NA, NA, NA, 100, 21.65, 3.25, 0.38,
                     100, 21.7, 3.46, 0.42, 100, 21.7, 3.46, 0.42, 100, 21.69,
                     3.45, 0.42, 100, 21.69, 3.45, 0.42, 100, 33.55, 5.86, 0.65,
                     100, 33.55, 5.86, 0.65, 100, 33.67, 6.07, 0.72, 100, 33.67,
                     6.07, 0.72, NA, NA, NA, NA, NA, NA),
        iso_theo = c(NA, NA, NA, NA, NA, NA, NA, NA, "M", "M+1", "M+2", "M+3",
                     "M", "M+1", "M+2", "M+3", "M", "M+1", "M+2", "M+3", "M",
                     "M+1", "M+2", "M+3", "M", "M+1", "M+2", "M+3", "M", "M+1",
                     "M+2", "M+3", "M", "M+1", "M+2", "M+3", "M", "M+1", "M+2",
                     "M+3", "M", "M+1", "M+2", "M+3", NA, NA, NA, NA, NA, NA)
    )
    db <- db_connect(":memory:")
    db_record_ann(db, ann, spectras, spectra_infos)
    testthat::expect_identical(
        db_read_table(db, "ann"),
        ann
    )
    testthat::expect_equal(
        db_read_table(db, "spectra_infos"),
        spectra_infos
    )
    testthat::expect_identical(
        db_read_table(db, "spectras"),
        spectras
    )
    RSQLite::dbDisconnect(db)
})

testthat::test_that("record params", {
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
    filter_params <- FilterParam(cwt_params, ann_params)

    db <- db_connect(":memory:")
    db_record_params(
        db,
        filter_params,
        cwt_params,
        obw_params,
        pd_params,
        camera_params,
        ann_params
    )
    testthat::expect_identical(
        db_read_table(db, "filter_params"),
        params_to_dataframe(filter_params)
    )
    testthat::expect_equal(
        db_read_table(db, "cwt_params"),
        params_to_dataframe(cwt_params)
    )
    testthat::expect_equal(
        db_read_table(db, "obw_params"),
        params_to_dataframe(obw_params)
    )
    testthat::expect_identical(
        db_read_table(db, "pd_params"),
        params_to_dataframe(pd_params)
    )
    testthat::expect_equal(
        db_read_table(db, "camera_params"),
        params_to_dataframe(camera_params)
    )
    testthat::expect_identical(
        db_read_table(db, "ann_params"),
        params_to_dataframe(ann_params)
    )
    RSQLite::dbDisconnect(db)
})

testthat::test_that("get annotations", {
    ann <- data.frame(
        group_id = c(1, 2, 3, 3, 3, 3, 3, 3, 4, 4, 5, 6, 7, 8),
        formula = c(NA, NA, "C19H40N1O7P1", "C19H40N1O7P1", "C19H40N1O7P1",
                    "C19H40N1O7P1", "C19H40N1O7P1", "C19H40N1O7P1",
                    "C30H59N1O3", "C30H59N1O3", NA, NA, NA, NA),
        class = c(NA, NA, "LPC", "LPC", "LPC", "LPC", "LPC", "LPC", "Cer",
                  "Cer", NA, NA, NA, NA),
        name = c(NA, NA, "LPC 11:0", "LPC 11a:0", "LPC 11:0", "LPC 11a:0",
                 "LPC 11:0", "LPC 11a:0", "Cer (d18:1/C12:0)",
                 "Cer (d18:1/C12:0)", NA, NA, NA, NA),
        major_adduct = c(NA, NA, "[M+H]+", "[M+H]+", "[M+H]+", "[M+H]+",
                         "[M+H]+", "[M+H]+", "[M+H-H2O]+", "[M+H-H2O]+", NA, NA,
                         NA, NA),
        adduct = c(NA, NA, "[M+H-H2O]+", "[M+H-H2O]+", "[M+H]+", "[M+H]+",
                   "[M+Na]+", "[M+Na]+", "[M+H-H2O]+", "[M+Na]+", NA, NA, NA,
                   NA),
        ion_formula = c(NA, NA, "C19H39N1O6P1", "C19H39N1O6P1", "C19H41N1O7P1",
                        "C19H41N1O7P1", "C19H40N1O7P1Na1", "C19H40N1O7P1Na1",
                        "C30H58N1O2", "C30H59N1O3Na1", NA, NA, NA, NA),
        rtdiff = c(NA, NA, 8.99149999999997, 4.19150000000002, 8.99149999999997,
                   4.19150000000002, 8.99149999999997, 4.19150000000002,
                   2.37300000000002, 2.37300000000002, NA, NA, NA, NA),
        rt = c(279.141, 259.046, 286.8085, 286.8085, 286.8085, 286.8085,
               286.8085, 286.8085, 197.973, 197.973, 297.915, 308.494, 197.444,
               306.904),
        rtmin = c(291.037, 264.598, 287.864, 287.864, 293.152, 293.152, 292.095,
                  292.095, 199.559, 208.548, 303.202, 310.081, 201.145,
                  309.549),
        rtmax = c(279.407, 259.31, 286.81, 286.81, 286.81, 286.81, 286.81,
                  286.81, 197.973, 197.973, 297.915, 308.494, 197.444, 306.904),
        nsamples = c(2, 2, 2, 2, 2, 2, 2, 2, 0, 0, 0, 0, 0, 0),
        best_score = c(0, 0, 79.8211975097656, 79.8211975097656,
                       95.0912628173828, 95.0912628173828, 79.6432037353516,
                       79.6432037353516, 71.3979721069336, 71.1946487426758, 0,
                       0, 0, 0),
        best_deviation_mz = c(Inf, Inf, .0003662109375, .0003662109375,
                              .00048828125, .00048828125, .00018310546875,
                              .00018310546875, .0010986328125, .001312255859375,
                              Inf, Inf, Inf, Inf),
        best_npeak = c(0, 0, 1, 1, 2, 2, 1, 1, 1, 1, 0, 0, 0, 0),
        `220221CCM_global_POS_01_ssleu_filtered` = c(1, 3, NA, NA, 6, 6, 8, 8,
                                                     10, 12, 14, 15, 16, NA),
        `220221CCM_global_POS_02_ssleu_filtered` = c(2, 4, 5, 5, 7, 7, 9, 9, 11,
                                                     13, NA, NA, 17, 18),
        check.names = FALSE
    )
    db <- db_connect(":memory:")
    db_write_table(db, "ann", ann)

    # 1st test : get all annotations
    testthat::expect_identical(
        db_get_annotations(db),
        ann
    )

    # 2nd test : get all annotations for a compound name
    testthat::expect_identical(
        db_get_annotations(db, names = "Cer (d18:1/C12:0)"),
        data.frame(ann[which(ann$name == "Cer (d18:1/C12:0)"), ],
                   row.names = NULL, check.names = FALSE)
    )

    # 3rd test : get all annotations for a group id
    testthat::expect_identical(
        db_get_annotations(db, group_ids = 11),
        ann[ann$group_id == 11, ]
    )

    # 4th test : get the 9th annotation
    testthat::expect_identical(
        db_get_annotations(db, row = 9),
        data.frame(ann[9, ], row.names = NULL, check.names = FALSE)
    )
    RSQLite::dbDisconnect(db)
})

testthat::test_that("get spectra infos", {
    spectra_infos <- data.frame(
        spectra_id =  c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
                        17, 18),
        score = c(0, 0, 0, 0, 79.8211975097656, 95.0912628173828,
                  95.0683822631836, 79.6432037353516, 79.6432037353516,
                  71.3979721069336, 71.3979721069336, 71.1946487426758,
                  71.1946487426758, 0, 0, 0, 0, 0),
        deviation_mz = c(NaN, NaN, NaN, NaN, .0003662109375, .00048828125,
                         .0008392333984375, .00018310546875, .000701904296875,
                         .0010986328125, .001251220703125, .001312255859375,
                         .00177001953125, NaN, NaN, NaN, NaN, NaN),
        npeak = c(0, 0, 0, 0, 1, 2, 2, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0),
        basepeak_mz = c(428.267772595199, 428.268471709284, 428.267855601901,
                        428.268539638095, 408.251325886322, 426.261908233279,
                        426.262343531217, 448.243644005027, 448.244163142448,
                        464.447304014051, 464.447454557257, 504.440032161331,
                        504.44048100644, 428.2675574767, 428.267840674347,
                        505.443534603, 505.44383474773, 429.270782294993),
        basepeak_int = c(21634957.3317308, 19992518.2568646, 7556081.77126924,
                         7375409.9176154, 88824.635233072, 6139220.0505469,
                         6234084.85605467, 260064.992761365, 288524.169413714,
                         4945601.93026269, 5689144.27927454, 1287181.56877954,
                         1458245.19191226, 2235868.3566111, 753309.518850004,
                         401071.227087501, 444013.097852865, 323001.699462891),
        sum_int = c(0, 0, 0, 0, 88824.635233072, 7309860.00925784,
                    7420749.7462611, 260064.992761365, 288524.169413714,
                    4945601.93026269, 5689144.27927454, 1287181.56877954,
                    1458245.19191226, 0, 0, 0, 0, 0),
        sample = c("220221CCM_global_POS_01_ssleu_filtered",
                   "220221CCM_global_POS_02_ssleu_filtered",
                   "220221CCM_global_POS_01_ssleu_filtered",
                   "220221CCM_global_POS_02_ssleu_filtered",
                   "220221CCM_global_POS_02_ssleu_filtered",
                   "220221CCM_global_POS_01_ssleu_filtered",
                   "220221CCM_global_POS_02_ssleu_filtered",
                   "220221CCM_global_POS_01_ssleu_filtered",
                   "220221CCM_global_POS_02_ssleu_filtered",
                   "220221CCM_global_POS_01_ssleu_filtered",
                   "220221CCM_global_POS_02_ssleu_filtered",
                   "220221CCM_global_POS_01_ssleu_filtered",
                   "220221CCM_global_POS_02_ssleu_filtered",
                   "220221CCM_global_POS_01_ssleu_filtered",
                   "220221CCM_global_POS_01_ssleu_filtered",
                   "220221CCM_global_POS_01_ssleu_filtered",
                   "220221CCM_global_POS_02_ssleu_filtered",
                   "220221CCM_global_POS_02_ssleu_filtered"),
        rt  = c(279.407, 278.875, 258.782, 259.31, 286.278, 286.81, 286.807,
                286.81, 286.807, 197.973, 197.973, 197.444, 197.973, 297.915,
                308.494, 197.444, 197.444, 306.904)
    )
    db <- db_connect(":memory:")
    db_write_table(db, "spectra_infos", spectra_infos)
    testthat::expect_equal(
        db_get_spectra_infos(db, c(1, 2)),
        spectra_infos[spectra_infos$spectra_id %in% 1:2, ]
    )
    testthat::expect_equal(
        db_get_spectra_infos(db),
        spectra_infos
    )
    RSQLite::dbDisconnect(db)
})

testthat::test_that("get spectras", {
    spectras <- data.frame(
        spectra_id = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7,
                       7, 8, 8, 8, 8, 9, 9, 9, 9, 10, 10, 10, 10, 11, 11, 11,
                       11, 12, 12, 12, 12, 13, 13, 13, 13, 14, 14, 15, 16, 17,
                       18),
        feature_id = c(7, 9, 22, 24, 8, 10, 23, 17, 18, NA, NA, NA, 6, 4, NA,
                       NA, 21, 20, NA, NA, 5, NA, NA, NA, 19, NA, NA, NA, 1, NA,
                       NA, NA, 15, NA, NA, NA, 2, NA, NA, NA, 16, NA, NA, NA,
                       12, 11, 13, 3, 14, 25),
        mz = c(428.267772595199, 429.270423563444, 428.268471709284,
               429.271192885151, 428.267855601901, 429.270339913969,
               428.268539638095, 429.270885645465, 408.251325886321, NA, NA, NA,
               426.261908233279, 427.265397484755, NA, NA, 426.262343531217,
               427.265671264404, NA, NA, 448.243644005027, NA, NA, NA,
               448.244163142448, NA, NA, NA, 464.447304014051, NA, NA, NA,
               464.447454557257, NA, NA, NA, 504.440032161331, NA, NA, NA,
               504.44048100644, NA, NA, NA, 428.2675574767, 429.269607476748,
               428.267840674347, 505.443534603, 505.44383474773,
               429.270782294993),
        mzmin = c(428.267364501953, 429.269836425781, 428.268035888672,
                  429.270721435547, 428.267425537109, 429.269653320312,
                  428.267822265625, 429.270324707031, 408.251037597656, NA, NA,
                  NA, 426.261444091797, 427.264739990234, NA, NA,
                  426.262023925781, 427.264801025391, NA, NA, 448.243011474609,
                  NA, NA, NA, 448.242279052734, NA, NA, NA, 464.447021484375,
                  NA, NA, NA, 464.446746826172, NA, NA, NA, 504.439697265625,
                  NA, NA, NA, 504.439544677734, NA, NA, NA, 428.267242431641,
                  429.269256591797, 428.267364501953, 505.443084716797,
                  505.440704345703, 429.270416259766),
        mzmax = c(428.268310546875, 429.271362304688, 428.269195556641,
                  429.271636962891, 428.268615722656, 429.270904541016,
                  428.269287109375, 429.271575927734, 408.25146484375, NA, NA,
                  NA, 426.262420654297, 427.266052246094, NA, NA,
                  426.262786865234, 427.266052246094, NA, NA, 448.245391845703,
                  NA, NA, NA, 448.245422363281, NA, NA, NA, 464.447662353516,
                  NA, NA, NA, 464.447967529297, NA, NA, NA, 504.440490722656,
                  NA, NA, NA, 504.441101074219, NA, NA, NA, 428.268035888672,
                  429.269958496094, 428.2685546875, 505.443664550781,
                  505.444122314453, 429.271636962891),
        rt = c(279.407, 279.407, 278.875, 278.875, 258.782, 258.782, 259.31,
               258.782, 286.278, NA, NA, NA, 286.81, 286.81, NA, NA, 286.807,
               286.807, NA, NA, 286.81, NA, NA, NA, 286.807, NA, NA, NA,
               197.973, NA, NA, NA, 197.973, NA, NA, NA, 197.444, NA, NA, NA,
               197.973, NA, NA, NA, 297.915, 296.857, 308.494, 197.444, 197.444,
               306.904),
        rtmin = c(265.656, 265.656, 265.656, 265.127, 250.85, 250.85, 250.85,
                  250.85, 284.692, NA, NA, NA, 284.695, 284.695, NA, NA,
                  284.692, 283.105, NA, NA, 285.224, NA, NA, NA, 280.99, NA, NA,
                  NA, 196.386, NA, NA, NA, 195.858, NA, NA, NA, 193.743, NA, NA,
                  NA, 182.11, NA, NA, NA, 293.684, 295.271, 304.789, 196.386,
                  196.387, 301.084),
        rtmax = c(293.156, 294.742, 291.037, 291.037, 264.598, 264.598, 264.598,
                  264.069, 287.864, NA, NA, NA, 291.04, 291.04, NA, NA, 293.152,
                  292.623, NA, NA, 291.04, NA, NA, NA, 292.095, NA, NA, NA,
                  200.617, NA, NA, NA, 199.559, NA, NA, NA, 199.03, NA, NA, NA,
                  208.548, NA, NA, NA, 303.202, 307.966, 310.081, 199.03,
                  201.145, 309.549),
        int = c(21634957.3317308, 5360632.29847273, 19992518.2568646,
                4939357.04715561, 7556081.77126924, 1854836.50349039,
                7375409.9176154, 1621835.871345, 88824.635233072, NA, NA, NA,
                6139220.0505469, 1170639.95871094, NA, NA, 6234084.85605467,
                1186664.89020643, NA, NA, 260064.992761365, NA, NA, NA,
                288524.169413714, NA, NA, NA, 4945601.93026269, NA, NA, NA,
                5689144.27927454, NA, NA, NA, 1287181.56877954, NA, NA, NA,
                1458245.19191226, NA, NA, NA, 2235868.3566111, 450077.636764323,
                753309.518850004, 401071.227087501, 444013.097852865,
                323001.699462891),
        abd = c(100, 24.7776421107639, 100, 24.7060274433394, 100,
                24.5475970170559, 100, 21.9897726290631, 100, NA, NA, NA, 100,
                19.0682195632759, NA, NA, 100, 19.0351096850072, NA, NA, 100,
                NA, NA, NA, 100, NA, NA, NA, 100, NA, NA, NA, 100, NA, NA, NA,
                100, NA, NA, NA, 100, NA, NA, NA, 100, 20.1298808775354, 100,
                100, 100, 100),
        mz_theo = c(NA, NA, NA, NA, NA, NA, NA, NA, 408.25095, 409.25427,
                    410.25672, 411.25935, 426.26152, 427.26484, 428.26719,
                    429.26984, 426.26152, 427.26484, 428.26719, 429.26984,
                    448.24346, 449.24678, 450.24914, 451.25178, 448.24346,
                    449.24678, 450.24914, 451.25178, 464.44621, 465.44955,
                    466.45272, 467.45576, 464.44621, 465.44955, 466.45272,
                    467.45576, 504.43872, 505.44206, 506.44516, 507.44809,
                    504.43872, 505.44206, 506.44516, 507.44809, NA, NA, NA, NA,
                    NA, NA),
        abd_theo = c(NA, NA, NA, NA, NA, NA, NA, NA, 100, 21.65, 3.25, 0.38,
                     100, 21.7, 3.46, 0.42, 100, 21.7, 3.46, 0.42, 100, 21.69,
                     3.45, 0.42, 100, 21.69, 3.45, 0.42, 100, 33.55, 5.86, 0.65,
                     100, 33.55, 5.86, 0.65, 100, 33.67, 6.07, 0.72, 100, 33.67,
                     6.07, 0.72, NA, NA, NA, NA, NA, NA),
        iso_theo = c(NA, NA, NA, NA, NA, NA, NA, NA, "M", "M+1", "M+2", "M+3",
                     "M", "M+1", "M+2", "M+3", "M", "M+1", "M+2", "M+3", "M",
                     "M+1", "M+2", "M+3", "M", "M+1", "M+2", "M+3", "M", "M+1",
                     "M+2", "M+3", "M", "M+1", "M+2", "M+3", "M", "M+1", "M+2",
                     "M+3", "M", "M+1", "M+2", "M+3", NA, NA, NA, NA, NA, NA)
    )
    db <- db_connect(":memory:")
    db_write_table(db, "spectras", spectras)
    testthat::expect_identical(
        db_get_spectras(db, c(1, 2)),
        spectras[spectras$spectra_id %in% c(1, 2), ]
    )
    testthat::expect_identical(
        db_get_spectras(db),
        spectras
    )
    RSQLite::dbDisconnect(db)
})

testthat::test_that("get params", {
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
    filter_params <- FilterParam(cwt_params, ann_params)
    db <- db_connect(":memory:")
    db_record_params(
        db,
        filter_params,
        cwt_params,
        obw_params,
        pd_params,
        camera_params,
        ann_params
    )
    testthat::expect_identical(
        db_get_params(db),
        list(
            filter = params_to_dataframe(filter_params),
            cwt = params_to_dataframe(cwt_params),
            obw = params_to_dataframe(obw_params),
            pd = params_to_dataframe(pd_params),
            camera = params_to_dataframe(camera_params),
            ann = params_to_dataframe(ann_params)
        )
    )
    RSQLite::dbDisconnect(db)
})

testthat::test_that("count number of samples", {
    db <- db_connect(":memory:")

    # 1st test : test with no samples
    testthat::expect_identical(
        db_get_nsamples(db),
        0
    )

    # 2nd test : test with one sample
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
        db_get_nsamples(db),
        1
    )
    RSQLite::dbDisconnect(db)
})

testthat::test_that("resolve conflict", {
    ann <- data.frame(
        group_id = c(1, 2, 3, 3, 3, 3, 3, 3, 4, 4, 5, 6, 7, 8),
        formula = c(NA, NA, "C19H40N1O7P1", "C19H40N1O7P1", "C19H40N1O7P1",
                    "C19H40N1O7P1", "C19H40N1O7P1", "C19H40N1O7P1",
                    "C30H59N1O3", "C30H59N1O3", NA, NA, NA, NA),
        class = c(NA, NA, "LPC", "LPC", "LPC", "LPC", "LPC", "LPC", "Cer",
                  "Cer", NA, NA, NA, NA),
        name = c(NA, NA, "LPC 11:0", "LPC 11a:0", "LPC 11:0", "LPC 11a:0",
                 "LPC 11:0", "LPC 11a:0", "Cer (d18:1/C12:0)",
                 "Cer (d18:1/C12:0)", NA, NA, NA, NA),
        major_adduct = c(NA, NA, "[M+H]+", "[M+H]+", "[M+H]+", "[M+H]+",
                         "[M+H]+", "[M+H]+", "[M+H-H2O]+", "[M+H-H2O]+", NA, NA,
                         NA, NA),
        adduct = c(NA, NA, "[M+H-H2O]+", "[M+H-H2O]+", "[M+H]+", "[M+H]+",
                   "[M+Na]+", "[M+Na]+", "[M+H-H2O]+", "[M+Na]+", NA, NA, NA,
                   NA),
        ion_formula = c(NA, NA, "C19H39N1O6P1", "C19H39N1O6P1", "C19H41N1O7P1",
                        "C19H41N1O7P1", "C19H40N1O7P1Na1", "C19H40N1O7P1Na1",
                        "C30H58N1O2", "C30H59N1O3Na1", NA, NA, NA, NA),
        rtdiff = c(NA, NA, 8.99149999999997, 4.19150000000002, 8.99149999999997,
                   4.19150000000002, 8.99149999999997, 4.19150000000002,
                   2.37300000000002, 2.37300000000002, NA, NA, NA, NA),
        rt = c(279.141, 259.046, 286.8085, 286.8085, 286.8085, 286.8085,
               286.8085, 286.8085, 197.973, 197.973, 297.915, 308.494, 197.444,
               306.904),
        rtmin = c(291.037, 264.598, 287.864, 287.864, 293.152, 293.152, 292.095,
                  292.095, 199.559, 208.548, 303.202, 310.081, 201.145,
                  309.549),
        rtmax = c(279.407, 259.31, 286.81, 286.81, 286.81, 286.81, 286.81,
                  286.81, 197.973, 197.973, 297.915, 308.494, 197.444, 306.904),
        nsamples = c(2, 2, 2, 2, 2, 2, 2, 2, 0, 0, 0, 0, 0, 0),
        best_score = c(0, 0, 79.8211975097656, 79.8211975097656,
                       95.0912628173828, 95.0912628173828, 79.6432037353516,
                       79.6432037353516, 71.3979721069336, 71.1946487426758, 0,
                       0, 0, 0),
        best_deviation_mz = c(Inf, Inf, .0003662109375, .0003662109375,
                              .00048828125, .00048828125, .00018310546875,
                              .00018310546875, .0010986328125, .001312255859375,
                              Inf, Inf, Inf, Inf),
        best_npeak = c(0, 0, 1, 1, 2, 2, 1, 1, 1, 1, 0, 0, 0, 0),
        `220221CCM_global_POS_01_ssleu_filtered` = c(1, 3, NA, NA, 6, 6, 8, 8,
                                                     10, 12, 14, 15, 16, NA),
        `220221CCM_global_POS_02_ssleu_filtered` = c(2, 4, 5, 5, 7, 7, 9, 9, 11,
                                                     13, NA, NA, 17, 18),
        check.names = FALSE
    )
    db <- db_connect(":memory:")
    db_write_table(db, "ann", ann)
    db_resolve_conflict(db, 3, "LPC 11a:0")
    ann <- data.frame(ann[-which(
        ann$group_id == 3 & ann$name != "LPC 11a:0"), ],
        row.names = NULL, check.names = FALSE)
    testthat::expect_identical(
        db_read_table(db, "ann"),
        ann
    )
    RSQLite::dbDisconnect(db)
})

testthat::test_that("db record EICs", {
    eics <- data.frame(
        eic_id = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                   1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                   2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                   2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                   2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                   2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                   2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                   2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                   2, 2),
        rt = c(162.543, 163.072, 163.601, 164.13, 164.659, 165.187, 165.716,
               166.245, 166.774, 167.302, 167.831, 168.36, 168.889, 169.418,
               169.946, 170.475, 171.004, 171.533, 172.061, 172.59, 173.119,
               173.648, 174.177, 174.705, 175.234, 175.763, 176.292, 176.82,
               177.349, 177.878, 178.407, 178.935, 179.464, 179.993, 180.522,
               181.051, 181.579, 182.108, 182.637, 183.166, 183.695, 184.223,
               184.752, 185.281, 185.81, 186.338, 186.867, 187.396, 187.925,
               188.453, 188.982, 189.511, 190.04, 190.569, 191.097, 191.626,
               192.156, 192.685, 193.214, 193.743, 194.271, 194.8, 195.329,
               195.858, 196.386, 196.915, 197.444, 197.973, 198.502, 199.03,
               199.559, 200.088, 200.617, 201.145, 201.674, 202.203, 202.732,
               203.261, 203.789, 204.318, 204.847, 205.376, 205.904, 206.433,
               206.962, 207.491, 208.02, 208.548, 209.077, 209.606, 210.135,
               210.663, 211.192, 211.721, 212.25, 212.779, 213.307, 213.836,
               214.365, 214.894, 215.422, 215.951, 216.48, 217.009, 217.538,
               218.066, 218.595, 219.124, 219.653, 220.181, 220.71, 221.239,
               221.768, 222.296, 222.825, 223.354, 223.883, 224.412, 224.94,
               225.469, 225.998, 226.527, 227.055, 227.584, 228.113, 228.642,
               229.171, 229.699, 230.228, 230.757, 231.286, 231.814, 232.343,
               232.872, 233.401, 233.93, 234.458, 234.987, 235.516, 236.045,
               236.573, 162.543, 163.072, 163.601, 164.13, 164.659, 165.187,
               165.716, 166.245, 166.774, 167.302, 167.831, 168.36, 168.889,
               169.418, 169.946, 170.475, 171.004, 171.533, 172.061, 172.59,
               173.119, 173.648, 174.177, 174.705, 175.234, 175.763, 176.292,
               176.82, 177.349, 177.878, 178.407, 178.935, 179.464, 179.993,
               180.522, 181.051, 181.579, 182.108, 182.637, 183.166, 183.695,
               184.223, 184.752, 185.281, 185.81, 186.338, 186.867, 187.396,
               187.925, 188.453, 188.982, 189.511, 190.04, 190.569, 191.097,
               191.626, 192.156, 192.685, 193.214, 193.743, 194.271, 194.8,
               195.329, 195.858, 196.386, 196.915, 197.444, 197.973, 198.502,
               199.03, 199.559, 200.088, 200.617, 201.145, 201.674, 202.203,
               202.732, 203.261, 203.789, 204.318, 204.847, 205.376, 205.904,
               206.433, 206.962, 207.491, 208.02, 208.548, 209.077, 209.606,
               210.135, 210.663, 211.192, 211.721, 212.25, 212.779, 213.307,
               213.836, 214.365, 214.894, 215.422, 215.951, 216.48, 217.009,
               217.538, 218.066, 218.595, 219.124, 219.653, 220.181, 220.71,
               221.239, 221.768, 222.296, 222.825, 223.354, 223.883, 224.412,
               224.94, 225.469, 225.998, 226.527, 227.055, 227.584, 228.113,
               228.642, 229.171, 229.699, 230.228, 230.757, 231.286, 231.814,
               232.343, 232.872, 233.401, 233.93, 234.458, 234.987, 235.516,
               236.045, 236.573),
        `220221CCM_global_POS_01_ssleu_filtered` = c(0, 0, 0, 0, 0,
             199.861572265625, 0, 0, 394.248779296875, 0, 299.23583984375, 0, 0,
             0, 244.023681640625, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
             441.681396484375, 0, 0, 0, 0, 0, 0, 253, 253, 243.43212890625,
             341.361328125, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
             0, 509.606201171875, 509.606201171875, 0, 515.10986328125,
             515.10986328125, 0, 0, 0, 0, 769199.5, 4056878, 4130684, 388595.25,
             0, 9644.6875, 5816.8359375, 4466.09375, 0, 2891.658203125, 0, 0, 0,
             0, 458.009765625, 0, 0, 0, 0, 0, 0, 0, 0, 1336.8076171875, 0, 0, 0,
             0, 0, 0, 0, 422.3251953125, 0, 0, 0, 0, 0, 0, 752.5478515625, 0, 0,
             0, 259.13916015625, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
             0, 0, 0, 0, 0, 0, 0, 0, 237.429443359375, 0, 0, 0, 0, 0, 0, 0, 0,
             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
             0, 0, 0, 182.1103515625, 0, 0, 156.200927734375, 156.200927734375,
             0, 173.464599609375, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
             258.693115234375, 135.357055664062, 0, 0, 0, 0, 0, 0,
             186.248168945312, 0, 0, 0, 0, 0, 0, 0, 522.54443359375, 0, 0, 0, 0,
             148792.875, 1395886, 816974.5, 72440.25, 0, 0, 0, 638.46875, 0, 0,
             0, 466.333984375, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
             0, 0, 0, 0, 0, 0, 0, 0, 555.8876953125, 0, 0, 186.524780273438, 0,
             0, 0, 284.146484375, 0, 0, 183.346557617188, 0, 0, 0, 0, 0,
             211.48828125, 0, 0, 151.522827148438, 223.859008789062, 0, 0, 0, 0,
             0, 464.28271484375, 0, 0, 325.0712890625, 0, 0, 0, 0, 0, 0, 0, 0,
             0, 0),
        `220221CCM_global_POS_02_ssleu_filtered` = c(0, 0, 0, 0, 0, 0, 0, 0, 0,
             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
             0, 0, 0, 0, 0, 0, 256.14990234375, 0, 0, 0, 0, 429.933837890625,
             429.933837890625, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
             0, 0, 0, 0, 8866.2734375, 1640658.2734375, 5734716, 3129146,
             224961.25, 21989.9375, 0, 6092.8828125, 0, 0, 0, 0, 0, 0, 0,
             660.07958984375, 660.07958984375, 0, 0, 819.021484375, 0, 0, 0, 0,
             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
             507.470947265625, 0, 0, 643.7119140625, 0, 242.248413085938, 0, 0,
             0, 0, 0, 0, 0, 0, 283.0126953125, 0, 0, 0, 254.658447265625, 0, 0,
             391.944580078125, 103.657653808594, 0, 376.037353515625, 0, 0, 0,
             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 315.4375, 0, 0, 0, 0, 0, 0, 0, 0,
             0, 0, 0, 0, 0, 0, 0, 281.672119140625, 281.672119140625,
             309.37060546875, 309.37060546875, 0, 331.314697265625,
             331.314697265625, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 353.10546875, 0, 0,
             0, 0, 0, 0, 0, 402.763916015625, 402.763916015625, 0, 0, 0, 0, 0,
             0, 130.551635742188, 130.551635742188, 134.438720703125, 0, 0, 0,
             0, 0, 0, 1104.9599609375, 0, 339133.5, 1723138, 642538.5,
             42233.40625, 3690.009765625, 3690.009765625, 0, 0, 0,
             796.83251953125, 796.83251953125, 178.636962890625, 0, 0, 0, 0,
             329.189453125, 0, 0, 0, 490.173828125, 490.173828125,
             149.362548828125, 0, 219.229248046875, 0, 0, 0, 0, 0, 0, 0, 0, 0,
             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 415.034423828125, 0, 0,
             221.470703125, 488.4794921875, 488.4794921875, 0, 0, 0, 0, 0, 0, 0,
             0, 0, 0, 159.431274414062, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
             0),
        check.names = FALSE
    )

    tmp_file <- tempfile(fileext = ".sqlite")
    file.copy(
        system.file(
            "testdata",
            "220221CCM_global.sqlite",
            package = "SPRING"
        ),
        tmp_file,
        overwrite = TRUE
    )
    db <- db_connect(tmp_file)
    # light the database
    db_execute(db, "DELETE FROM ann WHERE group_id != 4;")
    db_execute(db, "DELETE FROM eic;")
    db_record_eics(db)
    testthat::expect_equal(
        db_get_query(db, "SELECT * FROM EIC"),
        eics
    )
    RSQLite::dbDisconnect(db)
})

testthat::test_that("db get eic", {
    eic <- data.frame(
        eic_id = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                   1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                   2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                   2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                   2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                   2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                   2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                   2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                   2, 2),
        rt = c(162.543, 163.072, 163.601, 164.13, 164.659, 165.187, 165.716,
               166.245, 166.774, 167.302, 167.831, 168.36, 168.889, 169.418,
               169.946, 170.475, 171.004, 171.533, 172.061, 172.59, 173.119,
               173.648, 174.177, 174.705, 175.234, 175.763, 176.292, 176.82,
               177.349, 177.878, 178.407, 178.935, 179.464, 179.993, 180.522,
               181.051, 181.579, 182.108, 182.637, 183.166, 183.695, 184.223,
               184.752, 185.281, 185.81, 186.338, 186.867, 187.396, 187.925,
               188.453, 188.982, 189.511, 190.04, 190.569, 191.097, 191.626,
               192.156, 192.685, 193.214, 193.743, 194.271, 194.8, 195.329,
               195.858, 196.386, 196.915, 197.444, 197.973, 198.502, 199.03,
               199.559, 200.088, 200.617, 201.145, 201.674, 202.203, 202.732,
               203.261, 203.789, 204.318, 204.847, 205.376, 205.904, 206.433,
               206.962, 207.491, 208.02, 208.548, 209.077, 209.606, 210.135,
               210.663, 211.192, 211.721, 212.25, 212.779, 213.307, 213.836,
               214.365, 214.894, 215.422, 215.951, 216.48, 217.009, 217.538,
               218.066, 218.595, 219.124, 219.653, 220.181, 220.71, 221.239,
               221.768, 222.296, 222.825, 223.354, 223.883, 224.412, 224.94,
               225.469, 225.998, 226.527, 227.055, 227.584, 228.113, 228.642,
               229.171, 229.699, 230.228, 230.757, 231.286, 231.814, 232.343,
               232.872, 233.401, 233.93, 234.458, 234.987, 235.516, 236.045,
               236.573, 162.543, 163.072, 163.601, 164.13, 164.659, 165.187,
               165.716, 166.245, 166.774, 167.302, 167.831, 168.36, 168.889,
               169.418, 169.946, 170.475, 171.004, 171.533, 172.061, 172.59,
               173.119, 173.648, 174.177, 174.705, 175.234, 175.763, 176.292,
               176.82, 177.349, 177.878, 178.407, 178.935, 179.464, 179.993,
               180.522, 181.051, 181.579, 182.108, 182.637, 183.166, 183.695,
               184.223, 184.752, 185.281, 185.81, 186.338, 186.867, 187.396,
               187.925, 188.453, 188.982, 189.511, 190.04, 190.569, 191.097,
               191.626, 192.156, 192.685, 193.214, 193.743, 194.271, 194.8,
               195.329, 195.858, 196.386, 196.915, 197.444, 197.973, 198.502,
               199.03, 199.559, 200.088, 200.617, 201.145, 201.674, 202.203,
               202.732, 203.261, 203.789, 204.318, 204.847, 205.376, 205.904,
               206.433, 206.962, 207.491, 208.02, 208.548, 209.077, 209.606,
               210.135, 210.663, 211.192, 211.721, 212.25, 212.779, 213.307,
               213.836, 214.365, 214.894, 215.422, 215.951, 216.48, 217.009,
               217.538, 218.066, 218.595, 219.124, 219.653, 220.181, 220.71,
               221.239, 221.768, 222.296, 222.825, 223.354, 223.883, 224.412,
               224.94, 225.469, 225.998, 226.527, 227.055, 227.584, 228.113,
               228.642, 229.171, 229.699, 230.228, 230.757, 231.286, 231.814,
               232.343, 232.872, 233.401, 233.93, 234.458, 234.987, 235.516,
               236.045, 236.573),
        `220221CCM_global_POS_01_ssleu_filtered` = c(0, 0, 0, 0, 0,
             199.861572265625, 0, 0, 394.248779296875, 0, 299.23583984375, 0, 0,
             0, 244.023681640625, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
             441.681396484375, 0, 0, 0, 0, 0, 0, 253, 253, 243.43212890625,
             341.361328125, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
             0, 509.606201171875, 509.606201171875, 0, 515.10986328125,
             515.10986328125, 0, 0, 0, 0, 769199.5, 4056878, 4130684, 388595.25,
             0, 9644.6875, 5816.8359375, 4466.09375, 0, 2891.658203125, 0, 0, 0,
             0, 458.009765625, 0, 0, 0, 0, 0, 0, 0, 0, 1336.8076171875, 0, 0, 0,
             0, 0, 0, 0, 422.3251953125, 0, 0, 0, 0, 0, 0, 752.5478515625, 0, 0,
             0, 259.13916015625, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
             0, 0, 0, 0, 0, 0, 0, 0, 237.429443359375, 0, 0, 0, 0, 0, 0, 0, 0,
             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
             0, 0, 0, 182.1103515625, 0, 0, 156.200927734375, 156.200927734375,
             0, 173.464599609375, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
             258.693115234375, 135.357055664062, 0, 0, 0, 0, 0, 0,
             186.248168945312, 0, 0, 0, 0, 0, 0, 0, 522.54443359375, 0, 0, 0, 0,
             148792.875, 1395886, 816974.5, 72440.25, 0, 0, 0, 638.46875, 0, 0,
             0, 466.333984375, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
             0, 0, 0, 0, 0, 0, 0, 0, 555.8876953125, 0, 0, 186.524780273438, 0,
             0, 0, 284.146484375, 0, 0, 183.346557617188, 0, 0, 0, 0, 0,
             211.48828125, 0, 0, 151.522827148438, 223.859008789062, 0, 0, 0, 0,
             0, 464.28271484375, 0, 0, 325.0712890625, 0, 0, 0, 0, 0, 0, 0, 0,
             0, 0),
        `220221CCM_global_POS_02_ssleu_filtered` = c(0, 0, 0, 0, 0, 0, 0, 0, 0,
             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
             0, 0, 0, 0, 0, 0, 256.14990234375, 0, 0, 0, 0, 429.933837890625,
             429.933837890625, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
             0, 0, 0, 0, 8866.2734375, 1640658.2734375, 5734716, 3129146,
             224961.25, 21989.9375, 0, 6092.8828125, 0, 0, 0, 0, 0, 0, 0,
             660.07958984375, 660.07958984375, 0, 0, 819.021484375, 0, 0, 0, 0,
             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
             507.470947265625, 0, 0, 643.7119140625, 0, 242.248413085938, 0, 0,
             0, 0, 0, 0, 0, 0, 283.0126953125, 0, 0, 0, 254.658447265625, 0, 0,
             391.944580078125, 103.657653808594, 0, 376.037353515625, 0, 0, 0,
             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 315.4375, 0, 0, 0, 0, 0, 0, 0, 0,
             0, 0, 0, 0, 0, 0, 0, 281.672119140625, 281.672119140625,
             309.37060546875, 309.37060546875, 0, 331.314697265625,
             331.314697265625, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 353.10546875, 0, 0,
             0, 0, 0, 0, 0, 402.763916015625, 402.763916015625, 0, 0, 0, 0, 0,
             0, 130.551635742188, 130.551635742188, 134.438720703125, 0, 0, 0,
             0, 0, 0, 1104.9599609375, 0, 339133.5, 1723138, 642538.5,
             42233.40625, 3690.009765625, 3690.009765625, 0, 0, 0,
             796.83251953125, 796.83251953125, 178.636962890625, 0, 0, 0, 0,
             329.189453125, 0, 0, 0, 490.173828125, 490.173828125,
             149.362548828125, 0, 219.229248046875, 0, 0, 0, 0, 0, 0, 0, 0, 0,
             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 415.034423828125, 0, 0,
             221.470703125, 488.4794921875, 488.4794921875, 0, 0, 0, 0, 0, 0, 0,
             0, 0, 0, 159.431274414062, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
             0),
        check.names = FALSE
    )

    db <- db_connect(":memory:")
    db_write_table(db, "eic", eic)
    testthat::expect_identical(
        db_get_eic(db, 3),
        eic[0, -1]
    )
    testthat::expect_identical(
        db_get_eic(db, 1),
        eic[eic$eic_id == 1, -1]
    )
    RSQLite::dbDisconnect(db)
})
