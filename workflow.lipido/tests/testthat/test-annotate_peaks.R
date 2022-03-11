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
    xset <- annotate_peaklists(xset, sample_names, ann_params)
    testthat::expect_equal(
        xset@ann,
        data.frame(
            group_id = c(1, 2, 9, 10, 11),
            name = c("LPC 11:0", "LPC 11:0", "LPC 11:0", "Cer (d18:1/C12:0)",
                     "Cer (d18:1/C12:0)"),
            formula = c("C19H40N1O7P1", "C19H40N1O7P1", "C19H40N1O7P1",
                        "C30H59N1O3", "C30H59N1O3"),
            adduct = c("[M+H-H2O]+", "[M+H]+", "[M+Na]+", "[M+H-H2O]+",
                       "[M+Na]+"),
            ion_formula = c("C19H39N1O6P1", "C19H41N1O7P1", "C19H40N1O7P1Na1",
                            "C30H58N1O2", "C30H59N1O3Na1"),
            rtdiff = c(9.52199999999993, 8.99149999999997, 8.99299999999994,
                       5.97300000000001, 5.70849999999999),
            rt = c(286.278, 286.8085, 286.807, 201.573, 201.3085),
            rtmin = c(284.692, 284.6935, 282.048, 199.712, 186.2245),
            rtmax = c(287.864, 292.0965, 292.095, 203.673, 207.034),
            nsamples = c(1, 2, 1, 2, 2),
            best_score = c(79.8211975097656, 95.1391906738281,
                           79.6432037353516, 71.3979721069336,
                           89.4345550537109),
            best_deviation_mz = c(0.0003662109375, 0.0008544921875,
                                  0.000701904296875, 0.0010986328125,
                                  0.00140380859375),
            best_npeak = c(1, 2, 1, 1, 2),
            `220221CCM_global_POS_01_ssleu_filtered` = c(NA, 2, NA, 5, 7),
            `220221CCM_global_POS_02_ssleu_filtered` = c(1, 3, 4, 6, 8)
        ),
        ignore_attr = TRUE
    )
    testthat::expect_equal(
        xset@spectra_infos,
        data.frame(
            spectra_id = c(1, 2, 3, 4, 5, 6, 7, 8),
            score = c(79.8211975097656, 94.9317855834961, 95.1391906738281,
                      79.6432037353516, 71.3979721069336, 71.3979721069336,
                      89.4345550537109, 88.4888916015625),
            deviation_mz = c(0.0003662109375, 0.000457763671875,
                             0.0008544921875, 0.000701904296875,
                             0.0010986328125, 0.001251220703125,
                             0.00140380859375, 0.0017852783203125),
            npeak = c(1, 2, 2, 1, 1, 1, 2, 2),
            basepeak_int = c(88824.635233072, 6214416.44108707,
                             6201250.27168528, 288290.748778874,
                             4945601.93026269, 5689144.27927454,
                             1448292.29379181, 1662831.19267642),
            sum_int = c(88824.635233072, 7385056.39979801, 7388017.8361341,
                        288290.748778874, 4945601.93026269, 5689144.27927454,
                        1849363.52087931, 2106914.27998355),
            sample = c("220221CCM_global_POS_02_ssleu_filtered",
                       "220221CCM_global_POS_01_ssleu_filtered",
                       "220221CCM_global_POS_02_ssleu_filtered",
                       "220221CCM_global_POS_02_ssleu_filtered",
                       "220221CCM_global_POS_01_ssleu_filtered",
                       "220221CCM_global_POS_02_ssleu_filtered",
                       "220221CCM_global_POS_01_ssleu_filtered",
                       "220221CCM_global_POS_02_ssleu_filtered"),
            rt = c(286.278, 286.81, 286.807, 286.807, 197.973, 205.173, 197.444,
                   205.173)
        ),
        ignore_attr = TRUE
    )
    testthat::expect_equal(
        xset@spectras,
        data.frame(
            spectra_id = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5,
                           5, 5, 6, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8),
            feature_id = c(NA, NA, NA, 17, NA, 4, NA, 5, NA, 19, NA, 20, NA, NA,
                           NA, 18, NA, 1, NA, NA, NA, 14, NA, NA, 2, NA, NA, 3,
                           15, NA, NA, 13),
            mz = c(NA, NA, NA, 408.251325886321, NA, 427.265348519813, NA,
                   426.261913381751, NA, 427.265704881008, NA, 426.262333104945,
                   NA, NA, NA, 448.244170162955, NA, 464.447304014051, NA, NA,
                   NA, 464.447454557257, NA, NA, 504.440032161331, NA, NA,
                   505.443534603, 504.44048100644, NA, NA, 505.44383474773),
            int = c(NA, NA, NA, 88824.635233072, NA, 1170639.95871094, NA,
                    6214416.44108707, NA, 1186767.56444882, NA,
                    6201250.27168528, NA, NA, NA, 288290.748778874, NA,
                    4945601.93026269, NA, NA, NA, 5689144.27927454, NA, NA,
                    1448292.29379181, NA, NA, 401071.227087501,
                    1662831.19267642, NA, NA, 444083.087307128),
            abd = c(NA, NA, NA, 100, NA, 18.8374881182917, NA, 100, NA,
                    19.1375531135642, NA, 100, NA, NA, NA, 100, NA, 100, NA, NA,
                    NA, 100, NA, NA, 100, NA, NA, 27.6926991054717, 100, NA, NA,
                    26.7064443620612),
            ion_id_theo = c(71, 71, 71, 71, 76, 76, 76, 76, 76, 76, 76, 76, 74,
                            74, 74, 74, 317, 317, 317, 317, 317, 317, 317, 317,
                            322, 322, 322, 322, 322, 322, 322, 322),
            mz_theo = c(410.25672, 409.25427, 411.25935, 408.25095, 429.26984,
                        427.26484, 428.26719, 426.26152, 429.26984, 427.26484,
                        428.26719, 426.26152, 449.24678, 450.24914, 451.25178,
                        448.24346, 467.45576, 464.44621, 465.44955, 466.45272,
                        467.45576, 464.44621, 465.44955, 466.45272, 504.43872,
                        506.44516, 507.44809, 505.44206, 504.43872, 506.44516,
                        507.44809, 505.44206),
            abd_theo = c(3.25, 21.65, 0.38, 100, 0.42, 21.7, 3.46, 100, 0.42,
                         21.7, 3.46, 100, 21.69, 3.45, 0.42, 100, 0.65, 100,
                         33.55, 5.86, 0.65, 100, 33.55, 5.86, 100, 6.07, 0.72,
                         33.67, 100, 6.07, 0.72, 33.67),
            iso_theo = c("M+2", "M+1", "M+3", "M", "M+3", "M+1", "M+2", "M",
                         "M+3", "M+1", "M+2", "M", "M+1", "M+2", "M+3", "M",
                         "M+3", "M", "M+1", "M+2", "M+3", "M", "M+1", "M+2",
                         "M", "M+2", "M+3", "M+1", "M", "M+2", "M+3", "M+1")
        ),
        ignore_attr = TRUE
    )
    RSQLite::dbDisconnect(db)
})

testthat::test_that("filtrate annotations", {
    # check if only compound seen with only one adduct
    ann <- data.frame(
        group_id = 1,
        name = "LPC 11:0",
        formula = "C19H40N1O7P1",
        adduct = "[M+H-H2O]+",
        ion_formula = "C19H39N1O6P1",
        rtdiff = 9.52199999999993,
        rt = 286.278,
        rtmin = 284.692,
        rtmax = 287.864,
        nsamples = 1,
        best_score = 79.8211975097656,
        best_deviation_mz = 0.0003662109375,
        best_npeak = 1,
        `220221CCM_global_POS_01_ssleu_filtered` = NA,
        `220221CCM_global_POS_02_ssleu_filtered` = 1
    )
    spectra_infos <- data.frame(
        spectra_id = 1,
        score = 79.8211975097656,
        deviation_mz = 0.0003662109375,
        npeak = 1,
        basepeak_int = 88824.635233072,
        sum_int = 88824.635233072,
        sample = "220221CCM_global_POS_02_ssleu_filtered",
        rt = 286.278
    )
    testthat::expect_equal(
        filtrate_ann(ann, spectra_infos),
        ann,
        ignore_attr = TRUE
    )

    # check if compound see with two adducts which respect fwhm & not
    # in theory it will be the [M+H]+ used as reference
    # the [M+Na]+ will be rejected cause the [M+H]+ is at 286 sec & the [M+Na]+
        # at 2860 sec
    # the [M+H-H2O]+ is kept cause the rT is at 286 sec
    ann <- data.frame(
        group_id = c(1, 2, 9),
        name = c("LPC 11:0", "LPC 11:0", "LPC 11:0"),
        formula = c("C19H40N1O7P1", "C19H40N1O7P1", "C19H40N1O7P1"),
        adduct = c("[M+H-H2O]+", "[M+H]+", "[M+Na]+"),
        ion_formula = c("C19H39N1O6P1", "C19H41N1O7P1", "C19H40N1O7P1Na1"),
        rtdiff = c(9.52199999999993, 8.99149999999997, 8.99299999999994),
        rt = c(286.278, 286.8085, 2860.807),
        rtmin = c(284.692, 284.6935, 282.048),
        rtmax = c(287.864, 292.0965, 292.095),
        nsamples = c(1, 2, 1),
        best_score = c(79.8211975097656, 95.1391906738281,
                       79.6432037353516),
        best_deviation_mz = c(0.0003662109375, 0.0008544921875,
                              0.000701904296875),
        best_npeak = c(1, 2, 1),
        `220221CCM_global_POS_01_ssleu_filtered` = c(NA, 2, NA),
        `220221CCM_global_POS_02_ssleu_filtered` = c(1, 3, 4)
    )
    spectra_infos <- data.frame(
        spectra_id = c(1, 2, 3, 4),
        score = c(79.8211975097656, 94.9317855834961, 95.1391906738281,
                  79.6432037353516),
        deviation_mz = c(0.0003662109375, 0.000457763671875, 0.0008544921875,
                         0.000701904296875),
        npeak = c(1, 2, 2, 1),
        basepeak_int = c(88824.635233072, 6214416.44108707, 6201250.27168528,
                         288290.748778874),
        sum_int = c(88824.635233072, 7385056.39979801, 7388017.8361341,
                    288290.748778874),
        sample = c("220221CCM_global_POS_02_ssleu_filtered",
                   "220221CCM_global_POS_01_ssleu_filtered",
                   "220221CCM_global_POS_02_ssleu_filtered",
                   "220221CCM_global_POS_02_ssleu_filtered"),
        rt = c(286.278, 286.81, 286.807, 286.807)
    )
    testthat::expect_equal(
        filtrate_ann(ann),
        ann[c(2, 1), ],
        ignore_attr = TRUE
    )

    # test the case where two annotation were not grouped by XCMS
    # first test when the fusion of the two lines is easy
    testthat::expect_equal(
        filtrate_ann(
            data.frame(
                group_id = c(1, 2, 3),
                name = c("LPC 11:0", "LPC 11:0", "LPC 11:0"),
                formula = c("C19H40N1O7P1", "C19H40N1O7P1", "C19H40N1O7P1"),
                adduct = c("[M+H]+", "[M+H]+", "[M+Na]+"),
                ion_formula = c("C19H41N1O7P1", "C19H41N1O7P1",
                                "C19H40N1O7P1Na1"),
                rtdiff = c(9.52199999999993, 8.99149999999997,
                           8.99299999999994),
                rt = c(286.278, 286.8085, 286.807),
                rtmin = c(284.692, 284.6935, 282.048),
                rtmax = c(287.864, 292.0965, 292.095),
                nsamples = c(1, 1, 1),
                best_score = c(94.9317855834961, 95.1391906738281,
                               79.6432037353516),
                best_deviation_mz = c(0.000457763671875, 0.0008544921875,
                                      0.000701904296875),
                best_npeak = c(2, 2, 1),
                `220221CCM_global_POS_01_ssleu_filtered` = c(2, NA, NA),
                `220221CCM_global_POS_02_ssleu_filtered` = c(NA, 3, 4)
            ),
            spectra_infos = spectra_infos <- data.frame(
                spectra_id = c(2, 3, 4),
                score = c(94.9317855834961, 95.1391906738281, 79.6432037353516),
                deviation_mz = c(0.000457763671875, 0.0008544921875,
                                 0.000701904296875),
                npeak = c(2, 2, 1),
                basepeak_int = c(6214416.44108707, 6201250.27168528,
                                 288290.748778874),
                sum_int = c(7385056.39979801, 7388017.8361341,
                            288290.748778874),
                sample = c("220221CCM_global_POS_01_ssleu_filtered",
                           "220221CCM_global_POS_02_ssleu_filtered",
                           "220221CCM_global_POS_02_ssleu_filtered"),
                rt = c(286.81, 286.807, 286.807)
            )
        ),
        data.frame(
            group_id = c(2, 3),
            name = c("LPC 11:0", "LPC 11:0"),
            formula = c("C19H40N1O7P1", "C19H40N1O7P1"),
            adduct = c("[M+H]+", "[M+Na]+"),
            ion_formula = c("C19H41N1O7P1", "C19H40N1O7P1Na1"),
            rtdiff = c(8.99149999999997, 8.99299999999994),
            rt = c(286.8085, 286.807),
            rtmin = c(284.6935, 282.048),
            rtmax = c(292.0965, 292.095),
            nsamples = c(2, 1),
            best_score = c(95.1391906738281, 79.6432037353516),
            best_deviation_mz = c(0.000457763671875, 0.000701904296875),
            best_npeak = c(2, 1),
            `220221CCM_global_POS_01_ssleu_filtered` = c(2, NA),
            `220221CCM_global_POS_02_ssleu_filtered` = c(3, 4)
        ),
        ignore_attr = TRUE
    )

    # test the case where two annotation were not grouped by XCMS
    # second test when the fusion of the lines ask to choose between two
    # spectras
    testthat::expect_equal(
        filtrate_ann(
            data.frame(
                group_id = c(1, 2, 3),
                name = c("LPC 11:0", "LPC 11:0", "LPC 11:0"),
                formula = c("C19H40N1O7P1", "C19H40N1O7P1", "C19H40N1O7P1"),
                adduct = c("[M+H]+", "[M+H]+", "[M+Na]+"),
                ion_formula = c("C19H41N1O7P1", "C19H41N1O7P1",
                                "C19H40N1O7P1Na1"),
                rtdiff = c(9.52199999999993, 8.99149999999997,
                           8.99299999999994),
                rt = c(286.278, 286.8085, 286.807),
                rtmin = c(284.692, 284.6935, 282.048),
                rtmax = c(287.864, 292.0965, 292.095),
                nsamples = c(1, 1, 1),
                best_score = c(94.9317855834961, 95.1391906738281,
                               79.6432037353516),
                best_deviation_mz = c(0.000457763671875, 0.0008544921875,
                                      0.000701904296875),
                best_npeak = c(2, 2, 1),
                `220221CCM_global_POS_01_ssleu_filtered` = c(2, NA, NA),
                `220221CCM_global_POS_02_ssleu_filtered` = c(4, 3, 5)
            ),
            data.frame(
                spectra_id = c(2, 3, 4, 5),
                score = c(94.9317855834961, 95.1391906738281, 79.6432037353516,
                          79.6432037353516),
                deviation_mz = c(0.000457763671875, 0.0008544921875,
                                 0.000701904296875, 0.000701904296875),
                npeak = c(2, 2, 1, 1),
                basepeak_int = c(6214416.44108707, 6201250.27168528,
                                 288290.748778874, 288290.748778874),
                sum_int = c(7385056.39979801, 7388017.8361341,
                            288290.748778874, 288290.748778874),
                sample = c("220221CCM_global_POS_01_ssleu_filtered",
                           "220221CCM_global_POS_02_ssleu_filtered",
                           "220221CCM_global_POS_02_ssleu_filtered",
                           "220221CCM_global_POS_02_ssleu_filtered"),
                rt = c(286.81, 286.807, 286.807, 286.807)
            )
        ),
        data.frame(
            group_id = c(2, 3),
            name = c("LPC 11:0", "LPC 11:0"),
            formula = c("C19H40N1O7P1", "C19H40N1O7P1"),
            adduct = c("[M+H]+", "[M+Na]+"),
            ion_formula = c("C19H41N1O7P1", "C19H40N1O7P1Na1"),
            rtdiff = c(8.99149999999997, 8.99299999999994),
            rt = c(286.8085, 286.807),
            rtmin = c(284.6935, 282.048),
            rtmax = c(292.0965, 292.095),
            nsamples = c(2, 1),
            best_score = c(95.1391906738281, 79.6432037353516),
            best_deviation_mz = c(0.000457763671875, 0.000701904296875),
            best_npeak = c(2, 1),
            `220221CCM_global_POS_01_ssleu_filtered` = c(2, NA),
            `220221CCM_global_POS_02_ssleu_filtered` = c(3, 5)
        ),
        ignore_attr = TRUE
    )
})

testthat::test_that("split conflicts", {
    ann <- data.frame(
        group_id = c(1, 1, 10, 11),
        name = c("LPC 11a:0", "LPC 11:0", "Cer (d18:1/C12:0)",
                 "Cer (d18:1/C12:0)"),
        formula = c("C19H40N1O7P1", "C19H40N1O7P1", "C30H59N1O3", "C30H59N1O3"),
        adduct = c("[M+H]+", "[M+H]+", "[M+H-H2O]+", "[M+Na]+"),
        ion_formula = c("C19H41N1O7P1", "C19H41N1O7P1", "C30H58N1O2",
                        "C30H59N1O3Na1"),
        rtdiff = c(8.99149999999997, 8.99149999999997, 5.97300000000001,
                   5.70849999999999),
        rt = c(286.8085, 286.8085, 201.573, 201.3085),
        rtmin = c(284.6935, 284.6935, 199.712, 186.2245),
        rtmax = c(292.0965, 292.0965, 203.673, 207.034),
        nsamples = c(2, 2, 2, 2),
        best_score = c(95.1391906738281, 95.1391906738281,
                       71.3979721069336, 89.4345550537109),
        best_deviation_mz = c(0.0008544921875, 0.0008544921875,
                              0.0010986328125, 0.00140380859375),
        best_npeak = c(2, 2, 1, 2),
        `220221CCM_global_POS_01_ssleu_filtered` = c(2, 2, 5, 7),
        `220221CCM_global_POS_02_ssleu_filtered` = c(3, 3, 6, 8)
    )
    testthat::expect_equal(
        split_conflicts(ann),
        list(
            no_conflicts = ann[3:4, ],
            conflicts = list(
                `1` = ann[1:2, ]
            )
        ),
        ignore_attr = TRUE
    )
})

testthat::test_that("get int in annotation df", {
    testthat::expect_equal(
        get_int_ann(
            data.frame(),
            data.frame()
        ),
        data.frame(matrix(, nrow = 0, ncol = 8,
            dimnames = list(c(),
                          c("name", "rT (min)", "Diff rT (sec)", "Adduct",
                            "nSamples", "Best score (%)", "Best m/z dev (mDa)",
                            "Max iso")
            )
        ), check.names = FALSE)
    )
    testthat::expect_equal(
        get_int_ann(
            data.frame(
                group_id = 1,
                name = "LPC 11:0",
                formula = "C19H40N1O7P1",
                adduct = "[M+H-H2O]+",
                ion_formula = "C19H39N1O6P1",
                rtdiff = 9.52199999999993,
                rt = 286.278,
                rtmin = 284.692,
                rtmax = 287.864,
                nsamples = 1,
                best_score = 79.8211975097656,
                best_deviation_mz = 0.0003662109375,
                best_npeak = 1,
                `220221CCM_global_POS_01_ssleu_filtered` = NA,
                `220221CCM_global_POS_02_ssleu_filtered` = 1
            ),
            data.frame(
                spectra_id = 1,
                score = 79.8211975097656,
                deviation_mz = 0.0003662109375,
                npeak = 1,
                basepeak_int = 88824.635233072,
                sum_int = 88824.635233072,
                sample = "220221CCM_global_POS_02_ssleu_filtered",
                rt = 286.278
            )
        ),
        data.frame(
            name = "LPC 11:0",
            `rT (min)` = 4.77,
            `Diff rT (sec)` = 10,
            Adduct = "[M+H-H2O]+",
            nSamples = 1,
            `Best score (%)` = 80,
            `Best m/z dev (mDa)` = 0,
            `Max iso` = 1,
            X220221CCM_global_POS_01_ssleu_filtered = as.numeric(NA),
            X220221CCM_global_POS_02_ssleu_filtered = 88824.635233072,
            check.names = FALSE
        )
    )
})

testthat::test_that("summarise ann df", {
    testthat::expect_equal(
        summarise_ann(
            data.frame(),
            data.frame()
        ),
        data.frame(matrix(, nrow = 0, ncol = 10,
            dimnames = list(c(),
                          c("name", "rT (min)", "Diff rT (sec)", "Adducts",
                            "nSamples", "Most intense ion", "Best score (%)",
                            "Best m/z dev (mDa)", "Max iso", "X")
            )
        ), check.names = FALSE)
    )
    testthat::expect_equal(
        summarise_ann(
            data.frame(
                group_id = c(1, 2),
                name = c("LPC 11:0", "LPC 11:0"),
                formula = c("C19H40N1O7P1", "C19H40N1O7P1"),
                adduct = c("[M+H-H2O]+", "[M+H]+"),
                ion_formula = c("C19H39N1O6P1", "C19H41N1O7P1"),
                rtdiff = c(9.52199999999993, 8.99149999999997),
                rt = c(286.278, 286.8085),
                rtmin = c(284.692, 284.6935),
                rtmax = c(287.864, 292.0965),
                nsamples = c(1, 2),
                best_score = c(79.8211975097656, 95.1391906738281),
                best_deviation_mz = c(0.0003662109375, 0.0008544921875),
                best_npeak = c(1, 2),
                `220221CCM_global_POS_01_ssleu_filtered` = c(NA, 2),
                `220221CCM_global_POS_02_ssleu_filtered` = c(1, 3)
            ),
            data.frame(
                spectra_id = c(1, 2, 3),
                score = c(79.8211975097656, 94.9317855834961, 95.1391906738281),
                deviation_mz = c(0.0003662109375, 0.000457763671875,
                                 0.0008544921875),
                npeak = c(1, 2, 2),
                basepeak_int = c(88824.635233072, 6214416.44108707,
                                 6201250.27168528),
                sum_int = c(88824.635233072, 7385056.39979801, 7388017.8361341),
                sample = c("220221CCM_global_POS_02_ssleu_filtered",
                           "220221CCM_global_POS_01_ssleu_filtered",
                           "220221CCM_global_POS_02_ssleu_filtered"),
                rt = c(286.278, 286.81, 286.807)
            )
        ),
        data.frame(
            name = "LPC 11:0",
            `rT (min)` = 4.78,
            `Diff rT (sec)` = 9,
            Adducts = "[M+H-H2O]+ [M+H]+",
            nSamples = 2,
            `Most intense ion` = as.factor("[M+H]+"),
            `Best score (%)` = 95,
            `Best m/z dev (mDa)` = 0,
            `Max iso` = 2,
            X220221CCM_global_POS_01_ssleu_filtered = 6214416.44108707,
            X220221CCM_global_POS_02_ssleu_filtered = 6290074.90691835,
            check.names = FALSE
        ),
        ignore_attr = TRUE
    )
})
