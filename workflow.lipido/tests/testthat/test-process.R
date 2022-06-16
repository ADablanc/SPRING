testthat::test_that("workflow", {
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
        best_deviation_mz = c(NA, NA, .0003662109375, .0003662109375,
                              .00048828125, .00048828125, .00018310546875,
                              .00018310546875, .0010986328125, .001312255859375,
                              NA, NA, NA, NA),
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

    # initialize parameters
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
    testthat::expect_equal(
        dbReadTable(db, "ann"),
        ann
    )
    testthat::expect_equal(
        dbReadTable(db, "spectra_infos"),
        spectra_infos
    )
    testthat::expect_equal(
        dbReadTable(db, "spectras"),
        spectras
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
        ann
    )
    testthat::expect_equal(
        dbReadTable(db, "spectra_infos"),
        spectra_infos
    )
    testthat::expect_equal(
        dbReadTable(db, "spectras"),
        spectras
    )
    RSQLite::dbDisconnect(db)
})

testthat::test_that("export annotations", {
    excel_file <- tempfile(fileext = ".xlsx")
    sqlite_file <- system.file(
        "testdata",
        "220221CCM_global.sqlite",
        package = "workflow.lipido"
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
