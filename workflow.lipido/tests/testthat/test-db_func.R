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
        )
    )
    ms_file <- decompress(dbGetQuery(
        db,
        "SELECT ms_file_positive FROM sample LIMIT 1"
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
        )
    )
    expect_equal(
        db_read_ms_file(db, "small", "positive")@scanindex,
        c(0, 1810)
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
    ms_file <- db_read_ms_file(db, "small", "positive")
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
        group_id = c(11, 10, 13, 2, 1, 9),
        class = c(rep("Cer", 2), "FA", rep("LPC", 3)),
        name = c("Cer (d18:1/C12:0)", "Cer (d18:1/C12:0)", "FA 17:0",
                 "LPC 11:0", "LPC 11:0", "LPC 11:0"),
        formula = c("C30H59N1O3", "C30H59N1O3", "C17H34O2", "C19H40N1O7P1",
                    "C19H40N1O7P1", "C19H40N1O7P1"),
        adduct = c("[M+Na]+", "[M+H-H2O]+", "[M-H]-", "[M+H]+",
                   "[M+H-H2O]+", "[M+Na]+"),
        ion_formula = c("C30H59N1O3Na1", "C30H58N1O2", "C17H33O2",
                        "C19H41N1O7P1", "C19H39N1O6P1", "C19H40N1O7P1Na1"),
        rtdiff = c(5.70849999999999, 5.97300000000001, 2.18899999999999,
                   8.99149999999997, 9.52199999999993, 8.99299999999994),
        rt = c(201.3085, 201.573, 178.411, 286.8085, 286.278, 286.807),
        rtmin = c(186.2245, 199.712, 175.3665, 284.6935, 284.692, 282.048),
        rtmax = c(207.034, 203.673, 180.985, 292.0965, 287.864, 292.095),
        nsamples = c(2, 2, 2, 2, 1, 1),
        best_score = c(89.4345550537109, 71.3979721069336, 99.5300903320312,
                       95.1391906738281, 79.8211975097656,
                       79.6432037353516),
        best_deviation_mz = c(0.00140380859375, 0.0010986328125,
                              -0.000111897788883653, 0.0008544921875,
                              0.0003662109375, 0.000701904296875),
        best_npeak = c(2, 1, 3, 2, 1, 1),
        `220221CCM_global__01_ssleu_filtered` = c(7, 5, 9, 2, NA, NA),
        `220221CCM_global__02_ssleu_filtered` = c(8, 6, 10, 3, 1, 4)
    )
    spectras <- data.frame(
        spectra_id = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5,
                       5, 5, 6, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8, 9, 9, 9,
                       10, 10, 10),
        feature_id = c(17, NA, NA, NA, 4, NA, NA, 5, 19, NA, NA, 20, NA, NA,
                       NA, 18, 1, NA, NA, NA, 14, NA, NA, NA, 2, 3, NA, NA,
                       15, 13, NA, NA, 25, NA, NA, 31, 30, 29),
        mz = c(408.251325886321, NA, NA, NA, 427.265348519813, NA, NA,
               426.261913381751, 427.265704881008, NA, NA, 426.262333104945,
               NA, NA, NA, 448.244170162955, 464.447304014051, NA, NA, NA,
               464.447454557257, NA, NA, NA, 504.440032161331,
               505.443534603, NA, NA, 504.44048100644, 505.44383474773, NA,
               NA, 269.247528218739, NA, NA, 269.248411023224,
               270.251620537443, 271.255056943515),
        int = c(88824.635233072, NA, NA, NA, 1170639.95871094, NA, NA,
                6214416.44108707, 1186767.56444882, NA, NA,
                6201250.27168528, NA, NA, NA, 288290.748778874,
                4945601.93026269, NA, NA, NA, 5689144.27927454, NA, NA, NA,
                1448292.29379181, 401071.227087501, NA, NA,
                1662831.19267642, 444083.087307128, NA, NA,
                8927664.85468527, NA, NA, 4593482.66518999,
                899482.748384234, 87688.1346123047),
        abd = c(100, NA, NA, NA, 18.8374881182917, NA, NA, 100,
                19.1375531135642, NA, NA, 100, NA, NA, NA, 100, 100, NA, NA,
                NA, 100, NA, NA, NA, 100, 27.6926991054717, NA, NA, 100,
                26.7064443620612, NA, NA, 100, NA, NA, 100,
                19.5817164000777, 1.90896844515855),
        ion_id_theo = c(62, 62, 62, 62, 66, 66, 66, 66, 66, 66, 66, 66, 64,
                        64, 64, 64, 278, 278, 278, 278, 278, 278, 278, 278,
                        281, 281, 281, 281, 281, 281, 281, 281, 8, 8, 8, 8,
                        8, 8),
        mz_theo = c(408.25095, 409.25427, 410.25672, 411.25935, 427.26484,
                    428.26719, 429.26984, 426.26152, 427.26484, 428.26719,
                    429.26984, 426.26152, 449.24678, 450.24914, 451.25178,
                    448.24346, 464.44621, 465.44955, 466.45272, 467.45576,
                    464.44621, 465.44955, 466.45272, 467.45576, 504.43872,
                    505.44206, 506.44516, 507.44809, 504.43872, 505.44206,
                    506.44516, 507.44809, 269.2486, 270.25202, 271.25481,
                    269.2486, 270.25202, 271.25481),
        abd_theo = c(100, 21.65, 3.25, 0.38, 21.7, 3.46, 0.42, 100, 21.7,
                     3.46, 0.42, 100, 21.69, 3.45, 0.42, 100, 100, 33.55,
                     5.86, 0.65, 100, 33.55, 5.86, 0.65, 100, 33.67, 6.07,
                     0.72, 100, 33.67, 6.07, 0.72, 100, 18.84, 2.02, 100,
                     18.84, 2.02),
        iso_theo = c("M", "M+1", "M+2", "M+3", "M+1", "M+2", "M+3", "M",
                     "M+1", "M+2", "M+3", "M", "M+1", "M+2", "M+3", "M",
                     "M", "M+1", "M+2", "M+3", "M", "M+1", "M+2", "M+3",
                     "M", "M+1", "M+2", "M+3", "M", "M+1", "M+2", "M+3",
                     "M", "M+1", "M+2", "M", "M+1", "M+2")
    )
    spectra_infos <- data.frame(
        spectra_id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
        score = c(79.8211975097656, 94.9317855834961, 95.1391906738281,
                  79.6432037353516, 71.3979721069336, 71.3979721069336,
                  89.4345550537109, 88.4888916015625, 82.740364074707,
                  99.5300903320312),
        deviation_mz = c(0.0003662109375, 0.000457763671875,
                         0.0008544921875, 0.000701904296875,
                         0.0010986328125, 0.001251220703125,
                         0.00140380859375, 0.0017852783203125,
                         -0.001068115234375, -0.000111897788883653),
        npeak = c(1, 2, 2, 1, 1, 1, 2, 2, 1, 3),
        basepeak_int = c(88824.635233072, 6214416.44108707,
                         6201250.27168528, 288290.748778874,
                         4945601.93026269, 5689144.27927454,
                         1448292.29379181, 1662831.19267642,
                         8927664.85468527, 4593482.66518999),
        sum_int = c(88824.635233072, 7385056.39979801, 7388017.8361341,
                    288290.748778874, 4945601.93026269, 5689144.27927454,
                    1849363.52087931, 2106914.27998355, 8927664.85468527,
                    5580653.54818653),
        sample = c("220221CCM_global__02_ssleu_filtered",
                   "220221CCM_global__01_ssleu_filtered",
                   "220221CCM_global__02_ssleu_filtered",
                   "220221CCM_global__02_ssleu_filtered",
                   "220221CCM_global__01_ssleu_filtered",
                   "220221CCM_global__02_ssleu_filtered",
                   "220221CCM_global__01_ssleu_filtered",
                   "220221CCM_global__02_ssleu_filtered",
                   "220221CCM_global__01_ssleu_filtered",
                   "220221CCM_global__02_ssleu_filtered"),
        rt = c(286.278, 286.81, 286.807, 286.807, 197.973, 205.173, 197.444,
               205.173, 178.504, 178.318)
    )
    peaks <- data.frame(
        feature_id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
                       16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28,
                       29, 30, 31),
        mz = c(464.447304014051, 504.440032161331, 505.443534603,
               427.265348519813, 426.261913381751, 428.267835743959,
               428.267896400125, 428.267904280982, 429.270493526998,
               429.270341280206, 429.270256788594, 429.270175958258,
               505.44383474773, 464.447454557257, 504.44048100644,
               429.270861378734, 408.251325886321, 448.244170162955,
               427.265704881008, 426.262333104945, 428.268466489791,
               428.268561934985, 429.270782294993, 270.25119624422,
               269.247528218739, 269.248194873815, 270.251307324138,
               269.248075605062, 271.255056943515, 270.251620537443,
               269.248411023224),
        mzmin = c(464.447021484375, 504.439697265625, 505.443084716797,
                  427.264587402344, 426.261444091797, 428.267547607422,
                  428.267425537109, 428.267456054688, 429.270263671875,
                  429.269653320312, 429.269836425781, 429.269622802734,
                  505.440704345703, 464.446746826172, 504.439544677734,
                  429.270660400391, 408.251037597656, 448.242279052734,
                  427.265258789062, 426.262023925781, 428.268035888672,
                  428.267822265625, 429.270416259766, 270.250793457031,
                  269.247406005859, 269.24755859375, 270.250823974609,
                  269.247589111328, 271.254425048828, 270.251403808594,
                  269.247833251953),
        mzmax = c(464.447662353516, 504.440490722656, 505.443664550781,
                  427.266052246094, 426.262420654297, 428.268310546875,
                  428.268615722656, 428.268249511719, 429.270660400391,
                  429.270904541016, 429.270751953125, 429.270874023438,
                  505.444122314453, 464.447967529297, 504.441101074219,
                  429.271148681641, 408.25146484375, 448.245422363281,
                  427.266052246094, 426.262786865234, 428.269195556641,
                  428.269287109375, 429.271636962891, 270.251953125,
                  269.247985839844, 269.249572753906, 270.25244140625,
                  269.248596191406, 271.256134033203, 270.251831054688,
                  269.248565673828),
        rt = c(197.973, 197.444, 197.444, 286.81, 286.81, 279.407, 260.368,
               291.569, 279.407, 258.253, 291.569, 301.616, 204.644,
               205.173, 205.173, 261.426, 286.278, 286.807, 286.807,
               286.807, 278.875, 258.253, 306.914, 147.056, 178.504,
               207.703, 146.538, 146.11, 178.318, 178.318, 178.318),
        rtmin = c(196.386, 194.271, 196.386, 284.695, 284.695, 275.706,
                  232.343, 287.339, 275.706, 232.343, 287.339, 298.443,
                  203.577, 203.038, 178.178, 258.253, 284.692, 282.048,
                  283.105, 284.692, 266.184, 232.343, 301.084, 144.809,
                  175.697, 203.298, 144.81, 144.81, 175.036, 175.036,
                  175.036),
        rtmax = c(200.617, 199.03, 199.03, 291.04, 292.098, 287.339,
                  261.426, 295.799, 286.81, 265.656, 296.328, 305.847,
                  207.757, 206.729, 215.038, 264.069, 287.864, 292.095,
                  292.095, 292.095, 292.095, 265.656, 309.559, 149.306,
                  180.384, 212.035, 150.968, 150.968, 179.727, 181.586,
                  181.586),
        int = c(4945601.93026269, 1448292.29379181, 401071.227087501,
                1170639.95871094, 6214416.44108707, 10859267.7547045,
                15396334.0742375, 3559573.81687499, 2574017.18247619,
                4567668.49476563, 847985.874378674, 392347.130863636,
                444083.087307128, 5689144.27927454, 1662831.19267642,
                801559.236409095, 88824.635233072, 288290.748778874,
                1186767.56444882, 6201250.27168528, 20076653.1797449,
                18139587.026625, 323001.699462891, 15852.5738923645,
                8927664.85468527, 18758.2195488281, 18160.7346019531,
                136136.373335937, 87688.1346123047, 899482.748384234,
                4593482.66518999),
        intb = c(4945464.15722767, 1448264.72345856, 401069.111887501,
                 1170634.14246094, 6212404.90921921, 10279271.7340996,
                 14009363.2233988, 3181331.80055223, 2490170.67943366,
                 4327555.39189945, 790817.501274091, 248786.599772727,
                 444001.578713656, 5689141.10698883, 1662613.35060178,
                 801553.420409095, 88821.9918997387, 287904.836179229,
                 1185632.54587715, 6201242.86868528, 18364665.4121469,
                 15982556.4399262, 209052.408698731, 13737.5420380623,
                 8654644.93987011, 12054.5931199951, 15262.9337143676,
                 118521.494192171, 75981.4533509554, 886826.38870081,
                 4540712.99687556),
        maxo = c(4130684, 1395886, 434808.75, 676902.5, 3501824, 1096165,
                 592012.5, 533287, 266683.75, 168154.5, 123908.25,
                 84350.125, 524308, 5734716, 1723138, 150635.375,
                 70041.5625, 186253.875, 673140, 3489368, 1078367, 657937,
                 68280.5, 8709.96875, 6265286.5, 2761.9111328125,
                 8407.2734375, 71703.6875, 53069.5625, 734161, 3772552),
        sn = c(8329, 22672, 434808, 676902, 3587, 10, 7, 4, 12, 7, 5, 1,
               4386, 5734715, 14423, 145000, 70041, 675, 1134, 3489367, 9,
               5, 1, 13, 15, 3, 11, 11, 3, 109, 298),
        egauss = as.numeric(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                              NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                              NA, NA, NA, NA, NA, NA, NA, NA, NA)),
        mu = as.numeric(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                          NA, NA, NA, NA, NA, NA, NA, NA, NA)),
        sigma = as.numeric(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                             NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                             NA, NA, NA, NA, NA, NA, NA, NA, NA)),
        h = as.numeric(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA)),
        f = c(1, 2, 3, 4, 6, 7, 7, 7, 8, 8, 8, 11, 1, 2, 3, 4, 5, 6, 7, 8,
              9, 9, 14, 1, 2, 3, 1, 2, 6, 7, 8),
        dppm = c(1, 2, 1, 2, 1, 2, 1, 0, 1, 1, 1, 2, 7, 2, 3, 1, 1, 2, 1, 1,
                 0, 1, 3, 3, 2, 4, 6, 3, 6, 2, 1),
        scale = c(3, 3, 3, 5, 5, 3, 15, 5, 3, 13, 5, 3, 3, 13, 19, 3, 3, 5,
                  5, 5, 11, 13, 5, 2, 2, 2, 3, 3, 3, 3, 3),
        scpos = c(58, 57, 57, 187, 187, 173, 137, 196, 173, 133, 196, 215,
                  57, 58, 58, 136, 183, 184, 184, 184, 169, 130, 222, 5, 44,
                  75, 5, 4, 52, 52, 52),
        scmin = c(55, 54, 54, 182, 182, 170, 122, 191, 170, 120, 191, 212,
                  54, 45, 39, 133, 180, 179, 179, 179, 158, 117, 217, 3, 42,
                  73, 2, 1, 49, 49, 49),
        scmax = c(61, 60, 60, 192, 192, 176, 152, 201, 176, 146, 201, 218,
                  60, 71, 77, 139, 186, 189, 189, 189, 180, 143, 227, 7, 46,
                  77, 8, 7, 55, 55, 55),
        lmin = c(55, 52, 55, 87, 87, 134, 87, 156, 134, 87, 156, 85, 55, 54,
                 20, 101, 87, 82, 84, 87, 116, 87, 79, 1, 40, 66, 1, 1, 47,
                 47, 47),
        lmax = c(63, 60, 60, 99, 101, 156, 107, 172, 155, 115, 173, 96, 63,
                 61, 77, 112, 93, 101, 101, 101, 165, 115, 95, 9, 48, 74,
                 11, 11, 55, 58, 58),
        sample = c("220221CCM_global__01_ssleu_filtered",
                   "220221CCM_global__01_ssleu_filtered",
                   "220221CCM_global__01_ssleu_filtered",
                   "220221CCM_global__01_ssleu_filtered",
                   "220221CCM_global__01_ssleu_filtered",
                   "220221CCM_global__01_ssleu_filtered",
                   "220221CCM_global__01_ssleu_filtered",
                   "220221CCM_global__01_ssleu_filtered",
                   "220221CCM_global__01_ssleu_filtered",
                   "220221CCM_global__01_ssleu_filtered",
                   "220221CCM_global__01_ssleu_filtered",
                   "220221CCM_global__01_ssleu_filtered",
                   "220221CCM_global__02_ssleu_filtered",
                   "220221CCM_global__02_ssleu_filtered",
                   "220221CCM_global__02_ssleu_filtered",
                   "220221CCM_global__02_ssleu_filtered",
                   "220221CCM_global__02_ssleu_filtered",
                   "220221CCM_global__02_ssleu_filtered",
                   "220221CCM_global__02_ssleu_filtered",
                   "220221CCM_global__02_ssleu_filtered",
                   "220221CCM_global__02_ssleu_filtered",
                   "220221CCM_global__02_ssleu_filtered",
                   "220221CCM_global__02_ssleu_filtered",
                   "220221CCM_global__01_ssleu_filtered",
                   "220221CCM_global__01_ssleu_filtered",
                   "220221CCM_global__01_ssleu_filtered",
                   "220221CCM_global__02_ssleu_filtered",
                   "220221CCM_global__02_ssleu_filtered",
                   "220221CCM_global__02_ssleu_filtered",
                   "220221CCM_global__02_ssleu_filtered",
                   "220221CCM_global__02_ssleu_filtered"),
        polarity = c("positive", "positive", "positive", "positive",
                     "positive", "positive", "positive", "positive",
                     "positive", "positive", "positive", "positive",
                     "positive", "positive", "positive", "positive",
                     "positive", "positive", "positive", "positive",
                     "positive", "positive", "positive",
                     "negative", "negative", "negative", "negative",
                     "negative", "negative", "negative", "negative")
    )
    peak_groups <- data.frame(
        group_id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
                     16, 17, 18),
        polarity = c("positive", "positive", "positive", "positive",
                     "positive", "positive", "positive", "positive",
                     "positive", "positive", "positive", "positive",
                     "negative", "negative", "negative", "negative",
                     "negative", "negative"),
        mzmed = c(408.251325886321, 426.262123243348, 427.265526700411,
                  428.267904280982, 428.268229167555, 429.27060132947,
                  429.270256788594, 429.270493526998, 448.244170162955,
                  464.447379285654, 504.440256583886, 505.443684675365,
                  269.247969620981, 269.248194873815, 269.248075605062,
                  270.251251784179, 270.251620537443, 271.255056943515),
        mzmin = c(408.251325886321, 426.261913381751, 427.265348519813,
                  428.267835743959, 428.267896400125, 429.270341280206,
                  429.270175958258, 429.270493526998, 448.244170162955,
                  464.447304014051, 504.440032161331, 505.443534603,
                  269.247528218739, 269.248194873815, 269.248075605062,
                  270.25119624422, 270.251620537443, 271.255056943515),
        mzmax = c(408.251325886321, 426.262333104945, 427.265704881008,
                  428.268466489791, 428.268561934985, 429.270861378734,
                  429.270782294993, 429.270493526998, 448.244170162955,
                  464.447454557257, 504.44048100644, 505.44383474773,
                  269.248411023224, 269.248194873815, 269.248075605062,
                  270.251307324138, 270.251620537443, 271.255056943515),
        rtmed = c(286.278, 286.8085, 286.8085, 279.407, 259.3105, 259.8395,
                  301.616, 279.407, 286.807, 201.573, 201.3085, 201.044,
                  178.411, 207.703, 146.11, 146.797, 178.318, 178.318),
        rtmin = c(286.278, 286.807, 286.807, 278.875, 258.253, 258.253,
                  291.569, 279.407, 286.807, 197.973, 197.444, 197.444,
                  178.318, 207.703, 146.11, 146.538, 178.318, 178.318),
        rtmax = c(286.278, 286.81, 286.81, 291.569, 260.368, 261.426,
                  306.914, 279.407, 286.807, 205.173, 205.173, 204.644,
                  178.504, 207.703, 146.11, 147.056, 178.318, 178.318),
        npeaks = c(1, 2, 2, 3, 2, 2, 3, 1, 1, 2, 2, 2, 2, 1, 1, 2, 1, 1),
        `220221CCM_global__01_ssleu_filtered` = c(NA, 5, 4, 6, 7, 10, 12,
                                                  9, NA, 1, 2, 3, 25, 26,
                                                  NA, 24, NA, NA),
        `220221CCM_global__02_ssleu_filtered` = c(17, 20, 19, 21, 22, 16,
                                                  23, NA, 18, 14, 15, 13,
                                                  31, NA, 28, 27, 30, 29),
        check.names = FALSE
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
        instrument = "QTOF_XevoG2-S_R25000@200",
        cpd_classes = c("LPC", "Cer", "FA")
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

testthat::test_that("get annotations", {
    ann <- data.frame(
        group_id = c(11, 10, 13, 2, 1, 9),
        class = c(rep("Cer", 2), "FA", rep("LPC", 3)),
        name = c("Cer (d18:1/C12:0)", "Cer (d18:1/C12:0)", "FA 17:0",
                 "LPC 11:0", "LPC 11:0", "LPC 11:0"),
        formula = c("C30H59N1O3", "C30H59N1O3", "C17H34O2", "C19H40N1O7P1",
                    "C19H40N1O7P1", "C19H40N1O7P1"),
        adduct = c("[M+Na]+", "[M+H-H2O]+", "[M-H]-", "[M+H]+",
                   "[M+H-H2O]+", "[M+Na]+"),
        ion_formula = c("C30H59N1O3Na1", "C30H58N1O2", "C17H33O2",
                        "C19H41N1O7P1", "C19H39N1O6P1", "C19H40N1O7P1Na1"),
        rtdiff = c(5.70849999999999, 5.97300000000001, 2.18899999999999,
                   8.99149999999997, 9.52199999999993, 8.99299999999994),
        rt = c(201.3085, 201.573, 178.411, 286.8085, 286.278, 286.807),
        rtmin = c(186.2245, 199.712, 175.3665, 284.6935, 284.692, 282.048),
        rtmax = c(207.034, 203.673, 180.985, 292.0965, 287.864, 292.095),
        nsamples = c(2, 2, 2, 2, 1, 1),
        best_score = c(89.4345550537109, 71.3979721069336, 99.5300903320312,
                       95.1391906738281, 79.8211975097656,
                       79.6432037353516),
        best_deviation_mz = c(0.00140380859375, 0.0010986328125,
                              -0.000111897788883653, 0.0008544921875,
                              0.0003662109375, 0.000701904296875),
        best_npeak = c(2, 1, 3, 2, 1, 1),
        `220221CCM_global__01_ssleu_filtered` = c(7, 5, 9, 2, NA, NA),
        `220221CCM_global__02_ssleu_filtered` = c(8, 6, 10, 3, 1, 4)
    )
    db <- db_connect(":memory:")
    dbWriteTable(db, "ann", ann)

    # 1st test : get all annotations
    testthat::expect_identical(
        db_get_annotations(db),
        ann
    )

    # 2nd test : get all annotations for a compound name
    testthat::expect_identical(
        db_get_annotations(db, names = "Cer (d18:1/C12:0)"),
        ann[ann$name == "Cer (d18:1/C12:0)", ]
    )

    # 3rd test : get all annotations for a group id
    testthat::expect_identical(
        db_get_annotations(db, group_ids = 11),
        ann[ann$group_id == 11, ]
    )

    # 4th test : get all annotations in positive
    testthat::expect_equal(
        db_get_annotations(db, polarity = "positive"),
        data.frame(ann[grepl("\\+$", ann$adduct), ], row.names = NULL)
    )

    # 5th test : get all annotations for a compound name and
    # only positive adduct
    testthat::expect_identical(
        db_get_annotations(
            db,
            polarity = "positive",
            names = "Cer (d18:1/C12:0)"
        ),
        data.frame(ann[which(
            ann$name == "Cer (d18:1/C12:0)" &
                grepl("\\+$", ann$adduct)), ], row.names = NULL)
    )
    testthat::expect_identical(
        db_get_annotations(
            db,
            polarity = "positive",
            names = "FA 17:0"
        ),
        ann[0, ]
    )

    # 6th test : get all annotations for a group id and
    # only positive adduct
    testthat::expect_identical(
        db_get_annotations(
            db,
            polarity = "positive",
            group_ids = 11
        ),
        data.frame(ann[which(
            ann$group_id == 11 &
                grepl("\\+$", ann$adduct)), ], row.names = NULL)
    )
    testthat::expect_identical(
        db_get_annotations(
            db,
            polarity = "positive",
            group_ids = 13
        ),
        ann[0, ]
    )

    # 7th test : get all annotations in negative
    testthat::expect_equal(
        db_get_annotations(db, polarity = "negative"),
        data.frame(ann[grepl("\\-$", ann$adduct), ], row.names = NULL)
    )

    # 8th test : get all annotations for a compound name and
    # only negative adduct
    testthat::expect_identical(
        db_get_annotations(
            db,
            polarity = "negative",
            names = "Cer (d18:1/C12:0)"
        ),
        ann[0, ]
    )
    testthat::expect_identical(
        db_get_annotations(
            db,
            polarity = "negative",
            names = "FA 17:0"
        ),
        data.frame(ann[which(
            ann$name == "FA 17:0" &
                grepl("\\-$", ann$adduct)), ], row.names = NULL)
    )

    # 9th test : get all annotations for a group id and
    # only positive adduct
    testthat::expect_identical(
        db_get_annotations(
            db,
            polarity = "negative",
            group_ids = 11
        ),
        ann[0, ]
    )
    testthat::expect_identical(
        db_get_annotations(
            db,
            polarity = "negative",
            group_ids = 13
        ),
        data.frame(ann[which(
            ann$group_id == 13 &
                grepl("\\-$", ann$adduct)), ], row.names = NULL)
    )
    RSQLite::dbDisconnect(db)
})

testthat::test_that("get spectra infos", {
    spectra_infos <- data.frame(
        spectra_id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
        score = c(79.8211975097656, 94.9317855834961, 95.1391906738281,
                  79.6432037353516, 71.3979721069336, 71.3979721069336,
                  89.4345550537109, 88.4888916015625, 82.740364074707,
                  99.5300903320312),
        deviation_mz = c(0.0003662109375, 0.000457763671875,
                         0.0008544921875, 0.000701904296875,
                         0.0010986328125, 0.001251220703125,
                         0.00140380859375, 0.0017852783203125,
                         -0.001068115234375, -0.000111897788883653),
        npeak = c(1, 2, 2, 1, 1, 1, 2, 2, 1, 3),
        basepeak_int = c(88824.635233072, 6214416.44108707,
                         6201250.27168528, 288290.748778874,
                         4945601.93026269, 5689144.27927454,
                         1448292.29379181, 1662831.19267642,
                         8927664.85468527, 4593482.66518999),
        sum_int = c(88824.635233072, 7385056.39979801, 7388017.8361341,
                    288290.748778874, 4945601.93026269, 5689144.27927454,
                    1849363.52087931, 2106914.27998355, 8927664.85468527,
                    5580653.54818653),
        sample = c("220221CCM_global__02_ssleu_filtered",
                   "220221CCM_global__01_ssleu_filtered",
                   "220221CCM_global__02_ssleu_filtered",
                   "220221CCM_global__02_ssleu_filtered",
                   "220221CCM_global__01_ssleu_filtered",
                   "220221CCM_global__02_ssleu_filtered",
                   "220221CCM_global__01_ssleu_filtered",
                   "220221CCM_global__02_ssleu_filtered",
                   "220221CCM_global__01_ssleu_filtered",
                   "220221CCM_global__02_ssleu_filtered"),
        rt = c(286.278, 286.81, 286.807, 286.807, 197.973, 205.173, 197.444,
               205.173, 178.504, 178.318)
    )
    db <- db_connect(":memory:")
    dbWriteTable(db, "spectra_infos", spectra_infos)
    testthat::expect_identical(
        db_get_spectra_infos(db, c(1, 2)),
        spectra_infos[1:2, ]
    )
    testthat::expect_identical(
        db_get_spectra_infos(db),
        spectra_infos
    )
    RSQLite::dbDisconnect(db)
})

testthat::test_that("get spectras", {
    spectras <- data.frame(
        spectra_id = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5,
                       5, 5, 6, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8, 9, 9, 9,
                       10, 10, 10),
        feature_id = c(17, NA, NA, NA, 4, NA, NA, 5, 19, NA, NA, 20, NA, NA,
                       NA, 18, 1, NA, NA, NA, 14, NA, NA, NA, 2, 3, NA, NA,
                       15, 13, NA, NA, 25, NA, NA, 31, 30, 29),
        mz = c(408.251325886321, NA, NA, NA, 427.265348519813, NA, NA,
               426.261913381751, 427.265704881008, NA, NA, 426.262333104945,
               NA, NA, NA, 448.244170162955, 464.447304014051, NA, NA, NA,
               464.447454557257, NA, NA, NA, 504.440032161331,
               505.443534603, NA, NA, 504.44048100644, 505.44383474773, NA,
               NA, 269.247528218739, NA, NA, 269.248411023224,
               270.251620537443, 271.255056943515),
        int = c(88824.635233072, NA, NA, NA, 1170639.95871094, NA, NA,
                6214416.44108707, 1186767.56444882, NA, NA,
                6201250.27168528, NA, NA, NA, 288290.748778874,
                4945601.93026269, NA, NA, NA, 5689144.27927454, NA, NA, NA,
                1448292.29379181, 401071.227087501, NA, NA,
                1662831.19267642, 444083.087307128, NA, NA,
                8927664.85468527, NA, NA, 4593482.66518999,
                899482.748384234, 87688.1346123047),
        abd = c(100, NA, NA, NA, 18.8374881182917, NA, NA, 100,
                19.1375531135642, NA, NA, 100, NA, NA, NA, 100, 100, NA, NA,
                NA, 100, NA, NA, NA, 100, 27.6926991054717, NA, NA, 100,
                26.7064443620612, NA, NA, 100, NA, NA, 100,
                19.5817164000777, 1.90896844515855),
        ion_id_theo = c(62, 62, 62, 62, 66, 66, 66, 66, 66, 66, 66, 66, 64,
                        64, 64, 64, 278, 278, 278, 278, 278, 278, 278, 278,
                        281, 281, 281, 281, 281, 281, 281, 281, 8, 8, 8, 8,
                        8, 8),
        mz_theo = c(408.25095, 409.25427, 410.25672, 411.25935, 427.26484,
                    428.26719, 429.26984, 426.26152, 427.26484, 428.26719,
                    429.26984, 426.26152, 449.24678, 450.24914, 451.25178,
                    448.24346, 464.44621, 465.44955, 466.45272, 467.45576,
                    464.44621, 465.44955, 466.45272, 467.45576, 504.43872,
                    505.44206, 506.44516, 507.44809, 504.43872, 505.44206,
                    506.44516, 507.44809, 269.2486, 270.25202, 271.25481,
                    269.2486, 270.25202, 271.25481),
        abd_theo = c(100, 21.65, 3.25, 0.38, 21.7, 3.46, 0.42, 100, 21.7,
                     3.46, 0.42, 100, 21.69, 3.45, 0.42, 100, 100, 33.55,
                     5.86, 0.65, 100, 33.55, 5.86, 0.65, 100, 33.67, 6.07,
                     0.72, 100, 33.67, 6.07, 0.72, 100, 18.84, 2.02, 100,
                     18.84, 2.02),
        iso_theo = c("M", "M+1", "M+2", "M+3", "M+1", "M+2", "M+3", "M",
                     "M+1", "M+2", "M+3", "M", "M+1", "M+2", "M+3", "M",
                     "M", "M+1", "M+2", "M+3", "M", "M+1", "M+2", "M+3",
                     "M", "M+1", "M+2", "M+3", "M", "M+1", "M+2", "M+3",
                     "M", "M+1", "M+2", "M", "M+1", "M+2")
    )
    db <- db_connect(":memory:")
    dbWriteTable(db, "spectras", spectras)
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
        instrument = "QTOF_XevoG2-S_R25000@200",
        cpd_classes = c("LPC", "Cer", "FA")
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
    peaks <- data.frame(
        feature_id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
                       16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28,
                       29, 30, 31),
        mz = c(464.447304014051, 504.440032161331, 505.443534603,
               427.265348519813, 426.261913381751, 428.267835743959,
               428.267896400125, 428.267904280982, 429.270493526998,
               429.270341280206, 429.270256788594, 429.270175958258,
               505.44383474773, 464.447454557257, 504.44048100644,
               429.270861378734, 408.251325886321, 448.244170162955,
               427.265704881008, 426.262333104945, 428.268466489791,
               428.268561934985, 429.270782294993, 270.25119624422,
               269.247528218739, 269.248194873815, 270.251307324138,
               269.248075605062, 271.255056943515, 270.251620537443,
               269.248411023224),
        mzmin = c(464.447021484375, 504.439697265625, 505.443084716797,
                  427.264587402344, 426.261444091797, 428.267547607422,
                  428.267425537109, 428.267456054688, 429.270263671875,
                  429.269653320312, 429.269836425781, 429.269622802734,
                  505.440704345703, 464.446746826172, 504.439544677734,
                  429.270660400391, 408.251037597656, 448.242279052734,
                  427.265258789062, 426.262023925781, 428.268035888672,
                  428.267822265625, 429.270416259766, 270.250793457031,
                  269.247406005859, 269.24755859375, 270.250823974609,
                  269.247589111328, 271.254425048828, 270.251403808594,
                  269.247833251953),
        mzmax = c(464.447662353516, 504.440490722656, 505.443664550781,
                  427.266052246094, 426.262420654297, 428.268310546875,
                  428.268615722656, 428.268249511719, 429.270660400391,
                  429.270904541016, 429.270751953125, 429.270874023438,
                  505.444122314453, 464.447967529297, 504.441101074219,
                  429.271148681641, 408.25146484375, 448.245422363281,
                  427.266052246094, 426.262786865234, 428.269195556641,
                  428.269287109375, 429.271636962891, 270.251953125,
                  269.247985839844, 269.249572753906, 270.25244140625,
                  269.248596191406, 271.256134033203, 270.251831054688,
                  269.248565673828),
        rt = c(197.973, 197.444, 197.444, 286.81, 286.81, 279.407, 260.368,
               291.569, 279.407, 258.253, 291.569, 301.616, 204.644,
               205.173, 205.173, 261.426, 286.278, 286.807, 286.807,
               286.807, 278.875, 258.253, 306.914, 147.056, 178.504,
               207.703, 146.538, 146.11, 178.318, 178.318, 178.318),
        rtmin = c(196.386, 194.271, 196.386, 284.695, 284.695, 275.706,
                  232.343, 287.339, 275.706, 232.343, 287.339, 298.443,
                  203.577, 203.038, 178.178, 258.253, 284.692, 282.048,
                  283.105, 284.692, 266.184, 232.343, 301.084, 144.809,
                  175.697, 203.298, 144.81, 144.81, 175.036, 175.036,
                  175.036),
        rtmax = c(200.617, 199.03, 199.03, 291.04, 292.098, 287.339,
                  261.426, 295.799, 286.81, 265.656, 296.328, 305.847,
                  207.757, 206.729, 215.038, 264.069, 287.864, 292.095,
                  292.095, 292.095, 292.095, 265.656, 309.559, 149.306,
                  180.384, 212.035, 150.968, 150.968, 179.727, 181.586,
                  181.586),
        int = c(4945601.93026269, 1448292.29379181, 401071.227087501,
                1170639.95871094, 6214416.44108707, 10859267.7547045,
                15396334.0742375, 3559573.81687499, 2574017.18247619,
                4567668.49476563, 847985.874378674, 392347.130863636,
                444083.087307128, 5689144.27927454, 1662831.19267642,
                801559.236409095, 88824.635233072, 288290.748778874,
                1186767.56444882, 6201250.27168528, 20076653.1797449,
                18139587.026625, 323001.699462891, 15852.5738923645,
                8927664.85468527, 18758.2195488281, 18160.7346019531,
                136136.373335937, 87688.1346123047, 899482.748384234,
                4593482.66518999),
        intb = c(4945464.15722767, 1448264.72345856, 401069.111887501,
                 1170634.14246094, 6212404.90921921, 10279271.7340996,
                 14009363.2233988, 3181331.80055223, 2490170.67943366,
                 4327555.39189945, 790817.501274091, 248786.599772727,
                 444001.578713656, 5689141.10698883, 1662613.35060178,
                 801553.420409095, 88821.9918997387, 287904.836179229,
                 1185632.54587715, 6201242.86868528, 18364665.4121469,
                 15982556.4399262, 209052.408698731, 13737.5420380623,
                 8654644.93987011, 12054.5931199951, 15262.9337143676,
                 118521.494192171, 75981.4533509554, 886826.38870081,
                 4540712.99687556),
        maxo = c(4130684, 1395886, 434808.75, 676902.5, 3501824, 1096165,
                 592012.5, 533287, 266683.75, 168154.5, 123908.25,
                 84350.125, 524308, 5734716, 1723138, 150635.375,
                 70041.5625, 186253.875, 673140, 3489368, 1078367, 657937,
                 68280.5, 8709.96875, 6265286.5, 2761.9111328125,
                 8407.2734375, 71703.6875, 53069.5625, 734161, 3772552),
        sn = c(8329, 22672, 434808, 676902, 3587, 10, 7, 4, 12, 7, 5, 1,
               4386, 5734715, 14423, 145000, 70041, 675, 1134, 3489367, 9,
               5, 1, 13, 15, 3, 11, 11, 3, 109, 298),
        egauss = as.numeric(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                              NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                              NA, NA, NA, NA, NA, NA, NA, NA, NA)),
        mu = as.numeric(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                          NA, NA, NA, NA, NA, NA, NA, NA, NA)),
        sigma = as.numeric(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                             NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                             NA, NA, NA, NA, NA, NA, NA, NA, NA)),
        h = as.numeric(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA)),
        f = c(1, 2, 3, 4, 6, 7, 7, 7, 8, 8, 8, 11, 1, 2, 3, 4, 5, 6, 7, 8,
              9, 9, 14, 1, 2, 3, 1, 2, 6, 7, 8),
        dppm = c(1, 2, 1, 2, 1, 2, 1, 0, 1, 1, 1, 2, 7, 2, 3, 1, 1, 2, 1, 1,
                 0, 1, 3, 3, 2, 4, 6, 3, 6, 2, 1),
        scale = c(3, 3, 3, 5, 5, 3, 15, 5, 3, 13, 5, 3, 3, 13, 19, 3, 3, 5,
                  5, 5, 11, 13, 5, 2, 2, 2, 3, 3, 3, 3, 3),
        scpos = c(58, 57, 57, 187, 187, 173, 137, 196, 173, 133, 196, 215,
                  57, 58, 58, 136, 183, 184, 184, 184, 169, 130, 222, 5, 44,
                  75, 5, 4, 52, 52, 52),
        scmin = c(55, 54, 54, 182, 182, 170, 122, 191, 170, 120, 191, 212,
                  54, 45, 39, 133, 180, 179, 179, 179, 158, 117, 217, 3, 42,
                  73, 2, 1, 49, 49, 49),
        scmax = c(61, 60, 60, 192, 192, 176, 152, 201, 176, 146, 201, 218,
                  60, 71, 77, 139, 186, 189, 189, 189, 180, 143, 227, 7, 46,
                  77, 8, 7, 55, 55, 55),
        lmin = c(55, 52, 55, 87, 87, 134, 87, 156, 134, 87, 156, 85, 55, 54,
                 20, 101, 87, 82, 84, 87, 116, 87, 79, 1, 40, 66, 1, 1, 47,
                 47, 47),
        lmax = c(63, 60, 60, 99, 101, 156, 107, 172, 155, 115, 173, 96, 63,
                 61, 77, 112, 93, 101, 101, 101, 165, 115, 95, 9, 48, 74,
                 11, 11, 55, 58, 58),
        sample = c("220221CCM_global__01_ssleu_filtered",
                   "220221CCM_global__01_ssleu_filtered",
                   "220221CCM_global__01_ssleu_filtered",
                   "220221CCM_global__01_ssleu_filtered",
                   "220221CCM_global__01_ssleu_filtered",
                   "220221CCM_global__01_ssleu_filtered",
                   "220221CCM_global__01_ssleu_filtered",
                   "220221CCM_global__01_ssleu_filtered",
                   "220221CCM_global__01_ssleu_filtered",
                   "220221CCM_global__01_ssleu_filtered",
                   "220221CCM_global__01_ssleu_filtered",
                   "220221CCM_global__01_ssleu_filtered",
                   "220221CCM_global__02_ssleu_filtered",
                   "220221CCM_global__02_ssleu_filtered",
                   "220221CCM_global__02_ssleu_filtered",
                   "220221CCM_global__02_ssleu_filtered",
                   "220221CCM_global__02_ssleu_filtered",
                   "220221CCM_global__02_ssleu_filtered",
                   "220221CCM_global__02_ssleu_filtered",
                   "220221CCM_global__02_ssleu_filtered",
                   "220221CCM_global__02_ssleu_filtered",
                   "220221CCM_global__02_ssleu_filtered",
                   "220221CCM_global__02_ssleu_filtered",
                   "220221CCM_global__01_ssleu_filtered",
                   "220221CCM_global__01_ssleu_filtered",
                   "220221CCM_global__01_ssleu_filtered",
                   "220221CCM_global__02_ssleu_filtered",
                   "220221CCM_global__02_ssleu_filtered",
                   "220221CCM_global__02_ssleu_filtered",
                   "220221CCM_global__02_ssleu_filtered",
                   "220221CCM_global__02_ssleu_filtered"),
        polarity = c("positive", "positive", "positive", "positive",
                     "positive", "positive", "positive", "positive",
                     "positive", "positive", "positive", "positive",
                     "positive", "positive", "positive", "positive",
                     "positive", "positive", "positive", "positive",
                     "positive", "positive", "positive",
                     "negative", "negative", "negative", "negative",
                     "negative", "negative", "negative", "negative")
    )
    db <- db_connect(":memory:")
    dbWriteTable(db, "peaks", peaks)
    testthat::expect_identical(
        db_get_peaks(db, c(1, 2)),
        peaks[1:2, ]
    )
    testthat::expect_identical(
        db_get_peaks(db),
        peaks
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
        group_id = c(6, 6, 7, 1, 2, 5),
        class = c(rep("Cer", 3), rep("LPC", 3)),
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
    db <- db_connect(":memory:")
    dbWriteTable(db, "ann", ann)
    db_resolve_conflict(db, ann[2, "group_id"], ann[2, "name"])
    ann2 <- ann[-1, ]
    rownames(ann2) <- seq(nrow(ann2))
    testthat::expect_identical(
        dbReadTable(db, "ann"),
        ann2
    )
    RSQLite::dbDisconnect(db)
})
