testthat::test_that("annotate peaks", {
    ann_params <- AnnotationParam(
        da_tol = .015,
        rt_tol = 10,
        abd_tol = 25,
        adduct_names = c(
            "[M+Na]+",
            "[M+NH4]+",
            "[M+H-H2O]+",
            "[M+H]+"
        ),
        instrument = "QTOF_XevoG2-S_R25000@200"
    )
    xset <- methods::new("xcmsSet")
    sample_names <- c("220221CCM_global_POS_01_ssleu_filtered",
                      "220221CCM_global_POS_02_ssleu_filtered",
                      "220221CCM_global_POS_03_ssleu_filtered")
    xset@phenoData <- data.frame(
        class = rep(runif(1), 3),
        row.names = sample_names
    )

    # 1st test: with no peaks
    xset <- annotate_peaklists(xset, ann_params)
    testthat::expect_equal(
        xset@ann,
        data.frame(
            matrix(, nrow = 0, ncol = 13 + length(sample_names),
                   dimnames = list(
                c(), c("group_id", "name", "formula", "adduct", "ion_formula",
                       "rtdiff", "rt", "rtmin", "rtmax", "nsamples",
                       "best_score", "best_deviation_mz", "best_npeak",
                       sample_names))
            ),
            check.names = FALSE
        )
    )
    testthat::expect_equal(
        xset@spectra_infos,
        data.frame(matrix(, nrow = 0, ncol = 8, dimnames = list(
            c(), c("spectra_id", "score", "deviation_mz", "npeak",
                   "basepeak_int", "sum_int", "sample", "rt")
        )))
    )
    testthat::expect_equal(
        xset@spectras,
        data.frame(matrix(, nrow = 0, ncol = 9, dimnames = list(
            c(), c("spectra_id", "feature_id", "mz", "int", "abd",
                   "ion_id_theo", "mz_theo", "abd_theo", "iso_theo")
        )))
    )

    # 2nd test: wrong adducts
    xset@groupidx <- list(
        17,
        c(5, 20),
        c(4, 19),
        c(6, 8, 21),
        c(7, 22),
        c(10, 16),
        c(11, 12, 23),
        9,
        18,
        c(1, 14),
        c(2, 15),
        c(3, 13)
    )
    xset@groups <- matrix(
        c(408.251325886321, 426.262123243348, 427.265526700411,
          428.267904280982, 428.268229167555, 429.27060132947, 429.270256788594,
          429.270493526998, 448.244170162955, 464.447379285654,
          504.440256583886, 505.443684675365, 408.251325886321,
          426.261913381751, 427.265348519813, 428.267835743959,
          428.267896400125, 429.270341280206, 429.270175958258,
          429.270493526998, 448.244170162955, 464.447304014051,
          504.440032161331, 505.443534603, 408.251325886321, 426.262333104945,
          427.265704881008, 428.268466489791, 428.268561934985,
          429.270861378734, 429.270782294993, 429.270493526998,
          448.244170162955, 464.447454557257, 504.44048100644, 505.44383474773,
          286.278, 286.8085, 286.8085, 279.407, 259.3105, 259.8395, 301.616,
          279.407, 286.807, 201.573, 201.3085, 201.044, 286.278, 286.807,
          286.807, 278.875, 258.253, 258.253, 291.569, 279.407, 286.807,
          197.973, 197.444, 197.444, 286.278, 286.81, 286.81, 291.569, 260.368,
          261.426, 306.914, 279.407, 286.807, 205.173, 205.173, 204.644, 1, 2,
          2, 3, 2, 2, 3, 1, 1, 2, 2, 2, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1,
          1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        nrow = 12, ncol = 10, dimnames = list(
            c(), c("mzmed", "mzmin", "mzmax", "rtmed", "rtmin", "rtmax",
                   "npeaks", "1", "2", "3")
        )
    )
    xset@peaks <- matrix(
        c(464.447304014051, 504.440032161331, 505.443534603,
          427.265348519813, 426.261913381751, 428.267835743959,
          428.267896400125, 428.267904280982, 429.270493526998,
          429.270341280206, 429.270256788594, 429.270175958258,
          505.44383474773, 464.447454557257, 504.44048100644,
          429.270861378734, 408.251325886321, 448.244170162955,
          427.265704881008, 426.262333104945, 428.268466489791,
          428.268561934985, 429.270782294993, 464.447021484375,
          504.439697265625, 505.443084716797, 427.264587402344,
          426.261444091797, 428.267547607422, 428.267425537109,
          428.267456054688, 429.270263671875, 429.269653320312,
          429.269836425781, 429.269622802734, 505.440704345703,
          464.446746826172, 504.439544677734, 429.270660400391,
          408.251037597656, 448.242279052734, 427.265258789062,
          426.262023925781, 428.268035888672, 428.267822265625,
          429.270416259766, 464.447662353516, 504.440490722656,
          505.443664550781, 427.266052246094, 426.262420654297,
          428.268310546875, 428.268615722656, 428.268249511719,
          429.270660400391, 429.270904541016, 429.270751953125,
          429.270874023438, 505.444122314453, 464.447967529297,
          504.441101074219, 429.271148681641, 408.25146484375,
          448.245422363281, 427.266052246094, 426.262786865234,
          428.269195556641, 428.269287109375, 429.271636962891, 197.973,
          197.444, 197.444, 286.81, 286.81, 279.407, 260.368, 291.569, 279.407,
          258.253, 291.569, 301.616, 204.644, 205.173, 205.173, 261.426,
          286.278, 286.807, 286.807, 286.807, 278.875, 258.253, 306.914,
          196.386, 194.271, 196.386, 284.695, 284.695, 275.706, 232.343,
          287.339, 275.706, 232.343, 287.339, 298.443, 203.577, 203.038,
          178.178, 258.253, 284.692, 282.048, 283.105, 284.692, 266.184,
          232.343, 301.084, 200.617, 199.03, 199.03, 291.04, 292.098, 287.339,
          261.426, 295.799, 286.81, 265.656, 296.328, 305.847, 207.757, 206.729,
          215.038, 264.069, 287.864, 292.095, 292.095, 292.095, 292.095,
          265.656, 309.559, 4945601.93026269, 1448292.29379181,
          401071.227087501, 1170639.95871094, 6214416.44108707,
          10859267.7547045, 15396334.0742375, 3559573.81687499,
          2574017.18247619, 4567668.49476563, 847985.874378674,
          392347.130863636, 444083.087307128, 5689144.27927454,
          1662831.19267642, 801559.236409095, 88824.635233072, 288290.748778874,
          1186767.56444882, 6201250.27168528, 20076653.1797449, 18139587.026625,
          323001.699462891, 4945464.15722767, 1448264.72345856,
          401069.111887501, 1170634.14246094, 6212404.90921921,
          10279271.7340996, 14009363.2233988, 3181331.80055223,
          2490170.67943366, 4327555.39189945, 790817.501274091,
          248786.599772727, 444001.578713656, 5689141.10698883,
          1662613.35060178, 801553.420409095, 88821.9918997387,
          287904.836179229, 1185632.54587715, 6201242.86868528,
          18364665.4121469, 15982556.4399262, 209052.408698731, 4130684,
          1395886, 434808.75, 676902.5, 3501824, 1096165, 592012.5, 533287,
          266683.75, 168154.5, 123908.25, 84350.125, 524308, 5734716, 1723138,
          150635.375, 70041.5625, 186253.875, 673140, 3489368, 1078367, 657937,
          68280.5, 8329, 22672, 434808, 676902, 3587, 10, 7, 4, 12, 7, 5, 1,
          4386, 5734715, 14423, 145000, 70041, 675, 1134, 3489367, 9, 5, 1, NA,
          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
          NA, NA, NA, NA, NA, NA, 1, 2, 3, 4, 6, 7, 7, 7, 8, 8, 8, 11, 1, 2, 3,
          4, 5, 6, 7, 8, 9, 9, 14, 1, 2, 1, 2, 1, 2, 1, 0, 1, 1, 1, 2, 7, 2, 3,
          1, 1, 2, 1, 1, 0, 1, 3, 3, 3, 3, 5, 5, 3, 15, 5, 3, 13, 5, 3, 3, 13,
          19, 3, 3, 5, 5, 5, 11, 13, 5, 58, 57, 57, 187, 187, 173, 137, 196,
          173, 133, 196, 215, 57, 58, 58, 136, 183, 184, 184, 184, 169, 130,
          222, 55, 54, 54, 182, 182, 170, 122, 191, 170, 120, 191, 212, 54, 45,
          39, 133, 180, 179, 179, 179, 158, 117, 217, 61, 60, 60, 192, 192, 176,
          152, 201, 176, 146, 201, 218, 60, 71, 77, 139, 186, 189, 189, 189,
          180, 143, 227, 55, 52, 55, 87, 87, 134, 87, 156, 134, 87, 156, 85, 55,
          54, 20, 101, 87, 82, 84, 87, 116, 87, 79, 63, 60, 60, 99, 101, 156,
          107, 172, 155, 115, 173, 96, 63, 61, 77, 112, 93, 101, 101, 101, 165,
          115, 95, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2,
          2, 2, 2),
        nrow = 23, ncol = 23, dimnames = list(
            c(), c("mz", "mzmin", "mzmax", "rt", "rtmin", "rtmax", "into",
                   "intb", "maxo", "sn", "egauss", "mu", "sigma", "h", "f",
                   "dppm", "scale", "scpos", "scmin", "scmax", "lmin", "lmax",
                   "sample")
        )
    )
    xset <- annotate_peaklists(
        xset,
        restrict_adducts_polarity(ann_params, "negative")
    )
    testthat::expect_equal(
        xset@ann,
        data.frame(
            matrix(, nrow = 0, ncol = 13 + length(sample_names),
                   dimnames = list(
                       c(), c("group_id", "name", "formula", "adduct",
                              "ion_formula", "rtdiff", "rt", "rtmin", "rtmax",
                              "nsamples", "best_score", "best_deviation_mz",
                              "best_npeak", sample_names))
            ),
            check.names = FALSE
        )
    )
    testthat::expect_equal(
        xset@spectra_infos,
        data.frame(matrix(, nrow = 0, ncol = 8, dimnames = list(
            c(), c("spectra_id", "score", "deviation_mz", "npeak",
                   "basepeak_int", "sum_int", "sample", "rt")
        )))
    )
    testthat::expect_equal(
        xset@spectras,
        data.frame(matrix(, nrow = 0, ncol = 9, dimnames = list(
            c(), c("spectra_id", "feature_id", "mz", "int", "abd",
                   "ion_id_theo", "mz_theo", "abd_theo", "iso_theo")
        )))
    )

    # 3rd test: no hits
    ann_params_no_hits <- ann_params
    ann_params_no_hits@da_tol <- 10**-9
    xset <- annotate_peaklists(xset, ann_params_no_hits)
    testthat::expect_equal(
        xset@ann,
        data.frame(
            matrix(, nrow = 0, ncol = 13 + length(sample_names),
                   dimnames = list(
                       c(), c("group_id", "name", "formula", "adduct",
                              "ion_formula", "rtdiff", "rt", "rtmin", "rtmax",
                              "nsamples", "best_score", "best_deviation_mz",
                              "best_npeak", sample_names))
            ),
            check.names = FALSE
        )
    )
    testthat::expect_equal(
        xset@spectra_infos,
        data.frame(matrix(, nrow = 0, ncol = 8, dimnames = list(
            c(), c("spectra_id", "score", "deviation_mz", "npeak",
                   "basepeak_int", "sum_int", "sample", "rt")
        )))
    )
    testthat::expect_equal(
        xset@spectras,
        data.frame(matrix(, nrow = 0, ncol = 9, dimnames = list(
            c(), c("spectra_id", "feature_id", "mz", "int", "abd",
                   "ion_id_theo", "mz_theo", "abd_theo", "iso_theo")
        )))
    )

    # 4th test: normal
    xset <- annotate_peaklists(xset, ann_params)
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
            `220221CCM_global_POS_02_ssleu_filtered` = c(1, 3, 4, 6, 8),
            `220221CCM_global_POS_03_ssleu_filtered` = c(NA, NA, NA, NA, NA),
            check.names = FALSE
        )
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
        )
    )
    testthat::expect_equal(
        xset@spectras,
        data.frame(
            spectra_id = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5,
                           5, 5, 6, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8),
            feature_id = c(NA, NA, 17, NA, 5, 4, NA, NA, 20, 19, NA, NA, NA, NA,
                           NA, 18, 1, NA, NA, NA, 14, NA, NA, NA, 3, 2, NA, NA,
                           13, 15, NA, NA),
            mz = c(NA, NA, 408.251325886321, NA, 426.261913381751,
                   427.265348519813, NA, NA, 426.262333104945, 427.265704881008,
                   NA, NA, NA, NA, NA, 448.244170162955, 464.447304014051, NA,
                   NA, NA, 464.447454557257, NA, NA, NA, 505.443534603,
                   504.440032161331, NA, NA, 505.44383474773, 504.44048100644,
                   NA, NA),
            int = c(NA, NA, 88824.635233072, NA, 6214416.44108707,
                    1170639.95871094, NA, NA, 6201250.27168528,
                    1186767.56444882, NA, NA, NA, NA, NA, 288290.748778874,
                    4945601.93026269, NA, NA, NA, 5689144.27927454, NA, NA, NA,
                    401071.227087501, 1448292.29379181, NA, NA,
                    444083.087307128, 1662831.19267642, NA, NA),
            abd = c(NA, NA, 100, NA, 100, 18.8374881182917, NA, NA, 100,
                    19.1375531135642, NA, NA, NA, NA, NA, 100, 100, NA, NA, NA,
                    100, NA, NA, NA, 27.6926991054717, 100, NA, NA,
                    26.7064443620612, 100, NA, NA),
            ion_id_theo = c(62, 62, 62, 62, 66, 66, 66, 66, 66, 66, 66, 66, 64,
                            64, 64, 64, 278, 278, 278, 278, 278, 278, 278, 278,
                            281, 281, 281, 281, 281, 281, 281, 281),
            mz_theo = c(411.25935, 409.25427, 408.25095, 410.25672, 426.26152,
                        427.26484, 428.26719, 429.26984, 426.26152, 427.26484,
                        428.26719, 429.26984, 449.24678, 450.24914, 451.25178,
                        448.24346, 464.44621, 465.44955, 466.45272, 467.45576,
                        464.44621, 465.44955, 466.45272, 467.45576, 505.44206,
                        504.43872, 506.44516, 507.44809, 505.44206, 504.43872,
                        506.44516, 507.44809),
            abd_theo = c(0.38, 21.65, 100, 3.25, 100, 21.7, 3.46, 0.42, 100,
                         21.7, 3.46, 0.42, 21.69, 3.45, 0.42, 100, 100, 33.55,
                         5.86, 0.65, 100, 33.55, 5.86, 0.65, 33.67, 100, 6.07,
                         0.72, 33.67, 100, 6.07, 0.72),
            iso_theo = c("M+3", "M+1", "M", "M+2", "M", "M+1", "M+2", "M+3",
                         "M", "M+1", "M+2", "M+3", "M+1", "M+2", "M+3", "M",
                         "M", "M+1", "M+2", "M+3", "M", "M+1", "M+2", "M+3",
                         "M+1", "M", "M+2", "M+3", "M+1", "M", "M+2", "M+3")
        )
    )
})

testthat::test_that("filtrate annotations", {
    sample_names <- c("220221CCM_global_POS_01_ssleu_filtered",
                      "220221CCM_global_POS_02_ssleu_filtered",
                      "220221CCM_global_POS_03_ssleu_filtered")
    empty_ann <- data.frame(
        matrix(, nrow = 0, ncol = 13 + length(sample_names),
               dimnames = list(
                   c(), c("group_id", "name", "formula", "adduct",
                          "ion_formula", "rtdiff", "rt", "rtmin", "rtmax",
                          "nsamples", "best_score", "best_deviation_mz",
                          "best_npeak", sample_names))
        ),
        check.names = FALSE
    )
    testthat::expect_equal(
        filtrate_ann(empty_ann, data.frame()),
        empty_ann
    )

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
        `220221CCM_global_POS_02_ssleu_filtered` = 1,
        `220221CCM_global_POS_03_ssleu_filtered` = NA,
        check.names = FALSE
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
        ann
    )

    # check if compound see with two adducts which respect fwhm & not
    # in theory it will be the [M+H]+ used as reference
    # the [M+Na]+ will be rejected cause the [M+H]+ is at 286 sec & the [M+Na]+
        # at 2860 sec
    # the [M+H-H2O]+ is kept cause the rT is at 286 sec
    ann <- data.frame(
        group_id = c(2, 1, 9),
        name = c("LPC 11:0", "LPC 11:0", "LPC 11:0"),
        formula = c("C19H40N1O7P1", "C19H40N1O7P1", "C19H40N1O7P1"),
        adduct = c("[M+H]+", "[M+H-H2O]+", "[M+Na]+"),
        ion_formula = c("C19H41N1O7P1", "C19H39N1O6P1", "C19H40N1O7P1Na1"),
        rtdiff = c(8.99149999999997, 9.52199999999993, 8.99299999999994),
        rt = c(286.8085, 286.278, 2860.807),
        rtmin = c(284.6935, 284.692, 282.048),
        rtmax = c(292.0965, 287.864, 292.095),
        nsamples = c(2, 1, 1),
        best_score = c(95.1391906738281, 79.8211975097656,
                       79.6432037353516),
        best_deviation_mz = c(0.0008544921875, 0.0003662109375,
                              0.000701904296875),
        best_npeak = c(2, 1, 1),
        `220221CCM_global_POS_01_ssleu_filtered` = c(2, NA, NA),
        `220221CCM_global_POS_02_ssleu_filtered` = c(3, 1, 4),
        `220221CCM_global_POS_03_ssleu_filtered` = c(NA, NA, NA),
        check.names = FALSE
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
        ann[1:2, ]
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
                `220221CCM_global_POS_02_ssleu_filtered` = c(NA, 3, 4),
                `220221CCM_global_POS_03_ssleu_filtered` = c(NA, NA, NA)
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
            `220221CCM_global_POS_02_ssleu_filtered` = c(3, 4),
            `220221CCM_global_POS_03_ssleu_filtered` = as.numeric(c(NA, NA))
        )
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
                `220221CCM_global_POS_02_ssleu_filtered` = c(4, 3, 5),
                `220221CCM_global_POS_03_ssleu_filtered` = c(NA, NA, NA)
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
            `220221CCM_global_POS_02_ssleu_filtered` = c(3, 5),
            `220221CCM_global_POS_03_ssleu_filtered` = as.numeric(c(NA, NA))
        )
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
        `220221CCM_global_POS_02_ssleu_filtered` = c(3, 3, 6, 8),
        `220221CCM_global_POS_03_ssleu_filtered` = c(NA, NA, NA, NA)
    )
    testthat::expect_equal(
        split_conflicts(ann[0, ]),
        list(
            no_conflicts = ann[0, ],
            conflicts = list()
        )
    )
    testthat::expect_equal(
        split_conflicts(ann[3:4, ]),
        list(
            no_conflicts = ann[3:4, ],
            conflicts = list()
        )
    )
    testthat::expect_equal(
        split_conflicts(ann[1:2, ]),
        list(
            no_conflicts = ann[0, ],
            conflicts = list(
                ann[1:2, ]
            )
        )
    )
    testthat::expect_equal(
        split_conflicts(ann),
        list(
            no_conflicts = ann[3:4, ],
            conflicts = list(
                ann[1:2, ]
            )
        )
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
                `220221CCM_global_POS_02_ssleu_filtered` = 1,
                `220221CCM_global_POS_03_ssleu_filtered` = NA,
                check.names = FALSE
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
            `220221CCM_global_POS_01_ssleu_filtered` = as.numeric(NA),
            `220221CCM_global_POS_02_ssleu_filtered` = 88824.635233072,
            `220221CCM_global_POS_03_ssleu_filtered` = as.numeric(NA),
            check.names = FALSE
        )
    )
})

testthat::test_that("summarise ann df", {
    ann <- data.frame(
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
        `220221CCM_global_POS_02_ssleu_filtered` = c(1, 3),
        `220221CCM_global_POS_03_ssleu_filtered` = c(NA, NA),
        check.names = FALSE
    )
    spectra_infos <- data.frame(
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

    testthat::expect_equal(
        summarise_ann(ann[0, ], spectras[0, ]),
        data.frame(matrix(, nrow = 0, ncol = 10,
            dimnames = list(c(),
                          c("name", "rT (min)", "Diff rT (sec)", "Adducts",
                            "nSamples", "Most intense ion", "Best score (%)",
                            "Best m/z dev (mDa)", "Max iso", "X")
            )
        ), check.names = FALSE)
    )

    testthat::expect_equal(
        summarise_ann(ann[1, ], spectra_infos[1, ]),
        data.frame(
            name = "LPC 11:0",
            `rT (min)` = 4.77,
            `Diff rT (sec)` = 10,
            Adducts = "[M+H-H2O]+",
            nSamples = 1,
            `Most intense ion` = as.factor("[M+H-H2O]+"),
            `Best score (%)` = 80,
            `Best m/z dev (mDa)` = 0,
            `Max iso` = 1,
            `220221CCM_global_POS_01_ssleu_filtered` = 0,
            `220221CCM_global_POS_02_ssleu_filtered` = 88824.635233072,
            `220221CCM_global_POS_03_ssleu_filtered` = 0,
            check.names = FALSE
        )
    )

    testthat::expect_equal(
        summarise_ann(ann, spectra_infos),
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
            `220221CCM_global_POS_01_ssleu_filtered` = 6214416.44108707,
            `220221CCM_global_POS_02_ssleu_filtered` = 6290074.90691835,
            `220221CCM_global_POS_03_ssleu_filtered` = 0,
            check.names = FALSE
        )
    )
})
