testthat::test_that("get_ions", {
    adduct <- adducts[which(adducts$Name == "[M-H]-"), ]
    instrument <- "QTOF_XevoG2-S_R25000@200"
    empty_df_ions <- data.frame(matrix(, nrow = 0, ncol = 6, dimnames = list(
        c(), c("formula", "adduct", "ion_formula", "charge", "mz", "abd")
    )))

    # should return nothing cause the formula C2N2 don't contain any H
    testthat::expect_identical(
        get_ions("C2N2", adduct, instrument),
        empty_df_ions
    )
    # should return nothing cause the m/z ion is out of the resolution list
    # of the instrument obtained from enviPat
    testthat::expect_identical(
        get_ions("C1H1", adduct, instrument),
        empty_df_ions
    )

    testthat::expect_identical(
        get_ions(
            c("C2N2", "C1H1", "C18H38N1O7P1", "C43H76O2"),
            adduct,
            instrument
        ),
        data.frame(
            formula = c(rep("C18H38N1O7P1", 4), rep("C43H76O2", 5)),
            adduct = rep("[M-H]-", 9),
            ion_formula = c(rep("C18H37N1O7P1", 4), rep("C43H75O2", 5)),
            charge = rep(-1, 9),
            mz = c(410.23131, 411.23463, 412.23692, 413.23958, 623.57726,
                   624.58066, 625.58398, 626.58705, 627.59067),
           abd = c(100, 20.58, 3.24, 0.39, 100, 47.45, 11.41, 1.76, 0.17),
           iso = c("M", "M+1", "M+2", "M+3", "M", "M+1", "M+2", "M+3", "M+4")
        )
    )
})

testthat::test_that("load_db", {
    testthat::expect_equal(
        load_db(
            c("[2M+H]+", "[M-H]-"),
            "QTOF_XevoG2-S_R25000@200",
            c("LPE 13:0", "CE 16:0")
        ),
        data.frame(
            formula = c(rep("C18H38N1O7P1", 9), rep("C43H76O2", 11)),
            name = c(rep("LPE 13:0", 9), rep("CE 16:0", 11)),
            rt = c(rep(372.6, 9), rep(49.2, 11)),
            ion_id = c(rep(2, 5), rep(1, 4), rep(4, 6), rep(3, 5)),
            adduct = c(rep("[2M+H]+", 5), rep("[M-H]-", 4),
                       rep("[2M+H]+", 6), rep("[M-H]-", 5)),
            ion_formula = c(rep("C36H77N2O14P2", 5), rep("C18H37N1O7P1", 4),
                            rep("C86H153O4", 6), rep("C43H75O2", 5)),
            charge = c(rep(1, 5), rep(-1, 4), rep(1, 6), rep(-1, 5)),
            mz = c(823.48445, 825.49047, 826.49316, 827.49541, 824.48777,
                   410.23131, 411.23463, 412.23692, 413.23958, 1250.17634,
                   1251.17975, 1252.18311, 1253.18643, 1254.1897, 1255.19269,
                   626.58705, 623.57726, 624.58066, 625.58398, 627.59067),
            abd = c(100, 11.11, 2.03, 0.21, 41.14, 100, 20.58, 3.24, 0.39, 100,
                    95, 45.43, 14.5, 3.5, 0.62, 1.76, 100, 47.45, 11.41, 0.17),
            iso = c("M", "M+2", "M+3", "M+4", "M+1", "M", "M+1", "M+2", "M+3",
                    "M", "M+1", "M+2", "M+3", "M+4", "M+5", "M+3", "M", "M+1",
                    "M+2", "M+4")
        )
    )
})

testthat::test_that("compare spectras", {
    l_spectras <- list(data.frame(
        mz = c(734.56943147, 735.57280138, 736.57581325, 737.57870281,
               738.5815316, 739.58374177),
        abd = c(100, 44.925872, 11.492671, 2.131437, 0.300167, 0.020691),
        iso = c("M", "M+1", "M+2", "M+3", "M+4", "M+5")
    ))

    # original spectra
    # should return a very good isotopic score
    q_spectra <- data.frame(
        mz = c(734.570997942433, 735.574556763962, 736.564411787157,
               737.56455162997, 738.614212665807),
        abd = c(100, 40.9469794525382, 8.37980024341253, 3.71016159721596,
                1.05998504396349)
    )
    testthat::expect_equal(
        compare_spectras(q_spectra, l_spectras),
        list(list(
            spectra = data.frame(
                mz = c(734.570997942433, 735.574556763962, 736.564411787157,
                       737.56455162997, 738.614212665807, NA),
                abd = c(100, 40.9469794525382, 8.37980024341253,
                        3.71016159721596, 1.05998504396349, NA),
                mz_theo = c(734.56943147, 735.57280138, 736.57581325,
                            737.57870281, 738.5815316, 739.58374177),
                abd_theo = c(100, 44.925872, 11.492671, 2.131437, 0.300167,
                             0.020691),
                iso_theo = c("M", "M+1", "M+2", "M+3", "M+4", "M+5"),
                row.names = c(as.character(1:5), "NA")
            ),
            score = 94.49513245,
            deviation_mz = 0.00208740239,
            npeak = 5
        ))
    )

    # without other isotopologues
    # should have a correct score, nothing more
    q_spectra <- data.frame(
            mz = c(734.570997942433),
            abd = c(100)
    )
    testthat::expect_equal(
        compare_spectras(q_spectra, l_spectras),
        list(list(
            spectra = data.frame(
                mz = c(734.570997942433, rep(NA, 5)),
                abd = c(100, rep(NA, 5)),
                mz_theo = c(734.56943147, 735.57280138, 736.57581325,
                            737.57870281, 738.5815316, 739.58374177),
                abd_theo = c(100, 44.925872, 11.492671, 2.131437, 0.300167,
                             0.020691),
                iso_theo = c("M", "M+1", "M+2", "M+3", "M+4", "M+5"),
                row.names = c(1, "NA", paste("NA", 1:4, sep = "."))
            ),
            score = 62.94421387,
            deviation_mz = 0.001525878906,
            npeak = 1
        ))
    )

    # with problem on abundance : M+1 too much intense (needs deisotoping)
    # should be quasi the same than the original spectra
    # the M+1 should paired with its corresponding theoretical M+1
    # only the score is a little impacted (in case of)
    q_spectra <- data.frame(
        mz = c(734.570997942433, 735.574556763962, 736.564411787157,
               737.56455162997, 738.614212665807),
        abd = c(100, 40.9469794525382, 128.37980024341253,
                3.71016159721596, 1.05998504396349)
    )
    testthat::expect_equal(
        compare_spectras(q_spectra, l_spectras),
        list(list(
            spectra = data.frame(
                mz = c(734.570997942433, 735.574556763962, 736.564411787157,
                       737.56455162997, 738.614212665807, NA),
                abd = c(100, 40.9469794525382, 128.379800243,
                        3.71016159721596, 1.05998504396349, NA),
                mz_theo = c(734.56943147, 735.57280138, 736.57581325,
                            737.57870281, 738.5815316, 739.58374177),
                abd_theo = c(100, 44.925872, 11.492671, 2.131437, 0.300167,
                             0.020691),
                iso_theo = c("M", "M+1", "M+2", "M+3", "M+4", "M+5"),
                row.names = c(as.character(1:5), "NA")
            ),
            score = 88.16189575,
            deviation_mz = 0.00208740239,
            npeak = 5
        ))
    )

    # huge mz deviation
    # it never impacted the isotopic score !
    # only the m/z deviation between observed & theoretical
    q_spectra <- data.frame(
        mz = c(734.570997942433, 735.614556763962, 736.564411787157,
               737.56455162997, 738.614212665807),
        abd = c(100, 40.9469794525382, 8.37980024341253, 3.71016159721596,
                1.05998504396349)
    )
    testthat::expect_equal(
        compare_spectras(q_spectra, l_spectras),
        list(list(
            spectra = data.frame(
                mz = c(734.570997942433, 735.614556763962, 736.564411787157,
                       737.56455162997, 738.614212665807, NA),
                abd = c(100, 40.9469794525382, 8.37980024341253,
                        3.71016159721596, 1.05998504396349, NA),
                mz_theo = c(734.56943147, 735.57280138, 736.57581325,
                            737.57870281, 738.5815316, 739.58374177),
                abd_theo = c(100, 44.925872, 11.492671, 2.131437, 0.300167,
                             0.020691),
                iso_theo = c("M", "M+1", "M+2", "M+3", "M+4", "M+5"),
                row.names = c(as.character(1:5), "NA")
            ),
            score = 94.49513245,
            deviation_mz = 0.01008300763,
            npeak = 5
        ))
    )

    # missing A+1 (2nd most abundant)
    # the pairing should stop directly at the M : even if we have the M+2
        # it has no sens to pair it since the M+1 is not founded
    q_spectra <- data.frame(
        mz = c(734.570997942433, 736.564411787157, 737.56455162997,
               738.614212665807),
        abd = c(100, 8.37980024341253, 3.71016159721596, 1.05998504396349)
    )
    testthat::expect_equal(
        compare_spectras(q_spectra, l_spectras),
        list(list(
            spectra = data.frame(
                mz = c(734.570997942433, rep(NA, 5), 736.564411787157,
                       737.56455162997, 738.614212665807),
                abd = c(100, rep(NA, 5), 8.37980024341253, 3.71016159721596,
                        1.05998504396349),
                mz_theo = c(734.56943147, 735.57280138, 736.57581325,
                            737.57870281, 738.5815316, 739.58374177,
                            rep(NA, 3)),
                abd_theo = c(100, 44.925872, 11.492671, 2.131437, 0.300167,
                             0.020691, rep(NA, 3)),
                iso_theo = c("M", "M+1", "M+2", "M+3", "M+4", "M+5",
                             rep(NA, 3)),
                row.names = c(1, "NA", paste("NA", 1:4, sep = "."), 2:4)
            ),
            score = 62.94421387,
            deviation_mz = 0.001525878906,
            npeak = 1
        ))
    )
})

testthat::test_that("get eic", {
    ms_file <- xcms::xcmsRaw(
        system.file(
            "testdata",
            "220221CCM_global_POS_01_ssleu_filtered.mzML",
            package = "workflow.lipido"
        ),
        profstep = 0
    )
    # test the LPC 11:0 in [M+H]+ at 4.81 min
    mz_range <- 426.2615156 + c(-.001, .001)
    rt_range <- 4.81 * 60 + c(-5, 5)
    testthat::expect_equal(
        get_eic(ms_file, mz_range, rt_range),
        data.frame(
            rt = c(283.637, 284.166, 284.695, 285.224, 285.753, 286.281,
                   286.81, 287.339, 287.868, 288.396, 288.925, 289.454,
                   289.983, 290.512, 291.04, 291.569, 292.098, 292.627,
                   293.156),
            int = c(0, 0, 0, 273337.25, 2152744, 3501824, 1892321, 852784.5,
                    566891.5, 437327.25, 474893.5, 653133, 463749.5, 217667.5,
                    124145.0625, 83991.9375, 57429.65625, 39043.34375,
                    28808.65625)
        )
    )
})
