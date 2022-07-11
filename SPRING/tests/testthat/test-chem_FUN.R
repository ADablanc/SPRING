testthat::test_that("load_chem_db", {
    empty_chem_db <- data.frame(matrix(, nrow = 0, ncol = 5, dimnames = list(
        c(), c("class", "adduct", "name", "formula", "rt"))))
    chem_db <- data.frame(
        class = c("Cer", rep("LPC", 2)),
        adduct = c("[M+H-H2O]+", rep("[M+H]+", 2)),
        name = c("Cer (d18:1/C12:0)", "LPC 11:0", "LPC 11a:0"),
        formula = c("C30H59N1O3", rep("C19H40N1O7P1", 2)),
        rt = c(195.59999999999999, 295.79999999999995, 291)
    )
    database <- "test"
    testthat::expect_identical(
        load_chem_db(database),
        chem_db
    )
    testthat::expect_identical(
        load_chem_db(database, "positive"),
        chem_db
    )
    testthat::expect_identical(
        nrow(load_chem_db(database, "negative")),
        nrow(empty_chem_db)
    )
    testthat::expect_identical(
        nrow(load_chem_db(database, "positive", "HBCDD")),
        nrow(empty_chem_db)
    )
    testthat::expect_identical(
        load_chem_db(database, "positive", "LPC"),
        chem_db[chem_db$class == "LPC", ]
    )
    testthat::expect_identical(
        load_chem_db(database, "positive", cpd_names = "LPC 11a:0"),
        chem_db[chem_db$name == "LPC 11a:0", ]
    )
    testthat::expect_identical(
        load_chem_db(database, "positive"),
        chem_db
    )
})

testthat::test_that("load_ion_db", {
    empty_chem_db <- data.frame(matrix(, nrow = 0, ncol = 11, dimnames = list(
        c(), c("class", "formula", "name", "rt", "ion_id", "adduct",
               "ion_formula", "charge", "mz", "abd", "iso")
    )))
    chem_db <- data.frame(
        formula = c(rep("C19H40N1O7P1", 8), rep("C30H59N1O3", 4)),
        adduct = c(rep("[M+H]+", 8), rep("[M+H-H2O]+", 4)),
        class = c(rep("LPC", 8), rep("Cer", 4)),
        name = c(rep("LPC 11:0", 4), rep("LPC 11a:0", 4),
                 rep("Cer (d18:1/C12:0)", 4)),
        rt = c(rep(295.8, 4), rep(291, 4), rep(195.6, 4)),
        ion_id = c(rep(1, 8), rep(2, 4)),
        ion_formula = c(rep("C19H41N1O7P1", 8), rep("C30H58N1O2", 4)),
        charge = rep(1, 12),
        mz = c(426.26152, 427.26484, 428.26719, 429.26984, 426.26152, 427.26484,
               428.26719, 429.26984, 464.44621, 465.44955, 466.45272,
               467.45576),
        abd = c(100, 21.7, 3.46, 0.42, 100, 21.7, 3.46, 0.42, 100, 33.55, 5.86,
                0.65),
        iso = c("M", "M+1", "M+2", "M+3", "M", "M+1", "M+2", "M+3", "M", "M+1",
                "M+2", "M+3")
    )
    database <- "test"
    instrument <- "QTOF_XevoG2-S_R25000@200"
    polarity <- "positive"
    testthat::expect_equal(
        data.frame(load_ion_db(database, instrument, polarity),
                   row.names = NULL),
        chem_db
    )
    testthat::expect_identical(
        load_ion_db(database, instrument, "negative"),
        empty_chem_db
    )
    testthat::expect_identical(
        load_ion_db(database, instrument, polarity, "HBCDD"),
        empty_chem_db
    )
    testthat::expect_equal(
        data.frame(load_ion_db(database, instrument, polarity, "LPC"),
                   row.names = NULL),
        data.frame(chem_db[chem_db$class == "LPC", ], row.names = NULL)
    )
    testthat::expect_equal(
        data.frame(load_ion_db(
            database,
            instrument,
            polarity,
            cpd_names = "LPC 11a:0"
        ), row.names = NULL),
        data.frame(chem_db[chem_db$name == "LPC 11a:0", ], row.names = NULL)
    )
    testthat::expect_equal(
        data.frame(load_ion_db(
            database,
            instrument,
            polarity
        ), row.names = NULL),
        data.frame(chem_db, row.names = NULL)
    )
})

testthat::test_that("get_ions", {
    instrument <- "QTOF_XevoG2-S_R25000@200"

    # should return nothing cause the formula C2N2 don't contain any H
    testthat::expect_identical(
        get_ions(
            "C2N2",
            adducts[which(adducts$name == "[M-H]-"), ],
            instrument
        ),
        list()
    )
    # should return nothing cause the m/z ion is out of the resolution list
    # of the instrument obtained from enviPat
    testthat::expect_identical(
        get_ions(
            "C1H1",
            adducts[which(adducts$name == "[M-H]-"), ],
            instrument
        ),
        list()
    )

    # should return only the C18H38N1O7P1 with [2M+H]+
    testthat::expect_equal(
        get_ions(
            c("C1H1", "C18H38N1O7P1"),
            adducts[which(adducts$name == "[2M+H]+"), ],
            instrument
        ),
        list(data.frame(
            formula = c("C18H38N1O7P1", "C18H38N1O7P1", "C18H38N1O7P1",
                        "C18H38N1O7P1", "C18H38N1O7P1"),
            adduct = c("[2M+H]+", "[2M+H]+", "[2M+H]+", "[2M+H]+", "[2M+H]+"),
            ion_formula = c("C36H77N2O14P2", "C36H77N2O14P2", "C36H77N2O14P2",
                            "C36H77N2O14P2", "C36H77N2O14P2"),
            charge = c(1, 1, 1, 1, 1),
            mz = c(823.48445, 824.48777, 825.49047, 826.49316, 827.49541),
            abd = c(100, 41.14, 11.11, 2.03, 0.21),
            iso = c("M", "M+1", "M+2", "M+3", "M+4")
        ))
    )

    # should return only the C18H38N1O7P1 with [M-H]-
    testthat::expect_equal(
        get_ions(
            c("C2N2", "C18H38N1O7P1"),
            adducts[which(adducts$name == "[2M+H]+"), ],
            instrument
        ),
        list(
            data.frame(
                formula = c("C18H38N1O7P1", "C18H38N1O7P1", "C18H38N1O7P1",
                            "C18H38N1O7P1", "C18H38N1O7P1"),
                adduct = c("[2M+H]+", "[2M+H]+", "[2M+H]+", "[2M+H]+",
                           "[2M+H]+"),
                ion_formula = c("C36H77N2O14P2", "C36H77N2O14P2",
                                "C36H77N2O14P2", "C36H77N2O14P2",
                                "C36H77N2O14P2"),
                charge = c(1, 1, 1, 1, 1),
                mz = c(823.48445, 824.48777, 825.49047, 826.49316, 827.49541),
                abd = c(100, 41.14, 11.11, 2.03, 0.21),
                iso = c("M", "M+1", "M+2", "M+3", "M+4")
            )
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
    db <- db_connect(system.file(
        "testdata",
        "220221CCM_global.sqlite",
        package = "SPRING"
    ))
    ms_file <- db_read_ms_file(
        db,
        "220221CCM_global_POS_02_ssleu_filtered"
    )

    # test the Cer (d18:1/C12:0) in [M+Na]+ at 3.26 min
    mz_range <- 504.4387158 + c(-.015, .015)
    rt_range <- 3.26 * 60 + c(-15, 15)

    # 1st test : with no file
    testthat::expect_equal(
        get_eic(NULL, mz_range, rt_range),
        data.frame(rt = 0, int = 0)
    )

    # 2nd test : with no file with NA value
    testthat::expect_equal(
        get_eic(NULL, mz_range, rt_range, NA_values = TRUE),
        data.frame(rt = NA, int = NA)
    )

    # 3rd test : with the mzmin upper than the mzrange of the file
    testthat::expect_equal(
        get_eic(ms_file, c(1000, 2000), rt_range),
        data.frame(rt = seq(rt_range[1], rt_range[2]), int = 0)
    )

    # 4th test : with the mzmin upper than the mzrange of the file with NA value
    testthat::expect_equal(
        get_eic(ms_file, c(1000, 2000), rt_range, NA_values = TRUE),
        data.frame(rt = seq(rt_range[1], rt_range[2]), int = NA)
    )

    # 5th test : with the mzmax lower than the mzrange of the file
    testthat::expect_equal(
        get_eic(ms_file, c(-1, 0), rt_range),
        data.frame(rt = seq(rt_range[1], rt_range[2]), int = 0)
    )

    # 6th test : with the mzmax lower than the mzrange of the file with NA value
    testthat::expect_equal(
        get_eic(ms_file, c(-1, 0), rt_range, NA_values = TRUE),
        data.frame(rt = seq(rt_range[1], rt_range[2]), int = NA)
    )

    # 7th test : with the rtmin upper than the rtrange of the file
    testthat::expect_equal(
        get_eic(ms_file, mz_range, c(500, 501)),
        data.frame(rt = c(500, 501), int = 0)
    )

    # 8th test : with the rtmin upper than the rtrange of the file with NA value
    testthat::expect_equal(
        get_eic(ms_file, mz_range, c(500, 501), NA_values = TRUE),
        data.frame(rt = c(500, 501), int = NA)
    )

    # 9th test : with the rtmax lower than the rtrange of the file
    testthat::expect_equal(
        get_eic(ms_file, mz_range, c(0, 1)),
        data.frame(rt = c(0, 1), int = 0)
    )

    # 10th test : with the rtmax lower than the rtrange of the file with NA
                                                                         # value
    testthat::expect_equal(
        get_eic(ms_file, mz_range, c(0, 1), NA_values = TRUE),
        data.frame(rt = c(0, 1), int = NA)
    )

    # 11th test : normal
    eic <- data.frame(
        rt = c(181.050979614258, 181.579986572266, 182.109024047852,
               182.637054443359, 183.166076660156, 183.695083618164,
               184.22412109375, 184.752136230469, 185.281158447266,
               185.810180664062, 186.339202880859, 186.868225097656,
               187.396240234375, 187.925262451172, 188.454284667969,
               188.983291625977, 189.511306762695, 190.040344238281,
               190.569366455078, 191.098373413086, 191.626403808594,
               192.155426025391, 192.684432983398, 193.213455200195,
               193.742462158203, 194.270477294922, 194.799499511719,
               195.328521728516, 195.857543945312, 196.38655090332,
               196.914566040039, 197.443588256836, 197.972610473633,
               198.501617431641, 199.029632568359, 199.558654785156,
               200.087677001953, 200.616683959961, 201.14469909668,
               201.673706054688, 202.202728271484, 202.731735229492,
               203.260757446289, 203.788772583008, 204.317779541016,
               204.846801757812, 205.375823974609, 205.903823852539,
               206.432830810547, 206.961853027344, 207.490859985352,
               208.018859863281, 208.547882080078, 209.076889038086,
               209.605911254883, 210.134918212891),
        int = c(0, 0, 0, 353.10546875, 0, 0, 0, 0, 0, 0, 0,
                402.763916015625, 402.763916015625, 0, 0, 0, 0, 0, 0,
                130.551635742188, 130.551635742188, 134.438720703125, 0, 0,
                0, 0, 0, 0, 1104.9599609375, 0, 339133.5, 1723138, 642538.5,
                42233.40625, 3690.009765625, 3690.009765625, 0, 0, 0,
                796.83251953125, 796.83251953125, 178.636962890625, 0, 0, 0,
                0, 329.189453125, 0, 0, 0, 490.173828125, 490.173828125,
                149.362548828125, 0, 219.229248046875, 0)
    )
    testthat::expect_equal(
        get_eic(ms_file, mz_range, rt_range),
        eic
    )

    # 12th test : normal with NA value
    eic$int <- replace(eic$int, eic$int == 0, NA)
    testthat::expect_equal(
        get_eic(ms_file, mz_range, rt_range, NA_values = TRUE),
        eic
    )

    # 13th test : without the slot `scantime_corrected`
    RSQLite::dbDisconnect(db)
    ms_file <- xcms::xcmsRaw(
        system.file(
            "testdata",
            "220221CCM_global_POS_02_ssleu_filtered.mzML",
            package = "SPRING"
        ),
        profstep = 0
    )
    eic <- data.frame(
        rt = c(181.052, 181.581, 182.11, 182.638, 183.167, 183.696, 184.225,
               184.753, 185.282, 185.811, 186.34, 186.869, 187.397, 187.926,
               188.455, 188.984, 189.512, 190.041, 190.57, 191.099, 191.627,
               192.156, 192.685, 193.214, 193.743, 194.271, 194.8, 195.329,
               195.858, 196.387, 196.915, 197.444, 197.973, 198.502, 199.03,
               199.559, 200.088, 200.617, 201.145, 201.674, 202.203,
               202.732, 203.261, 203.789, 204.318, 204.847, 205.376,
               205.904, 206.433, 206.962, 207.491, 208.019, 208.548,
               209.077, 209.606, 210.135),
        int = c(0, 0, 0, 353.10546875, 0, 0, 0, 0, 0, 0, 0,
                402.763916015625, 402.763916015625, 0, 0, 0, 0, 0, 0,
                130.551635742188, 130.551635742188, 134.438720703125, 0, 0,
                0, 0, 0, 0, 1104.9599609375, 0, 339133.5, 1723138, 642538.5,
                42233.40625, 3690.009765625, 3690.009765625, 0, 0, 0,
                796.83251953125, 796.83251953125, 178.636962890625, 0, 0, 0,
                0, 329.189453125, 0, 0, 0, 490.173828125, 490.173828125,
                149.362548828125, 0, 219.229248046875, 0)
    )
    testthat::expect_equal(
        get_eic(ms_file, mz_range, rt_range),
        eic
    )

    # 14th test : without the slot `scantime_corrected` with NA value
    eic$int <- replace(eic$int, eic$int == 0, NA)
    testthat::expect_equal(
        get_eic(ms_file, mz_range, rt_range, NA_values = TRUE),
        eic
    )
})

testthat::test_that("get mzdev", {
    db <- db_connect(system.file(
        "testdata",
        "220221CCM_global.sqlite",
        package = "SPRING"
    ))
    ms_file <- db_read_ms_file(
        db,
        "220221CCM_global_POS_02_ssleu_filtered"
    )

    # test the Cer (d18:1/C12:0) in [M+Na]+ at 3.26 min
    mz_range <- 504.4387158 + c(-.015, .015)
    rt_range <- 3.26 * 60 + c(-15, 15)

    # 1st test : with no file
    testthat::expect_equal(
        get_mzdev(NULL, mz_range, rt_range),
        data.frame(rt = 0, mz = NA)
    )

    # 2nd test : with the mzmin upper than the mzrange of the file
    testthat::expect_equal(
        get_mzdev(ms_file, c(1000, 2000), rt_range),
        data.frame(rt = seq(rt_range[1], rt_range[2]), mz = NA)
    )

    # 3rd test : with the mzmax lower than the mzrange of the file
    testthat::expect_equal(
        get_mzdev(ms_file, c(-1, 0), rt_range),
        data.frame(rt = seq(rt_range[1], rt_range[2]), mz = NA)
    )

    # 4th test : with the rtmin upper than the rtrange of the file
    testthat::expect_equal(
        get_mzdev(ms_file, mz_range, c(500, 501)),
        data.frame(rt = c(500, 501), mz = NA)
    )

    # 5th test : with the rtmax lower than the rtrange of the file
    testthat::expect_equal(
        get_mzdev(ms_file, mz_range, c(0, 1)),
        data.frame(rt = c(0, 1), mz = NA)
    )

    # 6th test : normal
    testthat::expect_equal(
        get_mzdev(ms_file, mz_range, rt_range),
        data.frame(
            rt = c(182.637054443359, 186.868225097656, 191.098373413086,
                   192.155426025391, 195.857543945312, 196.914566040039,
                   197.443588256836, 197.972610473633, 198.501617431641,
                   199.029632568359, 201.673706054688, 202.731735229492,
                   205.375823974609, 207.490859985352, 208.547882080078,
                   209.605911254883),
            mz = c(504.440399169922, 504.4375, 504.437103271484,
                   504.436248779297, 504.441131591797, 504.441040039062,
                   504.440490722656, 504.440124511719, 504.441101074219,
                   504.439544677734, 504.440490722656, 504.440826416016,
                   504.44091796875, 504.439544677734, 504.436370849609,
                   504.437866210938)
        )
    )

    # 7th test : without the slot `scantime_corrected`
    RSQLite::dbDisconnect(db)
    ms_file <- xcms::xcmsRaw(
        system.file(
            "testdata",
            "220221CCM_global_POS_02_ssleu_filtered.mzML",
            package = "SPRING"
        ),
        profstep = 0
    )
    testthat::expect_equal(
        get_mzdev(ms_file, mz_range, rt_range),
        data.frame(
            rt = c(182.638, 186.869, 191.099, 192.156, 195.858, 196.915,
                   197.444, 197.973, 198.502, 199.03, 201.674, 202.732, 205.376,
                   207.491, 208.548, 209.606),
            mz = c(504.440399169922, 504.4375, 504.437103271484,
                   504.436248779297, 504.441131591797, 504.441040039062,
                   504.440490722656, 504.440124511719, 504.441101074219,
                   504.439544677734, 504.440490722656, 504.440826416016,
                   504.44091796875, 504.439544677734, 504.436370849609,
                   504.437866210938)
        )
    )
})

testthat::test_that("convert ppm to Da", {
    testthat::expect_equal(
        convert_ppm_da(5, 464.447304014051),
        0.00232223652
    )
})

testthat::test_that("get_mz_range", {
    testthat::expect_equal(
        get_mz_range(464.447304014051, 5),
        c(464.4449818, 464.4496263)
    )
})

testthat::test_that("kendrick", {
    testthat::expect_equal(
        get_kendrick_mass(c(428.26777, 504.44048)),
        list(
            x = c(427.789562382051, 503.87721725357),
            y = c(0.789562382051429, 0.877217253570052)
        )
    )
})

testthat::test_that("get raw tic", {
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
    msaccess <- tools::file_path_as_absolute(
        "~/GitHub/SPRING/pwiz/msaccess.exe")

    # test with missing file
    testthat::expect_error(
        invisible(capture.output(
            get_raw_tic("toto.csv", msaccess, polarity = "negative")
        )),
        "no files were generated by msaccess !"
    )

    # test with incorrect mzML file
    raw_file <- tempfile(fileext = ".raw")
    invisible(capture.output(file.create(raw_file)))
    testthat::expect_error(
        invisible(capture.output(
            get_raw_tic(raw_file, msaccess, polarity = "negative")
        )),
        "no files were generated by msaccess !"
    )

    # test with polarity "negative"
    empty_df <- data.frame(
        index = logical(0),
        id = logical(0),
        event = logical(0),
        analyzer = logical(0),
        msLevel = logical(0),
        rt = logical(0),
        int = logical(0)
    )
    testthat::expect_identical(
        get_raw_tic(raw_files, msaccess, polarity = "negative"),
        list(
            `220221CCM_global_POS_01_ssleu_filtered.mzML` = empty_df,
            `220221CCM_global_POS_02_ssleu_filtered.mzML` = empty_df
        )
    )

    # test with polarity "positive"
    testthat::expect_equal(
        get_raw_tic(raw_files, msaccess, polarity = "positive"),
        list(
            `220221CCM_global_POS_01_ssleu_filtered.mzML` = data.frame(
                index = 0:300,
                id = paste0("scan=", 306:606),
                event = rep(0, 301),
                analyzer = rep("Unknown", 301),
                msLevel = rep("ms1", 301),
                rt = c(162.54, 163.07, 163.6, 164.13, 164.66, 165.19, 165.72,
                       166.25, 166.77, 167.3, 167.83, 168.36, 168.89, 169.42,
                       169.95, 170.47, 171, 171.53, 172.06, 172.59, 173.12,
                       173.65, 174.18, 174.71, 175.23, 175.76, 176.29, 176.82,
                       177.35, 177.88, 178.41, 178.94, 179.46, 179.99, 180.52,
                       181.05, 181.58, 182.11, 182.64, 183.17, 183.69, 184.22,
                       184.75, 185.28, 185.81, 186.34, 186.87, 187.4, 187.93,
                       188.45, 188.98, 189.51, 190.04, 190.57, 191.1, 191.63,
                       192.16, 192.69, 193.21, 193.74, 194.27, 194.8, 195.33,
                       195.86, 196.39, 196.91, 197.44, 197.97, 198.5, 199.03,
                       199.56, 200.09, 200.62, 201.15, 201.67, 202.2, 202.73,
                       203.26, 203.79, 204.32, 204.85, 205.38, 205.9, 206.43,
                       206.96, 207.49, 208.02, 208.55, 209.08, 209.61, 210.13,
                       210.66, 211.19, 211.72, 212.25, 212.78, 213.31, 213.84,
                       214.37, 214.89, 215.42, 215.95, 216.48, 217.01, 217.54,
                       218.07, 218.59, 219.12, 219.65, 220.18, 220.71, 221.24,
                       221.77, 222.3, 222.82, 223.35, 223.88, 224.41, 224.94,
                       225.47, 226, 226.53, 227.06, 227.58, 228.11, 228.64,
                       229.17, 229.7, 230.23, 230.76, 231.29, 231.81, 232.34,
                       232.87, 233.4, 233.93, 234.46, 234.99, 235.52, 236.04,
                       236.57, 237.1, 237.63, 238.16, 238.69, 239.22, 239.75,
                       240.28, 240.8, 241.33, 241.86, 242.39, 242.92, 243.45,
                       243.98, 244.5, 245.03, 245.56, 246.09, 246.62, 247.15,
                       247.68, 248.21, 248.74, 249.26, 249.79, 250.32, 250.85,
                       251.38, 251.91, 252.44, 252.97, 253.49, 254.02, 254.55,
                       255.08, 255.61, 256.14, 256.67, 257.2, 257.72, 258.25,
                       258.78, 259.31, 259.84, 260.37, 260.9, 261.43, 261.95,
                       262.48, 263.01, 263.54, 264.07, 264.6, 265.13, 265.66,
                       266.19, 266.71, 267.24, 267.77, 268.3, 268.83, 269.36,
                       269.89, 270.42, 270.94, 271.47, 272, 272.53, 273.06,
                       273.59, 274.12, 274.65, 275.18, 275.71, 276.24, 276.76,
                       277.29, 277.82, 278.35, 278.88, 279.41, 279.94, 280.46,
                       280.99, 281.52, 282.05, 282.58, 283.11, 283.64, 284.17,
                       284.69, 285.22, 285.75, 286.28, 286.81, 287.34, 287.87,
                       288.4, 288.93, 289.45, 289.98, 290.51, 291.04, 291.57,
                       292.1, 292.63, 293.16, 293.68, 294.21, 294.74, 295.27,
                       295.8, 296.33, 296.86, 297.39, 297.92, 298.44, 298.97,
                       299.5, 300.03, 300.56, 301.09, 301.62, 302.14, 302.67,
                       303.2, 303.73, 304.26, 304.79, 305.32, 305.85, 306.38,
                       306.91, 307.44, 307.97, 308.49, 309.02, 309.55, 310.08,
                       310.61, 311.14, 311.67, 312.2, 312.73, 313.25, 313.78,
                       314.31, 314.84, 315.37, 315.9, 316.43, 316.95, 317.48,
                       318.01, 318.54, 319.07, 319.6, 320.13, 320.66, 321.19),
                int = c(705.6152, 996.1626, 0, 143.0039, 548.1299, 559.6572,
                        711.3052, 0, 1414.2231, 1340.1852, 371.2358, 0,
                        565.6597, 189.0759, 776.7297, 688.6877, 846.6965,
                        590.5485, 0, 2240.4888, 1078.9087, 222.2466, 1623.5732,
                        1421.5518, 0, 1140.6711, 310.9917, 1256.8782, 324.1583,
                        684.938, 0, 1035.4229, 818.5321, 736.7451, 253,
                        1541.7134, 1177.1798, 1543.51, 721.2373, 643.5892,
                        272.4285, 1034.677, 332.9221, 0, 1156.5615, 844.0067,
                        1233.3602, 319.7092, 1154.9651, 459.6598, 1702.5718,
                        753.4624, 1183.5792, 292.8206, 0, 0, 509.6062, 538.1757,
                        1360.4429, 515.1099, 522.5444, 808.1667, 0, 789.9119,
                        732.9109, 1250784.4375, 6043027.1311, 6856944.7188,
                        624515.7344, 7679.0522, 10633.1477, 6324.1978,
                        6099.1853, 1576.2824, 3808.6616, 138.7205, 466.334,
                        1056.7131, 272.5475, 880.4163, 0, 662.5189, 185.4005,
                        278.802, 203.9965, 1257.575, 827.3008, 587.002,
                        2845.3381, 354.3186, 446.8418, 571.1555, 0, 567.3691, 0,
                        516.9733, 861.2205, 531.6895, 247.2848, 334.0291,
                        489.1198, 555.8877, 917.996, 1406.0382, 408.6395,
                        720.2625, 664.3589, 549.7644, 284.1465, 1134.3596, 0,
                        309.1261, 842.8672, 330.0208, 351.4126, 374.7803,
                        501.0879, 687.8593, 106.5397, 762.6731, 299.8018,
                        433.0139, 921.7212, 422.9451, 200.913, 300.1504,
                        521.3822, 1053.2964, 299.4099, 820.5555, 325.0713,
                        532.7762, 618.0771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                        0, 0, 0, 0, 632045.2209, 577697.522, 561646.97,
                        613229.3923, 645914.3162, 639462.5737, 639045.8467,
                        656793.8418, 649006.625, 656151.186, 669816.3521,
                        683579.7988, 667588.2075, 685979.6748, 688265.0845,
                        688974.6265, 693879.375, 699816.8708, 721510.0126,
                        740122.125, 746645.6932, 744191.25, 759445.1587,
                        769473.3242, 787546.0601, 806896.028, 818874.0969,
                        859984.3171, 869169.75, 876473, 923535.2029, 923772.5,
                        948305.3115, 983070.7953, 1010825.9193, 1014337.7563,
                        1056227.9136, 1048115.3994, 935650.1775, 937022.5,
                        1065423.4088, 1096275.5361, 1131228.7959, 1132174.125,
                        1146546.2034, 1170681.8384, 1170115.1221, 1202529.25,
                        1244556.875, 1245364.2944, 1270198.8989, 1339532.3269,
                        1318930.7441, 1347344.6162, 1363979.5195, 1280146.5676,
                        1283636.6304, 1230410.377, 1159275.3, 1132778.1621,
                        1132131.2173, 1084219.375, 1040853, 939172.3179,
                        1119120.1562, 3552358.625, 5253975.8438, 3095073.1194,
                        1683160.5459, 1331035.9531, 1144419.8496, 1217774.5024,
                        1417509.6863, 1167215.5371, 829682.6855, 672079.6426,
                        634748.6641, 588148.9216, 546259.1013, 529680.0974,
                        501752.3142, 493779.6348, 416.4458, 308835.75,
                        483760.4932, 465881.2922, 463307.9043, 439908.2725,
                        451769.759, 441976.5312, 383763.3901, 977.3396,
                        405981.1875, 406799.2236, 386720.5654, 390363.7104,
                        372013.0698, 371225.5122, 255394.375, 0, 0, 0,
                        334961.5869, 511.2009, 308957.373, 301759.7559, 1386.48,
                        291125.4062, 289229.626, 271427.9292, 211438.8564, 0,
                        271061.397, 215959.9453, 150214.8081, 246866.4868,
                        3801.5098, 1123.8845, 727.7974, 190157.7188,
                        195317.6893, 0, 227372.0735, 170984.125, 173217.5852,
                        171311.8499, 167484.8293, 156542.8872, 153510.0613,
                        163048.5608, 162764.1294, 163319.9067, 161447.1218)
            ),
            `220221CCM_global_POS_02_ssleu_filtered.mzML` = data.frame(
                index = 0:300,
                id = paste0("scan=", 306:606),
                event = rep(0, 301),
                analyzer = rep("Unknown", 301),
                msLevel = rep("ms1", 301),
                rt = c(162.54, 163.07, 163.6, 164.13, 164.66, 165.19, 165.72,
                       166.25, 166.78, 167.3, 167.83,  168.36, 168.89, 169.42,
                       169.95, 170.48, 171, 171.53, 172.06, 172.59, 173.12,
                       173.65, 174.18, 174.71, 175.24, 175.76, 176.29, 176.82,
                       177.35, 177.88, 178.41, 178.94, 179.47, 179.99, 180.52,
                       181.05, 181.58, 182.11, 182.64, 183.17, 183.7, 184.22,
                       184.75, 185.28, 185.81, 186.34, 186.87, 187.4, 187.93,
                       188.46, 188.98, 189.51, 190.04, 190.57, 191.1, 191.63,
                       192.16, 192.69, 193.21, 193.74, 194.27, 194.8, 195.33,
                       195.86, 196.39, 196.91, 197.44, 197.97, 198.5, 199.03,
                       199.56, 200.09, 200.62, 201.15, 201.67, 202.2, 202.73,
                       203.26, 203.79, 204.32, 204.85, 205.38, 205.9, 206.43,
                       206.96, 207.49, 208.02, 208.55, 209.08, 209.61, 210.13,
                       210.66, 211.19, 211.72, 212.25, 212.78, 213.31, 213.84,
                       214.37, 214.89, 215.42, 215.95, 216.48, 217.01, 217.54,
                       218.07, 218.59, 219.12, 219.65, 220.18, 220.71, 221.24,
                       221.77, 222.3, 222.82, 223.35, 223.88, 224.41, 224.94,
                       225.47, 226, 226.53, 227.06, 227.58, 228.11, 228.64,
                       229.17, 229.7, 230.23, 230.76, 231.29, 231.81, 232.34,
                       232.87, 233.4, 233.93, 234.46, 234.99, 235.52, 236.04,
                       236.57, 237.1, 237.63, 238.16, 238.69, 239.22, 239.75,
                       240.28, 240.8, 241.33, 241.86, 242.39, 242.92, 243.45,
                       243.98, 244.5, 245.03, 245.56, 246.09, 246.62, 247.15,
                       247.68, 248.21, 248.74, 249.26, 249.79, 250.32, 250.85,
                       251.38, 251.91, 252.44, 252.97, 253.49, 254.02, 254.55,
                       255.08, 255.61, 256.14, 256.67, 257.19, 257.72, 258.25,
                       258.78, 259.31, 259.84, 260.37, 260.9, 261.43, 261.95,
                       262.48, 263.01, 263.54, 264.07, 264.6, 265.13, 265.66,
                       266.18, 266.71, 267.24, 267.77, 268.3, 268.83, 269.36,
                       269.89, 270.42, 270.94, 271.47, 272, 272.53, 273.06,
                       273.59, 274.12, 274.64, 275.17, 275.7, 276.23, 276.76,
                       277.29, 277.82, 278.35, 278.88, 279.4, 279.93, 280.46,
                       280.99, 281.52, 282.05, 282.58, 283.11, 283.63, 284.16,
                       284.69, 285.22, 285.75, 286.28, 286.81, 287.34, 287.86,
                       288.39, 288.92, 289.45, 289.98, 290.51, 291.04, 291.57,
                       292.1, 292.62, 293.15, 293.68, 294.21, 294.74, 295.27,
                       295.8, 296.32, 296.85, 297.38, 297.91, 298.44, 298.97,
                       299.5, 300.03, 300.56, 301.08, 301.61, 302.14, 302.67,
                       303.2, 303.73, 304.26, 304.79, 305.31, 305.84, 306.38,
                       306.91, 307.44, 307.97, 308.5, 309.03, 309.56, 310.09,
                       310.62, 311.14, 311.67, 312.2, 312.73, 313.26, 313.79,
                       314.32, 314.85, 315.38, 315.91, 316.44, 316.96, 317.49,
                       318.02, 318.55, 319.08, 319.61, 320.14, 320.67, 321.19),
                int = c(1731.7786, 503.677, 424.4534, 553.7495, 669.6021,
                        459.3698, 446.8698, 254.1489, 300.6943, 260.2268,
                        876.7905, 972.5618, 1213.5697, 307.4348, 308.8462,
                        935.7751, 0, 541.114, 0, 0, 787.4885, 521.2363,
                        348.5039, 623.0569, 0, 682.7086, 442.3147, 0, 552.2695,
                        162.3333, 0, 623.2852, 725.1176, 0, 291.4463, 554.2588,
                        363.377, 710.9836, 1132.8701, 442.9924, 0, 1214.6537,
                        429.9338, 723.9954, 876.8881, 490.6516, 914.9739,
                        1092.9399, 221.4362, 586.0938, 696.3885, 270.6675,
                        338.4827, 270.5352, 390.2501, 53.5216, 134.4387,
                        2087.162, 0, 1193.386, 1630.7579, 0, 439.575, 2613.7888,
                        8866.2734, 2701680.8672, 10185088.8203, 5186383.9062,
                        279552.5046, 32192.9882, 2479.1348, 6933.7416, 779.2871,
                        378.2632, 1404.5568, 1255.8789, 178.637, 870.1305,
                        216.2878, 660.0796, 799.9424, 712.9993, 999.5616,
                        1294.172, 919.8231, 490.1738, 0, 363.7987, 1203.1066,
                        219.2292, 801.7917, 0, 0, 711.0121, 552.3759, 1115.8335,
                        512.3483, 129.8031, 0, 109.1254, 850.6979, 883.9302,
                        317.5759, 672.9824, 1186.4885, 944.3882, 657.4952,
                        632.0102, 180, 625.26, 2374.1628, 440.8591, 594.5692,
                        221.4707, 630.6202, 824.6165, 0, 0, 238.9515, 554.5533,
                        1401.7013, 580.2192, 979.8523, 585.7683, 0, 1104.5176,
                        838.7096, 44, 661.6656, 505.467, 205.5619, 1075.48,
                        1296.1617, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                        0, 621429.9958, 548948.5397, 545285.375, 592501.0232,
                        624518.0068, 615380.4058, 613549.1509, 615353.948,
                        630729.0625, 640954.4824, 627502.9326, 650025.3545,
                        666802.75, 664155.9794, 668211.25, 682668.4371,
                        681686.5869, 696343.673, 703770.9758, 730411.6001,
                        717084.1323, 720631.6155, 742982.345, 761298.0405,
                        624800.4543, 813889.2144, 794864.9434, 816547.5001,
                        855250.7417, 873085.625, 892785.0793, 892788.375,
                        922070.5898, 952510.9097, 966119.875, 987423.125,
                        1000806.9324, 983250.1945, 939222.0859, 953003.657,
                        1002503.1899, 1058130.625, 1094161.5571, 1092228.1941,
                        1113394.2024, 1113864.5, 1138125.6465, 1191934.7722,
                        1214870.8291, 1268009.0137, 1267179.625, 1277752.3337,
                        1290886.4712, 1286329.7148, 1339619.3599, 1246572.0767,
                        1177770.6458, 1163515.0105, 1063504.5242, 1082235.75,
                        1085724.2017, 1041924.8877, 972870.7109, 871893.1692,
                        1163846.8262, 3654313.4258, 5205433.8359, 2898059.4375,
                        1708151.3594, 1282798.6406, 1096737.366, 1181419.4124,
                        1434516.3896, 1083324.896, 766139.5938, 669583.0447,
                        606293.6936, 565945.4297, 526918.1245, 501911.9446,
                        479973.187, 472867.4033, 448054.1924, 448992.7769,
                        447713.1924, 449724.6289, 316871.8345, 414423.2407,
                        416404.5586, 378189.6211, 439.6592, 395371.7141,
                        392374.6875, 386613.6255, 387497.6279, 371599.1582,
                        252105.25, 362485.7668, 339033.3125, 347260.9473,
                        46956.332, 327537.4182, 934.2847, 300887.4136,
                        298419.2156, 298940.7915, 288342.0068, 233955.375,
                        271137.3125, 671.165, 269596.4062, 220595.625,
                        263890.6675, 206342.821, 252495.2812, 7139.6182,
                        236866.4138, 0, 190860.0498, 232989.4043, 152481.0137,
                        273.2827, 141817.875, 173200.1641, 168875.4421,
                        164138.1299, 152675.1699, 150766.2554, 155360.189,
                        155034.5918, 193032.6543, 154878.9614, 146051.1426)
                )
        )
    )
})
