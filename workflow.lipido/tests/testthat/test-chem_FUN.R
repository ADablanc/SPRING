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
        package = "workflow.lipido"
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

    # 2nd test : with the mzmin upper than the mzrange of the file
    testthat::expect_equal(
        get_eic(ms_file, c(1000, 2000), rt_range),
        data.frame(rt = seq(rt_range[1], rt_range[2]), int = 0)
    )

    # 3rd test : with the mzmax lower than the mzrange of the file
    testthat::expect_equal(
        get_eic(ms_file, c(-1, 0), rt_range),
        data.frame(rt = seq(rt_range[1], rt_range[2]), int = 0)
    )

    # 4th test : with the rtmin upper than the rtrange of the file
    testthat::expect_equal(
        get_eic(ms_file, mz_range, c(500, 501)),
        data.frame(rt = c(500, 501), int = 0)
    )

    # 5th test : with the rtmax lower than the rtrange of the file
    testthat::expect_equal(
        get_eic(ms_file, mz_range, c(0, 1)),
        data.frame(rt = c(0, 1), int = 0)
    )

    # 6th test : norma
    testthat::expect_equal(
        get_eic(ms_file, mz_range, rt_range),
        data.frame(
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
    )

    # 7th test : without the slot `scantime_corrected`
    RSQLite::dbDisconnect(db)
    ms_file <- xcms::xcmsRaw(
        system.file(
            "testdata",
            "220221CCM_global_POS_02_ssleu_filtered.mzML",
            package = "workflow.lipido"
        ),
        profstep = 0
    )
    testthat::expect_equal(
        get_eic(ms_file, mz_range, rt_range),
        data.frame(
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
    )
})

testthat::test_that("get mzdev", {
    db <- db_connect(system.file(
        "testdata",
        "220221CCM_global.sqlite",
        package = "workflow.lipido"
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
            package = "workflow.lipido"
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
