testthat::test_that("get_ions", {
    instrument <- "QTOF_XevoG2-S_R25000@200"
    empty_df_ions <- data.frame(matrix(, nrow = 0, ncol = 6, dimnames = list(
        c(), c("formula", "adduct", "ion_formula", "charge", "mz", "abd")
    )))

    # should return nothing cause the formula C2N2 don't contain any H
    testthat::expect_identical(
        get_ions(
            "C2N2",
            adducts[which(adducts$Name == "[M-H]-"), ],
            instrument
        ),
        empty_df_ions
    )
    # should return nothing cause the m/z ion is out of the resolution list
    # of the instrument obtained from enviPat
    testthat::expect_identical(
        get_ions(
            "C1H1",
            adducts[which(adducts$Name == "[M-H]-"), ],
            instrument
        ),
        empty_df_ions
    )

    # should return only the C18H38N1O7P1 with [2M+H]+
    testthat::expect_identical(
        get_ions(
            c("C1H1", "C18H38N1O7P1"),
            adducts[which(adducts$Name == "[2M+H]+"), ],
            instrument
        ),
        data.frame(
            formula = c("C18H38N1O7P1", "C18H38N1O7P1", "C18H38N1O7P1",
                        "C18H38N1O7P1", "C18H38N1O7P1"),
            adduct = c("[2M+H]+", "[2M+H]+", "[2M+H]+", "[2M+H]+", "[2M+H]+"),
            ion_formula = c("C36H77N2O14P2", "C36H77N2O14P2", "C36H77N2O14P2",
                            "C36H77N2O14P2", "C36H77N2O14P2"),
            charge = c(1, 1, 1, 1, 1),
            mz = c(823.48445, 824.48777, 825.49047, 826.49316, 827.49541),
            abd = c(100, 41.14, 11.11, 2.03, 0.21),
            iso = c("M", "M+1", "M+2", "M+3", "M+4")
        )
    )

    # should return only the C18H38N1O7P1 with [M-H]-
    testthat::expect_identical(
        get_ions(
            c("C2N2", "C18H38N1O7P1"),
            adducts[which(adducts$Name == "[2M+H]+"), ],
            instrument
        ),
        data.frame(
            formula = c("C18H38N1O7P1", "C18H38N1O7P1", "C18H38N1O7P1",
                        "C18H38N1O7P1", "C18H38N1O7P1"),
            adduct = c("[2M+H]+", "[2M+H]+", "[2M+H]+", "[2M+H]+", "[2M+H]+"),
            ion_formula = c("C36H77N2O14P2", "C36H77N2O14P2", "C36H77N2O14P2",
                            "C36H77N2O14P2", "C36H77N2O14P2"),
            charge = c(1, 1, 1, 1, 1),
            mz = c(823.48445, 824.48777, 825.49047, 826.49316, 827.49541),
            abd = c(100, 41.14, 11.11, 2.03, 0.21),
            iso = c("M", "M+1", "M+2", "M+3", "M+4")
        )
    )
})

testthat::test_that("load_chem_db", {
    empty_db <- data.frame(matrix(, nrow = 0, ncol = 11, dimnames = list(
        c(), c("class", "formula", "name", "rt", "ion_id", "adduct",
               "ion_formula", "charge", "mz", "abd", "iso")
    )))

    # 1st test : without adducts
    testthat::expect_equal(
        load_chem_db(c(), "QTOF_XevoG2-S_R25000@200"),
        empty_db
    )

    # 2nd test : normal
    testthat::expect_equal(
        load_chem_db(
            c("[2M+H]+", "[M-H]-"),
            "QTOF_XevoG2-S_R25000@200"
        )[1:10, ],
        data.frame(
            formula = c(rep("C10H19N1O4", 7), rep("C13H23N1O4", 3)),
            class = rep("Car", 10),
            name = c(rep("CAR 3:0", 7), rep("CAR 6:1", 3)),
            rt = c(rep(314.4, 7), rep(322.8, 3)),
            ion_id = c(99, 99, 99, 99, 42, 42, 42, 130, 75, 130),
            adduct = c(rep("[2M+H]+", 4), rep("[M-H]-", 3), "[2M+H]+", "[M-H]-",
                       "[2M+H]+"),
            ion_formula = c(rep("C20H39N2O8", 4), rep("C10H18N1O4", 3),
                            "C26H47N2O8", "C13H22N1O4", "C26H47N2O8"),
            charge = c(rep(1, 4), rep(-1, 3), 1, -1, 1),
            mz = c(435.27009, 436.27332, 437.27555, 438.2784, 216.12413,
                   217.12735, 218.12934, 515.33269, 256.15543, 516.33595),
            abd = c(100, 23.17, 4.04, 0.5, 100, 11.59, 1.36, 100, 100, 29.75),
            iso = c("M", "M+1", "M+2", "M+3", "M", "M+1", "M+2", "M", "M",
                    "M+1")
        )
    )

    # 3rd test : with a compound name which doesn't exists
    testthat::expect_equal(
        load_chem_db(
            c("[2M+H]+", "[M-H]-"),
            "QTOF_XevoG2-S_R25000@200",
            "toto"
        ),
        empty_db
    )

    # 4th test : by compound name restriction
    testthat::expect_equal(
        load_chem_db(
            c("[2M+H]+", "[M-H]-"),
            "QTOF_XevoG2-S_R25000@200",
            cpd_names = c("LPE 13:0", "CE 16:0")
        ),
        data.frame(
            formula = c(rep("C18H38N1O7P1", 9), rep("C43H76O2", 11)),
            class = c(rep("LPE", 9), rep("CE", 11)),
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

    # 5th test : with a compound class which doesn't exists
    testthat::expect_equal(
        load_chem_db(
            c("[2M+H]+", "[M-H]-"),
            "QTOF_XevoG2-S_R25000@200",
            cpd_classes = "IPO"
        ),
        empty_db
    )

    # 6th test : compound class restriction
    testthat::expect_equal(
        load_chem_db(
            c("[2M+H]+", "[M-H]-"),
            "QTOF_XevoG2-S_R25000@200",
            cpd_classes = "LPC"
        )[1:12, ],
        data.frame(
            formula = rep("C19H40N1O7P1", 12),
            class = rep("LPC", 12),
            name = c(rep("LPC 11:0", 9), rep("LPC 11a:0", 3)),
            rt = c(rep(295.8, 9), rep(291, 3)),
            ion_id = c(rep(24, 5), rep(1, 4), rep(24, 3)),
            adduct = c(rep("[2M+H]+", 5), rep("[M-H]-", 4), rep("[2M+H]+", 3)),
            ion_formula = c(rep("C38H81N2O14P2", 5), rep("C19H39N1O7P1", 4),
                            rep("C38H81N2O14P2", 3)),
            charge = c(rep(1, 5), rep(-1, 4), rep(1, 3)),
            mz = c(851.51575, 852.51908, 853.52182, 854.52452, 855.52745,
                   424.24696, 425.25028, 426.25264, 427.25529, 851.51575,
                   852.51908, 853.52182),
            abd = c(100, 43.35, 12.03, 2.26, 0.34, 100, 21.68, 3.46, 0.42, 100,
                    43.35, 12.03),
            iso = c("M", "M+1", "M+2", "M+3", "M+4", "M", "M+1", "M+2", "M+3",
                    "M", "M+1", "M+2")
        )
    )

    # 7th test : with a NULL compound name & a compound class
    testthat::expect_equal(
        load_chem_db(
            c("[2M+H]+", "[M-H]-"),
            "QTOF_XevoG2-S_R25000@200",
            cpd_name = "OCO",
            cpd_classes = "LPC"
        ),
        empty_db
    )

    # 8th test : with a compound name & a NULL compound class
    testthat::expect_equal(
        load_chem_db(
            c("[2M+H]+", "[M-H]-"),
            "QTOF_XevoG2-S_R25000@200",
            cpd_name = "LPC 11a:0",
            cpd_classes = "IPO"
        ),
        empty_db
    )

    # 9th test : with a compound name & a compound class
    testthat::expect_equal(
        load_chem_db(
            c("[2M+H]+", "[M-H]-"),
            "QTOF_XevoG2-S_R25000@200",
            cpd_names = "LPC 11a:0",
            cpd_classes = "LPC"
        ),
        data.frame(
            formula = rep("C19H40N1O7P1", 9),
            class = rep("LPC", 9),
            name = rep("LPC 11a:0", 9),
            rt = rep(291, 9),
            ion_id = c(rep(2, 5), rep(1, 4)),
            adduct = c(rep("[2M+H]+", 5), rep("[M-H]-", 4)),
            ion_formula = c(rep("C38H81N2O14P2", 5), rep("C19H39N1O7P1", 4)),
            charge = c(rep(1, 5), rep(-1, 4)),
            mz = c(851.51575, 852.51908, 853.52182, 854.52452, 855.52745,
                   424.24696, 425.25028, 426.25264, 427.25529),
            abd = c(100, 43.35, 12.03, 2.26, 0.34, 100, 21.68, 3.46, 0.42),
            iso = c("M", "M+1", "M+2", "M+3", "M+4", "M", "M+1", "M+2", "M+3")
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
        "220221CCM_global__02_ssleu_filtered",
        "positive"
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
            rt = c(186.663589477539, 187.293151855469, 187.91943359375,
                   188.541213989258, 189.160888671875, 190.390289306641,
                   190.998886108398, 191.605331420898, 192.20849609375,
                   192.808334350586, 193.404907226562, 193.997055053711,
                   194.587020874023, 195.173706054688, 195.757110595703,
                   196.336120605469, 196.912948608398, 197.48649597168,
                   198.056732177734, 198.622650146484, 199.186340332031,
                   199.74674987793, 200.85774230957, 201.407287597656,
                   202.498657226562, 203.039428710938, 203.576934814453,
                   204.110153198242, 204.641128540039, 205.168853759766,
                   205.693283081055, 206.213485717773, 206.731414794922,
                   207.246078491211, 207.75749206543, 208.264694213867,
                   208.769592285156, 209.271240234375, 209.769638061523,
                   210.264801025391, 210.755767822266, 211.244415283203,
                   211.729843139648, 212.212005615234, 212.690032958984,
                   213.165710449219, 213.638168334961, 214.107376098633,
                   215.035217285156, 215.494720458984, 215.95100402832,
                   216.403915405273),
            int = c(0, 0, 0, 353.10546875, 0, 0, 0, 0, 0, 0, 402.763916015625,
                    402.763916015625, 0, 0, 0, 0, 0, 0, 130.551635742188,
                    130.551635742188, 134.438720703125, 0, 0, 0, 0,
                    1104.9599609375, 0, 339133.5, 1723138, 642538.5,
                    42233.40625, 3690.009765625, 3690.009765625, 0, 0, 0,
                    796.83251953125, 796.83251953125, 178.636962890625, 0, 0, 0,
                    0, 329.189453125, 0, 0, 0, 490.173828125, 149.362548828125,
                    0, 219.229248046875, 0)
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
        "220221CCM_global__02_ssleu_filtered",
        "positive"
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
            rt = c(188.541213989258, 193.404907226562, 198.056732177734,
                   199.186340332031, 203.039428710938, 204.110153198242,
                   204.641128540039, 205.168853759766, 205.693283081055,
                   206.213485717773, 208.769592285156, 209.769638061523,
                   212.212005615234, 214.107376098633, 215.035217285156,
                   215.95100402832),
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

testthat::test_that("get cpd classes", {
    testthat::expect_equal(
        get_cpd_classes(),
        c("CE", "FA", "Cer", "SM", "LPC", "PE", "LPE", "TG", "PC", "MHCer",
          "PG", "PI", "PS", "Car", "DG", "GlcCer", "LacCer")
    )
})
