testthat::test_that("load_chem_db", {
    testthat::expect_error(
        load_chem_db("toto"),
        "toto doesn't exist in software"
    )

    empty_file <- file.path(
        system.file("extdata", "database", package = "workflow.lipido"),
        "empty_db.csv"
    )
    write.csv(
        matrix(, nrow = 0, ncol = 4, dimnames = list(
            c(), c("class", "name", "formula", "rt"))),
        empty_file,
        row.names = FALSE
    )
    testthat::expect_error(
        load_chem_db("empty_db"),
        "no compound in database"
    )
    file.remove(empty_file)

    testthat::expect_equal(
        load_chem_db("test"),
        data.frame(
            class = c("FA", "Cer", "LPC", "LPC"),
            name = c("FA 17:0", "Cer (d18:1/C12:0)", "LPC 11:0", "LPC 11a:0"),
            formula = c("C17H34O2", "C30H59N1O3", "C19H40N1O7P1",
                        "C19H40N1O7P1"),
            rt = c(3.01, 3.26, 4.93, 4.85)
        )
    )
})

testthat::test_that("load_ion_db", {
    empty_db <- data.frame(matrix(, nrow = 0, ncol = 11, dimnames = list(
        c(), c("class", "formula", "name", "rt", "ion_id", "adduct",
               "ion_formula", "charge", "mz", "abd", "iso")
    )))

    # 1st test : with an empty database
    empty_file <- file.path(
        system.file("extdata", "database", package = "workflow.lipido"),
        "empty_database.csv"
    )
    write.csv(
        matrix(, nrow = 0, ncol = 4, dimnames = list(
            c(), c("class", "name", "formula", "rt"))),
        empty_file,
        row.names = FALSE
    )
    testthat::expect_error(
        load_ion_db(c(), "QTOF_XevoG2-S_R25000@200", "empty_database"),
        "no compound in database"
    )
    file.remove(empty_file)

    # 2nd test : without adducts
    testthat::expect_equal(
        load_ion_db(c(), "QTOF_XevoG2-S_R25000@200", "test"),
        empty_db
    )

    # 3rd test : normal
    testthat::expect_equal(
        data.frame(load_ion_db(
            c("[2M+H]+", "[M-H]-"),
            "QTOF_XevoG2-S_R25000@200",
            "test"
        ), row.names = NULL),
        data.frame(
            formula = c("C30H59N1O3", "C30H59N1O3", "C30H59N1O3", "C30H59N1O3",
                       "C30H59N1O3", "C30H59N1O3", "C30H59N1O3", "C30H59N1O3",
                       "C30H59N1O3", "C17H34O2", "C17H34O2", "C17H34O2",
                       "C17H34O2", "C17H34O2", "C17H34O2", "C17H34O2",
                       "C19H40N1O7P1", "C19H40N1O7P1", "C19H40N1O7P1",
                       "C19H40N1O7P1", "C19H40N1O7P1", "C19H40N1O7P1",
                       "C19H40N1O7P1", "C19H40N1O7P1", "C19H40N1O7P1",
                       "C19H40N1O7P1", "C19H40N1O7P1", "C19H40N1O7P1",
                       "C19H40N1O7P1", "C19H40N1O7P1", "C19H40N1O7P1",
                       "C19H40N1O7P1", "C19H40N1O7P1", "C19H40N1O7P1"),
            class = c("Cer", "Cer", "Cer", "Cer", "Cer", "Cer", "Cer", "Cer",
                      "Cer", "FA", "FA", "FA", "FA", "FA", "FA", "FA", "LPC",
                      "LPC", "LPC", "LPC", "LPC", "LPC", "LPC", "LPC", "LPC",
                      "LPC", "LPC", "LPC", "LPC", "LPC", "LPC", "LPC", "LPC",
                      "LPC"),
            name = c("Cer (d18:1/C12:0)", "Cer (d18:1/C12:0)",
                     "Cer (d18:1/C12:0)", "Cer (d18:1/C12:0)",
                     "Cer (d18:1/C12:0)", "Cer (d18:1/C12:0)",
                     "Cer (d18:1/C12:0)", "Cer (d18:1/C12:0)",
                     "Cer (d18:1/C12:0)", "FA 17:0", "FA 17:0", "FA 17:0",
                     "FA 17:0", "FA 17:0", "FA 17:0", "FA 17:0", "LPC 11:0",
                     "LPC 11:0", "LPC 11:0", "LPC 11:0", "LPC 11:0", "LPC 11:0",
                     "LPC 11:0", "LPC 11:0", "LPC 11:0", "LPC 11a:0",
                     "LPC 11a:0", "LPC 11a:0", "LPC 11a:0", "LPC 11a:0",
                     "LPC 11a:0", "LPC 11a:0", "LPC 11a:0", "LPC 11a:0"),
            rt = c(195.6, 195.6, 195.6, 195.6, 195.6, 195.6, 195.6, 195.6,
                   195.6, 180.6, 180.6, 180.6, 180.6, 180.6, 180.6, 180.6,
                   295.8, 295.8, 295.8, 295.8, 295.8, 295.8, 295.8, 295.8,
                   295.8, 291, 291, 291, 291, 291, 291, 291, 291, 291),
            ion_id = c(6, 6, 6, 6, 6, 3, 3, 3, 3, 4, 4, 4, 4, 1, 1, 1, 5, 5, 5,
                       5, 5, 2, 2, 2, 2, 5, 5, 5, 5, 5, 2, 2, 2, 2),
            adduct = c("[2M+H]+", "[2M+H]+", "[2M+H]+", "[2M+H]+", "[2M+H]+",
                       "[M-H]-", "[M-H]-", "[M-H]-", "[M-H]-", "[2M+H]+",
                       "[2M+H]+", "[2M+H]+", "[2M+H]+", "[M-H]-", "[M-H]-",
                       "[M-H]-", "[2M+H]+", "[2M+H]+", "[2M+H]+", "[2M+H]+",
                       "[2M+H]+", "[M-H]-", "[M-H]-", "[M-H]-", "[M-H]-",
                       "[2M+H]+", "[2M+H]+", "[2M+H]+", "[2M+H]+", "[2M+H]+",
                       "[M-H]-", "[M-H]-", "[M-H]-", "[M-H]-"),
            ion_formula = c("C60H119N2O6", "C60H119N2O6", "C60H119N2O6",
                            "C60H119N2O6", "C60H119N2O6", "C30H58N1O3",
                            "C30H58N1O3", "C30H58N1O3", "C30H58N1O3",
                            "C34H69O4", "C34H69O4", "C34H69O4", "C34H69O4",
                            "C17H33O2", "C17H33O2", "C17H33O2", "C38H81N2O14P2",
                            "C38H81N2O14P2", "C38H81N2O14P2", "C38H81N2O14P2",
                            "C38H81N2O14P2", "C19H39N1O7P1", "C19H39N1O7P1",
                            "C19H39N1O7P1", "C19H39N1O7P1", "C38H81N2O14P2",
                            "C38H81N2O14P2", "C38H81N2O14P2", "C38H81N2O14P2",
                            "C38H81N2O14P2", "C19H39N1O7P1", "C19H39N1O7P1",
                            "C19H39N1O7P1", "C19H39N1O7P1"),
            charge = c(1, 1, 1, 1, 1, -1, -1, -1, -1, 1, 1, 1, 1, -1, -1, -1, 1,
                       1, 1, 1, 1, -1, -1, -1, -1, 1, 1, 1, 1, 1, -1, -1, -1,
                       -1),
            mz = c(963.90627, 964.90961, 965.91283, 966.91595, 967.919,
                   480.44222, 481.44557, 482.44866, 483.45159, 541.51904,
                   542.52246, 543.52559, 544.5284, 269.2486, 270.25202,
                   271.25481, 851.51575, 852.51908, 853.52182, 854.52452,
                   855.52745, 424.24696, 425.25028, 426.25264, 427.25529,
                   851.51575, 852.51908, 853.52182, 854.52452, 855.52745,
                   424.24696, 425.25028, 426.25264, 427.25529),
            abd = c(100, 67.29, 23.5, 5.58, 0.93, 100, 33.66, 6.07, 0.72, 100,
                    37.79, 7.7, 1.07, 100, 18.84, 2.02, 100, 43.35, 12.03, 2.26,
                    0.34, 100, 21.68, 3.46, 0.42, 100, 43.35, 12.03, 2.26, 0.34,
                    100, 21.68, 3.46, 0.42),
            iso = c("M", "M+1", "M+2", "M+3", "M+4", "M", "M+1", "M+2", "M+3",
                    "M", "M+1", "M+2", "M+3", "M", "M+1", "M+2", "M", "M+1",
                    "M+2", "M+3", "M+4", "M", "M+1", "M+2", "M+3", "M", "M+1",
                    "M+2", "M+3", "M+4", "M", "M+1", "M+2", "M+3")
        )
    )

    # 4th test : with a compound name which doesn't exists
    testthat::expect_equal(
        load_ion_db(
            c("[2M+H]+", "[M-H]-"),
            "QTOF_XevoG2-S_R25000@200",
            "test",
            "toto"
        ),
        empty_db
    )

    # 5th test : by compound name restriction
    testthat::expect_equal(
        load_ion_db(
            c("[2M+H]+", "[M-H]-"),
            "QTOF_XevoG2-S_R25000@200",
            "test",
            cpd_names = "FA 17:0"
        ),
        data.frame(
            formula = c("C17H34O2", "C17H34O2", "C17H34O2", "C17H34O2",
                        "C17H34O2", "C17H34O2", "C17H34O2"),
            class = c("FA", "FA", "FA", "FA", "FA", "FA", "FA"),
            name = c("FA 17:0", "FA 17:0", "FA 17:0", "FA 17:0", "FA 17:0",
                     "FA 17:0", "FA 17:0"),
            rt = c(180.6, 180.6, 180.6, 180.6, 180.6, 180.6, 180.6),
            ion_id = c(2, 2, 2, 2, 1, 1, 1),
            adduct = c("[2M+H]+", "[2M+H]+", "[2M+H]+", "[2M+H]+", "[M-H]-",
                       "[M-H]-", "[M-H]-"),
            ion_formula = c("C34H69O4", "C34H69O4", "C34H69O4", "C34H69O4",
                            "C17H33O2", "C17H33O2", "C17H33O2"),
            charge = c(1, 1, 1, 1, -1, -1, -1),
            mz = c(541.51904, 542.52246, 543.52559, 544.5284, 269.2486,
                   270.25202, 271.25481),
            abd = c(100, 37.79, 7.7, 1.07, 100, 18.84, 2.02),
            iso = c("M", "M+1", "M+2", "M+3", "M", "M+1", "M+2")
        )
    )

    # 6th test : with a compound class which doesn't exists
    testthat::expect_equal(
        load_ion_db(
            c("[2M+H]+", "[M-H]-"),
            "QTOF_XevoG2-S_R25000@200",
            "test",
            cpd_classes = "IPO"
        ),
        empty_db
    )

    # 7th test : compound class restriction
    testthat::expect_equal(
        load_ion_db(
            c("[2M+H]+", "[M-H]-"),
            "QTOF_XevoG2-S_R25000@200",
            "test",
            cpd_classes = "LPC"
        ),
        data.frame(
            formula = c("C19H40N1O7P1", "C19H40N1O7P1", "C19H40N1O7P1",
                        "C19H40N1O7P1", "C19H40N1O7P1", "C19H40N1O7P1",
                        "C19H40N1O7P1", "C19H40N1O7P1", "C19H40N1O7P1",
                        "C19H40N1O7P1", "C19H40N1O7P1", "C19H40N1O7P1",
                        "C19H40N1O7P1", "C19H40N1O7P1", "C19H40N1O7P1",
                        "C19H40N1O7P1", "C19H40N1O7P1", "C19H40N1O7P1"),
            class = c("LPC", "LPC", "LPC", "LPC", "LPC", "LPC", "LPC", "LPC",
                      "LPC", "LPC", "LPC", "LPC", "LPC", "LPC", "LPC", "LPC",
                      "LPC", "LPC"),
            name = c("LPC 11:0", "LPC 11:0", "LPC 11:0", "LPC 11:0", "LPC 11:0",
                     "LPC 11:0", "LPC 11:0", "LPC 11:0", "LPC 11:0",
                     "LPC 11a:0", "LPC 11a:0", "LPC 11a:0", "LPC 11a:0",
                     "LPC 11a:0", "LPC 11a:0", "LPC 11a:0", "LPC 11a:0",
                     "LPC 11a:0"),
            rt = c(295.8, 295.8, 295.8, 295.8, 295.8, 295.8, 295.8, 295.8,
                   295.8, 291, 291, 291, 291, 291, 291, 291, 291, 291),
            ion_id = c(2, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1, 1),
            adduct = c("[2M+H]+", "[2M+H]+", "[2M+H]+", "[2M+H]+", "[2M+H]+",
                       "[M-H]-", "[M-H]-", "[M-H]-", "[M-H]-", "[2M+H]+",
                       "[2M+H]+", "[2M+H]+", "[2M+H]+", "[2M+H]+", "[M-H]-",
                       "[M-H]-", "[M-H]-", "[M-H]-"),
            ion_formula = c("C38H81N2O14P2", "C38H81N2O14P2", "C38H81N2O14P2",
                            "C38H81N2O14P2", "C38H81N2O14P2", "C19H39N1O7P1",
                            "C19H39N1O7P1", "C19H39N1O7P1", "C19H39N1O7P1",
                            "C38H81N2O14P2", "C38H81N2O14P2", "C38H81N2O14P2",
                            "C38H81N2O14P2", "C38H81N2O14P2", "C19H39N1O7P1",
                            "C19H39N1O7P1", "C19H39N1O7P1", "C19H39N1O7P1"),
            charge = c(1, 1, 1, 1, 1, -1, -1, -1, -1, 1, 1, 1, 1, 1, -1, -1, -1,
                       -1),
            mz = c(851.51575, 852.51908, 853.52182, 854.52452, 855.52745,
                   424.24696, 425.25028, 426.25264, 427.25529, 851.51575,
                   852.51908, 853.52182, 854.52452, 855.52745, 424.24696,
                   425.25028, 426.25264, 427.25529),
            abd = c(100, 43.35, 12.03, 2.26, 0.34, 100, 21.68, 3.46, 0.42, 100,
                    43.35, 12.03, 2.26, 0.34, 100, 21.68, 3.46, 0.42),
            iso = c("M", "M+1", "M+2", "M+3", "M+4", "M", "M+1", "M+2", "M+3",
                    "M", "M+1", "M+2", "M+3", "M+4", "M", "M+1", "M+2", "M+3")
        )
    )

    # 8th test : with a NULL compound name & a compound class
    testthat::expect_equal(
        load_ion_db(
            c("[2M+H]+", "[M-H]-"),
            "QTOF_XevoG2-S_R25000@200",
            "test",
            cpd_name = "OCO",
            cpd_classes = "LPC"
        ),
        empty_db
    )

    # 9th test : with a compound name & a NULL compound class
    testthat::expect_equal(
        load_ion_db(
            c("[2M+H]+", "[M-H]-"),
            "QTOF_XevoG2-S_R25000@200",
            "test",
            cpd_name = "LPC 11a:0",
            cpd_classes = "IPO"
        ),
        empty_db
    )

    # 10th test : with a compound name & a compound class
    testthat::expect_equal(
        load_ion_db(
            c("[2M+H]+", "[M-H]-"),
            "QTOF_XevoG2-S_R25000@200",
            "test",
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
