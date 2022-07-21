testthat::test_that("plot empty MS", {
    p <- plot_empty_ms()
    p2 <- readRDS(system.file(
        "testdata",
        "plots.RDS",
        package = "SPRING"
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$attrs) == unlist(p2$plot_empty_ms[[1]]$attrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$layoutAttrs) == unlist(p2$plot_empty_ms[[1]]$layoutAttrs)
    ))
})

testthat::test_that("plot composite MS", {
    spectras <- data.frame(
        spectra_id = c(1, 1, 1, 1, 3, 3, 3, 3, 4, 4, 4, 4),
        feature_id = c(NA, 17, NA, NA, 20, 19, NA, NA, 18, NA, NA, NA),
        mz = c(NA, 408.251325886321, NA, NA, 426.262333104945, 427.265704881008,
               NA, NA, 448.244170162955, NA, NA, NA),
        int = c(NA, 88824.635233072, NA, NA, 6201250.27168528, 1186767.56444882,
                NA, NA, 288290.748778874, NA, NA, NA),
        abd = c(NA, 100, NA, NA, 100, 19.1375531135642, NA, NA, 100, NA, NA,
                NA),
        ion_id_theo = c(62, 62, 62, 62, 66, 66, 66, 66, 64, 64, 64, 64),
        mz_theo = c(411.25935, 408.25095, 409.25427, 410.25672, 426.26152,
                    427.26484, 428.26719, 429.26984, 448.24346, 449.24678,
                    450.24914, 451.25178),
        abd_theo = c(0.38, 100, 21.65, 3.25, 100, 21.7, 3.46, 0.42, 100, 21.69,
                     3.45, 0.42),
        iso_theo = c("M+3", "M", "M+1", "M+2", "M", "M+1", "M+2", "M+3", "M",
                     "M+1", "M+2", "M+3")
    )
    spectras <- split(spectras, spectras$spectra_id)
    names(spectras) <- c("[M+H-H2O]+", "[M+H]+", "[M+Na]+")

    p <- plot_composite_ms(spectras)
    p2 <- readRDS(system.file(
        "testdata",
        "plots.RDS",
        package = "SPRING"
    ))
    testthat::expect_true(all(na.omit(
        unlist(p[[1]]$attrs) == unlist(p2$plot_composite_ms[[1]]$attrs)
    )))
    testthat::expect_true(all(
        unlist(p[[1]]$config) == unlist(p2$plot_composite_ms[[1]]$config)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$layoutAttrs) ==
            unlist(p2$plot_composite_ms[[1]]$layoutAttrs)
    ))
    testthat::expect_true(all(
        unlist(p[[8]]) == unlist(p2$plot_composite_ms[[8]])
    ))
})

testthat::test_that("plot annotation MS", {
    db <- db_connect(system.file(
        "testdata",
        "220221CCM_global.sqlite",
        package = "SPRING"
    ))

    # 1st test if we have a connection
    testthat::expect_error(
        plot_annotation_ms(NULL, NULL),
        "db must be a connection to the sqlite database"
    )

    # 2nd test if we have a name
    testthat::expect_error(
        plot_annotation_ms(db, NULL),
        "name must be a character"
    )

    # 3rd test if we have a single name
    testthat::expect_error(
        plot_annotation_ms(db, c("FA 17:0", "LPC 11a:0")),
        "name must be only ONE compound"
    )

    # 4th test if the compound doesn't exist in database
    # the plot should be empty
    p <- plot_annotation_ms(db, "methanol")
    p2 <- readRDS(system.file(
        "testdata",
        "plots.RDS",
        package = "SPRING"
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$attrs) == unlist(p2$plot_annotation_ms[[1]][[1]]$attrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$layoutAttrs) ==
            unlist(p2$plot_annotation_ms[[1]][[1]]$layoutAttrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$config) == unlist(p2$plot_annotation_ms[[1]][[1]]$config)
    ))
    testthat::expect_true(all(
        unlist(p[[8]]) == unlist(p2$plot_annotation_ms[[1]][[8]])
    ))

    # 5th test : normal
    p <- plot_annotation_ms(db, "LPC 11a:0")
    RSQLite::dbDisconnect(db)
    testthat::expect_true(all(na.omit(
        unlist(p[[1]]$attrs) == unlist(p2$plot_annotation_ms[[2]][[1]]$attrs)
    )))
    testthat::expect_true(all(
        unlist(p[[1]]$layoutAttrs) ==
            unlist(p2$plot_annotation_ms[[2]][[1]]$layoutAttrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$config) == unlist(p2$plot_annotation_ms[[2]][[1]]$config)
    ))
    testthat::expect_true(all(
        unlist(p[[8]]) == unlist(p2$plot_annotation_ms[[2]][[8]])
    ))
})

testthat::test_that("empty chromato", {
    p <- plot_empty_chromato()
    p2 <- readRDS(system.file(
        "testdata",
        "plots.RDS",
        package = "SPRING"
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$attrs) == unlist(p2$plot_empty_chromato[[1]]$attrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$layoutAttrs) ==
            unlist(p2$plot_empty_chromato[[1]]$layoutAttrs)
    ))
})

testthat::test_that("plot EIC", {
    db_empty <- db_connect(":memory:")
    db <- db_connect(system.file(
        "testdata",
        "220221CCM_global.sqlite",
        package = "SPRING"
    ))
    p2 <- readRDS(system.file(
        "testdata",
        "plots.RDS",
        package = "SPRING"
    ))

    # 1st test : with no db
    testthat::expect_error(
        plot_eic(NULL, NULL),
        "db must be a connection to the sqlite database"
    )

    # 2nd test : with no group ID
    testthat::expect_error(
        plot_eic(db, NULL),
        "group_id must be numerical"
    )

    # 3rd test : with more than one group ID
    testthat::expect_error(
        plot_eic(db, c(1, 2)),
        "only one group_id is required"
    )

    # 4th test : with no sample files in database (not yet processed)
    p <- plot_eic(db_empty, 2)
    RSQLite::dbDisconnect(db_empty)
    testthat::expect_true(all(na.omit(
        unlist(p[[1]]$attrs) == unlist(p2$plot_eic[[1]][[1]]$attrs)
    )))
    testthat::expect_true(all(
        unlist(p[[1]]$layoutAttrs) == unlist(p2$plot_eic[[1]][[1]]$layoutAttrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$config) == unlist(p2$plot_eic[[1]][[1]]$config)
    ))
    testthat::expect_true(all(
        unlist(p[[8]]) == unlist(p2$plot_eic[[1]][[8]])
    ))

    # 5th test : group ID not exists
    p <- plot_eic(db, 999)
    testthat::expect_true(all(na.omit(
        unlist(p[[1]]$attrs) == unlist(p2$plot_eic[[1]][[1]]$attrs)
    )))
    testthat::expect_true(all(
        unlist(p[[1]]$layoutAttrs) == unlist(p2$plot_eic[[1]][[1]]$layoutAttrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$config) == unlist(p2$plot_eic[[1]][[1]]$config)
    ))
    testthat::expect_true(all(
        unlist(p[[8]]) == unlist(p2$plot_eic[[1]][[8]])
    ))

    # 6th test : feature integrated but in only one sample
    p <- plot_eic(db, 1)
    testthat::expect_true(all(na.omit(
        unlist(p[[1]]$attrs) == unlist(p2$plot_eic[[2]][[1]]$attrs)
    )))
    testthat::expect_true(all(
        unlist(p[[1]]$layoutAttrs) == unlist(p2$plot_eic[[2]][[1]]$layoutAttrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$config) == unlist(p2$plot_eic[[2]][[1]]$config)
    ))
    testthat::expect_true(all(
        unlist(p[[8]]) == unlist(p2$plot_eic[[2]][[8]])
    ))

    # 7th test : feature integrated in all the samples
    p <- plot_eic(db, 2)
    RSQLite::dbDisconnect(db)
    testthat::expect_true(all(na.omit(
        unlist(p[[1]]$attrs) == unlist(p2$plot_eic[[3]][[1]]$attrs)
    )))
    testthat::expect_true(all(
        unlist(p[[1]]$layoutAttrs) == unlist(p2$plot_eic[[3]][[1]]$layoutAttrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$config) == unlist(p2$plot_eic[[3]][[1]]$config)
    ))
    testthat::expect_true(all(
        unlist(p[[8]]) == unlist(p2$plot_eic[[3]][[8]])
    ))
})

testthat::test_that("plot raw TIC", {
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

    # 1st test : without the filepaths of the raw files
    testthat::expect_error(
        plot_raw_tic(c(1, 2)),
        "raw_files must be a vector of filepaths"
    )

    # 2nd test : raw files doesn't exists
    testthat::expect_error(
        plot_raw_tic("toto.mzML"),
        "cannot find toto.mzML"
    )

    # 3rd test : msacccess is not a character
    testthat::expect_error(
        plot_raw_tic(raw_files, 1),
        "msaccess must be a filepath to msaccess.exe"
    )

    # 4th test : more than one filepaths for msaccess
    testthat::expect_error(
        plot_raw_tic(raw_files, c("a", "b")),
        "msaccess must be a unique filepath"
    )

    # 5th test : msaccess filepath is not found
    testthat::expect_error(
        plot_raw_tic(raw_files, "a"),
        "cannot find msaccess executable"
    )

    # 6th test : polarity is not a character
    testthat::expect_error(
        plot_raw_tic(raw_files, msaccess, 1),
        "polarity must be a character"
    )

    # 7th test : more than one polarity
    testthat::expect_error(
        plot_raw_tic(raw_files, msaccess, c("a", "b")),
        "only one polarity is authorized"
    )

    # 8th test : polarity is not correct
    testthat::expect_error(
        plot_raw_tic(raw_files, msaccess, "toto"),
        "polarity must be \"positive\" or \"negative\""
    )

    p2 <- readRDS(system.file(
        "testdata",
        "plots.RDS",
        package = "SPRING"
    ))

    # 9th test : test with a negative polarity (should be empty)
    p <- plot_raw_tic(raw_files, msaccess, "negative")
    testthat::expect_true(all(na.omit(
        unlist(p[[1]]$attrs) == unlist(p2$plot_raw_tic[[1]][[1]]$attrs)
    )))
    testthat::expect_true(all(
        unlist(p[[1]]$layoutAttrs) ==
            unlist(p2$plot_raw_tic[[1]][[1]]$layoutAttrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$config) == unlist(p2$plot_raw_tic[[1]][[1]]$config)
    ))
    testthat::expect_true(all(
        unlist(p[[8]]) == unlist(p2$plot_raw_tic[[1]][[8]])
    ))

    # 10th test : test with a positive polarity
    p <- plot_raw_tic(raw_files, msaccess, "positive")
    testthat::expect_true(all(na.omit(
        unlist(p[[1]]$attrs) == unlist(p2$plot_raw_tic[[2]][[1]]$attrs)
    )))
    testthat::expect_true(all(
        unlist(p[[1]]$layoutAttrs) ==
            unlist(p2$plot_raw_tic[[2]][[1]]$layoutAttrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$config) == unlist(p2$plot_raw_tic[[2]][[1]]$config)
    ))
    testthat::expect_true(all(
        unlist(p[[8]]) == unlist(p2$plot_raw_tic[[2]][[8]])
    ))
})

testthat::test_that("plot Peak spot", {
    db_empty <- db_connect(":memory:")
    db <- db_connect(system.file(
        "testdata",
        "220221CCM_global.sqlite",
        package = "SPRING"
    ))

    # 1st test : without db
    testthat::expect_error(
        plot_peak_spot(NULL),
        "db must be a connection to the sqlite database"
    )

    # 2nd test : with no sample files in database (not yet processed)
    p <- plot_peak_spot(db_empty)
    p2 <- readRDS(system.file(
        "testdata",
        "plots.RDS",
        package = "SPRING"
    ))
    RSQLite::dbDisconnect(db_empty)
    testthat::expect_true(all(
        unlist(p[[1]]$attrs) == unlist(p2$plot_peak_spot[[1]][[1]]$attrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$layoutAttrs) ==
            unlist(p2$plot_peak_spot[[1]][[1]]$layoutAttrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$config) == unlist(p2$plot_peak_spot[[1]][[1]]$config)
    ))
    testthat::expect_true(all(
        unlist(p[[8]]) == unlist(p2$plot_peak_spot[[1]][[8]])
    ))

    # 3rd test : error on type of plot
    testthat::expect_error(
        plot_peak_spot(db, type = "type error"),
        "type must be \"Peak spot\" or \"Kendrick plot\""
    )

    # 4th test : Peak spot
    p <- plot_peak_spot(db, type = "Peak spot")
    testthat::expect_true(all(
        unlist(p[[1]]$attrs) == unlist(p2$plot_peak_spot[[2]][[1]]$attrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$layoutAttrs) ==
            unlist(p2$plot_peak_spot[[2]][[1]]$layoutAttrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$config) == unlist(p2$plot_peak_spot[[2]][[1]]$config)
    ))
    testthat::expect_true(all(
        unlist(p[[8]]) == unlist(p2$plot_peak_spot[[2]][[8]])
    ))


    # 5th test : test kendrick mass defect plot
    p <- plot_peak_spot(db, type = "Kendrick plot")
    testthat::expect_true(all(
        unlist(p[[1]]$attrs) == unlist(p2$plot_peak_spot[[3]][[1]]$attrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$layoutAttrs) ==
            unlist(p2$plot_peak_spot[[3]][[1]]$layoutAttrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$config) == unlist(p2$plot_peak_spot[[3]][[1]]$config)
    ))
    testthat::expect_true(all(
        unlist(p[[8]]) == unlist(p2$plot_peak_spot[[3]][[8]])
    ))


    # 6th test : error with annotation_filter
    testthat::expect_error(
        plot_peak_spot(db, annotation_filter = 1),
        "annotation_filter must be a character"
    )

    # 7th test : error with annotation_filter
    testthat::expect_error(
        plot_peak_spot(db, annotation_filter = "test"),
        "annotation_filter must be \"no annotated\", \"annotated\" or
            \"all\""
    )

    # 8th test : no annotated on Peak spot
    p <- plot_peak_spot(
        db,
        type = "Peak spot",
        annotation_filter = "no annotated"
    )
    testthat::expect_true(all(
        unlist(p[[1]]$attrs) == unlist(p2$plot_peak_spot[[4]][[1]]$attrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$layoutAttrs) ==
            unlist(p2$plot_peak_spot[[4]][[1]]$layoutAttrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$config) == unlist(p2$plot_peak_spot[[4]][[1]]$config)
    ))
    testthat::expect_true(all(
        unlist(p[[8]]) == unlist(p2$plot_peak_spot[[4]][[8]])
    ))

    # 9th test : no annotated on Kendrick plot
    p <- plot_peak_spot(
        db,
        type = "Kendrick plot",
        annotation_filter = "no annotated"
    )
    testthat::expect_true(all(
        unlist(p[[1]]$attrs) == unlist(p2$plot_peak_spot[[5]][[1]]$attrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$layoutAttrs) ==
            unlist(p2$plot_peak_spot[[5]][[1]]$layoutAttrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$config) == unlist(p2$plot_peak_spot[[5]][[1]]$config)
    ))
    testthat::expect_true(all(
        unlist(p[[8]]) == unlist(p2$plot_peak_spot[[5]][[8]])
    ))


    # 10th test : annotated on Peak spot
    p <- plot_peak_spot(db, type = "Peak spot", annotation_filter = "annotated")
    testthat::expect_true(all(
        unlist(p[[1]]$attrs) == unlist(p2$plot_peak_spot[[6]][[1]]$attrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$layoutAttrs) ==
            unlist(p2$plot_peak_spot[[6]][[1]]$layoutAttrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$config) == unlist(p2$plot_peak_spot[[6]][[1]]$config)
    ))
    testthat::expect_true(all(
        unlist(p[[8]]) == unlist(p2$plot_peak_spot[[6]][[8]])
    ))

    # 11th test : annotated on Kendrick map
    p <- plot_peak_spot(
        db,
        type = "Kendrick plot",
        annotation_filter = "annotated"
    )
    testthat::expect_true(all(
        unlist(p[[1]]$attrs) == unlist(p2$plot_peak_spot[[7]][[1]]$attrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$layoutAttrs) ==
            unlist(p2$plot_peak_spot[[7]][[1]]$layoutAttrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$config) == unlist(p2$plot_peak_spot[[7]][[1]]$config)
    ))
    testthat::expect_true(all(
        unlist(p[[8]]) == unlist(p2$plot_peak_spot[[7]][[8]])
    ))

    # 12th test : error on threshold
    testthat::expect_error(
        plot_peak_spot(db, int_threshold = "a"),
        "int_treshold must be a numeric"
    )

    # 13th test : test with a too high threshold (no possible points)
    p <- plot_peak_spot(db, type = "Peak spot", int_threshold = 10**12)
    testthat::expect_true(all(
        unlist(p[[1]]$attrs) == unlist(p2$plot_peak_spot[[1]][[1]]$attrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$layoutAttrs) ==
            unlist(p2$plot_peak_spot[[1]][[1]]$layoutAttrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$config) == unlist(p2$plot_peak_spot[[1]][[1]]$config)
    ))
    testthat::expect_true(all(
        unlist(p[[8]]) == unlist(p2$plot_peak_spot[[1]][[8]])
    ))

    # 14th test : threshold at 6M on Peak spot
    p <- plot_peak_spot(db, type = "Peak spot", int_threshold = 6000000)
    testthat::expect_true(all(
        unlist(p[[1]]$attrs) == unlist(p2$plot_peak_spot[[8]][[1]]$attrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$layoutAttrs) ==
            unlist(p2$plot_peak_spot[[8]][[1]]$layoutAttrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$config) == unlist(p2$plot_peak_spot[[8]][[1]]$config)
    ))
    testthat::expect_true(all(
        unlist(p[[8]]) == unlist(p2$plot_peak_spot[[8]][[8]])
    ))

    # 15th test : threshold at 6M on kendrick plot
    p <- plot_peak_spot(db, type = "Kendrick plot", int_threshold = 6000000)
    testthat::expect_true(all(
        unlist(p[[1]]$attrs) == unlist(p2$plot_peak_spot[[9]][[1]]$attrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$layoutAttrs) ==
            unlist(p2$plot_peak_spot[[9]][[1]]$layoutAttrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$config) == unlist(p2$plot_peak_spot[[9]][[1]]$config)
    ))
    testthat::expect_true(all(
        unlist(p[[8]]) == unlist(p2$plot_peak_spot[[9]][[8]])
    ))

    # 16th test : no annotated & threshold at 6M on Peak spot
    p <- plot_peak_spot(
        db,
        type = "Peak spot",
        annotation_filter = "no annotated",
        int_threshold = 6000000
    )
    testthat::expect_true(all(
        unlist(p[[1]]$attrs) == unlist(p2$plot_peak_spot[[10]][[1]]$attrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$layoutAttrs) ==
            unlist(p2$plot_peak_spot[[10]][[1]]$layoutAttrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$config) == unlist(p2$plot_peak_spot[[10]][[1]]$config)
    ))
    testthat::expect_true(all(
        unlist(p[[8]]) == unlist(p2$plot_peak_spot[[10]][[8]])
    ))

    # 17th test : no annotated & threshold at 6M on kendrick plot
    p <- plot_peak_spot(
        db,
        type = "Kendrick plot",
        annotation_filter = "no annotated",
        int_threshold = 6000000
    )
    testthat::expect_true(all(
        unlist(p[[1]]$attrs) == unlist(p2$plot_peak_spot[[11]][[1]]$attrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$layoutAttrs) ==
            unlist(p2$plot_peak_spot[[11]][[1]]$layoutAttrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$config) == unlist(p2$plot_peak_spot[[11]][[1]]$config)
    ))
    testthat::expect_true(all(
        unlist(p[[8]]) == unlist(p2$plot_peak_spot[[11]][[8]])
    ))

    # 18th test : annotated & threshold at 6M on Peak spot
    p <- plot_peak_spot(
        db,
        type = "Peak spot",
        annotation_filter = "annotated",
        int_threshold = 6000000
    )
    testthat::expect_true(all(
        unlist(p[[1]]$attrs) == unlist(p2$plot_peak_spot[[12]][[1]]$attrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$layoutAttrs) ==
            unlist(p2$plot_peak_spot[[12]][[1]]$layoutAttrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$config) == unlist(p2$plot_peak_spot[[12]][[1]]$config)
    ))
    testthat::expect_true(all(
        unlist(p[[8]]) == unlist(p2$plot_peak_spot[[12]][[8]])
    ))

    # 19th test : annotated & threshold at 6M on Kendrick plot
    p <- plot_peak_spot(
        db,
        type = "Kendrick plot",
        annotation_filter = "annotated",
        int_threshold = 6000000
    )
    RSQLite::dbDisconnect(db)
    testthat::expect_true(all(
        unlist(p[[1]]$attrs) == unlist(p2$plot_peak_spot[[13]][[1]]$attrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$layoutAttrs) ==
            unlist(p2$plot_peak_spot[[13]][[1]]$layoutAttrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$config) == unlist(p2$plot_peak_spot[[13]][[1]]$config)
    ))
    testthat::expect_true(all(
        unlist(p[[8]]) == unlist(p2$plot_peak_spot[[13]][[8]])
    ))
})
