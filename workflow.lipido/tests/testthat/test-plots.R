testthat::test_that("plot empty MS", {
    p <- plot_empty_MS()
    p2 <- readRDS(system.file(
        "testdata",
        "plots.RDS",
        package = "workflow.lipido"
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
        package = "workflow.lipido"
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
        package = "workflow.lipido"
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
        package = "workflow.lipido"
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

testthat::test_that("plot empty heatmap", {
    p <- plot_empty_heatmap()
    p2 <- readRDS(system.file(
        "testdata",
        "plots.RDS",
        package = "workflow.lipido"
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$attrs) == unlist(p2$plot_empty_heatmap[[1]]$attrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$layoutAttrs) ==
            unlist(p2$plot_empty_heatmap[[1]]$layoutAttrs)
    ))
})

testthat::test_that("plot heatmap", {
    db_empty <- db_connect(":memory:")
    db <- db_connect(system.file(
        "testdata",
        "220221CCM_global-plots.sqlite",
        package = "workflow.lipido"
    ))

    # 1st test : without db
    testthat::expect_error(
        plot_heatmap(NULL, NULL),
        "db must be a connection to the sqlite database"
    )

    # 2nd test : without compounds
    testthat::expect_error(
        plot_heatmap(db, NULL),
        "name must be a character"
    )

    # 3rd test : without no compounds
    testthat::expect_error(
        plot_heatmap(db, character(0)),
        "name must contain at least ONE compound"
    )

    # 4th test : with no samples in database (no processing yet)
    # must return an empty heatmap
    p <- plot_heatmap(db_empty, "LPC 11:0")
    p2 <- readRDS(system.file(
        "testdata",
        "plots.RDS",
        package = "workflow.lipido"
    ))
    RSQLite::dbDisconnect(db_empty)
    testthat::expect_true(all(
        unlist(p[[1]]$attrs) == unlist(p2$plot_heatmap[[1]][[1]]$attrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$layoutAttrs) ==
            unlist(p2$plot_heatmap[[1]][[1]]$layoutAttrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$config) == unlist(p2$plot_heatmap[[1]][[1]]$config)
    ))
    testthat::expect_true(all(
        unlist(p[[8]]) == unlist(p2$plot_heatmap[[1]][[8]])
    ))

    # 5th test : normal
    p <- plot_heatmap(db, c("LPC 11:0", "Cer (d18:1/C12:0)"))
    RSQLite::dbDisconnect(db)
    testthat::expect_true(all(na.omit(
        unlist(p[[1]]$attrs) == unlist(p2$plot_heatmap[[2]][[1]]$attrs)
    )))
    testthat::expect_true(all(
        unlist(p[[1]]$layoutAttrs) ==
            unlist(p2$plot_heatmap[[2]][[1]]$layoutAttrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$config) == unlist(p2$plot_heatmap[[2]][[1]]$config)
    ))
    testthat::expect_true(all(
        unlist(p[[8]]) == unlist(p2$plot_heatmap[[2]][[8]])
    ))
})

testthat::test_that("empty chromato", {
    p <- plot_empty_chromato()
    p2 <- readRDS(system.file(
        "testdata",
        "plots.RDS",
        package = "workflow.lipido"
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
        "220221CCM_global-plots.sqlite",
        package = "workflow.lipido"
    ))

    # 1st test : with no db
    testthat::expect_error(
        plot_eic(NULL, NULL, NULL),
        "db must be a connection to the sqlite database"
    )

    # 2nd test : with no sample name
    testthat::expect_error(
        plot_eic(db, NULL, NULL),
        "sample name must be a character"
    )

    # 3rd test : with more than one sample name
    testthat::expect_error(
        plot_eic(
            db,
            c("220221CCM_global_POS_01_ssleu_filtered",
              "220221CCM_global_POS_02_ssleu_filtered"),
            NULL
        ),
        "sample name must contain only ONE sample name"
    )

    # 4th test : with no compound name
    testthat::expect_error(
        plot_eic(db, "220221CCM_global_POS_01_ssleu_filtered", NULL),
        "name must be a character"
    )

    # 5th test : with more than one compound name
    testthat::expect_error(
        plot_eic(
            db,
            "220221CCM_global_POS_01_ssleu_filtered",
            c("LPC 11:0", "Cer (d18:1/C12:0)")
        ),
        "name must contain only ONE compound"
    )

    # 6th test : no processing was made (no files in database yet)
    p <- plot_eic(
        db_empty,
        "220221CCM_global_POS_01_ssleu_filtered",
        "LPC 11:0"
    )
    p2 <- readRDS(system.file(
        "testdata",
        "plots.RDS",
        package = "workflow.lipido"
    ))
    RSQLite::dbDisconnect(db_empty)
    testthat::expect_true(all(
        unlist(p[[1]]$attrs) == unlist(p2$plot_eic[[1]][[1]]$attrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$layoutAttrs) == unlist(p2$plot_eic[[1]][[1]]$layoutAttrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$config) == unlist(p2$plot_eic[[1]][[1]]$config)
    ))
    testthat::expect_true(all(
        unlist(p[[8]]) == unlist(p2$plot_eic[[1]][[8]])
    ))

    # 7th test : compound not detected & not integrated
    p <- plot_eic(
        db,
        "220221CCM_global_POS_01_ssleu_filtered",
        "Cer (d18:1/C12:0)"
    )
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

    # 8th test : compound not detected but integrated
    p <- plot_eic(
        db,
        "220221CCM_global_POS_02_ssleu_filtered",
        "Cer (d18:1/C12:0)"
    )
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

    # 9th test : compound detected & with more than one adduct but one is
         # missing for this file & not the other one
    p <- plot_eic(db, "220221CCM_global_POS_01_ssleu_filtered", "LPC 11:0")
    testthat::expect_true(all(na.omit(
        unlist(p[[1]]$attrs) == unlist(p2$plot_eic[[4]][[1]]$attrs)
    )))
    testthat::expect_true(all(
        unlist(p[[1]]$layoutAttrs) == unlist(p2$plot_eic[[4]][[1]]$layoutAttrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$config) == unlist(p2$plot_eic[[4]][[1]]$config)
    ))
    testthat::expect_true(all(
        unlist(p[[8]]) == unlist(p2$plot_eic[[4]][[8]])
    ))

    # 10th test : compound detected & with more than one adduct
    p <- plot_eic(db, "220221CCM_global_POS_02_ssleu_filtered", "LPC 11:0")
    RSQLite::dbDisconnect(db)
    testthat::expect_true(all(na.omit(
        unlist(p[[1]]$attrs) == unlist(p2$plot_eic[[5]][[1]]$attrs)
    )))
    testthat::expect_true(all(
        unlist(p[[1]]$layoutAttrs) == unlist(p2$plot_eic[[5]][[1]]$layoutAttrs)
    ))

    testthat::expect_true(all(
        unlist(p[[1]]$config) == unlist(p2$plot_eic[[5]][[1]]$config)
    ))
    testthat::expect_true(all(
        unlist(p[[8]]) == unlist(p2$plot_eic[[5]][[8]])
    ))
})

testthat::test_that("plot empty m/z dev", {
    p <- plot_empty_mzdev()
    p2 <- readRDS(system.file(
        "testdata",
        "plots.RDS",
        package = "workflow.lipido"
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$attrs) == unlist(p2$plot_empty_mzdev[[1]]$attrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$layoutAttrs) ==
            unlist(p2$plot_empty_mzdev[[1]]$layoutAttrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$config) == unlist(p2$plot_empty_mzdev[[1]]$config)
    ))
    testthat::expect_true(all(
        unlist(p[[8]]) == unlist(p2$plot_empty_mzdev[[8]])
    ))
})

testthat::test_that("plot m/z dev", {
    db_empty <- db_connect(":memory:")
    db <- db_connect(system.file(
        "testdata",
        "220221CCM_global-plots.sqlite",
        package = "workflow.lipido"
    ))

    # 1st test : without db
    testthat::expect_error(
        plot_mzdev(NULL, NULL, NULL),
        "db must be a connection to the sqlite database"
    )

    # 2nd test : without sample name
    testthat::expect_error(
        plot_mzdev(db, NULL, NULL),
        "sample name must be a character"
    )

    # 3rd test : with more than one sample name
    testthat::expect_error(
        plot_mzdev(
            db,
            c("220221CCM_global_POS_01_ssleu_filtered",
              "220221CCM_global_POS_02_ssleu_filtered"),
            NULL
        ),
        "sample name must contain only ONE sample name"
    )

    # 4th test : without compound name
    testthat::expect_error(
        plot_mzdev(db, "220221CCM_global_POS_01_ssleu_filtered", NULL),
        "name must be a character"
    )

    # 5th test : with more than one compound name
    testthat::expect_error(
        plot_mzdev(
            db,
            "220221CCM_global_POS_01_ssleu_filtered",
            c("LPC 11:0", "Cer (d18:1/C12:0)")
        ),
        "name must contain only ONE compound"
    )

    # 7th test : with no sample files in database (not yet processed)
    p <- plot_mzdev(
            db_empty,
            "220221CCM_global_POS_01_ssleu_filtered",
            "LPC 11:0"
    )
    p2 <- readRDS(system.file(
        "testdata",
        "plots.RDS",
        package = "workflow.lipido"
    ))
    RSQLite::dbDisconnect(db_empty)
    testthat::expect_true(all(
        unlist(p[[1]]$attrs) == unlist(p2$plot_mzdev[[1]][[1]]$attrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$layoutAttrs) ==
            unlist(p2$plot_mzdev[[1]][[1]]$layoutAttrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$config) == unlist(p2$plot_mzdev[[1]][[1]]$config)
    ))
    testthat::expect_true(all(
        unlist(p[[8]]) == unlist(p2$plot_mzdev[[1]][[8]])
    ))

    # 8th test : with the wrong file
    p <- plot_mzdev(
        db,
        "220221CCM_global_POS_03_ssleu_filtered",
        "LPC 11:0"
    )
    testthat::expect_true(all(
        unlist(p[[1]]$attrs) == unlist(p2$plot_mzdev[[2]][[1]]$attrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$layoutAttrs) ==
            unlist(p2$plot_mzdev[[2]][[1]]$layoutAttrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$config) == unlist(p2$plot_mzdev[[2]][[1]]$config)
    ))
    testthat::expect_true(all(
        unlist(p[[8]]) == unlist(p2$plot_mzdev[[2]][[8]])
    ))

    # 9th test : compound not integrated
    p <- plot_mzdev(
        db,
        "220221CCM_global_POS_02_ssleu_filtered",
        "Cer (d18:1/C12:0)"
    )
    testthat::expect_true(all(
        unlist(p[[1]]$attrs) == unlist(p2$plot_mzdev[[3]][[1]]$attrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$layoutAttrs) ==
            unlist(p2$plot_mzdev[[3]][[1]]$layoutAttrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$config) == unlist(p2$plot_mzdev[[3]][[1]]$config)
    ))
    testthat::expect_true(all(
        unlist(p[[8]]) == unlist(p2$plot_mzdev[[3]][[8]])
    ))

    # 10th test : normal
    p <- plot_mzdev(
        db,
        "220221CCM_global_POS_01_ssleu_filtered",
        "LPC 11:0"
    )
    RSQLite::dbDisconnect(db)
    testthat::expect_true(all(
        unlist(p[[1]]$attrs) == unlist(p2$plot_mzdev[[4]][[1]]$attrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$layoutAttrs) ==
            unlist(p2$plot_mzdev[[4]][[1]]$layoutAttrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$config) == unlist(p2$plot_mzdev[[4]][[1]]$config)
    ))
    testthat::expect_true(all(
        unlist(p[[8]]) == unlist(p2$plot_mzdev[[4]][[8]])
    ))
})

testthat::test_that("plot EIC m/z dev", {
    db_empty <- db_connect(":memory:")
    db <- db_connect(system.file(
        "testdata",
        "220221CCM_global-plots.sqlite",
        package = "workflow.lipido"
    ))

    # 1st test : with no db
    testthat::expect_error(
        plot_eic_mzdev(NULL, NULL, NULL),
        "db must be a connection to the sqlite database"
    )

    # 2nd test : with no sample name
    testthat::expect_error(
        plot_eic_mzdev(db, NULL, NULL),
        "sample name must be a character"
    )

    # 3rd test : with more than one sample name
    testthat::expect_error(
        plot_eic_mzdev(
            db,
            c("220221CCM_global_POS_01_ssleu_filtered",
              "220221CCM_global_POS_02_ssleu_filtered"),
            NULL
        ),
        "sample name must contain only ONE sample name"
    )

    # 4th test : with no compound name
    testthat::expect_error(
        plot_eic_mzdev(db, "220221CCM_global_POS_01_ssleu_filtered", NULL),
        "name must be a character"
    )

    # 5th test : with more than one compound name
    testthat::expect_error(
        plot_eic_mzdev(
            db,
            "220221CCM_global_POS_01_ssleu_filtered",
            c("LPC 11:0", "PS 24:0")
        ),
        "name must contain only ONE compound"
    )

    # 6th test : no processing was made (no files in database yet)
    p <- plot_eic_mzdev(
        db_empty,
        "220221CCM_global_POS_01_ssleu_filtered",
        "LPC 11:0"
    )
    p2 <- readRDS(system.file(
        "testdata",
        "plots.RDS",
        package = "workflow.lipido"
    ))
    RSQLite::dbDisconnect(db_empty)
    testthat::expect_true(all(
        unlist(p[[1]]$attrs) == unlist(p2$plot_eic_mzdev[[1]][[1]]$attrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$layoutAttrs) ==
            unlist(p2$plot_eic_mzdev[[1]][[1]]$layoutAttrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$config) == unlist(p2$plot_eic_mzdev[[1]][[1]]$config)
    ))
    testthat::expect_true(all(
        unlist(p[[8]]) == unlist(p2$plot_eic_mzdev[[1]][[8]])
    ))

    # 7th test : compound not detected & not integrated
    p <- plot_eic_mzdev(
        db,
        "220221CCM_global_POS_01_ssleu_filtered",
        "Cer (d18:1/C12:0)"
    )
    testthat::expect_true(all(na.omit(
        unlist(p[[1]]$attrs) == unlist(p2$plot_eic_mzdev[[2]][[1]]$attrs)
    )))
    testthat::expect_true(all(
        unlist(p[[1]]$layoutAttrs) ==
            unlist(p2$plot_eic_mzdev[[2]][[1]]$layoutAttrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$config) == unlist(p2$plot_eic_mzdev[[2]][[1]]$config)
    ))
    testthat::expect_true(all(
        unlist(p[[8]]) == unlist(p2$plot_eic_mzdev[[2]][[8]])
    ))

    # 8th test : compound not detected but integrated
    p <- plot_eic_mzdev(
        db,
        "220221CCM_global_POS_02_ssleu_filtered",
        "Cer (d18:1/C12:0)"
    )
    testthat::expect_true(all(na.omit(
        unlist(p[[1]]$attrs) == unlist(p2$plot_eic_mzdev[[3]][[1]]$attrs)
    )))
    testthat::expect_true(all(
        unlist(p[[1]]$layoutAttrs) ==
            unlist(p2$plot_eic_mzdev[[3]][[1]]$layoutAttrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$config) == unlist(p2$plot_eic_mzdev[[3]][[1]]$config)
    ))
    testthat::expect_true(all(
        unlist(p[[8]]) == unlist(p2$plot_eic_mzdev[[3]][[8]])
    ))

    # 9th test : compound detected & with more than one adduct but one is
        # missing for this file & not the other one
    p <- plot_eic_mzdev(
        db,
        "220221CCM_global_POS_01_ssleu_filtered",
        "LPC 11:0"
    )
    testthat::expect_true(all(na.omit(
        unlist(p[[1]]$attrs) == unlist(p2$plot_eic_mzdev[[4]][[1]]$attrs)
    )))
    testthat::expect_true(all(
        unlist(p[[1]]$layoutAttrs) ==
            unlist(p2$plot_eic_mzdev[[4]][[1]]$layoutAttrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$config) == unlist(p2$plot_eic_mzdev[[4]][[1]]$config)
    ))
    testthat::expect_true(all(
        unlist(p[[8]]) == unlist(p2$plot_eic_mzdev[[4]][[8]])
    ))

    # 10th test : compound detected & with more than one adduct
    p <- plot_eic_mzdev(
        db,
        "220221CCM_global_POS_02_ssleu_filtered",
        "LPC 11:0"
    )
    RSQLite::dbDisconnect(db)
    testthat::expect_true(all(na.omit(
        unlist(p[[1]]$attrs) == unlist(p2$plot_eic_mzdev[[5]][[1]]$attrs)
    )))
    testthat::expect_true(all(
        unlist(p[[1]]$layoutAttrs) ==
            unlist(p2$plot_eic_mzdev[[5]][[1]]$layoutAttrs)
    ))
    testthat::expect_true(all(
        unlist(p[[1]]$config) == unlist(p2$plot_eic_mzdev[[5]][[1]]$config)
    ))
    testthat::expect_true(all(
        unlist(p[[8]]) == unlist(p2$plot_eic_mzdev[[5]][[8]])
    ))
})
