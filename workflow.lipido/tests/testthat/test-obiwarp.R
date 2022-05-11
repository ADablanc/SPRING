testthat::test_that("obiwarp", {
    # intialize parameters
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
        ),
        file.path(
            system.file(
                "testdata",
                package = "workflow.lipido"
            ),
            "220221CCM_global_POS_03_ssleu_filtered.mzML"
        )
    )
    sqlite_path <- tempfile(fileext = ".sqlite")
    converter <- tools::file_path_as_absolute(
        "~/GitHub/workflow.lipido/pwiz/msconvert.exe"
    )
    filter_params <- FilterParam(
        mz_range = c(200, 1000),
        rt_range = c(.7 * 60, 6.3 * 60)
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

    # record files
    db <- db_connect(sqlite_path)
    sample_names <- tools::file_path_sans_ext(basename(raw_files))
    db_record_samples(db, sample_names)
    a <- lapply(raw_files, function(raw_file)
        import_ms_file(
            db,
            tools::file_path_sans_ext(basename(raw_file)),
            raw_file,
            converter,
            "positive",
            filter_params
        )
    )
    xsets <- lapply(sample_names, function(sample_name)
        find_chrompeaks(
            db_read_ms_file(db, sample_name, "positive"),
            cwt_params,
            sample_name
        )
    )
    empty_peaklist <- matrix(, nrow = 0, ncol = 23, dimnames = list(
        c(), c("mz", "mzmin", "mzmax", "rt", "rtmin", "rtmax", "into",
               "intb", "maxo", "sn", "egauss", "mu", "sigma", "h", "f",
               "dppm", "scale", "scpos", "scmin", "scmax", "lmin", "lmax",
               "sample")
    ))
    peaks1 <- xsets[[1]]@peaks
    peaks2 <- xsets[[2]]@peaks
    xsets[[1]]@peaks <- empty_peaklist
    xsets[[2]]@peaks <- empty_peaklist

    # 1st test: with no file
    xset <- obiwarp(
        sqlite_path,
        sample_names[3],
        "positive",
        xsets[3],
        obw_params
    )
    expect_equal(
        xset@rt$raw[[1]] - xset@rt$corrected[[1]],
        integer(0)
    )

    # 2nd test: with one sample (no rT correction)
    xset <- obiwarp(
        sqlite_path,
        sample_names[1],
        "positive",
        xsets[1],
        obw_params
    )
    expect_equal(
        xset@rt$raw[[1]] - xset@rt$corrected[[1]],
        c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    )

    # 3rd test: with no peaks
    xset <- obiwarp(
        sqlite_path,
        sample_names[1:2],
        "positive",
        xsets[1:2],
        obw_params
    )
    expect_equal(
        lapply(1:2, function(i)
            xsets[[i]]@peaks[, "rt"] -
                xset@peaks[which(xset@peaks[, "sample"] == i), "rt"]
        ),
        list(
            integer(0),
            integer(0)
        )
    )

    # 4th test: with only peak for the second sample (no rT correction)
    xsets[[2]]@peaks <- peaks2
    xset <- obiwarp(
        sqlite_path,
        sample_names[1:2],
        "positive",
        xsets[1:2],
        obw_params
    )
    expect_equal(
        lapply(1:2, function(i)
            xsets[[i]]@peaks[, "rt"] -
                xset@peaks[which(xset@peaks[, "sample"] == i), "rt"]
        ),
        list(
            numeric(0),
            c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        )
    )

    # 4th test: with only ONE peak
    xsets[[1]]@peaks <- peaks1
    xsets[[2]]@peaks <- peaks2[1, , drop = FALSE]
    xset <- obiwarp(
        sqlite_path,
        sample_names[1:2],
        "positive",
        xsets[1:2],
        obw_params
    )
    expect_equal(
        lapply(1:2, function(i)
            xsets[[i]]@peaks[, "rt"] -
                xset@peaks[which(xset@peaks[, "sample"] == i), "rt"]
        ),
        list(
            c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
            setNames(-7.19999999999999, "rt")
        )
    )

    # 5th test : normal
    xsets[[2]]@peaks <- peaks2
    xset <- obiwarp(
        sqlite_path,
        sample_names[1:2],
        "positive",
        xsets[1:2],
        obw_params
    )
    expect_equal(
        lapply(1:2, function(i)
            xsets[[i]]@peaks[, "rt"] -
                xset@peaks[which(xset@peaks[, "sample"] == i), "rt"]
        ),
        list(
            c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
            c(-7.19999999999999, -7.19999999999999, -7.19999999999999, 0, 0, 0,
              0, 0, 0, 0, 0)
        )
    )
    # check that the file recorded have their `scantime_corrected` slot updated
    ms_files <- lapply(sample_names, function(sample_name)
        db_read_ms_file(db, sample_name, "positive")
    )
    testthat::expect_equal(
        xset@rt$corrected[[1]],
        ms_files[[1]]@scantime_corrected
    )
    testthat::expect_equal(
        xset@rt$corrected[[2]],
        ms_files[[2]]@scantime_corrected
    )

    # 6th test: with an empty ms file
    xset <- obiwarp(
        sqlite_path,
        sample_names,
        "positive",
        xsets,
        obw_params
    )
    expect_equal(
        lapply(seq(sample_names), function(i)
            xsets[[i]]@peaks[, "rt"] -
                xset@peaks[which(xset@peaks[, "sample"] == i), "rt"]
        ),
        list(
            c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
            c(-7.19999999999999, -7.19999999999999, -7.19999999999999, 0, 0, 0,
              0, 0, 0, 0, 0),
            numeric(0)
        )
    )
    # check that the file recorded have their `scantime_corrected` slot updated
    ms_files <- lapply(sample_names, function(sample_name)
        db_read_ms_file(db, sample_name, "positive")
    )
    testthat::expect_equal(
        xset@rt$corrected[[1]],
        ms_files[[1]]@scantime_corrected
    )
    testthat::expect_equal(
        xset@rt$corrected[[2]],
        ms_files[[2]]@scantime_corrected
    )
    RSQLite::dbDisconnect(db)
})
