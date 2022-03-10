testthat::test_that("obiwarp", {
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
        )
    )
    sqlite_path <- tempfile(fileext = ".sqlite")
    converter <- tools::file_path_as_absolute(
        "~/GitHub/workflow.lipido/pwiz/msconvert.exe"
    )
    filter_params <- FilterParam(
        mz_range = c(300, 1000),
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
            cwt_params
        )
    )
    xsets <- obiwarp(
        sqlite_path,
        sample_names,
        "positive",
        xsets,
        obw_params
    )
    expect_equal(
        lapply(seq(sample_names), function(i)
            xsets@rt$raw[[i]] - xsets@rt$corrected[[i]]),
        list(
            rep(0, 247),
            c(0.00200073242186249, -0.215306640624988, -0.428853271484371,
              -0.639431518554687, -0.846668090820316, -1.05051721191407,
              -1.25066662597655, -1.44783239746093, -1.64164123535156,
              -1.8321083984375, -2.01921862792969, -2.2026444091797,
              -2.38307128906251, -2.56015649414061, -2.73388476562499,
              -2.90395910644531, -3.23473779296876, -3.70559204101562,
              -3.85597436523437, -4.00301501464844, -4.14674450683594,
              -4.42395776367186, -4.55769274902343, -4.81521398925781,
              -4.93874902343751, -5.1763575439453, -5.29019506835937,
              -5.50768542480469, -5.61158947753907, -5.71215185546876,
              -5.80943359374999, -5.90321398925781, -5.993888671875,
              -6.16528930664063, -6.24588610839845, -6.32333142089843,
              -6.39749609374999, -6.46833435058593, -6.5359072265625,
              -6.60005505371095, -6.66102087402345, -6.71870605468749,
              -6.77311059570312, -6.82412060546875, -6.87194860839844,
              -6.91649597167969, -6.95773217773439, -6.99565014648437,
              -7.03034033203124, -7.06174987792969, -7.11474230957032,
              -7.13628759765626, -7.16965722656249, -7.1814287109375,
              -7.18993481445312, -7.1951531982422, -7.19712854003907,
              -7.19585375976561, -7.19128308105468, -7.18348571777344,
              -7.17241479492188, -7.15807849121094, -7.1404920654297,
              -7.11969421386718, -7.09559228515624, -7.068240234375,
              -7.03763806152344, -7.00380102539063, -6.96676782226564,
              -6.92641528320311, -6.88284313964843, -6.83600561523437,
              -6.78603295898438, -6.73271044921876, -6.67616833496095,
              -6.6163760986328, -6.48721728515625, -6.41772045898438,
              -6.34500402832032, -6.26891540527345, -6.01985998535156,
              -5.92985534667969, -5.83653247070313, -5.73960034179689,
              -5.63918920898436, -5.42824328613281, -5.317501953125,
              -5.20334265136719, -5.08576538085939, -4.96475488281251,
              -4.84057873535156, -4.71276281738281, -4.58154418945313,
              -4.44693811035157, -4.30919689941408, -4.1678311767578,
              -4.02309326171874, -3.87498315429687, -3.72351611328125,
              -3.56897497558595, -3.41080932617189, -3.24930200195311,
              -2.74508911132813, -2.57027612304688, -2.39215197753907,
              -2.21073193359376, -2.02635986328124, -1.83834802246093,
              -1.452482421875, -1.25500305175783, -1.05389916992186,
              -0.849529907226554, -0.641910522460932, -0.431041015624999,
              -0.217341552734382, -2.31933594818656e-06, rep(0, 132)
          )
        )
    )
    RSQLite::dbDisconnect(db)
})
