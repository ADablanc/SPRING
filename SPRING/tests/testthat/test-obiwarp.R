testthat::test_that("obiwarp", {
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

    sqlite_path <- system.file(
        "testdata",
        "220221CCM_global.sqlite",
        package = "SPRING"
    )

    empty_peaklist <- matrix(, nrow = 0, ncol = 23, dimnames = list(
        c(), c("mz", "mzmin", "mzmax", "rt", "rtmin", "rtmax", "into",
               "intb", "maxo", "sn", "egauss", "mu", "sigma", "h", "f",
               "dppm", "scale", "scpos", "scmin", "scmax", "lmin", "lmax",
               "sample")
    ))
    filepaths <- c(
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
    sample_names <- tools::file_path_sans_ext(basename(filepaths))
    db <- db_connect(sqlite_path)
    ms_files <- lapply(sample_names, function(sample_name) {
        db_read_ms_file(db, sample_name)
    })
    RSQLite::dbDisconnect(db)
    xsets <- lapply(1:2, function(i) {
        object <- methods::new("xcmsSet")
        object@filepaths <- filepaths[i]
        from_paths <- xcms::phenoDataFromPaths(filepaths[i])
        object@phenoData <- from_paths
        rownames(object@phenoData) <- sample_names[i]
        object@profinfo <- xcms::profinfo(ms_files[[i]])
        object@peaks <- empty_peaklist
        object@rt <- list(ms_files[[i]]@scantime)
        attributes(object)$mzrange <- ms_files[[i]]@mzrange
        object@mslevel <- ms_files[[i]]@mslevel
        object
    })

    peaks <- matrix(c(464.447304014051, 504.440032161331, 505.443534603,
                      427.265397484755, 448.243644005027, 426.261908233279,
                      428.267772595199, 428.267855601901, 429.270423563444,
                      429.270339913969, 429.269607476748, 428.2675574767,
                      428.267840674347, 505.44383474773, 464.447454557257,
                      504.44048100644, 429.270885645465, 408.251325886321,
                      448.244163142448, 427.265671264404, 426.262343531217,
                      428.268471709284, 428.268539638095, 429.271192885151,
                      429.270782294993, 464.447021484375, 504.439697265625,
                      505.443084716797, 427.264739990234, 448.243011474609,
                      426.261444091797, 428.267364501953, 428.267425537109,
                      429.269836425781, 429.269653320312, 429.269256591797,
                      428.267242431641, 428.267364501953, 505.440704345703,
                      464.446746826172, 504.439544677734, 429.270324707031,
                      408.251037597656, 448.242279052734, 427.264801025391,
                      426.262023925781, 428.268035888672, 428.267822265625,
                      429.270721435547, 429.270416259766, 464.447662353516,
                      504.440490722656, 505.443664550781, 427.266052246094,
                      448.245391845703, 426.262420654297, 428.268310546875,
                      428.268615722656, 429.271362304688, 429.270904541016,
                      429.269958496094, 428.268035888672, 428.2685546875,
                      505.444122314453, 464.447967529297, 504.441101074219,
                      429.271575927734, 408.25146484375, 448.245422363281,
                      427.266052246094, 426.262786865234, 428.269195556641,
                      428.269287109375, 429.271636962891, 429.271636962891,
                      197.973, 197.444, 197.444, 286.81, 286.81, 286.81,
                      279.407, 258.782, 279.407, 258.782, 296.857, 297.915,
                      308.494, 197.444, 197.973, 197.973, 258.782, 286.278,
                      286.807, 286.807, 286.807, 278.875, 259.31, 278.875,
                      306.904, 196.386, 193.743, 196.386, 284.695, 285.224,
                      284.695, 265.656, 250.85, 265.656, 250.85, 295.271,
                      293.684, 304.789, 196.387, 195.858, 182.11, 250.85,
                      284.692, 280.99, 283.105, 284.692, 265.656, 250.85,
                      265.127, 301.084, 200.617, 199.03, 199.03, 291.04, 291.04,
                      291.04, 293.156, 264.598, 294.742, 264.598, 307.966,
                      303.202, 310.081, 201.145, 199.559, 208.548, 264.069,
                      287.864, 292.095, 292.623, 293.152, 291.037, 264.598,
                      291.037, 309.549, 4945601.93026269, 1287181.56877954,
                      401071.227087501, 1170639.95871094, 260064.992761365,
                      6139220.0505469, 21634957.3317308, 7556081.77126924,
                      5360632.29847273, 1854836.50349039, 450077.636764323,
                      2235868.3566111, 753309.518850004, 444013.097852865,
                      5689144.27927454, 1458245.19191226, 1621835.871345,
                      88824.635233072, 288524.169413714, 1186664.89020643,
                      6234084.85605467, 19992518.2568646, 7375409.9176154,
                      4939357.04715561, 323001.699462891, 4945487.79875439,
                      1287161.27013964, 401069.111887501, 1170297.42409805,
                      251531.954604928, 6137759.54643873, 20459048.501706,
                      6979304.74283053, 5183447.80457555, 1771087.19378474,
                      359253.245444391, 1369338.9068287, 436642.365357695,
                      443940.884671369, 5689141.10698883, 1458054.42380843,
                      1621823.181105, 88821.9918997387, 288157.380699577,
                      1185672.72711171, 6233652.48268746, 18599650.9700816,
                      6636339.71966301, 4303464.43650285, 210066.951190656,
                      4130684, 1395886, 434808.75, 676902.5, 183977.625,
                      3501824, 1096165, 647081.5, 266683.75, 158095.875,
                      90547.25, 387221, 271995.5, 524308, 5734716, 1723138,
                      150635.375, 70041.5625, 186253.875, 673140, 3489368,
                      1078367, 654868, 260907.75, 68280.5, 9148, 24876, 434808,
                      2254, 2, 3876, 10, 6, 13, 8, 2, 1, 1, 4708, 5734715,
                      14733, 150634, 70041, 719, 1279, 10826, 10, 6, 6, 1, NA,
                      NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                      NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                      NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                      NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                      NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                      NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                      NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                      NA, 1, 2, 3, 4, 5, 6, 7, 7, 8, 8, 9, 10, 13, 1, 2, 3, 4,
                      5, 6, 7, 8, 9, 9, 10, 14, 1, 2, 1, 2, 4, 1, 1, 1, 0, 0, 1,
                      1, 3, 7, 2, 3, 1, 1, 2, 1, 1, 0, 1, 1, 3, 4, 4, 4, 4, 6,
                      4, 10, 14, 10, 14, 12, 4, 4, 4, 14, 18, 14, 4, 6, 6, 6,
                      10, 14, 10, 6, 68, 67, 67, 236, 236, 236, 222, 183, 222,
                      183, 255, 257, 277, 67, 68, 68, 183, 235, 236, 236, 236,
                      221, 184, 221, 274, 64, 63, 63, 232, 230, 232, 212, 169,
                      212, 169, 243, 253, 273, 63, 54, 50, 169, 231, 230, 230,
                      230, 211, 170, 211, 268, 72, 71, 71, 240, 242, 240, 232,
                      197, 232, 197, 267, 261, 281, 71, 82, 86, 197, 239, 242,
                      242, 242, 231, 198, 231, 280, 65, 60, 65, 108, 104, 108,
                      136, 108, 136, 108, 108, 104, 103, 65, 64, 38, 108, 108,
                      101, 105, 108, 136, 108, 110, 100, 73, 70, 70, 120, 115,
                      120, 188, 134, 191, 134, 132, 122, 113, 74, 71, 88, 133,
                      114, 122, 123, 124, 184, 134, 159, 116, 1, 1, 1, 1, 1, 1,
                      1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2),
                    nrow = 25, ncol = 23, dimnames = list(
                        c(), c("mz", "mzmin", "mzmax", "rt", "rtmin", "rtmax",
                               "into", "intb", "maxo", "sn", "egauss", "mu",
                               "sigma", "h", "f", "dppm", "scale", "scpos",
                               "scmin", "scmax", "lmin", "lmax", "sample")))
    peaks1 <- peaks[peaks[, "sample"] == 1, ]
    peaks2 <- peaks[peaks[, "sample"] == 2, ]

    # 1st test: with no peaks
    testthat::expect_error(
        obiwarp(
            sqlite_path,
            sample_names,
            xsets,
            obw_params
        ),
        "No peaks where integrated in all the samples !"
    )

    # 2nd test: with one sample (no rT correction)
    xsets[[2]]@peaks <- peaks2
    xset <- obiwarp(
        sqlite_path,
        sample_names[2],
        xsets[2],
        obw_params
    )
    testthat::expect_equal(
        xset@rt$raw[[1]] - xset@rt$corrected[[1]],
        rep(0, 301)
    )

    # 3rd test: with only peak for the second sample (no rT correction)
    xset <- obiwarp(
        sqlite_path,
        sample_names,
        xsets,
        obw_params
    )
    expect_equal(
        lapply(1:2, function(i) {
            xsets[[i]]@peaks[, "rt"] -
                xset@peaks[which(xset@peaks[, "sample"] == i), "rt"]
        }),
        list(
            numeric(0),
            c(rep(0, 11), -.01)
        )
    )

    # 4th test: with only ONE peak
    xsets[[1]]@peaks <- peaks1
    xsets[[2]]@peaks <- peaks2[1, , drop = FALSE]
    xset <- obiwarp(
        sqlite_path,
        sample_names,
        xsets,
        obw_params
    )
    expect_equal(
        lapply(1:2, function(i) {
            xsets[[i]]@peaks[, "rt"] -
                xset@peaks[which(xset@peaks[, "sample"] == i), "rt"]
        }),
        list(
            rep(0, 13),
            setNames(0, "rt")
        )
    )

    # 5th test : normal
    xsets[[2]]@peaks <- peaks2
    xset <- obiwarp(
        sqlite_path,
        sample_names,
        xsets,
        obw_params
    )
    expect_equal(
        lapply(1:2, function(i) {
            xsets[[i]]@peaks[, "rt"] -
                xset@peaks[which(xset@peaks[, "sample"] == i), "rt"]
        }),
        list(
            rep(0, 13),
            rep(0, 12)
        )
    )
    # check that the file recorded have their `scantime_corrected` slot updated
    testthat::expect_equal(
        xset@rt$corrected[[1]],
        ms_files[[1]]@scantime_corrected
    )
    testthat::expect_equal(
        xset@rt$corrected[[2]],
        ms_files[[2]]@scantime_corrected
    )
})
