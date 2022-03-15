testthat::test_that("peak picking", {
    empty_peaklist <- matrix(, nrow = 0, ncol = 23, dimnames = list(
        c(), c("mz", "mzmin", "mzmax", "rt", "rtmin", "rtmax", "into",
               "intb", "maxo", "sn", "egauss", "mu", "sigma", "h", "f",
               "dppm", "scale", "scpos", "scmin", "scmax", "lmin", "lmax",
               "sample")
    ))
    cwt_params <- xcms::CentWaveParam(
        ppm = 30,
        peakwidth = c(4, 39),
        snthresh = 6.5,
        prefilter = c(2, 815),
        mzCenterFun = "wMean",
        integrate = 1,
        mzdiff = .041,
        fitgauss = FALSE,
        noise = 0,
        verboseColumns = TRUE,
        firstBaselineCheck = FALSE
    )

    # 1st test with no file
    testthat::expect_identical(
        find_chrompeaks(NULL, cwt_params, "small")@peaks,
        empty_peaklist
    )

    # 2nd test with no peaks
    ms_file <- xcms::xcmsRaw(
        system.file("testdata", "small.mzXML", package = "workflow.lipido"),
        profstep = 0
    )
    testthat::expect_identical(
        find_chrompeaks(ms_file, cwt_params, "small")@peaks,
        empty_peaklist
    )

    # 3rd test with peaks
    ms_file <- xcms::xcmsRaw(
        system.file(
            "testdata",
            "220221CCM_global_POS_01_ssleu_filtered.mzML",
            package = "workflow.lipido"
        ),
        profstep = 0
    )
    testthat::expect_equal(
        data.frame(find_chrompeaks(
            ms_file,
            cwt_params,
            "220221CCM_global_POS_01_ssleu_filtered"
        )@peaks),
        peaks <- data.frame(
            mz = c(464.447304014051, 504.440032161331, 505.443534603,
                   427.265397484755, 426.261908233279),
            mzmin = c(464.447021484375, 504.439697265625, 505.443084716797,
                      427.264739990234, 426.261444091797),
            mzmax = c(464.447662353516, 504.440490722656, 505.443664550781,
                      427.266052246094, 426.262420654297),
            rt = c(197.973, 197.444, 197.444, 286.81, 286.81),
            rtmin = c(196.386, 193.743, 196.386, 284.695, 284.695),
            rtmax = c(200.617, 199.03, 199.03, 291.04, 291.04),
            into = c(4945601.93026269, 1287181.56877954, 401071.227087501,
                     1170639.95871094, 6139220.0505469),
            intb = c(4945487.79875439, 1287161.27013964, 401069.111887501,
                     1170297.42409805, 6137759.54643873),
            maxo = c(4130684, 1395886, 434808.75, 676902.5, 3501824),
            sn = c(9148, 24876, 434808, 2254, 3876),
            egauss = as.numeric(c(NA, NA, NA, NA, NA)),
            mu = as.numeric(c(NA, NA, NA, NA, NA)),
            sigma = as.numeric(c(NA, NA, NA, NA, NA)),
            h = as.numeric(c(NA, NA, NA, NA, NA)),
            f = c(1, 2, 3, 4, 6),
            dppm = c(1, 2, 1, 2, 1),
            scale = c(4, 4, 4, 4, 4),
            scpos = c(68, 67, 67, 236, 236),
            scmin = c(64, 63, 63, 232, 232),
            scmax = c(72, 71, 71, 240, 240),
            lmin = c(65, 60, 65, 108, 108),
            lmax = c(73, 70, 70, 120, 120),
            sample = c(1, 1, 1, 1, 1)
        )
    )
})
