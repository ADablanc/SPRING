testthat::test_that("filter ms file", {
    filepath <- system.file(
        "testdata",
        "small.mzXML",
        package = "workflow.lipido"
    )

    ms_file <- xcms::xcmsRaw(filepath, profstep = 0)

    # test with an rt range outside of the ms file
    filter_params <- methods::new(
        "FilterParam",
        mz_range = c(200, 2000),
        rt_range = c(5, 6)
    )
    testthat::expect_error(
        filter_ms_file(ms_file, filter_params),
        "no spectras between rt filters"
    )

    # test to get only the first scan with a restrictive rt range
    filter_params <- methods::new(
        "FilterParam",
        mz_range = c(200, 2000),
        rt_range = c(0.2, 0.3)
    )
    testthat::expect_equal(
        filter_ms_file(ms_file, filter_params)@scanrange,
        c(1, 1)
    )

    # test with a non restrictive rt range
    filter_params <- methods::new(
        "FilterParam",
        mz_range = c(200, 2000),
        rt_range = c(0, 0.5)
    )
    testthat::expect_identical(
        filter_ms_file(ms_file, filter_params)@scanrange,
        c(1, 2)
    )
})

testthat::test_that("check_ms_file", {
    filepath_pos <- system.file(
        "testdata",
        "small.mzXML",
        package = "workflow.lipido"
    )
    filepath_pos_neg <- system.file(
        "testdata",
        "small_pos-neg.mzXML",
        package = "workflow.lipido"
    )
    tmp_file <- gsub("\\\\", "/", tempfile(fileext = ".mzXML"))
    # test with an empty file
    testthat::expect_error(
        check_ms_file(tmp_file, "positive"),
        sprintf("xcmsSource: file not found: %s", tmp_file)
    )

    # test with the wrong polarity
    testthat::expect_error(
        check_ms_file(filepath_pos, "negative"),
        "no scans detected in desired polarity"
    )

    # test to split the file according polarity
        # (should return only the first only positive scan)
    testthat::expect_equal(
        check_ms_file(filepath_pos_neg, "positive")@scanrange,
        c(1, 1)
    )

    # test with a polarity which should return the same original file
    testthat::expect_equal(
        check_ms_file(filepath_pos, "positive")@scanrange,
        c(1, 2)
    )
})

testthat::test_that("conversion", {
    filepath <- system.file(
        "testdata",
        "small.raw",
        package = "workflow.lipido"
    )
    converter <- tools::file_path_as_absolute(
        "~/GitHub/workflow.lipido/pwiz/msconvert.exe"
    )
    filter_params <- methods::new(
        "FilterParam",
        polarity = "positive",
        mz_range = c(200, 2000),
        rt_range = c(0, 0.5)
    )

    # test with an absent .wiff.scan
    missing_filepath <- tempfile(fileext = ".wiff")
    testthat::expect_error(
        convert_file(missing_filepath, converter, filter_params),
        "missing corresponding wiff.scan in same directory"
    )

    # test with a raw waters directory (which doesn't exist)
    missing_filepath <- paste(tempdir(), ".raw", sep = ".")
    testthat::expect_error(
        convert_file(missing_filepath, converter, filter_params),
        "msconvert error"
    )

    # test with an missing mzXML file
    missing_filepath <- gsub("\\\\", "/", tempfile(fileext = ".mzXML"))
    testthat::expect_error(
        convert_file(missing_filepath, converter, filter_params),
        sprintf(
            "xcmsSource: file not found: %s/.*\\.mzXML",
            dirname(missing_filepath)
        )
    )

    # test with an missing mzML file
    missing_filepath <- gsub("\\\\", "/", tempfile(fileext = ".mzML"))
    testthat::expect_error(
        convert_file(missing_filepath, converter, filter_params),
        sprintf(
            "xcmsSource: file not found: %s/.*\\.mzML",
            dirname(missing_filepath)
        )
    )

    # test with a missing msconvert.exe
    testthat::expect_error(
        convert_file(filepath, "msconvert.exe", filter_params),
        "'\"msconvert.exe\"' not found"
    )

    # test with the wrong polarity for the ms file
    filter_params@polarity <- "negative"
    testthat::expect_error(
        convert_file(filepath, converter, filter_params),
        "upper value must be greater than lower value"
    )

    # test with a too restrictive m/z range
    # filter_params <- methods::new(
    #     "FilterParam",
    #     polarity = "positive",
    #     mz_range = c(3000, 4000),
    #     rt_range = c(0, 0.5)
    # )
    # testthat::expect_error(
    #     convert_file(filepath, converter, filter_params),
    #     "file converted cannot be read"
    # )

    # test with a too restrictive rT range
    filter_params <- methods::new(
        "FilterParam",
        polarity = "positive",
        mz_range = c(200, 2000),
        rt_range = c(50, 1000)
    )
    testthat::expect_error(
        convert_file(filepath, converter, filter_params),
        "upper value must be greater than lower value"
    )

    # test the conversion of RAW file to mzXML
    filter_params <- methods::new(
        "FilterParam",
        polarity = "positive",
        mz_range = c(200, 2001),
        rt_range = c(0, 0.5)
    )
    testthat::expect_equal(
        convert_file(filepath, converter, filter_params)@scanindex,
        c(0, 1810)
    )
})
