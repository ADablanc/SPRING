testthat::test_that("filter ms file", {
    ms_file <- xcms::xcmsRaw(
        system.file(
            "testdata",
            "small.mzXML",
            package = "workflow.lipido"
        ),
        profstep = 0
    )

    # test with an rt range outside of the ms file
    filter_params <- FilterParam(
        mz_range = c(200, 2000),
        rt_range = c(5, 6)
    )
    testthat::expect_error(
        filter_ms_file(ms_file, filter_params),
        "no spectras between rt filters"
    )

    # test to get only the first scan with a restrictive rt range
    filter_params <- FilterParam(
        mz_range = c(200, 2000),
        rt_range = c(0.2, 0.3)
    )
    testthat::expect_equal(
        filter_ms_file(ms_file, filter_params)@scanrange,
        c(1, 1)
    )

    # test with a non restrictive rt range
    filter_params <- FilterParam(
        mz_range = c(200, 2000),
        rt_range = c(0, 0.5)
    )
    testthat::expect_identical(
        filter_ms_file(ms_file, filter_params)@scanrange,
        c(1, 2)
    )
})

testthat::test_that("check_ms_file", {
    # test with an empty file
    testthat::expect_error(
        check_ms_file(tempfile(fileext = ".mzXML"), "positive"),
        "file converted cannot be read"
    )

    # test with the wrong polarity
    testthat::expect_error(
        check_ms_file(
            system.file(
                "testdata",
                "small.mzXML",
                package = "workflow.lipido"
            ),
            "negative"
        ),
        "no scans detected in desired polarity"
    )

    # test to split the file according polarity
        # (should return only the first only positive scan)
    testthat::expect_equal(
        check_ms_file(
            system.file(
                "testdata",
                "small_pos-neg.mzXML",
                package = "workflow.lipido"
            ),
            "positive"
        )@scanrange,
        c(1, 1)
    )

    # test with a polarity which should return the same original file
    testthat::expect_equal(
        check_ms_file(
            system.file(
                "testdata",
                "small.mzXML",
                package = "workflow.lipido"
            ),
            "positive"
        )@scanrange,
        c(1, 2)
    )
})

testthat::test_that("conversion", {
    converter <- tools::file_path_as_absolute(
        "~/GitHub/workflow.lipido/pwiz/msconvert.exe"
    )
    filter_params <- FilterParam(
        mz_range = c(200, 2000),
        rt_range = c(0, 0.5)
    )

    # test with an absent .wiff.scan
    testthat::expect_error(
        convert_file(
            tempfile(fileext = ".wiff"),
            converter,
            "positive",
            filter_params
        ),
        "missing corresponding wiff.scan in same directory"
    )

    # test with a raw waters directory (which doesn't exist)
    testthat::expect_error(
        convert_file(
            paste(tempdir(), ".raw", sep = "."),
            converter,
            "positive",
            filter_params
        ),
        "msconvert error"
    )

    # test with an missing mzXML file
    testthat::expect_error(
        convert_file(
            tempfile(fileext = ".mzXML"),
            converter,
            "positive",
            filter_params
        ),
        "file converted cannot be read"
    )

    # test with an missing mzML file
    testthat::expect_error(
        convert_file(
            tempfile(fileext = ".mzML"),
            converter,
            "positive",
            filter_params
        ),
        "file converted cannot be read"
    )

    # test with a missing msconvert.exe
    testthat::expect_error(
        convert_file(
            system.file(
                "testdata",
                "small.raw",
                package = "workflow.lipido"
            ),
            "msconvert.exe",
            "positive",
            filter_params
        ),
        "'\"msconvert.exe\"' not found"
    )

    # test with a absurd polarity
    testthat::expect_error(
        convert_file(
            system.file(
                "testdata",
                "small.raw",
                package = "workflow.lipido"
            ),
            converter,
            "impossible polarity",
            filter_params
        ),
        "msconvert error"
    )

    # test with the wrong polarity for the ms file
    testthat::expect_error(
        convert_file(
            system.file(
                "testdata",
                "small.raw",
                package = "workflow.lipido"
            ),
            converter,
            "negative",
            filter_params
        ),
        "file converted cannot be read"
    )

    # test with a too restrictive m/z range
    filter_params <- FilterParam(
        mz_range = c(3000, 4000),
        rt_range = c(0, 0.5)
    )
    testthat::expect_error(
        convert_file(
            system.file(
                "testdata",
                "small.raw",
                package = "workflow.lipido"
            ),
            converter,
            "positive",
            filter_params
        ),
        "file converted cannot be read"
    )

    # test with a too restrictive rT range
    filter_params <- FilterParam(
        mz_range = c(200, 2000),
        rt_range = c(50, 1000)
    )
    testthat::expect_error(
        convert_file(
            system.file(
                "testdata",
                "small.raw",
                package = "workflow.lipido"
            ),
            converter,
            "positive",
            filter_params
        ),
        "file converted cannot be read"
    )

    # test the conversion of RAW file to mzXML
    filter_params <- FilterParam(
        mz_range = c(200, 2001),
        rt_range = c(0, 0.5)
    )
    testthat::expect_equal(
        convert_file(
            system.file(
                "testdata",
                "small.raw",
                package = "workflow.lipido"
            ),
            converter,
            "positive",
            filter_params
        )@scanindex,
        c(0, 1810)
    )
})
