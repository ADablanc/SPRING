testthat::test_that("filter ms file", {
    test_filepath <- system.file(
        "testdata",
        "small.mzXML",
        package = "SPRING"
    )
    filepath <- tempfile(fileext = ".mzXML")
    invisible(file.copy(test_filepath, filepath, overwrite = TRUE))
    ms_file <- MSnbase::readMSData(filepath, mode = "onDisk")

    # 1st test : with an rt range outside of the ms file
    filter_params <- methods::new(
        "FilterParam",
        mz_range = c(200, 2000),
        rt_range = c(5, 6)
    )
    testthat::expect_error(
        filter_ms_file(ms_file, filter_params@rt_range),
        "no spectras between rt filters"
    )

    # 2nd test : get only the first scan with a restrictive rt range
    filter_params <- methods::new(
        "FilterParam",
        mz_range = c(200, 2000),
        rt_range = c(0.2, 0.3)
    )
    suppressWarnings(
        new_filepath <- filter_ms_file(ms_file, filter_params@rt_range, FALSE)
    )
    new_ms_file <- MSnbase::readMSData(new_filepath, mode = "onDisk")
    # filepath should change cause the file was rewritten elsewhere
    testthat::expect_false(
        new_filepath == filepath
    )
    testthat::expect_equal(
        MSnbase::rtime(new_ms_file),
        MSnbase::rtime(ms_file)[1]
    )

    # 3rd test : get only the first scan with a restrictive rt range
    filter_params <- methods::new(
        "FilterParam",
        mz_range = c(200, 2000),
        rt_range = c(0.2, 0.3)
    )
    new_filepath <- filter_ms_file(ms_file, filter_params@rt_range)
    new_ms_file <- MSnbase::readMSData(new_filepath, mode = "onDisk")
    invisible(file.copy(test_filepath, filepath, overwrite = TRUE))
    # filepath should not change cause the file was overwritten
    testthat::expect_identical(
        new_filepath,
        normalizePath(filepath)
    )
    testthat::expect_equal(
        MSnbase::rtime(new_ms_file),
        MSnbase::rtime(ms_file)[1]
    )

    # 4th test : with a non restrictive rt range
    filter_params <- methods::new(
        "FilterParam",
        mz_range = c(200, 2000),
        rt_range = c(0, 0.5)
    )
    new_filepath <- filter_ms_file(ms_file, filter_params@rt_range, FALSE)
    new_ms_file <- MSnbase::readMSData(new_filepath, mode = "onDisk")
    # filepath should not change cause the file was not rewritten
    testthat::expect_identical(
        new_filepath,
        normalizePath(filepath)
    )
    testthat::expect_equal(
        MSnbase::rtime(new_ms_file),
        MSnbase::rtime(ms_file)
    )
})

testthat::test_that("split ms file", {
    test_filepath <- system.file(
        "testdata",
        "small_pos-neg.mzXML",
        package = "SPRING"
    )
    filepath <- tempfile(fileext = ".mzXML")
    invisible(file.copy(test_filepath, filepath, overwrite = TRUE))
    ms_file <- MSnbase::readMSData(filepath, mode = "onDisk")

    # 1st test : get only positive scans
    new_filepath <- suppressWarnings(split_ms_file(ms_file, "positive", FALSE))
    new_ms_file <- MSnbase::readMSData(new_filepath, mode = "onDisk")
    # filepath should change cause the file was rewritten elsewhere
    testthat::expect_false(
        new_filepath == filepath
    )
    suppressWarnings(testthat::expect_equal(
        MSnbase::peaksCount(new_ms_file)[[1]],
        MSnbase::peaksCount(ms_file)[[1]],
    ))

    # 2nd test : get only positive scans
    new_filepath <- split_ms_file(ms_file, "positive")
    new_ms_file <- MSnbase::readMSData(new_filepath, mode = "onDisk")
    # filepath should not change cause the file was overwritten
    invisible(file.copy(test_filepath, filepath, overwrite = TRUE))
    testthat::expect_false(
        new_filepath == filepath
    )
    suppressWarnings(testthat::expect_equal(
        MSnbase::peaksCount(new_ms_file)[[1]],
        MSnbase::peaksCount(ms_file)[[1]],
    ))

    # 3rd test : get only negative scans
    new_filepath <- split_ms_file(ms_file, "negative", FALSE)
    new_ms_file <- MSnbase::readMSData(new_filepath, mode = "onDisk")
    # filepath should change cause the file was rewritten elsewhere
    testthat::expect_false(
        new_filepath == filepath
    )
    suppressWarnings(testthat::expect_equal(
        MSnbase::peaksCount(new_ms_file)[[1]],
        MSnbase::peaksCount(ms_file)[[2]],
    ))

    # 2nd test : get only negative scans
    new_filepath <- split_ms_file(ms_file, "negative")
    new_ms_file <- MSnbase::readMSData(new_filepath, mode = "onDisk")
    # filepath should not change cause the file was overwritten
    testthat::expect_false(
        new_filepath == filepath
    )
    suppressWarnings(testthat::expect_equal(
        MSnbase::peaksCount(new_ms_file)[[1]],
        MSnbase::peaksCount(ms_file)[[2]],
    ))
})

testthat::test_that("check_ms_file", {
    test_filepath_pos <- system.file(
        "testdata",
        "small.mzXML",
        package = "SPRING"
    )
    test_filepath_pos_neg <- system.file(
        "testdata",
        "small_pos-neg.mzXML",
        package = "SPRING"
    )
    filepath_pos <- tempfile(fileext = ".mzXML")
    invisible(file.copy(test_filepath_pos, filepath_pos, overwrite = TRUE))
    filepath_pos_neg <- tempfile(fileext = ".mzXML")
    invisible(file.copy(
        test_filepath_pos_neg,
        filepath_pos_neg,
        overwrite = TRUE
    ))
    ms_file_pos <- MSnbase::readMSData(filepath_pos, mode = "onDisk")
    ms_file_pos_neg <- MSnbase::readMSData(filepath_pos_neg, mode = "onDisk")

    filter_param_pos <- methods::new(
        "FilterParam",
        polarity = "positive",
        mz_range = c(200, 2000),
        rt_range = c(0, 0.5)
    )
    filter_param_neg <- methods::new(
        "FilterParam",
        polarity = "negative",
        mz_range = c(200, 2000),
        rt_range = c(0, 0.5)
    )

    tmp_file <- gsub("\\\\", "/", tempfile(fileext = ".mzXML"))
    # test with an empty file
    testthat::expect_error(
        suppressWarnings(check_ms_file(tmp_file, filter_param_pos)),
        escape_regex("inherits(x, \"mzR\") is not TRUE")
    )

    # test with the wrong polarity
    testthat::expect_error(
        check_ms_file(filepath_pos, filter_param_neg),
        "no scans detected in desired polarity"
    )

    # test to split the file according polarity
        # (should return only the first only positive scan)
    new_filepath_pos_neg <- check_ms_file(filepath_pos_neg, filter_param_pos)
    new_ms_file_pos_neg <- MSnbase::readMSData(
        new_filepath_pos_neg,
        mode = "onDisk"
    )
    suppressWarnings(testthat::expect_equal(
        MSnbase::peaksCount(new_ms_file_pos_neg)[[1]],
        MSnbase::peaksCount(ms_file_pos_neg)[[1]],
    ))

    # test with a polarity which should return the same original file
    new_filepath_pos <- check_ms_file(filepath_pos, filter_param_pos)
    new_ms_file_pos <- MSnbase::readMSData(
        new_filepath_pos,
        mode = "onDisk"
    )
    suppressWarnings(testthat::expect_equal(
        MSnbase::peaksCount(new_ms_file_pos),
        MSnbase::peaksCount(ms_file_pos),
    ))
})

testthat::test_that("conversion", {
    raw_filepath <- system.file(
        "testdata",
        "small.raw",
        package = "SPRING"
    )
    mzxml_filepath <- system.file(
        "testdata",
        "small.mzxml",
        package = "SPRING"
    )
    ms_file <- MSnbase::readMSData(mzxml_filepath, mode = "onDisk")
    converter <- tools::file_path_as_absolute(
        "~/GitHub/SPRING/pwiz/msconvert.exe"
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
        invisible(capture.output(
            convert_file(missing_filepath, converter, filter_params)
        )),
        "msconvert error"
    )

    # test with an missing mzXML file
    missing_filepath <- gsub("\\\\", "/", tempfile(fileext = ".mzXML"))
    testthat::expect_error(
        suppressWarnings(
            convert_file(missing_filepath, converter, filter_params)
        ),
        escape_regex("inherits(x, \"mzR\") is not TRUE")
    )

    # test with an missing mzML file
    missing_filepath <- gsub("\\\\", "/", tempfile(fileext = ".mzML"))
    suppressWarnings(testthat::expect_error(
        convert_file(missing_filepath, converter, filter_params),
        escape_regex("inherits(x, \"mzR\") is not TRUE")
    ))

    # test with a missing msconvert.exe
    testthat::expect_error(
        convert_file(raw_filepath, "msconvert.exe", filter_params),
        "'\"msconvert.exe\"' not found"
    )

    # test with the wrong polarity for the ms file
    filter_params@polarity <- "negative"
    testthat::expect_error(
        suppressWarnings(convert_file(raw_filepath, converter, filter_params)),
        "attempt to set an attribute on NULL"
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
        suppressWarnings(convert_file(raw_filepath, converter, filter_params)),
        "attempt to set an attribute on NULL"
    )

    # test the conversion of RAW file to mzXML
    filter_params <- methods::new(
        "FilterParam",
        polarity = "positive",
        mz_range = c(200, 2001),
        rt_range = c(0, 0.5)
    )
    new_mzxml_filepath <- convert_file(raw_filepath, converter, filter_params)
    new_ms_file <- MSnbase::readMSData(new_mzxml_filepath, mode = "onDisk")
    suppressWarnings(testthat::expect_equal(
        MSnbase::peaksCount(new_ms_file),
        MSnbase::peaksCount(ms_file),
    ))
})
