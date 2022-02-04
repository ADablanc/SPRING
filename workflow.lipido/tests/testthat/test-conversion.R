testthat::test_that("conversion", {
    converter <-
        "C:/Users/sebastien.hutinet/Documents/GitHub/workflow.lipido/pwiz/msconvert.exe"
    if (!file.exists(converter))
        testthat::skip("no msconvert.exe founded")

    testthat::expect_error(
        convert_files(c(), converter = NULL, outdir = NULL),
        "you must give at least one raw file to convert"
    )

    testthat::expect_error(
        convert_files(12723, converter = NULL, outdir = NULL),
        "raw_files argument must be a vector of filepaths"
    )

    testthat::expect_error(
        convert_files("C:/small.txt", converter = NULL, outdir = NULL),
        "file extension of small.txt are not supported"
    )

    testthat::expect_error(
        convert_files(
            "C:/small.mzXML",
            converter = NULL,
            outdir = NULL
        ),
        escape_regex("file(s) C:/small.mzXML doesn't exist")
    )

    testthat::expect_error(
        convert_files(
            system.file("testdata", "small.RAW", package = "workflow.lipido"),
            converter = NULL,
            outdir = NULL
        ),
        "converter argument must be a filepath to the msconvert exe"
    )

    testthat::expect_error(
        convert_files(
            system.file("testdata", "small.RAW", package = "workflow.lipido"),
            converter = c("a", "b"),
            outdir = NULL
        ),
        "converter argument must contain only one filepath"
    )

    testthat::expect_error(
        convert_files(
            system.file("testdata", "small.RAW", package = "workflow.lipido"),
            converter = "C:/msconvert.exe",
            outdir = NULL
        ),
        escape_regex("converter is not found at C:/msconvert.exe")
    )

    testthat::expect_error(
        convert_files(
            system.file("testdata", "small.RAW", package = "workflow.lipido"),
            converter = converter,
            outdir = NULL
        ),
        "outdir argument must be a filepath"
    )

    testthat::expect_error(
        convert_files(
            system.file("testdata", "small.RAW", package = "workflow.lipido"),
            converter = converter,
            outdir = c("a", "b")
        ),
        "outdir argument must contain only one filepath"
    )

    wiff_file <- tempfile(fileext = ".WIFF")
    if (!file.exists(wiff_file))
        invisible(capture.output(file.create(wiff_file)))
    testthat::expect_identical(
        convert_files(
            wiff_file,
            converter = converter,
            outdir = tempdir(),
            show_txt_pb = FALSE
        ),
        matrix(
            c(
                tools::file_path_sans_ext(basename(wiff_file)),
                rep("missing corresponding wiff.scan in same directory", 2)
            ),
            nrow = 1,
            ncol = 3,
            dimnames = list(c(), c("sample", "pos", "neg"))
        )
    )

    cdf_file <- tempfile(fileext = ".CDF")
    if (!file.exists(cdf_file))
        invisible(capture.output(file.create(cdf_file)))
    testthat::expect_identical(
        convert_files(
            cdf_file,
            converter = converter,
            outdir = tempdir(),
            show_txt_pb = FALSE
        ),
        matrix(
            c(
                tools::file_path_sans_ext(basename(cdf_file)),
                rep("file converted cannot be read", 2)
            ),
            nrow = 1,
            ncol = 3,
            dimnames = list(c(), c("sample", "pos", "neg"))
        )
    )

    mzxml_file <- file.path(tempdir(), "a", "test.mzXML")
    if (!dir.exists(dirname(mzxml_file)))
        invisible(capture.output(dir.create(dirname(mzxml_file))))
    if (!file.exists(mzxml_file))
        invisible(capture.output(file.create(mzxml_file)))
    testthat::expect_identical(
        convert_files(
            mzxml_file,
            converter = converter,
            outdir = tempdir(),
            show_txt_pb = FALSE
        ),
        matrix(
            c(
                tools::file_path_sans_ext(basename(mzxml_file)),
                rep("file converted cannot be read", 2)
            ),
            nrow = 1,
            ncol = 3,
            dimnames = list(c(), c("sample", "pos", "neg"))
        )
    )

    raw_file <- tempfile(fileext = ".raw")
    if (!file.exists(raw_file))
        invisible(capture.output(file.create(raw_file)))
    testthat::expect_identical(
        convert_files(
            raw_file,
            converter = converter,
            outdir = tempdir(),
            show_txt_pb = FALSE
        ),
        matrix(
            c(
                tools::file_path_sans_ext(basename(raw_file)),
                rep("msconvert error", 2)
            ),
            nrow = 1,
            ncol = 3,
            dimnames = list(c(), c("sample", "pos", "neg"))
        )
    )

    testthat::expect_identical(
        convert_files(
            system.file("testdata", "small.RAW", package = "workflow.lipido"),
            converter = converter,
            outdir = tempdir(),
            show_txt_pb = FALSE
        ),
        matrix(
            c("small", "SUCCESS", "no scans detected"),
            nrow = 1,
            ncol = 3,
            dimnames = list(c(), c("sample", "pos", "neg"))
        )
    )
})

testthat::test_that("split ms file", {
    mzML_file <- tempfile(fileext = "mzML")
    if (!file.exists(mzML_file))
        invisible(capture.output(file.copy(
            system.file("testdata", "to_split.mzML",
                        package = "workflow.lipido"),
            mzML_file
        )))
    ms_file <- MSnbase::readMSData(mzML_file, mode = "onDisk")
    testthat::expect_identical(split_ms_file(ms_file, 0), "SUCCESS")
    testthat::expect_identical(split_ms_file(ms_file, 1), "SUCCESS")
})
