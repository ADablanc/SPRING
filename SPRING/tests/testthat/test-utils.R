testthat::test_that("escape_regex", {
    testthat::expect_equal(
        escape_regex("[M+CH3COOH]- doesn't exists in the adduct list"),
        "\\[M\\+CH3COOH\\]- doesn't exists in the adduct list"
    )
    testthat::expect_equal(
        escape_regex("file(s) C:/small.mzXML and C:/small.mzML doesn't exist"),
        "file\\(s\\) C:/small\\.mzXML and C:/small\\.mzML doesn't exist"
    )
    testthat::expect_equal(
        escape_regex("converter is not found at C:/pwiz/msconvert.exe"),
        "converter is not found at C:/pwiz/msconvert\\.exe"
    )
})

testthat::test_that("get_available_database", {
    # move all databases files for the test
    tmp_dir <- tempdir()
    databases <- list.files(system.file(
        "extdata",
        "database",
        package = "SPRING"
    ), full.names = TRUE)
    file.copy(databases, tmp_dir)
    file.remove(databases)
    testthat::expect_error(
        get_available_database(),
        "No database is available in application "
    )

    # normal test
    file.copy(file.path(tmp_dir, basename(databases)), dirname(databases[1]))
    testthat::expect_gt(length(get_available_database()), 0)
})
