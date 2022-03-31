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

testthat::test_that("without NA", {
    testthat::expect_equal(
        without_na(c(1, 2, 3)),
        c(1, 2, 3)
    )
    testthat::expect_equal(
        without_na(c(1, NA, 3, NA)),
        c(1, 3)
    )
})
