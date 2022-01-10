testthat::test_that("hello world", {
    testthat::expect_identical(
        hello_world(),
        list(c("foo", "bar"), c(0, 1))
    )
})
