testthat::test_that("resolve conflicts", {
    observed <- readRDS(system.file("testdata", "process.rds", 
        package = "workflow.lipido"))@ann
    # line1 is to test when we have to regroup two line 
    #       with the same compounds but with different spectras
    # line2 is to test when we have to regroup two line 
    #       with the same compounds but no different spectras
    line1 <- line2 <- observed[1, ]
    line2[1, 19:20] <- NA
    expect_identical(
        filtrate_ann(rbind(observed, line1, line2)), 
        readRDS(system.file("testdata", "filtrate_ann.rds", 
            package = "workflow.lipido"))
    )
})

testthat::test_that("resolve conflicts", {
    expect_identical(
        resolve_conflicts(readRDS(system.file("testdata", "process.rds", 
            package = "workflow.lipido"))@ann), 
        readRDS(system.file("testdata", "resolve_conflicts.rds", 
            package = "workflow.lipido"))
    )
})

testthat::test_that("summarise annotations", {
    expect_identical(
        summarise_ann(readRDS(system.file("testdata", "resolve_conflicts.rds", 
            package = "workflow.lipido"))), 
        readRDS(system.file("testdata", "summarise_ann.rds", 
            package = "workflow.lipido"))
    )
})
