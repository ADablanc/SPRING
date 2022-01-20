testthat::test_that("get_ions", {
    db <- read.csv(system.file("extdata", "database.csv", 
        package = "workflow.lipido"))
    adduct_names <- c("2M+H", "M+H-H2O")
    instrument <- "QTOF_XevoG2-S_R25000@200"
    
    expect_identical(
        nrow(get_ions("C2N2", 
            adducts[which(adducts$Name == "M-H"), ], 
            instrument)), integer(1)
    )
    
    expect_identical(
        nrow(get_ions("C1H1", 
            adducts[which(adducts$Name == "M-H"), ], 
            instrument)), integer(1)
    )
    
    expect_identical(
        nrow(get_ions(c("C1H1", "C12H18Br6"), 
            adducts[which(adducts$Name == "M-H"), ], 
            instrument)), as.integer(14)
    )
    
    ions <- do.call(rbind, lapply(adduct_names, function(adduct_name) 
        get_ions(
            unique(db$formula), 
            adducts[which(adducts$Name == adduct_name), ], 
            instrument)))
    expect_identical(
        ions, 
        readRDS(system.file("testdata", "ions.rds", 
            package = "workflow.lipido"))
    )
})

testthat::test_that("load_db_ions", {
    expect_identical(
        load_db_ions(
            c("M+Na", "M+NH4", "M+H-H2O", "M+H"), 
            "QTOF_XevoG2-S_R25000@200"
        ), 
        readRDS(system.file("testdata", "db_ions.rds", 
            package = "workflow.lipido"))
    )
})
