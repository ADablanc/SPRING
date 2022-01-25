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

testthat::test_that("load_db", {
    expect_identical(
        load_db(
            c("M+Na", "M+NH4", "M+H-H2O", "M+H"), 
            "QTOF_XevoG2-S_R25000@200"
        ), 
        readRDS(system.file("testdata", "load_db.rds", 
            package = "workflow.lipido"))
    )
})

testthat::test_that("compare spectras", {
    q_spectras <- list(
        # original
        data.frame(
            mz = c(734.570997942433, 735.574556763962, 736.564411787157, 
                737.56455162997, 738.614212665807), 
            abd = c(100, 40.9469794525382, 8.37980024341253, 3.71016159721596, 
                1.05998504396349)
        ), 
        
        # without iso
        data.frame(
            mz = c(734.570997942433), 
            abd = c(100)
        ), 
        
        # with problem on abundance (needs deisotoping)
        data.frame(
            mz = c(734.570997942433, 735.574556763962, 736.564411787157, 
                737.56455162997, 738.614212665807), 
            abd = c(100, 40.9469794525382, 128.37980024341253, 
                3.71016159721596, 1.05998504396349)
        ), 
        
        # huge mz deviation
        data.frame(
            mz = c(734.570997942433, 735.614556763962, 736.564411787157, 
                737.56455162997, 738.614212665807), 
            abd = c(100, 40.9469794525382, 8.37980024341253, 3.71016159721596, 
                1.05998504396349)
        ), 
        
        # missing A+1 (2nd most abundant)
        q_spectra <- data.frame(
            mz = c(734.570997942433, 736.564411787157, 737.56455162997, 
                738.614212665807), 
            abd = c(100, 8.37980024341253, 3.71016159721596, 1.05998504396349)
        )
    )
    l_spectras <- list(data.frame(
        mz = c(734.56943147, 735.57280138, 736.57581325, 737.57870281, 
            738.5815316, 739.58374177), 
        abd = c(100, 44.925872, 11.492671, 2.131437, 0.300167, 0.020691), 
        iso = c("M", "M+1", "M+2", "M+3", "M+4", "M+5")
    ))
    observed <- compare_spectras(q_spectras, l_spectras, da_tol = .05, 
        abd_tol = 25)[[1]]
    expected <- readRDS(system.file("testdata", "compare_spectras.rds", 
        package = "workflow.lipido"))[[1]]
    expect_identical(observed[[1]], expected[[1]])
    expect_identical(observed[[2]], expected[[2]])
    expect_identical(observed[[3]], expected[[3]])
    expect_identical(observed[[4]], expected[[4]])
    expect_identical(observed[[5]], expected[[5]])    
})
