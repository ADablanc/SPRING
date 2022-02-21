.workflow_lipido_env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
    utils::data(isotopes, package = "enviPat")
    utils::data(adducts, package = "enviPat")
    utils::data(resolution_list, package = "enviPat")
    # add adduct [M+H-H2O]+
    adducts <<- rbind(
        adducts,
        data.frame(
            Name = "M+H-H2O",
            calc = "M-19.01894",
            Charge = 1,
            Mult = 1,
            Mass = 19.01894,
            Ion_mode = "positive",
            Formula_add = "H1",
            Formula_ded = "H2O1",
            Multi = 1
        )
    )
    # rename all adducts this way: [M+...]+
    adducts$Name <<- paste0("[",
                            adducts$Name,
                            "]",
                            sapply(adducts$Charge, function(x)
                                paste0(if (abs(x) > 1)
                                    abs(x)
                                    else
                                        NULL,
                                    if (x < 1)
                                        "-"
                                    else
                                        "+")))
    adducts[which(adducts$Name == "[M+]+"), "Name"] <<- "[M]+"
    adducts[which(adducts$Name == "[M-]-"), "Name"] <<- "[M]-"
}
