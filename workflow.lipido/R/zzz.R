.workflow_lipido_env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
    methods::getClass("xcmsSet", where = "xcms")
    utils::data(isotopes, package = "enviPat")
    utils::data(resolution_list, package = "enviPat")
    adducts <<- read.csv(system.file(
        "extdata",
        "adducts.csv",
        package = "workflow.lipido"
    ))
}
