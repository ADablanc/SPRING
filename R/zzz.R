.onLoad <- function(libname, pkgname) {
    require(MSnbase)
    utils::data(isotopes, package = "enviPat")
    utils::data(adducts, package = "enviPat")
    utils::data(resolution_list, package = "enviPat")
    # add adduct [M+H-H2O]+
    adducts <<- rbind(adducts, data.frame(
        Name = "M+H-H2O", 
        calc = "M-19.01894",  
        Charge = 1, 
        Mult = 1, 
        Mass = 19.01894, 
        Ion_mode = "positive", 
        Formula_add = "H1", 
        Formula_ded = "H2O1", 
        Multi = 1
    ))
}
