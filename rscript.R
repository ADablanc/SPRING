pb <- utils::winProgressBar(
	title = sprintf("Starting app"),
	label = "Initializing ...")

Sys.setenv(RENV_DOWNLOAD_FILE_METHOD = "libcurl")
Sys.setenv(PATH = paste(tools::file_path_as_absolute("Rtools\\bin"), Sys.getenv("PATH"), sep = ";"))
options(stringsAsFactors = FALSE)
options(show.error.messages = TRUE)

utils::setWinProgressBar(pb, 0, label = "Activate application ...")
source("renv/activate.R")
# renv::upgrade(prompt = FALSE)
utils::setWinProgressBar(pb, value = .25,
    label = "Restore library...(could take some time if first time)")
renv::restore(library = "R-Portable/library", clean = TRUE, prompt = FALSE)
utils::setWinProgressBar(pb, value = 1, label = "Loading app...")
devtools::load_all("SPRING")
close(pb)
run_shiny_app(converter = "pwiz/msconvert.exe")
