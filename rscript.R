Sys.setenv(RENV_DOWNLOAD_FILE_METHOD = "libcurl")
options(stringsAsFactors = FALSE)
options(browser = tools::file_path_as_absolute("Chromium/Chrome.exe"))
options(show.error.messages = TRUE)

source("renv/activate.R")
renv::restore(prompt = FALSE)
devtools::load_all("workflow.lipido")
run_shiny_app(NULL)
