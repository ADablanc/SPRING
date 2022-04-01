app <- shinytest::ShinyDriver$new("../../", loadTimeout = 10000)
# app <- shinytest::ShinyDriver$new(
#     "workflow.lipido/inst/shiny_app",
#     loadTimeout = 10000
# )
app$snapshotInit("check_data")

# 1st test : no output cause empty project
# create empty project
app$waitForValue("project_create", ignore = list(NULL))
app$setInputs(project_create = "click")
app$setInputs(project_create_name = runif(1))
app$executeScript(sprintf("
    Shiny.setInputValue(
        \"project_create_path\",
        {
            path: [\"%s\"],
            roots: \"Windows (C:)\"
        }
    )",
    # create the project in a temp dir
    gsub("C:/", "", gsub("\\\\", "/", tempdir()))
))
app$setInputs(project_create_valid = "click")
app$waitForValue(
    "project_name",
    iotype = "output",
    ignore = list("")
)
app$executeScript("$(\"a[href=\\\"#shiny-tab-check_data\\\"]\").click()")
app$snapshot(
    items = list(
        output = c("ui_check_data_adduct", "check_data_heatmap",
                   "check_data_eic", "check_data_mzdev")
    ),
    screenshot = TRUE
)

# 2nd test : empty heatmap cause no files processed
app$setInputs(check_data_cpd = "LPC 11:0")
app$snapshot(
    items = list(
        output = c("ui_check_data_adduct", "check_data_heatmap",
                   "check_data_eic", "check_data_mzdev")
    ),
    screenshot = TRUE
)

# 3rd test : load a project
app$setInputs(check_data_cpd = "")
sqlite_file <- system.file(
    "testdata",
    "220221CCM_global.sqlite",
    package = "workflow.lipido"
)
sqlite_file2 <- gsub("\\\\", "/", tempfile(fileext = ".sqlite"))
invisible(file.copy(sqlite_file, sqlite_file2))
app$executeScript(sprintf(
    "Shiny.setInputValue(
        \"project_load\",
        {
            files: {
                0: [%s]
            },
            roots: \"Windows (C:)\"
        }
    )",
    paste(
        "\"",
        strsplit(sqlite_file2, "/")[[1]][-1],
        "\"",
        sep = "",
        collapse = ", "
    )
))
app$waitForValue(
    "conflict_id",
    iotype = "export",
    ignore = list(0)
)
app$snapshot(
    items = list(
        output = c("ui_check_data_adduct", "check_data_heatmap",
                   "check_data_eic", "check_data_mzdev")
    ),
    screenshot = TRUE
)

# 4th test : click on new adduct
app$setInputs(check_data_adduct = "[M+NH4]+")
app$snapshot(
    items = list(
        output = c("check_data_heatmap", "check_data_eic", "check_data_mzdev")
    ),
    screenshot = TRUE
)

# 5th test : load LPC 11:0
app$setInputs(check_data_cpd = "LPC 11:0")
app$snapshot(
    items = list(
        output = c("check_data_heatmap", "check_data_eic", "check_data_mzdev")
    ),
    screenshot = TRUE
)

# 6th test : load PS 24:0
app$setInputs(check_data_cpd = c("LPC 11:0", "PS 24a:0"))
app$snapshot(
    items = list(
        output = c("check_data_heatmap", "check_data_eic", "check_data_mzdev")
    ),
    screenshot = TRUE
)

# 7th test : click on LPC 11:0 & first file
app$executeScript(
    "Shiny.onInputChange(
        \"check_data_heatmap_click\", {
            sample: \"220221CCM_global__01_ssleu_filtered\",
            cpd_name: \"LPC 11:0\"
        })"
)
Sys.sleep(1)
app$snapshot(
    items = list(
        output = c("check_data_heatmap", "check_data_eic", "check_data_mzdev")
    ),
    screenshot = TRUE
)

# 8th test : click on adduct to change the mzdev plot
app$setInputs(check_data_adduct = "[M+H]+")
app$snapshot(
    items = list(
        output = c("check_data_heatmap", "check_data_eic", "check_data_mzdev")
    ),
    screenshot = TRUE
)

# 9th test : click on PS 24a:0 to empty eic & mzdev
app$executeScript(
    "Shiny.onInputChange(
        \"check_data_heatmap_click\", {
            sample: \"220221CCM_global__01_ssleu_filtered\",
            cpd_name: \"PS 24a:0\"
        })"
)
Sys.sleep(1)
app$snapshot(
    items = list(
        output = c("check_data_heatmap", "check_data_eic", "check_data_mzdev")
    ),
    screenshot = TRUE
)
