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
        input = "check_data_cpd", # NULL
        output = c(
            "check_data_heatmap", # empty
            "check_data_eic_mzdev" # empty
        )
    ),
    screenshot = TRUE
)

# 2nd test : load a project
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
        output = c(
            "check_data_heatmap", # empty
            "check_data_eic_mzdev" # empty
        )
    ),
    screenshot = TRUE
)

# 3rd test : load LPC 11:0
app$setInputs(check_data_cpd = "LPC 11:0")
app$snapshot(
    items = list(
        output = c(
            # "x":"LPC 11:0",
            # "y":["220221CCM_global__01_ssleu_filtered",
                # "220221CCM_global__02_ssleu_filtered"]
            "check_data_heatmap",
            "check_data_eic_mzdev" # empty
        )
    ),
    screenshot = TRUE
)

# 4th test : load PS 24:0
app$setInputs(check_data_cpd = c("LPC 11:0", "FA 17:0"))
app$snapshot(
    items = list(
        output = c(
            # "x":["LPC 11:0","PS 24a:0"],
            # "y":["220221CCM_global__01_ssleu_filtered",
                # "220221CCM_global__02_ssleu_filtered"]
            "check_data_heatmap",
            "check_data_eic_mzdev" # empty
        )
    ),
    screenshot = TRUE
)

# 5th test : click on LPC 11:0 & first file
app$executeScript(
    "Shiny.onInputChange(
        \"check_data_heatmap_click\", {
            sample: \"220221CCM_global__01_ssleu_filtered\",
            cpd_name: \"LPC 11:0\"
        })"
)
app$snapshot(
    items = list(
        output = c(
            # "x":["LPC 11:0","FA 17:0"],
            # "y":["220221CCM_global__01_ssleu_filtered",
                # "220221CCM_global__02_ssleu_filtered"]
            "check_data_heatmap",
            "check_data_eic_mzdev"
        )
    ),
    screenshot = TRUE
)

# 6th test : click on PS 24a:0 to empty eic & mzdev
app$executeScript(
    "Shiny.onInputChange(
        \"check_data_heatmap_click\", {
            sample: \"220221CCM_global__01_ssleu_filtered\",
            cpd_name: \"FA 17:0\"
        })"
)
Sys.sleep(.5)
app$snapshot(
    items = list(
        output = c(
            # "x":["LPC 11:0", "FA 17:0"],
            # "y":["220221CCM_global__01_ssleu_filtered",
                # "220221CCM_global__02_ssleu_filtered"]
            "check_data_heatmap",
            "check_data_eic_mzdev" # empty
        )
    ),
    screenshot = TRUE
)

## Interrupt shinyProcess so covr::save_trace can execute onExit
p <- app$.__enclos_env__$private$shinyProcess
p$interrupt()
p$wait()
