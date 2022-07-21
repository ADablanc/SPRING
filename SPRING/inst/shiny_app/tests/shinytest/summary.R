app <- shinytest::ShinyDriver$new("../../", loadTimeout = 10000)
# app <- shinytest::ShinyDriver$new(
#     "SPRING/inst/shiny_app",
#     loadTimeout = 10000
# )
app$snapshotInit("summary")

app$executeScript("$(\"a[href=\\\"#shiny-tab-summary\\\"]\").click()")
empty_summary_table <- app$waitForValue(
    "summary_table",
    iotype = "output",
    ignore = list(NULL)
)

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

# 1st test : get an empty summary table
app$waitForValue("summary_table", iotype = "output", ignore = list(NULL))
app$snapshot(
    items = list(
        output = "summary_table" # should be empty
    ),
    screenshot = TRUE
)

# load a project
sqlite_file <- system.file(
    "testdata",
    "220221CCM_global.sqlite",
    package = "SPRING"
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

# 2nd test : see if we have now a table
app$snapshot(
    items = list(
        output = "summary_table"
    ),
    screenshot = TRUE
)

# 3rd test : see if we can change the intensities reported
app$setInputs(summary_by = "all")
app$snapshot(
    items = list(
        output = "summary_table"
    ),
    screenshot = TRUE
)

# see if we can upload the xlsx file
# app$snapshotDownload("summary_export")

## Interrupt shinyProcess so covr::save_trace can execute onExit
p <- app$.__enclos_env__$private$shinyProcess
p$interrupt()
p$wait()
