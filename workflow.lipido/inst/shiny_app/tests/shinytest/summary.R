app <- ShinyDriver$new("../../")
# app <- ShinyDriver$new("workflow.lipido/inst/shiny_app")
app$snapshotInit("summary")

# init
empty_project_name <- app$waitForValue(
    "project_name",
    iotype = "output",
    ignore = list(NULL)
)
empty_ann <- app$waitForValue(
    "ann",
    iotype = "export",
    ignore = list(NULL)
)
app$executeScript("$(\"a[href=\\\"#shiny-tab-summary\\\"]\").click()")
empty_summary_table <- app$waitForValue(
    "summary_table",
    iotype = "output",
    ignore = list(NULL)
)

# load a project
app$waitForValue("project_load", ignore = list(NULL))
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
        strsplit(
            system.file(
                "testdata",
                "220221CCM_global.sqlite",
                package = "workflow.lipido"
            ),
            "/"
        )[[1]][-1],
        "\"",
        sep = "",
        collapse = ", "
    )
))
app$waitForValue(
    "project_name",
    iotype = "output",
    ignore = list(empty_project_name)
)
app$waitForValue(
    "ann",
    iotype = "export",
    ignore = list(empty_ann)
)

# see if we have a table
app$waitForValue(
    "summary_table",
    iotype = "export",
    ignore = list(empty_summary_table)
)
app$snapshot(items = list(output = "summary_table"), screenshot = TRUE)

# see if we can upload the xlsx file
# app$snapshotDownload("summary_export")
