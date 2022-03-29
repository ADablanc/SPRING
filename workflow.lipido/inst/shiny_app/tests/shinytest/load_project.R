app <- shinytest::ShinyDriver$new("../../")
app$snapshotInit("load_project")
app$waitForValue("project_load", ignore = list(NULL))
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
app$snapshot(
    items = list(
        output = c("project_name"),
        export = c("ann", "spectra_infos")
    ),
    screenshot = TRUE
)
