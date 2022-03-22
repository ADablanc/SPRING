app <- ShinyDriver$new("../../")
app$snapshotInit("load_project")
app$waitForValue("project_load", ignore = list(NULL))

app$setInputs(project_load = "click")
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
app$waitForValue("project_name", iotype = "output", ignore = list(NULL))
app$snapshot()
