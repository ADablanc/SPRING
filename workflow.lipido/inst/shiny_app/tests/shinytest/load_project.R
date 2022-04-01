app <- shinytest::ShinyDriver$new("../../", loadTimeout = 10000)
# app <- shinytest::ShinyDriver$new(
#     "workflow.lipido/inst/shiny_app",
#     loadTimeout = 10000
# )
app$snapshotInit("load_project")

Sys.sleep(2)
app$executeScript(paste0("Shiny.setInputValue(\"project_modal_visible\", $(\"#",
                         "project_modal\").length !=  0)"))
app$snapshot(
    items = list(
        input = "project_modal_visible",
        output = "project_name",
        export = "conflict_id"
    ),
    screenshot = TRUE
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

Sys.sleep(1)
app$executeScript(paste0("Shiny.setInputValue(\"project_modal_visible\", $(\"#",
                         "project_modal\").length !=  0)"))
app$snapshot(
    items = list(
        input = "project_modal_visible",
        output = "project_name",
        export = "conflict_id"
    ),
    screenshot = TRUE
)
