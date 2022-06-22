app <- shinytest::ShinyDriver$new("../../", loadTimeout = 10000)
# app <- shinytest::ShinyDriver$new(
#     "SPRING/inst/shiny_app",
#     loadTimeout = 10000
# )
app$snapshotInit("load_project")

app$waitForValue("project_load", ignore = list(NULL))
app$executeScript(paste0("Shiny.setInputValue(\"project_modal_visible\", $(\"#",
                         "project_modal\").length != 0)"))
Sys.sleep(.5)
app$snapshot(
    items = list(
        input = "project_modal_visible", # TRUE
        output = "project_name", # ""
        export = "conflict_id" # 0
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
                package = "SPRING"
            ),
            "/"
        )[[1]][-1],
        "\"",
        sep = "",
        collapse = ", "
    )
))

Sys.sleep(.5)
app$executeScript(paste0("Shiny.setInputValue(\"project_modal_visible\", $(\"#",
                         "project_modal\").length !=  0)"))
app$snapshot(
    items = list(
        input = "project_modal_visible", # FALSE
        output = "project_name", # "220221CCM_global"
        export = "conflict_id" # 1
    ),
    screenshot = TRUE
)

## Interrupt shinyProcess so covr::save_trace can execute onExit
p <- app$.__enclos_env__$private$shinyProcess
p$interrupt()
p$wait()
