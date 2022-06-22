app <- shinytest::ShinyDriver$new("../../", loadTimeout = 10000)
# app <- shinytest::ShinyDriver$new(
#     "SPRING/inst/shiny_app",
#     loadTimeout = 10000
# )
app$snapshotInit("database")

app$executeScript("$(\"a[href=\\\"#shiny-tab-database\\\"]\").click()")
app$waitForValue(
    "database_name",
    iotype = "output",
    ignore = list("")
)
app$setInputs(database_name = "test")
app$snapshot(
    items = list(
        output = "database_table" # contain all database entries
    ),
    screenshot = TRUE
)

## Interrupt shinyProcess so covr::save_trace can execute onExit
p <- app$.__enclos_env__$private$shinyProcess
p$interrupt()
p$wait()
