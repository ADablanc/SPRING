app <- shinytest::ShinyDriver$new("../../", loadTimeout = 10000)
# app <- shinytest::ShinyDriver$new(
#     "workflow.lipido/inst/shiny_app",
#     loadTimeout = 10000
# )
app$snapshotInit("database")

app$executeScript("$(\"a[href=\\\"#shiny-tab-database\\\"]\").click()")
app$waitForValue(
    "database_table",
    iotype = "output",
    ignore = list(NULL)
)
app$snapshot(items = list(output = c("database_table")), screenshot = TRUE)
