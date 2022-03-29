app <- shinytest::ShinyDriver$new("../../")
# app <- shinytest::ShinyDriver$new("workflow.lipido/inst/shiny_app")
app$snapshotInit("database")

app$executeScript("$(\"a[href=\\\"#shiny-tab-database\\\"]\").click()")
app$waitForValue(
    "database_table",
    iotype = "output",
    ignore = list(NULL)
)
app$snapshot(items = list(output = c("database_table")), screenshot = TRUE)
