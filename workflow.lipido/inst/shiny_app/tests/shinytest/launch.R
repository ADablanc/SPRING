app <- ShinyDriver$new("../../")
app$snapshotInit("launch")

app$waitForValue(
    "process_dt_files_imported",
    iotype = "output",
    ignore = list(NULL)
)

app$snapshot()
