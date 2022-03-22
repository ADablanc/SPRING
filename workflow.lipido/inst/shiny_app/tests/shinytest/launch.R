app <- ShinyDriver$new("../../")
app$snapshotInit("launch")

app$waitForValue("project_create", ignore = list(NULL))

app$snapshot()
