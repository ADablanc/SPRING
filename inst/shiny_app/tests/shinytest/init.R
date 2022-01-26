app <- ShinyDriver$new("../../")
app$snapshotInit("init")

app$setInputs(sidebarCollapsed = FALSE)
Sys.sleep(1)
app$snapshot()
