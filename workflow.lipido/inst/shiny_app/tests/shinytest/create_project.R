app <- ShinyDriver$new("../../")
app$snapshotInit("create_project")
app$waitForValue("project_create", ignore = list(NULL))

# access to the create project modal
app$setInputs(project_create = "click")
app$waitForValue(
    "project_create_path_display",
    iotype = "output",
    ignore = list(NULL)
)
app$snapshot()

# test cancel button
app$setInputs(project_create_cancel = "click")
app$waitForValue("project_create", ignore = list(NULL))
app$snapshot()

# test if we don't give a name and a directory path
app$setInputs(project_create = "click")
app$waitForValue(
    "project_create_path_display",
    iotype = "output",
    ignore = list(NULL)
)
app$setInputs(project_create_valid = "click")
app$snapshot()

# test if we don't give a directory path
app$setInputs(project_create_name = "test")
app$setInputs(project_create_valid = "click")
app$snapshot()

# test if we give a directory path
app$setInputs(project_create_path = "click")
app$executeScript("
    Shiny.setInputValue(
        \"project_create_path\",
        {
            path: [\"\"],
            roots: \"home\"
        }
    )
")
app$waitForValue(
    "project_create_path_display",
    iotype = "output",
    ignore = list(NULL)
)
app$snapshot()

# test if we forgot to give a name
app$setInputs(project_create_name = "")
app$setInputs(project_create_valid = "click")
app$snapshot()

# normal
app$setInputs(project_create_name = "test")
app$setInputs(project_create_valid = "click")
app$waitForValue("project_name", iotype = "output", ignore = list(NULL))
app$snapshot()
