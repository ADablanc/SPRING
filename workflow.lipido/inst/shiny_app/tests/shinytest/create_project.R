app <- ShinyDriver$new("../../")
app$snapshotInit("create_project")
app$waitForValue("project_create", ignore = list(NULL))
empty_project_name <- app$waitForValue(
    "project_name",
    iotype = "output",
    ignore = list(NULL)
)

# access to the create project modal
app$setInputs(project_create = "click")
empty_path <- app$waitForValue(
    "project_create_path_display",
    iotype = "output",
    ignore = list(NULL)
)
app$snapshot(
    items = list(output = c("project_create_path_display")),
    screenshot = TRUE
)

# test cancel button
app$setInputs(project_create_cancel = "click")
app$waitForValue("project_create", ignore = list(NULL))
app$snapshot(items = list(input = c("project_create")), screenshot = TRUE)

# test if we don't give a name and a directory path
app$setInputs(project_create = "click")
app$waitForValue(
    "project_create_path_display",
    iotype = "output",
    ignore = list(NULL)
)
app$setInputs(project_create_valid = "click")
app$snapshot(items = list(output = c("project_name")), screenshot = TRUE)

# test if we don't give a directory path
app$setInputs(project_create_name = "test")
app$setInputs(project_create_valid = "click")
app$snapshot(items = list(output = c("project_name")), screenshot = TRUE)

# test if we give a directory path
app$executeScript(sprintf("
    Shiny.setInputValue(
        \"project_create_path\",
        {
            path: [\"%s\"],
            roots: \"Windows (C:)\"
        }
    )",
    # create the project in a temp dir
    gsub("C:/", "", gsub("\\\\", "/", tempdir()))
))
app$waitForValue(
    "project_create_path_display",
    iotype = "output",
    ignore = list(empty_path)
)

# test if we forgot to give a name
app$setInputs(project_create_name = "")
app$setInputs(project_create_valid = "click")
app$snapshot(items = list(output = c("project_name")), screenshot = TRUE)

# normal
app$setInputs(project_create_name = "test")
app$setInputs(project_create_valid = "click")
app$waitForValue(
    "project_name",
    iotype = "output",
    ignore = list(empty_project_name)
)
app$snapshot(items = list(output = c("project_name")), screenshot = TRUE)
