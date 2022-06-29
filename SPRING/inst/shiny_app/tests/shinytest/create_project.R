app <- shinytest::ShinyDriver$new("../../", loadTimeout = 10000)
# app <- shinytest::ShinyDriver$new(
#     "SPRING/inst/shiny_app",
#     loadTimeout = 10000
# )
app$snapshotInit("create_project")

# 1st test : access to the create project modal
app$waitForValue("project_create", ignore = list(NULL))
app$setInputs(project_create = "click")
app$executeScript(paste0("Shiny.setInputValue(\"project_modal_visible\", $(\"#",
                         "project_modal\").length !=  0)"))
app$executeScript(paste0("Shiny.setInputValue(\"project_create_modal_visible\"",
                         ", $(\"#project_create_modal\").length !=  0)"))
app$snapshot(
    items = list(
        input = c(
            "project_create_modal_visible", # TRUE
            "project_modal_visible" # FALSE
        ),
        output = "project_name" # ""
    ),
    screenshot = TRUE
)

# 2nd test : test cancel button
app$setInputs(project_create_cancel = "click")
app$executeScript(paste0("Shiny.setInputValue(\"project_modal_visible\", $(\"#",
                         "project_modal\").length !=  0)"))
app$executeScript(paste0("Shiny.setInputValue(\"project_create_modal_visible\"",
                         ", $(\"#project_create_modal\").length !=  0)"))
app$snapshot(
    items = list(
        input = c(
            "project_create_modal_visible", # FALSE
            "project_modal_visible" # TRUE
        ),
        output = "project_name" # ""
    ),
    screenshot = TRUE
)

# 3rd test : test if we don't give a name and a directory path
app$setInputs(project_create = "click")
app$setInputs(project_create_valid = "click")
app$executeScript(paste0("Shiny.setInputValue(\"project_modal_visible\", $(\"#",
                         "project_modal\").length !=  0)"))
app$executeScript(paste0("Shiny.setInputValue(\"project_create_modal_visible\"",
                         ", $(\"#project_create_modal\").length !=  0)"))
app$snapshot(
    items = list(
        input = c(
            "project_create_modal_visible", # TRUE
            "project_modal_visible" # FALSE
        ),
        output = "project_name" # ""
    ),
    screenshot = TRUE
)

# 4th test : test if we don't give a directory path
app$setInputs(project_create_name = "test")
app$setInputs(project_create_valid = "click")
app$executeScript(paste0("Shiny.setInputValue(\"project_modal_visible\", $(\"#",
                         "project_modal\").length !=  0)"))
app$executeScript(paste0("Shiny.setInputValue(\"project_create_modal_visible\"",
                         ", $(\"#project_create_modal\").length !=  0)"))
app$snapshot(
    items = list(
        input = c(
            "project_create_modal_visible", # TRUE
            "project_modal_visible" # FALSE
        ),
        output = "project_name" # ""
    ),
    screenshot = TRUE
)

# 5th test : test if we give a directory path
app$executeScript(sprintf("
    Shiny.setInputValue(
        \"project_create_path\",
        {
            path: [\"%s\"],
            roots: \"Windows (C:)\"
        }
    )",
    # create the project in a temp dir
    gsub("C:/", "", gsub("\\\\", "/", dirname(tempdir())))
))
app$snapshot(
    items = list(
        input = "project_create_path" # "Users/SEBAST~1.HUT/AppData/Local/Temp"
    ),
    screenshot = TRUE
)

# 6th test : test if we forgot to give a name
app$setInputs(project_create_name = "")
app$setInputs(project_create_valid = "click")
app$executeScript(paste0("Shiny.setInputValue(\"project_modal_visible\", $(\"#",
                         "project_modal\").length !=  0)"))
app$executeScript(paste0("Shiny.setInputValue(\"project_create_modal_visible\"",
                         ", $(\"#project_create_modal\").length !=  0)"))
app$snapshot(
    items = list(
        input = c(
            "project_create_modal_visible", # TRUE
            "project_modal_visible" # FALSE
        ),
        output = "project_name" # ""
    ),
    screenshot = TRUE
)

# 7th test : normal
app$setInputs(project_create_name = "test")
app$setInputs(project_create_valid = "click")
app$executeScript(paste0("Shiny.setInputValue(\"project_modal_visible\", $(\"#",
                         "project_modal\").length !=  0)"))
app$executeScript(paste0("Shiny.setInputValue(\"project_create_modal_visible\"",
                        ", $(\"#project_create_modal\").length !=  0)"))
app$waitForValue("project_name", iotype = "output", ignore = list(""))
app$snapshot(
    items = list(
        input = c(
            "project_create_modal_visible", # FALSE
            "project_modal_visible" # FALSE
        ),
        output = "project_name" # "test"
    ),
    screenshot = TRUE
)

## Interrupt shinyProcess so covr::save_trace can execute onExit
p <- app$.__enclos_env__$private$shinyProcess
p$interrupt()
p$wait()
