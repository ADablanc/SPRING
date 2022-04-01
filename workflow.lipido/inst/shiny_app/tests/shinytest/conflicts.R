app <- shinytest::ShinyDriver$new("../../", loadTimeout = 10000)
# app <- shinytest::ShinyDriver$new(
#     "workflow.lipido/inst/shiny_app",
#     loadTimeout = 10000
# )
app$snapshotInit("conflicts")

# init
app$executeScript("$(\"a[href=\\\"#shiny-tab-conflicts\\\"]\").click()")
conflicts_table <- app$waitForValue(
    "conflicts_table",
    iotype = "output",
    ignore = list(NULL)
)
conflicts_ms <- app$waitForValue(
    "conflicts_ms",
    iotype = "output",
    ignore = list(NULL)
)

# 1st test : check that we cant update conflict_id by clicking on the left arrow
app$setInputs(
    conflicts_left_disabled = grepl(
        "disabled",
        app$findWidget("conflicts_left")$getHtml()
    ),
    allowInputNoBinding_ = TRUE,
    wait_ = FALSE,
    values_ = FALSE
)
app$setInputs(
    conflicts_right_disabled = grepl(
        "disabled",
        app$findWidget("conflicts_right")$getHtml()
    ),
    allowInputNoBinding_ = TRUE,
    wait_ = FALSE,
    values_ = FALSE
)
app$snapshot(
    items = list(
        input = c("conflicts_left_disabled", "conflicts_right_disabled"),
        output = c("conflicts_info", "conflicts_table", "conflicts_ms"),
        export = "conflict_id"
    ),
    screenshot = TRUE
)

# 2nd test : give a project file
sqlite_file <- system.file(
    "testdata",
    "220221CCM_global.sqlite",
    package = "workflow.lipido"
)
sqlite_file2 <- gsub("\\\\", "/", tempfile(fileext = ".sqlite"))
invisible(file.copy(sqlite_file, sqlite_file2))
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
        strsplit(sqlite_file2, "/")[[1]][-1],
        "\"",
        sep = "",
        collapse = ", "
    )
))
app$waitForValue(
    "conflict_id",
    iotype = "export",
    ignore = list(0)
)
app$setInputs(
    conflicts_left_disabled = grepl(
        "disabled",
        app$findWidget("conflicts_left")$getHtml()
    ),
    allowInputNoBinding_ = TRUE,
    wait_ = FALSE,
    values_ = FALSE
)
app$setInputs(
    conflicts_right_disabled = grepl(
        "disabled",
        app$findWidget("conflicts_right")$getHtml()
    ),
    allowInputNoBinding_ = TRUE,
    wait_ = FALSE,
    values_ = FALSE
)
app$snapshot(
    items = list(
        input = c("conflicts_left_disabled", "conflicts_right_disabled"),
        output = c("conflicts_info", "conflicts_table", "conflicts_ms"),
        export = "confict_id"
    ),
    screenshot = TRUE
)

# 3rd test : click two times on the right arrow
app$setInputs(conflicts_right = "click")
app$setInputs(conflicts_right = "click")
app$setInputs(
    conflicts_left_disabled = grepl(
        "disabled",
        app$findWidget("conflicts_left")$getHtml()
    ),
    allowInputNoBinding_ = TRUE,
    wait_ = FALSE,
    values_ = FALSE
)
app$setInputs(
    conflicts_right_disabled = grepl(
        "disabled",
        app$findWidget("conflicts_right")$getHtml()
    ),
    allowInputNoBinding_ = TRUE,
    wait_ = FALSE,
    values_ = FALSE
)
app$snapshot(
    items = list(
        input = c("conflicts_left_disabled", "conflicts_right_disabled"),
        output = c("conflicts_info", "conflicts_table", "conflicts_ms"),
        export = c("confict_id")
    ),
    screenshot = TRUE
)

# 4th test : click on the first line
app$executeScript("$(\"#conflicts_table button\").get(0).click()")
app$setInputs(
    conflicts_left_disabled = grepl(
        "disabled",
        app$findWidget("conflicts_left")$getHtml()
    ),
    allowInputNoBinding_ = TRUE,
    wait_ = FALSE,
    values_ = FALSE
)
app$setInputs(
    conflicts_right_disabled = grepl(
        "disabled",
        app$findWidget("conflicts_right")$getHtml()
    ),
    allowInputNoBinding_ = TRUE,
    wait_ = FALSE,
    values_ = FALSE
)
app$snapshot(
    items = list(
        input = c("conflicts_left_disabled", "conflicts_right_disabled"),
        output = c("conflicts_info", "conflicts_table", "conflicts_ms"),
        export = c("confict_id")
    ),
    screenshot = TRUE
)

# 5th test : click on the second line (the ms plot should change)
app$executeScript(paste0("$($(\"#conflicts_table\").data(\"datatable\").row(1)",
                         ".node()).click()"))
app$snapshot(
    items = list(
        input = c("conflicts_left_disabled", "conflicts_right_disabled"),
        output = c("conflicts_info", "conflicts_table", "conflicts_ms"),
        export = c("confict_id")
    ),
    screenshot = TRUE
)
