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
        input = c(
            "conflicts_left_disabled", # TRUE
            "conflicts_right_disabled" # TRUE
        ),
        output = c(
            "conflicts_info", # ""
            "conflicts_ms", # empty
            "conflicts_table" # empty
        ),
        export = "conflict_id" # 0
    ),
    screenshot = TRUE
)

# 2nd test : give a project file
sqlite_file <- system.file(
    "testdata",
    "220221CCM_global-conflicts.sqlite",
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
Sys.sleep(1)
app$snapshot(
    items = list(
        input = c(
            "conflicts_left_disabled", # TRUE
            "conflicts_right_disabled" # FALSE
        ),
        output = c(
            "conflicts_info", # "1/2"
            # "[M+H-H2O]+" "[M+H]+" "[M+Na]+"
            "conflicts_ms",
            # ["LPC 11:0","LPC 11a:0"],
            "conflicts_table"
        ),
        export = "conflict_id" # 1
    ),
    screenshot = TRUE
)

# 3rd test : click one time on the right arrow
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
        input = c(
            "conflicts_left_disabled", # FALSE
            "conflicts_right_disabled" # TRUE
        ),
        output = c(
            "conflicts_info", # "2/2"
            # "[M+H-H2O]+" "[M+Na]+"
            "conflicts_ms",
            # "Cer (d18:1/C12:0)" "Cer (d18:1/C12:0)-B"
            "conflicts_table"
        ),
        export = "conflict_id" # 2
    ),
    screenshot = TRUE
)

# 4th test : click on the second line (the ms plot should change)
app$executeScript(paste0("$($(\"#conflicts_table\").data(\"datatable\").row(1)",
                         ".node()).click()"))
Sys.sleep(1)
app$snapshot(
    items = list(
        input = c(
            "conflicts_left_disabled", # FALSE
            "conflicts_right_disabled" # TRUE
        ),
        output = c(
            "conflicts_info", # "2/2"
            # "[M+H-H2O]+"
            "conflicts_ms",
            # "Cer (d18:1/C12:0)" "Cer (d18:1/C12:0)-B"
            "conflicts_table"
        ),
        export = "conflict_id" # 2
    ),
    screenshot = TRUE
)

# 5th test : click on the first line
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
        input = c(
            "conflicts_left_disabled", # TRUE # dont know why but it doesnt
                                              # update properly
            "conflicts_right_disabled" # TRUE
        ),
        output = c(
            "conflicts_info", # "1/1"
            # "[M+H-H2O]+" "[M+H]+" "[M+Na]+"
            "conflicts_ms",
            # ["LPC 11:0","LPC 11a:0"] # dont know why but it doesnt update
            "conflicts_table"
        ),
        export = "conflict_id" # 1
    ),
    screenshot = TRUE
)

# 6th test : check that the table & plots are empty cause no conflicts anymore
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
        input = c(
            "conflicts_left_disabled", # TRUE
            "conflicts_right_disabled" # TRUE
        ),
        output = c(
            "conflicts_info", # ""
            # empty
            "conflicts_table",
            # empty
            "conflicts_ms"
        ),
        export = "conflict_id" # 0
    ),
    screenshot = TRUE
)

## Interrupt shinyProcess so covr::save_trace can execute onExit
p <- app$.__enclos_env__$private$shinyProcess
p$interrupt()
p$wait()
