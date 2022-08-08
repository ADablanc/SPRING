app <- shinytest::ShinyDriver$new("../../", loadTimeout = 10000)
# app <- shinytest::ShinyDriver$new(
#     "SPRING/inst/shiny_app",
#     loadTimeout = 10000
# )
app$snapshotInit("conflicts")

# init
app$executeScript("$(\"a[href=\\\"#shiny-tab-conflicts\\\"]\").click()")

# 1st test : check that we cant update conflict_id by clicking on the left arrow
ms <- app$waitForValue("conflicts_ms", iotype = "output", ignore = list(NULL))
eic <- app$waitForValue("conflicts_eic", iotype = "output", ignore = list(NULL))
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
app$waitForValue(
    "conflicts_right_disabled",
    iotype = "input",
    ignore = list(NULL)
)
app$snapshot(
    items = list(
        input = c(
            "conflicts_left_disabled", # TRUE
            "conflicts_right_disabled" # TRUE
        ),
        output = c(
            "conflicts_eic", # empty
            "conflicts_info", # ""
            "conflicts_ms", # empty
            "conflicts_table" # empty
        ),
        export = "conflict_id" # 0
    ),
    screenshot = TRUE
)

# 2nd test : give a project file
# insert a second conflict in database
sqlite_file <- system.file(
    "testdata",
    "220221CCM_global.sqlite",
    package = "SPRING"
)
sqlite_file_conflicts <- gsub("\\\\", "/", tempfile(fileext = ".sqlite"))
invisible(file.copy(sqlite_file, sqlite_file_conflicts, overwrite = TRUE))
db <- db_connect(sqlite_file)
db_conflicts <- db_connect(sqlite_file_conflicts)
ann <- db_get_annotations(db, row_ids = 9)[, -1]
ann$name <- paste0(ann$name, "-B")
db_write_table(db_conflicts, "ann", ann, append = TRUE)
RSQLite::dbDisconnect(db)
RSQLite::dbDisconnect(db_conflicts)
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
        strsplit(sqlite_file_conflicts, "/")[[1]][-1],
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
            "conflicts_eic", # LPC 11:0 [M+H]+
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
ms <- app$waitForValue("conflicts_ms", iotype = "output", ignore = list(ms))
eic <- app$waitForValue("conflicts_eic", iotype = "output", ignore = list(eic))
app$snapshot(
    items = list(
        input = c(
            "conflicts_left_disabled", # FALSE
            "conflicts_right_disabled" # TRUE
        ),
        output = c(
            "conflicts_eic", # Cer (d18:1/C12:0)<br />[M+H-H2O]+
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
app$waitForValue("conflicts_ms", iotype = "output", ignore = list(ms))
app$waitForValue("conflicts_eic", iotype = "output", ignore = list(eic))
app$snapshot(
    items = list(
        input = c(
            "conflicts_left_disabled", # FALSE
            "conflicts_right_disabled" # TRUE
        ),
        output = c(
            "conflicts_eic", # Cer (d18:1/C12:0)-B<br />[M+H-H2O]+
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

# 5th test : click on the valid bttn of the first line
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
            "conflicts_eic", # LPC 11:0 [M+H]+  # dont know why but it doesnt
            # update
            "conflicts_info", # "1/1"
            # "[M+H-H2O]+" "[M+H]+" "[M+Na]+"
            "conflicts_ms", # dont know why but it doesnt update
            # ["LPC 11:0","LPC 11a:0"]
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
            "conflicts_eic", # empty
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
