app <- shinytest::ShinyDriver$new("../../", loadTimeout = 10000)
# app <- shinytest::ShinyDriver$new(
#     "SPRING/inst/shiny_app",
#     loadTimeout = 10000
# )
app$snapshotInit("ms_map")

# 1st test : no output cause empty project
# create empty project
app$waitForValue("project_create", ignore = list(NULL))
app$setInputs(project_create = "click")
app$setInputs(project_create_name = runif(1))
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
app$setInputs(project_create_valid = "click")
app$waitForValue(
    "project_name",
    iotype = "output",
    ignore = list("")
)
app$executeScript("$(\"a[href=\\\"#shiny-tab-ms_map\\\"]\").click()")
app$waitForValue("ms_map_plot", iotype = "output", ignore = list(NULL))
app$setInputs(
    ms_map_max_int_threshold = stringr::str_extract(
        app$findWidget("ms_map_int_threshold")$getHtml(),
        "data-max=\"[[:digit:]]+\""
    ),
    allowInputNoBinding_ = TRUE,
    wait_ = FALSE,
    values_ = FALSE
)
app$snapshot(
    items = list(
        input = "ms_map_max_int_threshold", # should be 0
        output = "ms_map_plot" # empty
    ),
    screenshot = TRUE
)

# 2nd test : load a project
sqlite_file <- system.file(
    "testdata",
    "220221CCM_global.sqlite",
    package = "SPRING"
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
app$snapshot(
    items = list(
        input = "ms_map_max_int_threshold", # should be 0 but seems to not
                                                # update...
        output = "ms_map_plot" # contain all points
    ),
    screenshot = TRUE
)

# 3rd test : test with Kendrick plot
app$setInputs(ms_map_type = "Kendrick plot")
ms_map_eic <- app$waitForValue(
    "ms_map_eic",
    iotype = "output",
    ignore = list(NULL)
)
app$snapshot(
    items = list(
        output = "ms_map_plot" # should contain all points but with Kendrick
                                # scale
    ),
    screenshot = TRUE
)

# 4th test : click on a point to update the graph
app$executeScript("Shiny.setInputValue(\"ms_map_eic_id\", 5)")
ms_map_eic <- app$waitForValue(
    "ms_map_eic",
    iotype = "output",
    ignore = list(ms_map_eic)
)
app$snapshot(
    items = list(
        output = c("ms_map_plot", "ms_map_eic")
    ),
    screenshot = TRUE
)

# 5th test : update the graph by clicking on another point
app$executeScript("Shiny.setInputValue(\"ms_map_eic_id\", 3)")
app$waitForValue(
    "ms_map_eic",
    iotype = "output",
    ignore = list(ms_map_eic)
)
app$snapshot(
    items = list(
        output = c("ms_map_plot", "ms_map_eic")
    ),
    screenshot = TRUE
)

# 6th test : only no annotated on MS map
app$setInputs(ms_map_type = "MS map")
app$setInputs(ms_map_annotation_filter = "no annotated")
app$snapshot(
    items = list(
        output = "ms_map_plot" # should contain only no annotated points
    ),
    screenshot = TRUE
)

# 7th test : only no annotated on Kendrick plot
app$setInputs(ms_map_type = "Kendrick plot")
app$setInputs(ms_map_annotation_filter = "no annotated",
              wait_ = FALSE, values_ = FALSE)
app$snapshot(
    items = list(
        output = "ms_map_plot" # should contain only no annotated points
    ),
    screenshot = TRUE
)

# 8th test : only annotated on MS map
app$setInputs(ms_map_type = "MS map")
app$setInputs(ms_map_annotation_filter = "annotated")
app$snapshot(
    items = list(
        output = "ms_map_plot" # should contain only annotated points
    ),
    screenshot = TRUE
)

# 9th test : only annotated on Kendrick plot
app$setInputs(ms_map_type = "Kendrick plot")
app$setInputs(ms_map_annotation_filter = "annotated",
                  wait_ = FALSE, values_ = FALSE)
app$snapshot(
    items = list(
        output = "ms_map_plot" # should contain only annotated points
    ),
    screenshot = TRUE
)

# 10th test : intensity threshold on MS map
app$setInputs(ms_map_type = "MS map")
app$setInputs(ms_map_annotation_filter = "all")
app$setInputs(ms_map_int_threshold = 6000000,
              wait_ = FALSE, values_ = FALSE)
app$snapshot(
    items = list(
        output = "ms_map_plot" # should contain points above 6000000
                                    # doesn't work cause ms_map_int_threshold
                                    # dont update
    ),
    screenshot = TRUE
)

# 11th test : intensity threshold on Kendrick plot
app$setInputs(ms_map_type = "Kendrick plot")
app$setInputs(ms_map_annotation_filter = "all",
              wait_ = FALSE, values_ = FALSE)
app$setInputs(ms_map_int_threshold = 6000000)
app$snapshot(
    items = list(
        output = "ms_map_plot" # should contain points above 6000000
        # doesn't work cause ms_map_int_threshold
        # dont update
    ),
    screenshot = TRUE
)

# 12th test : no annotated points + intensity threshold on MS map
app$setInputs(ms_map_type = "MS map")
app$setInputs(ms_map_annotation_filter = "no annotated")
app$setInputs(ms_map_int_threshold = 6000000)
app$snapshot(
    items = list(
        output = "ms_map_plot" # should contain points not annotated and
                                    # above 6000000
                                    # doesn't work cause ms_map_int_threshold
                                    # dont update
    ),
    screenshot = TRUE
)

# 13th test : no annotated points + intensity threshold on Kendrick plot
app$setInputs(ms_map_type = "Kendrick plot")
app$setInputs(ms_map_annotation_filter = "no annotated")
app$setInputs(ms_map_int_threshold = 6000000,
              wait_ = FALSE, values_ = FALSE)
app$snapshot(
    items = list(
        output = "ms_map_plot" # should contain points not annotated and
        # above 6000000
        # doesn't work cause ms_map_int_threshold
        # dont update
    ),
    screenshot = TRUE
)

# 14th test : no annotated points + intensity threshold on MS map
app$setInputs(ms_map_type = "MS map")
app$setInputs(ms_map_annotation_filter = "annotated")
app$setInputs(ms_map_int_threshold = 6000000)
app$snapshot(
    items = list(
        output = "ms_map_plot" # should contain points annotated and
                                    # above 6000000
                                    # doesn't work cause ms_map_int_threshold
                                    # dont update
    ),
    screenshot = TRUE
)

# 15th test : no annotated points + intensity threshold on Kendrick plot
app$setInputs(ms_map_type = "Kendrick plot")
app$setInputs(ms_map_annotation_filter = "annotated")
app$setInputs(ms_map_int_threshold = 6000000,
              wait_ = FALSE, values_ = FALSE)
app$snapshot(
    items = list(
        output = "ms_map_plot" # should contain points annotated and
        # above 6000000
        # doesn't work cause ms_map_int_threshold
        # dont update
    ),
    screenshot = TRUE
)

## Interrupt shinyProcess so covr::save_trace can execute onExit
p <- app$.__enclos_env__$private$shinyProcess
p$interrupt()
p$wait()
