app <- shinytest::ShinyDriver$new("../../", loadTimeout = 10000)
# app <- shinytest::ShinyDriver$new(
#     "SPRING/inst/shiny_app",
#     loadTimeout = 10000
# )
app$snapshotInit("process")

# access to the process tab by creation of a project
app$waitForValue("project_create", ignore = list(NULL))
app$setInputs(project_create = "click")
app$setInputs(project_create_name = "test")
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

# 1st test : test without files
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(
        export = "conflict_id" # 0
    ),
    screenshot = TRUE
)

# 2nd test : give the files
process_dt_files_imported <- app$waitForValue(
    "process_dt_files_imported",
    iotype = "output",
    ignore = list(NULL)
)
raw_files <- c(
    system.file(
        "testdata",
        "220221CCM_global_POS_01_ssleu_filtered.mzML",
        package = "SPRING"
    ),
    system.file(
        "testdata",
        "220221CCM_global_POS_02_ssleu_filtered.mzML",
        package = "SPRING"
    )
)
app$executeScript(sprintf(
    "Shiny.setInputValue(
        \"process_files\",
        {
            files: {
                %s
            },
            roots: \"Windows (C:)\"
        }
    )",
    paste(
        seq(raw_files),
        ": [",
        lapply(strsplit(raw_files, "/"), function(x) {
            paste("\"", x[-1], "\"", sep = "", collapse = ", ")
        }),
        "]",
        sep = "",
        collapse = ", "
    )
))
app$waitForValue(
    "process_dt_files_imported",
    iotype = "output",
    ignore = list(process_dt_files_imported)
)
app$snapshot(
    items = list(
        output = "process_dt_files_imported" # should contain all the files
    ),
    screenshot = TRUE
)

# 3rd test : test with missing ppm
app$setInputs(process_ppm = "")
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(
        export = "conflict_id" # 0
    ),
    screenshot = TRUE
)

# 4th test : test with ppm < 0
app$setInputs(process_ppm = -2)
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(
        export = "conflict_id" # 0
    ),
    screenshot = TRUE
)

# 5th test : test with missing peakwidth min
app$setInputs(process_ppm = 30)
app$setInputs(process_peakwidth_min = "")
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(
        export = "conflict_id" # 0
    ),
    screenshot = TRUE
)

# 6th test : test with peakwidth min < 0
app$setInputs(process_peakwidth_min = -2)
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(
        export = "conflict_id" # 0
    ),
    screenshot = TRUE
)

# 7th test : test with missing peakwidth max
app$setInputs(process_peakwidth_min = 4)
app$setInputs(process_peakwidth_max = "")
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(
        export = "conflict_id" # 0
    ),
    screenshot = TRUE
)

# 8th test : test with peakwidth max < 0
app$setInputs(process_peakwidth_max = -2)
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(
        export = "conflict_id" # 0
    ),
    screenshot = TRUE
)

# 9th test : test with peakwidth max < rt min
app$setInputs(process_peakwidth_max = 2)
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(
        export = "conflict_id" # 0
    ),
    screenshot = TRUE
)

# 10th test : test with missing snthresh
app$setInputs(process_peakwidth_max = 39)
app$setInputs(process_snthresh = "")
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(
        export = "conflict_id" # 0
    ),
    screenshot = TRUE
)

# 11th test : test with snthresh < 0
app$setInputs(process_snthresh = -2)
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(
        export = "conflict_id" # 0
    ),
    screenshot = TRUE
)

# 12th test : test with missing prefilter step
app$setInputs(process_snthresh = 1)
app$setInputs(process_prefilter_step = "")
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(
        export = "conflict_id" # 0
    ),
    screenshot = TRUE
)

# 13th test : test with prefilter step < 0
app$setInputs(process_prefilter_step = -2)
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(
        export = "conflict_id" # 0
    ),
    screenshot = TRUE
)

# 14th test : test with missing prefilter level
app$setInputs(process_prefilter_step = 2)
app$setInputs(process_prefilter_level = "")
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(
        export = "conflict_id" # 0
    ),
    screenshot = TRUE
)

# 15th test : test with prefilter level < 0
app$setInputs(process_prefilter_level = -2)
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(
        export = "conflict_id" # 0
    ),
    screenshot = TRUE
)

# 16th test : test with missing m/z diff
app$setInputs(process_prefilter_level = 815)
app$setInputs(process_mzdiff = "")
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(
        export = "conflict_id" # 0
    ),
    screenshot = TRUE
)

# 17th test : test with missing noise
app$setInputs(process_mzdiff = .041)
app$setInputs(process_noise = "")
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(
        export = "conflict_id" # 0
    ),
    screenshot = TRUE
)

# 18th test : test with missing gap init
app$setInputs(process_noise = 0)
app$setInputs(process_gap_init = "")
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(
        export = "conflict_id" # 0
    ),
    screenshot = TRUE
)

# 19th test : test with gap init < 0
app$setInputs(process_gap_init = -2)
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(
        export = "conflict_id" # 0
    ),
    screenshot = TRUE
)

# 20th test : test with missing gap extend
app$setInputs(process_gap_init = .3)
app$setInputs(process_gap_extend = "")
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(
        export = "conflict_id" # 0
    ),
    screenshot = TRUE
)

# 21th test : test with gap extend < 0
app$setInputs(process_gap_extend = -2)
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(
        export = "conflict_id" # 0
    ),
    screenshot = TRUE
)

# 22th test : test with missing factor diag
app$setInputs(process_gap_extend = 2.4)
app$setInputs(process_factor_diag = "")
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(
        export = "conflict_id" # 0
    ),
    screenshot = TRUE
)

# 23th test : test with factor diag < 0
app$setInputs(process_factor_diag = -2)
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(
        export = "conflict_id" # 0
    ),
    screenshot = TRUE
)

# 24th test : test with missing factor gap
app$setInputs(process_factor_diag = 2)
app$setInputs(process_factor_gap = "")
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(
        export = "conflict_id" # 0
    ),
    screenshot = TRUE
)

# 25th test : test with factor gap < 0
app$setInputs(process_factor_gap = -2)
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(
        export = "conflict_id" # 0
    ),
    screenshot = TRUE
)

# 26th test : test with missing init penalty
app$setInputs(process_factor_gap = 1)
app$setInputs(process_init_penalty = "")
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(
        export = "conflict_id" # 0
    ),
    screenshot = TRUE
)

# 27th test : test with init penalty < 0
app$setInputs(process_init_penalty = -2)
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(
        export = "conflict_id" # 0
    ),
    screenshot = TRUE
)

# 28th test : test with missing bw
app$setInputs(process_init_penalty = 0)
app$setInputs(process_bw = "")
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(
        export = "conflict_id" # 0
    ),
    screenshot = TRUE
)

# 29th test : test with bw < 0
app$setInputs(process_bw = -2)
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(
        export = "conflict_id" # 0
    ),
    screenshot = TRUE
)

# 30th test : test with missing mzwid
app$setInputs(process_bw = 5)
app$setInputs(process_mzwid = "")
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(
        export = "conflict_id" # 0
    ),
    screenshot = TRUE
)

# 31th test : test with mzwid < 0
app$setInputs(process_mzwid = -2)
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(
        export = "conflict_id" # 0
    ),
    screenshot = TRUE
)

# 32th test : test with missing mda tol
app$setInputs(process_mzwid = 10)
app$setInputs(process_mda_tol = "")
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(
        export = "conflict_id" # 0
    ),
    screenshot = TRUE
)

# 33th test : test with mda tol < 0
app$setInputs(process_mda_tol = -2)
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(
        export = "conflict_id" # 0
    ),
    screenshot = TRUE
)

# 34th test : test with missing rt tol
app$setInputs(process_mda_tol = 15)
app$setInputs(process_rt_tol = "")
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(
        export = "conflict_id" # 0
    ),
    screenshot = TRUE
)

# 35th test : test with rt tol < 0
app$setInputs(process_rt_tol = -2)
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(
        export = "conflict_id" # 0
    ),
    screenshot = TRUE
)

# 36th test : check that the compound classes is updated
app$setInputs(process_rt_tol = 10)
app$setInputs(process_database = "test")
app$snapshot(
    items = list(
        input = "process_cpd_classes" # must be "Cer", "LPC"
    )
)

# 37th test : test with missing compound classes
app$setInputs(process_cpd_classes = NULL)
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(
        export = "conflict_id" # 0
    ),
    screenshot = TRUE
)

# 38th test : test with missing sigma
app$setInputs(process_cpd_classes = c("Cer", "LPC"))
app$setInputs(process_sigma = "")
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(
        export = "conflict_id" # 0
    ),
    screenshot = TRUE
)

# 39th test : test with sigma <= 0
app$setInputs(process_sigma = -2)
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(
        export = "conflict_id" # 0
    ),
    screenshot = TRUE
)

# 40th test : test with missing p-value
app$setInputs(process_sigma = 6)
app$setInputs(process_pval = "")
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(
        export = "conflict_id" # 0
    ),
    screenshot = TRUE
)

# 41th test : test with p-value <= 0
app$setInputs(process_pval = -2)
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(
        export = "conflict_id" # 0
    ),
    screenshot = TRUE
)

# 42th test : normal test
app$setInputs(process_pval = .05)
app$setInputs(process_launch = "click", wait_ = FALSE, values_ = FALSE)
app$snapshot(
    items = list(
        export = c(
            "conflicts", # 3
            "conflict_id" # 1
        )
    ),
    screenshot = TRUE
)

## Interrupt shinyProcess so covr::save_trace can execute onExit
p <- app$.__enclos_env__$private$shinyProcess
p$interrupt()
p$wait()
