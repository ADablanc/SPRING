app <- ShinyDriver$new("../../")
# app <- ShinyDriver$new("workflow.lipido/inst/shiny_app")
app$snapshotInit("process")
app$waitForValue("project_create", ignore = list(NULL))
empty_project_name <- app$waitForValue(
    "project_name",
    iotype = "output",
    ignore = list(NULL)
)
empty_ann <- app$waitForValue(
    "ann",
    iotype = "export",
    ignore = list(NULL)
)

# access to the process tab by creation of a project
app$setInputs(project_create = "click")
app$setInputs(project_create_name = "test2")
app$executeScript("
    Shiny.setInputValue(
        \"project_create_path\",
        {
            path: [\"\"],
            roots: \"home\"
        }
    )
")
app$setInputs(project_create_valid = "click")
app$waitForValue(
    "project_name",
    iotype = "output",
    ignore = list(empty_project_name)
)

# test without files
app$setInputs(process_launch = "click")
app$snapshot(items = list(export = c("ann")), screenshot = TRUE)

# give the files
process_dt_files_imported <- app$waitForValue(
    "process_dt_files_imported",
    iotype = "output",
    ignore = list(NULL)
)
raw_files <- c(
    system.file(
        "testdata",
        "220221CCM_global_POS_01_ssleu_filtered.mzML",
        package = "workflow.lipido"
    ),
    system.file(
        "testdata",
        "220221CCM_global_POS_02_ssleu_filtered.mzML",
        package = "workflow.lipido"
    ),
    system.file(
        "testdata",
        "220221CCM_global_NEG_01_ssleu_filtered.mzML",
        package = "workflow.lipido"
    ),
    system.file(
        "testdata",
        "220221CCM_global_NEG_02_ssleu_filtered.mzML",
        package = "workflow.lipido"
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
        lapply(strsplit(raw_files, "/"), function(x)
            paste("\"", x[-1], "\"", sep = "", collapse = ", ")
        ),
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
    items = list(output = c("process_dt_files_imported")),
    screenshot = TRUE
)

# test without m/z min
app$setInputs(process_filter_mz_min = "")
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(export = c("ann", "spectra_infos")),
    screenshot = TRUE
)

# test with m/z min < 0
app$setInputs(process_filter_mz_min = -2)
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(export = c("ann", "spectra_infos")),
    screenshot = TRUE
)

# test with missing m/z max
app$setInputs(process_filter_mz_min = 200)
app$setInputs(process_filter_mz_max = "")
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(export = c("ann", "spectra_infos")),
    screenshot = TRUE
)

# test with m/z max < 0
app$setInputs(process_filter_mz_max = -2)
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(export = c("ann", "spectra_infos")),
    screenshot = TRUE
)

# test with m/z max < m/z min
app$setInputs(process_filter_mz_max = 100)
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(export = c("ann", "spectra_infos")),
    screenshot = TRUE
)

# test with missing rt min
app$setInputs(process_filter_mz_max = 1000)
app$setInputs(process_filter_rt_min = "")
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(export = c("ann", "spectra_infos")),
    screenshot = TRUE
)

# test with rt min < 0
app$setInputs(process_filter_rt_min = -2)
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(export = c("ann", "spectra_infos")),
    screenshot = TRUE
)

# test with missing rt max
app$setInputs(process_filter_rt_min = .7)
app$setInputs(process_filter_rt_max = "")
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(export = c("ann", "spectra_infos")),
    screenshot = TRUE
)

# test with rt max < 0
app$setInputs(process_filter_rt_max = -2)
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(export = c("ann", "spectra_infos")),
    screenshot = TRUE
)

# test with rt max < rt min
app$setInputs(process_filter_rt_max = .5)
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(export = c("ann", "spectra_infos")),
    screenshot = TRUE
)

# test with missing ppm
app$setInputs(process_filter_rt_max = 6.3)
app$setInputs(process_ppm = "")
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(export = c("ann", "spectra_infos")),
    screenshot = TRUE
)

# test with ppm < 0
app$setInputs(process_ppm = -2)
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(export = c("ann", "spectra_infos")),
    screenshot = TRUE
)

# test with missing peakwidth min
app$setInputs(process_ppm = 30)
app$setInputs(process_peakwidth_min = "")
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(export = c("ann", "spectra_infos")),
    screenshot = TRUE
)

# test with peakwidth min < 0
app$setInputs(process_peakwidth_min = -2)
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(export = c("ann", "spectra_infos")),
    screenshot = TRUE
)

# test with missing peakwidth max
app$setInputs(process_peakwidth_min = 4)
app$setInputs(process_peakwidth_max = "")
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(export = c("ann", "spectra_infos")),
    screenshot = TRUE
)

# test with peakwidth max < 0
app$setInputs(process_peakwidth_max = -2)
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(export = c("ann", "spectra_infos")),
    screenshot = TRUE
)

# test with peakwidth max < rt min
app$setInputs(process_peakwidth_max = 2)
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(export = c("ann", "spectra_infos")),
    screenshot = TRUE
)

# test with missing snthresh
app$setInputs(process_peakwidth_max = 39)
app$setInputs(process_snthresh = "")
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(export = c("ann", "spectra_infos")),
    screenshot = TRUE
)

# test with snthresh < 0
app$setInputs(process_snthresh = -2)
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(export = c("ann", "spectra_infos")),
    screenshot = TRUE
)

# test with missing prefilter step
app$setInputs(process_snthresh = 1)
app$setInputs(process_prefilter_step = "")
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(export = c("ann", "spectra_infos")),
    screenshot = TRUE
)

# test with prefilter step < 0
app$setInputs(process_prefilter_step = -2)
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(export = c("ann", "spectra_infos")),
    screenshot = TRUE
)

# test with missing prefilter level
app$setInputs(process_prefilter_step = 2)
app$setInputs(process_prefilter_level = "")
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(export = c("ann", "spectra_infos")),
    screenshot = TRUE
)

# test with prefilter level < 0
app$setInputs(process_prefilter_level = -2)
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(export = c("ann", "spectra_infos")),
    screenshot = TRUE
)

# test with missing m/z diff
app$setInputs(process_prefilter_level = 815)
app$setInputs(process_mzdiff = "")
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(export = c("ann", "spectra_infos")),
    screenshot = TRUE
)

# test with missing noise
app$setInputs(process_mzdiff = .041)
app$setInputs(process_noise = "")
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(export = c("ann", "spectra_infos")),
    screenshot = TRUE
)

# test with missing gap init
app$setInputs(process_noise = 0)
app$setInputs(process_gap_init = "")
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(export = c("ann", "spectra_infos")),
    screenshot = TRUE
)

# test with gap init < 0
app$setInputs(process_gap_init = -2)
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(export = c("ann", "spectra_infos")),
    screenshot = TRUE
)

# test with missing gap extend
app$setInputs(process_gap_init = .3)
app$setInputs(process_gap_extend = "")
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(export = c("ann", "spectra_infos")),
    screenshot = TRUE
)

# test with gap extend < 0
app$setInputs(process_gap_extend = -2)
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(export = c("ann", "spectra_infos")),
    screenshot = TRUE
)

# test with missing factor diag
app$setInputs(process_gap_extend = 2.4)
app$setInputs(process_factor_diag = "")
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(export = c("ann", "spectra_infos")),
    screenshot = TRUE
)

# test with factor diag < 0
app$setInputs(process_factor_diag = -2)
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(export = c("ann", "spectra_infos")),
    screenshot = TRUE
)

# test with missing factor gap
app$setInputs(process_factor_diag = 2)
app$setInputs(process_factor_gap = "")
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(export = c("ann", "spectra_infos")),
    screenshot = TRUE
)

# test with factor gap < 0
app$setInputs(process_factor_gap = -2)
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(export = c("ann", "spectra_infos")),
    screenshot = TRUE
)

# test with missing init penalty
app$setInputs(process_factor_gap = 1)
app$setInputs(process_init_penalty = "")
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(export = c("ann", "spectra_infos")),
    screenshot = TRUE
)

# test with init penalty < 0
app$setInputs(process_init_penalty = -2)
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(export = c("ann", "spectra_infos")),
    screenshot = TRUE
)

# test with missing bw
app$setInputs(process_init_penalty = 0)
app$setInputs(process_bw = "")
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(export = c("ann", "spectra_infos")),
    screenshot = TRUE
)

# test with bw < 0
app$setInputs(process_bw = -2)
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(export = c("ann", "spectra_infos")),
    screenshot = TRUE
)

# test with missing mzwid
app$setInputs(process_bw = 5)
app$setInputs(process_mzwid = "")
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(export = c("ann", "spectra_infos")),
    screenshot = TRUE
)

# test with mzwid < 0
app$setInputs(process_mzwid = -2)
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(export = c("ann", "spectra_infos")),
    screenshot = TRUE
)

# test with missing mda tol
app$setInputs(process_mzwid = 10)
app$setInputs(process_mda_tol = "")
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(export = c("ann", "spectra_infos")),
    screenshot = TRUE
)

# test with mda tol < 0
app$setInputs(process_mda_tol = -2)
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(export = c("ann", "spectra_infos")),
    screenshot = TRUE
)

# test with missing rt tol
app$setInputs(process_mda_tol = 15)
app$setInputs(process_rt_tol = "")
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(export = c("ann", "spectra_infos")),
    screenshot = TRUE
)

# test with rt tol < 0
app$setInputs(process_rt_tol = -2)
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(export = c("ann", "spectra_infos")),
    screenshot = TRUE
)

# test with missing adducts
app$setInputs(process_rt_tol = 10)
app$setInputs(process_adducts = NULL)
app$setInputs(process_launch = "click")
app$snapshot(
    items = list(export = c("ann", "spectra_infos")),
    screenshot = TRUE
)

# normal test
app$setInputs(process_adducts = c("[M+H]+", "[M+NH4]+", "[M+Na]+", "[M-H]-",
                                  "[M+H-H2O]+"))
app$setInputs(process_launch = "click", wait_ = FALSE, values_ = FALSE)
app$snapshot(
    items = list(export = c("ann", "spectra_infos")),
    screenshot = TRUE
)
