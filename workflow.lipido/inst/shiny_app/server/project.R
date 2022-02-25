shinyFiles::shinyFileChoose(input, 'project_load2', roots = volumes,
                            filetypes = "sqlite")

observeEvent(input$project_load2, {
    if (is.integer(input$project_load2)) return(NULL)
    sqlite_path <- tools::file_path_as_absolute(
        shinyFiles::parseFilePaths(volumes,input$project_load2)$datapath)
    sqlite_path(sqlite_path)
    db(db_connect(sqlite_path))
    ann <- db_get_ann(db())
    if (nrow(ann) > 0) {
        ann <- split_conflicts(ann)
        ann(ann)
        if (length(ann$conflicts) > 0) conflict_id(1)
        else conflict_id(0)
        spectra_infos(db_get_spectra_infos(db()))
    }
    shinyjs::runjs(sprintf('load_db("%s")', tools::file_path_sans_ext(
        basename(shinyFiles::parseFilePaths(volumes,
                                            input$project_load2)$datapath))))
})

shinyFiles::shinyDirChoose(input, "project_create_path2", roots = volumes)

observeEvent(input$project_create_path2, {
    if (is.integer(input$project_create_path2)) return(NULL)
    shinyjs::runjs(
        sprintf(
            '$("#project_create_path_display").text("%s")',
            basename(shinyFiles::parseDirPath(
                volumes, input$project_create_path2))))
})

observeEvent(input$project_create, {
    params <- list(
        name = input$project_create$project_name,
        path = shinyFiles::parseDirPath(volumes, input$project_create_path2)
    )
    sqlite_path <- file.path(tools::file_path_as_absolute(params$path),
                             paste0(params$name, ".sqlite"))
    sqlite_path(sqlite_path)
    shinyjs::runjs(sprintf('load_db("%s")', tools::file_path_sans_ext(
        basename(sqlite_path))))
})
