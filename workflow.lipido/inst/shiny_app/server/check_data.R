shinyWidgets::updatePickerInput(session, inputId = "check_data_cpd",
                                label = "Choose compound", choices = utils::read.csv(
                                  system.file("extdata", "database.csv",
                                              package = "workflow.lipido"))$name)

output$check_data_heatmap <- plotly::renderPlotly({
    ann <- ann()$no_conflicts
    params <- list(
        cpd_names = input$check_data_cpd,
        nrow_ann = nrow(ann)
    )
    tryCatch({
        if (nrow(ann) == 0) custom_stop("invalid", "no annotation data")
        else if (length(params$cpd_names) == 0) custom_stop(
            "invalid", "no compound selected")
        ann <- summarise_ann(ann, spectra_infos())
        plot_heatmap(ann, params$cpd_names)
    }, invalid = function(i) {
        print("########## check_data_heatmap")
        print(params)
        print(i)
        plot_empty_heatmap()
    }, error = function(e) {
        print("########## check_data_heatmap")
        print(params)
        print(e)
        sweet_alert_error(e$message)
        plot_empty_heatmap()
    })
})

output$check_data_eic <- plotly::renderPlotly({
    params <- list(
        sample = input$check_data_heatmap_click$sample,
        cpd_name = input$check_data_heatmap_click$cpd_name
    )

    tryCatch({
        if (is.null(params$cpd_name)) custom_stop("invalid",
                                                  "no compound selected")
        else if (is.null(params$sample)) custom_stop("invalid",
                                                     "no sample selected")

        # search compound formula in database
        process_params <- db_get_params(db())
        adduct_names <- strsplit(process_params$ann$adduct_names, ";")[[1]]
        instrument <- process_params$ann$instrument
        ppm <- process_params$cwt$ppm
        rt_tol <- process_params$cwt$peakwidth_max + process_params$ann$rt_tol
        ions <- load_db(adduct_names, instrument, params$cpd_name)
        basepeaks <- ions[ions$iso == "M", , drop = FALSE]
        basepeaks$mz_tol <- basepeaks$mz * ppm * 10**-6

        # get eics
        ann <- ann()$no_conflicts
        ann <- ann[ann$name == params$cpd_name, , drop = FALSE]
        #p<- lapply(colnames(ann)[14:ncol(ann)], function(sample_name) {
        sample_name <- params$sample
            ms_file_pos <- db_read_ms_file(db(), sample_name,
                                           polarity ="positive")
            ms_file_neg <- db_read_ms_file(db(), sample_name,
                                           polarity = "negative")
            eics <- lapply(seq(nrow(basepeaks)), function(i)
                get_eic(
                    ms_file = if (basepeaks[i, "charge"] > 0) ms_file_pos
                              else ms_file_neg,
                    mz_range = basepeaks[i, "mz"] + c(-basepeaks[i, "mz_tol"],
                                                      basepeaks[i, "mz_tol"]),
                    rt_range = basepeaks[i, "rt"] + c(-rt_tol, rt_tol)
                )
            )
            names(eics) <- basepeaks$adduct

            # get peaks
            sub_ann <- ann[, c(4, which(colnames(ann) == sample_name))]
            colnames(sub_ann)[2] <- "spectra_id"
            sub_ann <- sub_ann[!is.na(sub_ann$spectra_id), , drop = FALSE]
            if (nrow(sub_ann) > 0) {
                spectras <- db_get_spectra(db(), sub_ann$spectra_id)
		spectras <- spectras[which(spectras$iso_theo == "M"), 
                                     c("spectra_id", "feature_id")]
		peaks <- db_get_peaks(db(), spectras$feature_id)
                peaks <- merge(sub_ann, 
                               merge(spectras, 
                                     peaks[, c("feature_id", "rtmin", "rtmax")], 
				     by = "feature_id"
                               ), 
                               by = "spectra_id"
                )
            } else peaks <- data.frame()

            plot_eic(eics, peaks)
        #})
        }, invalid = function(i) {
        print("########## check_data_eic")
        print(params)
        print(i)
        plot_empty_chromato(title = "EIC")
        }, error = function(e) {
        print("########## check_data_eic")
        print(params)
        print(e)
        sweet_alert_error(e$message)
        plot_empty_chromato(title = "EIC")
    })
})
