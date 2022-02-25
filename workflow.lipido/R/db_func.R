db_connect <- function(sqlite_path) {
    db <- RSQLite::dbConnect(RSQLite::SQLite(), sqlite_path)
    dbExecute(db, 'pragma temp_store = memory;')
    dbExecute(db, 'pragma synchronous = normal;')
    dbExecute(db, 'pragma locking_mode = normal;')
    dbExecute(db, 'pragma cache_size = 1000000;')
    dbExecute(db, 'pragma journal_mode = wal;')
    dbExecute(db, 'pragma auto_vacuum = FULL;')
    return(db)
}

dbWriteTable <- function(db, table_name, data, overwrite = FALSE,
                         append = FALSE, ...) {
	msg <- "database is locked"
	while (msg == "database is locked") {
		msg <- tryCatch({
			RSQLite::dbWriteTable(db, table_name, data, overwrite = overwrite,
			                      append = append, ...)
			"success"
			}, error = function(e) e$message)
	}
	if (msg != "success") stop(msg)
}

dbExecute <- function(db, query, ...) {
	msg <- "database is locked"
	while (msg == "database is locked") {
		msg <- tryCatch({
			RSQLite::dbExecute(db, query, ...)
			"success"
			}, error = function(e) e$message)
	}
	if (msg != "success") stop(msg)
}

dbGetQuery <- function(db, query, ...) {
    res <- tryCatch(RSQLite::dbGetQuery(db, query, ...),
             error = function(e) {
                 if (grepl("^no such table", e$message)) data.frame()
                 else e$message
             })
    if (class(res) != "data.frame") stop(res)
    else res
}

dbReadTable <- function(db, table_name, ...) {
    res <- tryCatch(RSQLite::dbReadTable(db, table_name, check.names = FALSE,
                                         ...),
                    error = function(e) {
                        if (grepl("^no such table", e$message)) data.frame()
                        else e$message
                    })
    if (class(res) != "data.frame") stop(res)
    else res
}

db_record_samples <- function(db, sample_names) dbWriteTable(
    db, "sample", data.frame(
        sample = sample_names,
        ms_file_positive = NA,
        ms_file_negative = NA,
        profile_positive = NA,
        profile_negative = NA
    ), overwrite = TRUE)

import_ms_file <- function(raw_file, converter, polarity, filter_params,
                           bin_size, db, sample_name) {
    ms_file <- tryCatch(
        convert_file(raw_file, converter, polarity, filter_params)
        , error = function(e) e$message
    )
    if (class(ms_file) == "xcmsRaw") {
        ms_file <- filter_ms_file(ms_file, filter_params)
        db_record_ms_file(db, sample_name, polarity, ms_file, bin_size)
        return("success")
    } else if (class(ms_file) == "character") return(ms_file)
    else return("fail without reasons")
}

db_record_ms_file <- function(db, sample_name, polarity, ms_file, bin_size) {
    profile <- compress(xcms::profMat(ms_file, step = bin_size))
    ms_file <- compress(ms_file)
    query <- sprintf(
        "UPDATE sample SET ms_file_%s = :a,
            profile_%s = :b WHERE sample == \"%s\";",
        polarity, polarity, sample_name)
    dbExecute(db, query, params = list(a = ms_file, b = profile))
    rm(ms_file)
    rm(profile)
    gc()
}

db_read_ms_file <- function(db, sample_name, polarity) {
    query <- sprintf(
        "select ms_file_%s from sample where sample == \"%s\";",
        polarity, sample_name)
    ms_file <- dbGetQuery(db, query)[1, 1][[1]]
    if (is.na(ms_file[1])) return(NULL)
    else decompress(ms_file)
}

db_get_profile <- function(db, sample_name, polarity) {
    query <- sprintf(
        "select profile_%s from sample where sample == \"%s\";",
        polarity, sample_name)
    profile <- dbGetQuery(db, query)[1, 1][[1]]
    if (is.na(profile[1])) return(NULL)
    else decompress(profile)
}

compress <- function(obj) {
    # blob::blob(fst::compress_fst(serialize(obj, NULL), compression = 100))
    blob::blob(serialize(obj, NULL))
}
decompress <- function(obj) {
    # unserialize(fst::decompress_fst(obj))
    unserialize(obj)
}

db_record_xsets <- function(db, ann, spectras, spectra_infos, peaks,
                            peak_groups) {
    dbWriteTable(db, "ann", ann, overwrite = TRUE)
    dbWriteTable(db, "spectras", spectras, overwrite = TRUE)
    dbWriteTable(db, "spectra_infos", spectra_infos, overwrite = TRUE)
    dbWriteTable(db, "peaks", peaks, overwrite = TRUE)
    dbWriteTable(db, "peak_groups", peak_groups, overwrite = TRUE)
}

db_record_params <- function(db, filter_params, cwt_params, obw_params,
                             pd_params, ann_params) {
    dbWriteTable(db, "filter_params",
                          params_to_dataframe(filter_params), overwrite = TRUE)
    dbWriteTable(db, "cwt_params",
                          params_to_dataframe(cwt_params), overwrite = TRUE)
    dbWriteTable(db, "obw_params",
                          params_to_dataframe(obw_params), overwrite = TRUE)
    dbWriteTable(db, "pd_params",
                          params_to_dataframe(pd_params), overwrite = TRUE)
    dbWriteTable(db, "ann_params",
                          params_to_dataframe(ann_params), overwrite = TRUE)
}

db_get_spectras <- function(db) dbReadTable(db, "spectras")
db_get_spectra_infos <- function(db) dbReadTable(db, "spectra_infos")
db_get_ann <- function(db) dbReadTable(db, "ann")
db_get_spectra <- function(db, spectra_id) dbGetQuery(db, sprintf(
    "select * from spectras where spectra_id == %s;", spectra_id))
db_get_params <- function(db) list(
    filter = dbReadTable(db, "filter_params")[1, ],
    cwt = dbReadTable(db, "cwt_params")[1, ],
    obw = dbReadTable(db, "obw_params")[1, ],
    pd = dbReadTable(db, "pd_params")[1, ],
    ann = dbReadTable(db, "ann_params")[1, ]
)
db_get_peaks <- function(db, feature_ids = NULL) {
    if (is.null(feature_ids)) dbReadTable(db, "peaks")
    else dbGetQuery(db, sprintf(
        "select * from peaks where feature_id in (%s);",
        paste(feature_ids, collapse = ", ")))
}

db_resolve_conflict <- function(db, conflict, i) {
    dbExecute(db, sprintf(
        "delete from ann where group_id == %s;",
        conflict[1, "group_id"]))
    dbWriteTable(db, "ann", conflict[i, , drop = FALSE], append = TRUE)
}
