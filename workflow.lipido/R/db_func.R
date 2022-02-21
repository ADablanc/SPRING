db_connect <- function(sqlite_path) {
    db <- RSQLite::dbConnect(RSQLite::SQLite(), sqlite_path)
    RSQLite::dbExecute(db, 'pragma temp_store = memory;')
    RSQLite::dbExecute(db, 'pragma synchronous = normal;')
    RSQLite::dbExecute(db, 'pragma locking_mode = normal;')
    RSQLite::dbExecute(db, 'pragma cache_size = 1000000;')
    RSQLite::dbExecute(db, 'pragma journal_mode = wal;')
    RSQLite::dbExecute(db, 'pragma auto_vacuum = FULL;')
    return(db)
}

db_record_samples <- function(db, sample_names) RSQLite::dbWriteTable(
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
    RSQLite::dbExecute(db, query, params = list(a = ms_file, b = profile))
    rm(ms_file)
    rm(profile)
    gc()
}

db_read_ms_file <- function(db, sample_name, polarity) {
    query <- sprintf(
        "select ms_file_%s from sample where sample == \"%s\";",
        polarity, sample_name)
    ms_file <- RSQLite::dbGetQuery(db, query)[1, 1][[1]]
    if (is.na(ms_file[1])) return(NULL)
    else decompress(ms_file)
}

db_get_profile <- function(db, sample_name, polarity) {
    query <- sprintf(
        "select profile_%s from sample where sample == \"%s\";",
        polarity, sample_name)
    profile <- RSQLite::dbGetQuery(db, query)[1, 1][[1]]
    if (is.na(profile[1])) return(NULL)
    else decompress(profile)
}

compress <- function(obj) {
    blob::blob(fst::compress_fst(serialize(obj, NULL), compression = 100))
    # blob::blob(serialize(obj, NULL))
}
decompress <- function(obj) {
    unserialize(fst::decompress_fst(obj))
    # unserialize(obj)
}

db_record_xsets <- function(db, ann, spectras, spectra_infos, peaks,
                            peak_groups) {
    RSQLite::dbWriteTable(db, "ann", ann, overwrite = TRUE)
    RSQLite::dbWriteTable(db, "spectras", spectras, overwrite = TRUE)
    RSQLite::dbWriteTable(db, "spectra_infos", spectra_infos, overwrite = TRUE)
    RSQLite::dbWriteTable(db, "peaks", peaks, overwrite = TRUE)
    RSQLite::dbWriteTable(db, "peak_groups", peak_groups, overwrite = TRUE)
}

db_record_params <- function(db, filter_params, cwt_params, obw_params,
                             pd_params, ann_params) {
    RSQLite::dbWriteTable(db, "filter_params",
                          params_to_dataframe(filter_params), overwrite = TRUE)
    RSQLite::dbWriteTable(db, "cwt_params",
                          params_to_dataframe(cwt_params), overwrite = TRUE)
    RSQLite::dbWriteTable(db, "obw_params",
                          params_to_dataframe(obw_params), overwrite = TRUE)
    RSQLite::dbWriteTable(db, "pd_params",
                          params_to_dataframe(pd_params), overwrite = TRUE)
    RSQLite::dbWriteTable(db, "ann_params",
                          params_to_dataframe(ann_params), overwrite = TRUE)
}

db_get_spectras <- function(db) RSQLite::dbReadTable(db, "spectras")
db_get_spectra_infos <- function(db) RSQLite::dbReadTable(db, "spectra_infos")
db_get_ann <- function(db) RSQLite::dbReadTable(db, "ann")
