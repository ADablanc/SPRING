 #' @title Create sqlite file
#'
#' @description
#' Create an sqlite file and optimize it via PRAGMA queries
#'
#' @param sqlite_path `character(1)` filepath to the sqlite file to create
#'
#' @return `SQLiteConnection`
#'
#' @seealso RSQLite::dbConnect
db_connect <- function(sqlite_path) {
    db <- RSQLite::dbConnect(RSQLite::SQLite(), sqlite_path)
    db_execute(db, "PRAGMA temp_store = memory;")
    db_execute(db, "PRAGMA synchronous = normal;")
    db_execute(db, "PRAGMA locking_mode = normal;")
    db_execute(db, "PRAGMA cache_size = 1000000;")
    db_execute(db, "PRAGMA journal_mode = wal;")
    db_execute(db, "PRAGMA auto_vacuum = FULL;")
    return(db)
}

#' @title Write a `DataFrame` in sqlite file
#'
#' @description
#' Override the default function `DBI::dbWriteTable` in order to force the
#' write process even if the database is locked by another process
#'
#' @param db `SQLiteConnection`
#' @param table_name `character(1)` name of the table
#' @param data `DataFrame`
#' @param overwrite `logical(1)` should overwrite if an existing table already
#' exists with that name ?
#' @param append `logical(1)` should append to the table if it exists
#' @param ... other parameters passed on the method
#'
#' @seealso `DBI::dbWriteTable`
db_write_table <- function(db,
                         table_name,
                         data,
                         overwrite = FALSE,
                         append = FALSE,
                         ...) {
    msg <- "database is locked"
    while (msg == "database is locked") {
        msg <- tryCatch({
            RSQLite::dbWriteTable(
                db,
                table_name,
                data,
                overwrite = overwrite,
                append = append,
                ...
            )
            "success"
        },
        error = function(e) {
            e$message
        })
    }
    if (msg != "success") {
        stop(msg)
    }
}

#' @title Send query to sqlite file
#'
#' @description
#' Override the default function `DBI::dbExecute` in order to force the
#' write process even if the database is locked by another process
#'
#' @param db `SQLiteConnection`
#' @param query `character(1)` query to send
#' @param ... other parameters passed on the method
#'
#' @seealso `DBI::dbExecute`
db_execute <- function(db, query, ...) {
    msg <- "database is locked"
    while (msg == "database is locked") {
        msg <- tryCatch({
            RSQLite::dbExecute(db, query, ...)
            "success"
        },
        error = function(e) {
            e$message
        })
    }
    if (msg != "success") {
        stop(msg)
    }
}

#' @title Get result query
#'
#' @description
#' Override the default function `DBI::dbGetQuery` in order to return an empty
#' dataframe if RSQLite or DBI return the error "no such table"
#'
#' @param db `SQLiteConnection`
#' @param query `character(1)`
#' @param ... other parameters passed on the method
#'
#' @return `DataFrame` of the result query
#'
#' @seealso `DBI::dbGetQuery`
db_get_query <- function(db, query, ...) {
    res <- tryCatch({
        RSQLite::dbGetQuery(db, query, ...)
    },
    error = function(e) {
         if (grepl("^no such table", e$message)) {
             data.frame()
         } else {
             e$message
         }
     })
    if (class(res) != "data.frame") {
        stop(res)
    } else {
        res
    }
}

#' @title Get table from sqlite file
#'
#' @description
#' Override the default function `DBI::dbReadTable` in order to return an empty
#' `DataFrame` if RSQLite or DBI return the error "no such table"
#'
#' @param db `SQLiteConnection`
#' @param table_name `character(1)` name of the table queried
#' @param ... other parameters passed on the method
#'
#' @return `DataFrame` of the table
#'
#' @seealso `DBI::dbReadTable`
db_read_table <- function(db, table_name, ...) {
    res <- tryCatch({
        RSQLite::dbReadTable(db, table_name, check.names = FALSE, ...)
    },
    error = function(e) {
        if (grepl("^no such table", e$message)) {
            data.frame()
        } else {
            e$message
        }
    })
    if (class(res) != "data.frame") {
        stop(res)
    } else {
        res
    }
}

#' @title Record samples
#'
#' @description
#' Create an empty table "sample" with all predefined sample names
#' The sample names will be the primary key of the table
#' It serve to be updated later with the function "import_ms_file"
#'
#' @param db `SQLiteConnection`
#' @param sample_names `character vector` sample names, should be unique !!!!
db_record_samples <- function(db, sample_names) {
    db_write_table(
        db,
        "sample",
        data.frame(
            sample = sample_names,
            ms_file = NA,
            xsa = NA
        ),
        overwrite = TRUE
    )
}

#' @title Compress object
#'
#' @description
#' Compress an R object in order to put it in a sqlite file with the `blob` type
#'
#' @param obj any R object
#'
#' @return `blob` object
compress <- function(obj) {
    blob::blob(
        serialize(obj, NULL)
    )
}

#' @title Decompress object
#'
#' @description
#' Decompress a `blob` object to obtain the initial R object
#'
#' @param obj `blob` object
#'
#' @return the initial R object
decompress <- function(obj) {
    unserialize(
        obj[[1]]
    )
}

#' @title Record ms file
#'
#' @description
#' Record a `xcmsRaw` file in the database
#' this object is compressed into a blob object before inserting in the
#' database.
#' Before doing it, we must first use the function `db_record_samples` in order
#' to prepopulate the database with the sample name (which is the primary key)
#'
#' @param db `SQLiteConnection`
#' @param sample_name `character(1)` sample name (primary key)
#' @param ms_file `xcmsRaw` object
db_record_ms_file <- function(db, sample_name, ms_file) {
    ms_file <- compress(ms_file)
    query <- sprintf(
        "UPDATE sample SET ms_file = :a WHERE sample == \"%s\";",
        sample_name
    )
    db_execute(db, query, params = list(a = ms_file))
    rm(ms_file)
    gc()
}

#' @title Get ms file
#'
#' @description
#' Get the `xcmsRaw` of corresponding sample
#'
#' @param db `SQLiteConnection`
#' @param sample_name `character(1)` sample name
#'
#' @return an `xcmsRaw` object
db_read_ms_file <- function(db, sample_name) {
    query <- sprintf(
        "SELECT ms_file
        FROM sample
        WHERE sample == \"%s\";",
        sample_name
    )
    ms_file <- db_get_query(db, query)[1, 1]
    if (is.null(ms_file)) {
        NULL
    } else if (is.na(ms_file)) {
        NULL
    } else {
        decompress(ms_file)
    }
}

#' @title Import ms file
#'
#' @description
#' First it try to convert the file in mzXML
#' Convert one RAW file to a mzXML file
#' It will try to convert the file and extract only the scans in positive or
#' negative
#' It also trim the file according the rt range and m/z range specified in
#' the filter_params object with msconvert
#' Check with XCMS package if the file can be converted
#' If the file is a CDF it will copy the file instead
#' If the conversion failed & the file is a mzML or mzXML
#' it will copy the file instead and try to trim only the rt range
#' then it record the `xcmsRaw` object obtained in the database if the
#' conversion was successful
#' the `xcmsRaw` object is compressed into a `blob` object before inserting in
#' the database.
#' Before doing it, we must first use the function `db_record_samples` in order
#' to prepopulate the database with the sample name (which is the primary key)
#'
#' @param db `SQLiteConnection`
#' @param sample_name `character(1)` sample name (primary key)
#' @param raw_file `character(1)` filepath to the raw file to convert
#' @param converter `character(1)` filepath to the msconvert.exe
#' @param filter_params `FilterParam` object
#'
#' @seealso `convert_file`, `record_ms_file`
import_ms_file <- function(db,
                           sample_name,
                           raw_file,
                           converter,
                           filter_params) {
    ms_file <- tryCatch({
            ms_file <- convert_file(
                raw_file,
                converter,
                filter_params
            )
            attributes(ms_file)$scantime_corrected <- ms_file@scantime
            filter_ms_file(ms_file, filter_params)
        },
        error = function(e) {
            e$message
        }
    )
    if (class(ms_file) == "xcmsRaw") {
        db_record_ms_file(db, sample_name, ms_file)
        "success"
    } else {
        ms_file
    }
}

#' @title Record `xsAnnotate` in the database
#'
#' @description
#' Record an `xsAnnotate`
#' This record is only use to debugging the `annotate_peaks` function to see why
#' some peaks were not annotated
#'
#' @param db `SQLiteConnection`
#' @param xsa `xsAnnotate`
#' @param sample_name `character(1)` sample name
db_record_xsa <- function(db, xsa, sample_name) {
    query <- sprintf(
        "UPDATE sample
        SET xsa = :a
        WHERE sample == \"%s\";",
        sample_name
    )
    db_execute(db, query, params = list(a = compress(xsa)))
}

#' @title Record the annotation results
#'
#' @description
#' Record the annotation results obtained from the processing workflow in the
#' database
#'
#' @param db `SQLiteConnection` sqlite connection
#' @param ann `DataFrame` each line correspond to a compound found
#' with the columns:
#' \itemize{
#'     \item group_id `integer` group ID
#'     \item eic_id `integer` EIC ID
#'     \item class `character` cpd class
#'     \item name `character` name
#'     \item formula `character` chemical formula
#'     \item adduct `character` adduct form
#'     \item ion_formula `character` ion chemical formula
#'     \item rtdiff `numeric` retention time difference between the measured &
#'     the expected
#'     \item rt `numeric` retention time measured meanned accross the samples
#'     \item rtmin `numeric` born min of retention time measured accross the
#'     samples
#'     \item rtmax `numeric` born max of the retention time measured accross the
#'      samples
#'     \item nsamples `integer` number of samples where the compound was found
#'     \item best_score `numeric` best isotopic score seen
#'     \item best_deviation_mz `numeric` best m/z deviation seen
#'     \item best_npeak `integer` best number of isotopologues found
#'     \item ... `integer` a column for each sample which contain the spectra ID
#' }
#' @param spectras `DataFrame`, each line correspond to a peak annotated with
#' its corresponding theoretical peak or the theoretical peak missed,
#' with the columns :
#' \itemize{
#'     \item spectra_id `integer` spectra ID
#'     \item feature_id `integer` feature ID
#'     \item mz `numeric` m/z
#'     \item int `numeric` area integrated
#'     \item abd `numeric` relative abundance
#'     \item ion_id_theo `integer` ignore
#'     \item mz_theo `numeric` theoretical m/z
#'     \item abd_theo `numeric` theoretical relative abundance
#'     \item iso_theo `character` theoretical isotopologue annotation
#' }
#' @param spectra_infos `DataFrame`, each line correspond to a spectra
#' annotated, with the columns :
#' \itemize{
#'     \item spectra_id `integer` spectra ID
#'     \item score `numeric` isotopic score observed
#'     \item deviation_mz `numeric` m/z deviation observed
#'     \item npeak `integer` number of isotopologue annotated
#'     \item basepeak_mz `numeric` m/z of the basepeak annotated
#'     \item basepeak_int `numeric` area of the basepeak annotated
#'     \item sum_int `numeric` cumulative sum off all the area of the
#'     isotopologues annotated
#'     \item rt `numeric` retention time
#' }
db_record_ann <- function(db,
                          ann,
                          spectras,
                          spectra_infos) {
    db_write_table(db, "ann", ann, overwrite = TRUE)
    db_write_table(db, "spectras", spectras, overwrite = TRUE)
    db_write_table(db, "spectra_infos", spectra_infos, overwrite = TRUE)
}

#' @title Record parameters in database
#'
#' @description
#' Record processing parameters in the database
#'
#' @param db `SQLiteConnection`
#' @param filter_params `FilterParam`
#' @param cwt_params `CentwaveParam`
#' @param obw_params `ObiwarpParam`
#' @param pd_params `PeakDensityParam`
#' @param camera_params `CameraParam`
#' @param ann_params `AnnotationParam`
db_record_params <- function(db,
                             filter_params,
                             cwt_params,
                             obw_params,
                             pd_params,
                             camera_params,
                             ann_params) {
    filter_params <- params_to_dataframe(filter_params)
    cwt_params <- params_to_dataframe(cwt_params)
    obw_params <- params_to_dataframe(obw_params)
    pd_params <- params_to_dataframe(pd_params)
    camera_params <- params_to_dataframe(camera_params)
    ann_params <- params_to_dataframe(ann_params)
    db_write_table(db, "filter_params", filter_params, overwrite = TRUE)
    db_write_table(db, "cwt_params", cwt_params, overwrite = TRUE)
    db_write_table(db, "obw_params", obw_params, overwrite = TRUE)
    db_write_table(db, "pd_params", pd_params, overwrite = TRUE)
    db_write_table(db, "camera_params", camera_params, overwrite = TRUE)
    db_write_table(db, "ann_params", ann_params, overwrite = TRUE)
}

#' @title Get Annotations
#'
#' @description
#' Get annotations from database
#'
#' @param db `SQLiteConnection`
#' @param names `character vector` the compound names, not mandatory
#' @param group_ids `numeric vector` the group IDs, not mandatory
#' @param row `numeric` the rowID to extract for the annotation table in the DB,
#' not mandatory
#'
#' @return `DataFrame` each line correspond to a compound found
#' with the columns:
#' \itemize{
#'         \item group_id `integer` group ID
#'         \item eic_id `integer` EIC ID
#'         \item class `character` cpd class
#'         \item name `character` name
#'         \item referent_adduct `character` referent adduct for the compound
#'         \item formula `character` chemical formula
#'         \item adduct `character` adduct form
#'         \item ion_formula `character` ion chemical formula
#'         \item rtdiff `numeric` retention time difference between the measured
#'          & the expected
#'         \item rt `numeric` retention time measured meanned accross the
#'         samples
#'         \item rtmin `numeric` born min of retention time measured accross the
#'         samples
#'         \item rtmax `numeric` born max of the retention time measured accross
#'          the samples
#'         \item nsamples `integer` number of samples where the compound was
#'         found
#'         \item best_score `numeric` best isotopic score seen
#'         \item best_deviation_mz `numeric` best m/z deviation seen
#'         \item best_npeak `integer` best number of isotopologues found
#'         \item ... `integer` a column for each sample which contain the
#'         spectra ID
#' }
db_get_annotations <- function(db, names = NULL, group_ids = NULL, row = NULL) {
    query <- "SELECT * FROM ann"
    if (!is.null(group_ids)) {
        query2 <- sprintf(
            "group_id IN (%s)",
            paste(group_ids, sep = "", collapse = ", ")
        )
    } else if (!is.null(names)) {
        query2 <- sprintf(
            "name IN (%s)",
            paste("\"", names, "\"", sep = "", collapse = ", ")
        )
    } else if (!is.null(row)) {
        query2 <- sprintf("ROWID == %s", row)
    } else {
        query2 <- NULL
    }
    if (!is.null(query2)) {
        query <- paste(query, "WHERE", query2, sep = " ")
    }
    db_get_query(db, query)
}

#' @title Get spectra infos
#'
#' @description
#' Get spectra infos from database
#'
#' @param db `SQLiteConnection`
#' @param spectra_ids `integer vector` the spectra ids
#'
#' @return `DataFrame`, each line correspond to a spectra
#' annotated, with the columns :
#' \itemize{
#'     \item spectra_id `integer` spectra ID
#'     \item score `numeric` isotopic score observed
#'     \item deviation_mz `numeric` m/z deviation observed
#'     \item npeak `integer` number of isotopologue annotated
#'     \item basepeak_mz `numeric` m/z of the basepeak annotated
#'     \item basepeak_int `numeric` area of the basepeak annotated
#'     \item sum_int `numeric` cumulative sum off all the area of the
#'     isotopologues annotated
#'     \item rt `numeric` retention time
#' }
db_get_spectra_infos <- function(db, spectra_ids = NULL) {
    if (is.null(spectra_ids)) {
        db_read_table(db, "spectra_infos")
    } else {
        db_get_query(db, sprintf(
            "SELECT *
            FROM spectra_infos
            WHERE spectra_id IN (%s)",
            paste(spectra_ids, collapse = ", ")
        ))
    }
}

#' @title Get spectras
#'
#' @description
#' Get spectras from database
#'
#' @param db `SQLiteConnection`
#' @param spectra_ids `integer vector` the spectra IDs
#'
#' @return `DataFrame`, each line correspond to a peak annotated with
#' its corresponding theoretical peak or the theoretical peak missed,
#' with the columns :
#' \itemize{
#'         \item spectra_id `integer` spectra ID
#'         \item feature_id `integer` feature ID
#'         \item mz `numeric` m/z
#'         \item mzmin `numeric` m/z born min
#'         \item mzmax `numeric` m/z born max
#'         \item rt `numeric` rT
#'         \item rtmin `numeric` rT born min
#'         \item rtmax `numeric` rT born max
#'         \item int `numeric` area integrated
#'         \item abd `numeric` relative abundance
#'         \item ion_id_theo `integer` ignore
#'         \item mz_theo `numeric` theoretical m/z
#'         \item abd_theo `numeric` theoretical relative abundance
#'         \item iso_theo `character` theoretical isotopologue annotation
#' }
db_get_spectras <- function(db, spectra_ids = NULL) {
    if (is.null(spectra_ids)) {
        db_read_table(db, "spectras")
    } else {
        db_get_query(
            db,
            sprintf(
                "SELECT *
                FROM spectras
                WHERE spectra_id IN (%s);",
                paste(spectra_ids, collapse = ", ")
            )
        )
    }
}

#' @title Get all parameters
#'
#' @description
#' Get all parameters recorded in database
#'
#' @param db `SQLiteConnection`
#'
#' @return `list` containing :
#' \itemize{
#'     \item filter `DataFrame` of one line with columns :
#'     \itemize{
#'         \item polarity `character` "positive" or "negative"
#'         \item mz_range_min `numeric` m/z range min
#'         \item mz_range_max `numeric` m/z range max
#'         \item rt_range_min `numeric` rT range min
#'         \item rt_range_max `numeric` rT range max
#'     }
#'     \item cwt `DataFrame` of one line with columns :
#'     \itemize{
#'         \item ppm `numeric` Maximal tolerated m/z deviation in consecutive
#'         scans in parts per million (ppm)
#'         \item peakwidth_min `numeric` Expected approximate peak width min in
#'         chromatographic space
#'         \item peakwidth_max `numeric` Expected approximate peak width max in
#'         chromatographic space
#'         \item snthresh `numeric` Signal to noise ratio cutoff
#'         \item prefilter_step `numeric` Mass traces are only retained if they
#'         contain at least k peaks with intensity >= I
#'         \item prefilter_level `numeric` Mass traces are only retained if they
#'         contain at least k peaks with intensity >= I
#'         \item mzCenterFun `character` Name of the function to calculate the
#'         m/z center of the chromatographic peak
#'         \item integrate `integer` Integration method. If unchecked the
#'         descent is done on the real data, if checked peak limits are found
#'         through descent on the mexican hat filtered data. Method 1 is very
#'         accurate but prone to noise, while method 2 is more robust to noise
#'         but less exact
#'         \item mzdiff `numeric` Minimum difference in m/z for peaks with
#'         overlapping retention times, can be negative to allow overlap
#'         \item fitgauss `integer` whether or not a Gaussian should be fitted
#'         to each peak. This affects mostly the retention time position of the
#'         peak
#'         \item noise `numeric` Optional argument which is useful for data that
#'          was centroided without any intensity threshold, centroids with
#'          intensity < noise are omitted from ROI detection
#'         \item verboseColumns `integer` whether additional peak meta data
#'         columns should be returned, ignore
#'         \item firstBaselineCheck `integer` Continuous data within regions of
#'         interest is checked to be above the first baseline
#'     }
#'     \item obw `DataFrame` of one line with columns :
#'     \itemize{
#'         \item binSize `numeric` slice of overlapping m/z groups
#'         \item response `numeric` Defining the responsiveness of warping with
#'         response = 0 giving linear warping on start and end points and
#'         response = 100 warping using all bijective anchors
#'         \item distFun `character` Distance function to be used.
#'         Allowed values are :
#'         \itemize{
#'             \item cor : Pearson's correlation
#'             \item cor_opt : calculate only 10% diagonal band of distance
#'             matrix; better runtime)
#'             \item cov : covariance
#'             \item prd : product
#'             \item euc : Euclidian distance
#'         }
#'         \item gapInit `numeric` Defining the penalty for gap opening
#'         \item gapExtend `numeric` Defining the penalty for gap enlargement
#'         \item factorDiag `numeric` Defining the local weight applied to
#'         diagonal moves in the alignment
#'         \item factorGap `numeric` Defining the local weight for gap moves in
#'         the alignment
#'         \item localAlignment `integer` Whether a local alignment should be
#'         performed instead of the default global alignment
#'         \item initPenalty `numeric` Defining the penalty for initiating an
#'         alignment (for local alignment only)
#'     }
#'     \item pd `DataFrame` of one line with columns :
#'     \itemize{
#'         \item bw `numeric` retention time standard deviation (s) allowed
#'         \item minFraction `numeric` defining the minimum fraction of samples
#'         in at least one sample group in which the peaks have to be present
#'         to be considered as a peak group (feature)
#'         \item minSamples `integer` with the minimum number of samples in at
#'         least one sample group in which the peaks have to be detected to be
#'         considered a peak group (feature)
#'         \item binSize `numeric` slice of overlapping m/z groups
#'         \item maxFeatures `integer` with the maximum number of peak groups
#'         to be identified in a single mz slice
#'     }
#'     \item camera `DataFrame` of one line with columns :
#'     \itemize{
#'         \item cores `numeric` number of cores for parallelization
#'         \item sigma `numeric` multiplier of the standard deviation
#'         \item perfwhm `numeric` percentage of the FWHM
#'         \item intval `character` "into", "maxo" or "intb"
#'         \item cor_eic_th `numeric` correlation threshold
#'         \item pval `numeric` significant correlation threshold
#'         \item graph_method `character` method selection for grouping peaks
#'          after correlation analysis into pseudospectra, could be "hcs" or
#'          "lpc"
#'         \item calcIso `logical` use isotopic relationship for peak grouping
#'         \item calcCiS `logical` use correlation inside samples for peak
#'          grouping
#'         \item calcCaS `logical` use correlation across samples for peak
#'          grouping
#'         \item maxiso `numeric(1)` max isotopologues
#'         \item ppm `numeric` ppm tolerance
#'         \item mzabs `numeric` mDa tolerance
#'         \item minfrac `numeric` percentage number of samples which must
#'          satisfy 12C/13C rule
#'         \item max_peaks `numeric` max how much peaks per thread
#'     }
#'     \item ann `DataFrame` of one line with columns :
#'     \itemize{
#'         \item da_tol `numeric` m/z tolerance in Dalton
#'         \item rt_tol `numeric` rT tolerance in sec
#'         \item abd_tol `numeric` relative abundance tolerance, each peak which
#'         have an higher difference of relative abundance with its
#'         corresponding theoretical peak will be discarded
#'         \item instrument `character` instrument names from the enviPat
#'         package
#'         \item database `character` name of the database used
#'         \item polarity `character` "positive" or "negative"
#'         \item cpd_classes `character vector` compound classes in database to
#'          used to restrict the annotations, collapsed with the character ";"

#'     }
#' }
db_get_params <- function(db) {
    list(
        filter = db_read_table(db, "filter_params")[1, ],
        cwt = db_read_table(db, "cwt_params")[1, ],
        obw = db_read_table(db, "obw_params")[1, ],
        pd = db_read_table(db, "pd_params")[1, ],
        camera = db_read_table(db, "camera_params")[1, ],
        ann = db_read_table(db, "ann_params")[1, ]
    )
}

#' @title Count nsamples
#'
#' @description
#' Count number of samples recorded in database
#'
#' @param db `SQLiteConnection`
#'
#' @return `numeric` number of samples recorded in database
db_get_nsamples <- function(db) {
    max(0, db_get_query(db, "select count(sample) from sample")[1, 1])
}

#' @title Resolve annotation conflict
#'
#' @description
#' Resolve an annotation conflict: it will remove all conflicts for a group of
#' annotations and only keep one
#'
#' @param db `SQLiteConnection`
#' @param group_id `numeric(1)` ID of the peak groups
#' @param name `character(1)` name of the annotation to keep instead of others
db_resolve_conflict <- function(db, group_id, name) {
    db_execute(db, sprintf(
        "DELETE FROM ann
            WHERE group_id == %s
                AND name != \"%s\";",
        group_id,
        name
    ))
}

#' @title Record EICs
#'
#' @description
#' Record EICs for each basepeak for each EIC ID for each sample
#' It will be faster than reloading each raw file from database & retrace the
#' corresponding EIC
#'
#' @param db `SQLiteConnection`
#' @param pb_fct `function` used to update the progress bar
db_record_eics <- function(db, pb_fct = NULL) {
    if (!is.null(pb_fct)) {
        pb_fct(n = 0, total = 1, title = "Generate EICs")
    }

    # get data
    params <- db_get_params(db)
    nsamples <- db_get_nsamples(db)
    mz_ann <- get_int_ann(
        db_get_annotations(db),
        db_get_spectra_infos(db),
        nsamples,
        val = "mz"
    )
    sample_names <- colnames(mz_ann)[(ncol(mz_ann) - nsamples + 1):ncol(mz_ann)]
    mz_ann <- mz_ann[!duplicated(mz_ann[, "EIC ID"]),
                     c("EIC ID", "rT (min)", sample_names),
                     drop = FALSE]
    mzs <- apply(mz_ann[, (ncol(mz_ann) - nsamples + 1):ncol(mz_ann)],
                 1, median, na.rm = TRUE)
    da_tol <- convert_ppm_da(params$cwt$ppm, mzs)
    rt_tol <- max(params$cwt$peakwidth_max, params$ann$rt_tol)
    data <- data.frame(
        eic_id = mz_ann[, "EIC ID"],
        mzmin = mzs - da_tol,
        mzmax = mzs + da_tol,
        rtmin = mz_ann[, "rT (min)"] * 60 - rt_tol,
        rtmax = mz_ann[, "rT (min)"] * 60 + rt_tol
    )

    # record eic first in a temporary database
    tmp_db <- db_connect(tempfile(fileext = ".sqlite"))
    db_execute(db, "DROP TABLE IF EXISTS eic")
    for (i in seq(length(sample_names))) {
        if (!is.null(pb_fct)) {
            # update the progress bar only every 21%
            if (i %% ceiling(length(sample_names) / 100) == 0) {
                pb_fct(i, length(sample_names), "Generate EICs")
            }
        }
        ms_file <- db_read_ms_file(db, sample_names[i])
        invisible(capture.output(lapply(seq(nrow(data)), function(j) {
            db_write_table(
                tmp_db,
                "eic",
                cbind(
                    eic_id = data[j, "eic_id"],
                    sample = sample_names[i],
                    get_eic(
                        ms_file,
                        mz_range = data[j, c("mzmin", "mzmax")],
                        rt_range = data[j, c("rtmin", "rtmax")],
                        NA_values = TRUE
                    )
                ),
                append = TRUE
            )
        })))
    }

    # now for each group id align the EICs
    db_execute(db, "DROP TABLE IF EXISTS eic")
    for (i in seq(nrow(data))) {
        if (!is.null(pb_fct)) {
            # update the progress bar only every 1%
            if (i %% ceiling(nrow(data) / 100) == 0) {
                pb_fct(i, nrow(data), "Align EICs")
            }
        }
        eics <- db_get_query(
            tmp_db,
            sprintf("SELECT * FROM eic WHERE eic_id == %s", data[i, "eic_id"])
        )
        db_write_table(
            db,
            "eic",
            cbind.data.frame(
                eic_id = data[i, "eic_id"],
                rt = eics[eics$sample == sample_names[1], "rt"],
                do.call(cbind, split(eics$int, eics$sample))
            ),
            append = TRUE
        )
    }
    RSQLite::dbDisconnect(tmp_db)
    if (!is.null(pb_fct)) {
        pb_fct(n = 1, total = 1, title = "Align EICs")
    }
}

#' @title Get EIC from data
#'
#' @description
#' Get all the EIC for all the files in the database for ONE basepeak mass
#'
#' @param db `SQLiteConnection`
#' @param eic_id `numeric(1)` eic ID in database, it corresponds to the row ID
#' for the annotation table in the database
#'
#' @return `DataFrame` with a variable number of columns
#'  (one sample = one column):
#' \itemize{
#'     \item rt `numeric` retention time (in sec)
#'     \item ... `numeric` intensity of the sample
#' }
db_get_eic <- function(db, eic_id) {
    db_get_query(db, sprintf(
        "SELECT * FROM eic WHERE eic_id == %s;",
        eic_id
    ))[, -1]
}

#' @title Get row ID
#' @description
#' Get the row ID from the database for a compound according its referent adduct
#'
#' @param db `SQLiteConnection`
#' @param cpd_name `character(1)` name of the compound
#'
#' @return `numeric(1)` the EIC ID
db_get_row_id <- function(db, cpd_name) {
    db_get_query(db, sprintf(
        "SELECT rowid FROM ann WHERE name == \"%s\" AND adduct == (
            SELECT referent_adduct FROM ann WHERE name == \"%s\" LIMIT 1);",
        cpd_name, cpd_name
    ))[1, "rowid"]
}
