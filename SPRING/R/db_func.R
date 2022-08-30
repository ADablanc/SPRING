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
    query <- gsub("= \"?NA\"?", "= null", query)
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

#' @title Record the annotation results
#'
#' @description
#' Record the annotation results obtained from the processing workflow in the
#' database
#'
#' @param db `SQLiteConnection` sqlite connection
#' @param xsf `DataFrame list` with items :
#' \itemize{
#'     \item ann `DataFrame` each line correspond to a compound found
#'     with the columns:
#'     \itemize{
#'         \item pcgroup_id `integer` group ID
#'         \item basepeak_group_id `integer` EIC ID
#'         \item class `character` cpd class
#'         \item name `character` name
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
#'     }
#'     \item spectras `DataFrame`, each line correspond to a peak annotated with
#'     its corresponding theoretical peak or the theoretical peak missed,
#'     with the columns :
#'         \itemize{
#'         \item spectra_id `integer` spectra ID
#'         \item feature_id `integer` feature ID
#'         \item mz `numeric` m/z
#'         \item int `numeric` area integrated
#'         \item abd `numeric` relative abundance
#'         \item ion_id_theo `integer` ignore
#'         \item mz_theo `numeric` theoretical m/z
#'         \item abd_theo `numeric` theoretical relative abundance
#'         \item iso_theo `character` theoretical isotopologue annotation
#'     }
#'     \item spectra_infos `DataFrame`, each line correspond to a spectra
#'     annotated, with the columns :
#'     \itemize{
#'         \item spectra_id `integer` spectra ID
#'         \item score `numeric` isotopic score observed
#'         \item deviation_mz `numeric` m/z deviation observed
#'         \item npeak `integer` number of isotopologue annotated
#'         \item basepeak_mz `numeric` m/z of the basepeak annotated
#'         \item basepeak_int `numeric` area of the basepeak annotated
#'         \item sum_int `numeric` cumulative sum off all the area of the
#'         isotopologues annotated
#'         \item rt `numeric` retention time
#'     }
#'     \item peakgroups `DataFrame`, each line correspond to a group annotated
#'     by XCMS and CAMERA
#'     \itemize{
#'         \item group_id `integer` group ID
#'         \item pcgroup_id `integer` pcgroup ID
#'         \item adduct `character` adduct annotation by CAMERA (NULL if absent)
#'         \item cluster_id `integer` cluster ID
#'         \item iso `character` could be "M" or "M+*"
#'         \item mzmed `float` m/z median computed by XCMS
#'         \item mzmin `float` m/z median born min (not the m/z born min !!)
#'         \item mzmax `float` m/z median born max (not the m/z born max !!)
#'         \item rtmed `float` rT median computed by XCMS
#'         \item rtmin `float` rT median born min (not the rT born min !!)
#'         \item rtmax `float` rT median born max (not the rT born max !!)
#'         \item npeaks `integer` number of peaks grouped accross samples
#'         \item ... `integer` a column for each sample which contain the
#'         feature ID (row ID from the peaktable)
#'     }
#'     \item peaks `DataFrame`, peaktable from XCMS
#'     \itemize{
#'         \item mz `float` m/z
#'         \item mzmin `float` m/z born min
#'         \item mzmax `float` m/z born max
#'         \item rt `float` rT
#'         \item rtmin `float` rT born min
#'         \item rtmax `float` rT born max
#'         \item into `float` area of the peak
#'         \item intb `float` area of the peak above baseline
#'         \item maxo `float` maximum intensity
#'         \item sn `float` signal/noise
#'         \item egauss `float` ignore
#'         \item mu `float` ignore
#'         \item sigma `float` ignore
#'         \item h `float` ignore
#'         \item f `integer` ID of the ROI
#'         \item dppm `float` ppm deviation
#'         \item scale `integer` width of the wave used for the peak detection
#'         \item scpos `integer` scan ID
#'         \item scmin `integer` scan ID born min of the wave detection
#'         \item scmax `integer` scan ID born max of the wave detection
#'         \item lmin `integer` scan ID after extension of the scmin
#'         \item lmax `integer` scan ID after extension of the scmax
#'     }
#' }
db_record_xsf <- function(db, xsf) {
    db_write_table(db, "ann", xsf$ann, overwrite = TRUE)
    db_write_table(db, "spectra_infos", xsf$spectra_infos, overwrite = TRUE)
    db_write_table(db, "spectras", xsf$spectras, overwrite = TRUE)
    db_write_table(db, "peakgroups", xsf$peakgroups, overwrite = TRUE)
    db_write_table(db, "peaks", xsf$peaks, overwrite = TRUE)
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
#' @param pcgroup_ids `numeric vector` the group IDs, not mandatory
#' @param row_ids `numeric(1)` the rowIDs to extract for the annotation table in
#'  the DB, not mandatory
#'
#' @return `DataFrame` each line correspond to a compound found
#' with the columns:
#' \itemize{
#'         \item pcgroup_id `integer` group ID
#'         \item basepeak_group_id `integer` EIC ID
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
db_get_annotations <- function(db,
                               names = NULL,
                               pcgroup_ids = NULL,
                               row_ids = NULL) {
    query <- "SELECT ROWID, * FROM ann WHERE pcgroup_id IS NOT NULL"
    if (!is.null(pcgroup_ids)) {
        query <- paste(query, "AND", sprintf(
            "pcgroup_id IN (%s)",
            paste(pcgroup_ids, collapse = ", ")
        ))
    } else if (!is.null(names)) {
        query <- paste(query, "AND", sprintf(
            "name IN (%s)",
            paste("\"", names, "\"", sep = "", collapse = ", ")
        ))
    } else if (!is.null(row_ids)) {
        query <- paste(query, "AND",sprintf(
            "ROWID IN (%s)",
            paste(row_ids, collapse = ", ")
        ))
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
#'         \item group_id `integer` group ID
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

#' @title Get peakgroups
#'
#' @description
#' Get peakgroups from database
#'
#' @param db `SQLiteConnection`
#' @param group_ids `numeric` group IDs
#'
#' @return `DataFrame`, each line correspond to a group annotated by
#' XCMS and CAMERA
#' \itemize{
#'     \item group_id `integer` group ID
#'     \item pcgroup_id `integer` pcgroup ID
#'     \item adduct `character` adduct annotation by CAMERA (NULL if absent)
#'     \item cluster_id `integer` cluster ID
#'     \item iso `character` could be "M" or "M+*"
#'     \item mzmed `float` m/z median computed by XCMS
#'     \item mzmin `float` m/z median born min (not the m/z born min !!)
#'     \item mzmax `float` m/z median born max (not the m/z born max !!)
#'     \item rtmed `float` rT median computed by XCMS
#'     \item rtmin `float` rT median born min (not the rT born min !!)
#'     \item rtmax `float` rT median born max (not the rT born max !!)
#'     \item npeaks `integer` number of peaks grouped accross samples
#'     \item ... `integer` a column for each sample which contain the
#'     feature ID (row ID from the peaktable)
#' }
db_get_peakgroups <- function(db, group_ids = NULL) {
    if (length(group_ids) > 0) {
        db_get_query(db, sprintf(
            "SELECT * FROM peakgroups WHERE group_id in (%s)",
            paste(group_ids, collapse = ", ")
        ))
    } else {
        db_read_table(db, "peakgroups")
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
#' @return `numeric(1)` number of samples recorded in database
db_get_nsamples <- function(db) {
    max(
        0,
        db_get_query(
            db,
            "select COUNT(DISTINCT(sample)) from spectra_infos"
        )[1, 1]
    )
}

#' @title Resolve annotation conflict
#'
#' @description
#' Resolve an annotation conflict: it will remove all conflicts for a group of
#' annotations and only keep one
#'
#' @param db `SQLiteConnection`
#' @param pcgroup_id `numeric(1)` pcgroup ID of the annotation
#' @param name `character(1)` name of the annotation to keep instead of others
db_resolve_conflict <- function(db, pcgroup_id, name) {
    db_execute(db, sprintf(
        "DELETE FROM ann
            WHERE pcgroup_id == %s
                AND name != \"%s\";",
        pcgroup_id,
        name
    ))
}

#' @title Record EICs
#'
#' @description
#' Record EICs and mz for each basepeak and sample
#' It will be faster than reloading each raw file from database & retrace the
#' corresponding EIC or the m/z deviations
#'
#' @param db `SQLiteConnection`
#' @param xset `xcmsSet` xcms object
db_record_mzdata <- function(db, xset) {
    # get data
    params <- db_get_params(db)
    nsamples <- db_get_nsamples(db)

    peakgroups <- db_get_peakgroups(db)
    sample_names <- colnames(peakgroups)[
        (ncol(peakgroups) - nsamples + 1):ncol(peakgroups)]
    da_tol <- convert_ppm_da(params$cwt$ppm, peakgroups$mzmed)
    rt_tol <- max(params$cwt$peakwidth_max, params$ann$rt_tol)
    peakgroups <- data.frame(
        group_id = peakgroups$group_id,
        mz = peakgroups$mzmed,
        mzmin = peakgroups$mzmed - da_tol,
        mzmax = peakgroups$mzmed + da_tol,
        rtmin = peakgroups$rtmed - rt_tol,
        rtmax = peakgroups$rtmed + rt_tol
    )

    # record eic & mzmat first in a temporary database foreach file
    tmp_db_file <- tempfile(fileext = ".sqlite")
    tmp_db <- db_connect(tmp_db_file)
    db_execute(tmp_db, "DROP TABLE IF EXISTS eic")
    db_execute(tmp_db, "DROP TABLE IF EXISTS mzmat")
    RSQLite::dbDisconnect(tmp_db)

    # check if BiocParallel was initialized
    parallelization <- class(BiocParallel::bpbackend())[1] == "SOCKcluster"
    if (!parallelization) {
        BiocParallel::register(BiocParallel::SerialParam())
        BiocParallel::bpstart()
    } else {
        parallel::clusterExport(
            BiocParallel::bpbackend(),
            list("db_write_table", "db_connect", "db_execute"),
            envir = pryr::where("db_connect")
        )
        parallel::clusterExport(
            BiocParallel::bpbackend(),
            list("xset", "peakgroups", "tmp_db_file", "sample_names"),
            envir = pryr::where("xset")
        )
    }

    suppressWarnings(suppressMessages(
        BiocParallel::bplapply(seq(nsamples), function(i) {
            tmp_db <- db_connect(tmp_db_file)
            ms_file <- xcms::getXcmsRaw(
                xset,
                sampleidx = i,
                profstep = 0,
                BPPARAM = BiocParallel::SerialParam()
            )
            lapply(seq(nrow(peakgroups)), function(j) {
                rawmat <- xcms::rawMat(
                    ms_file,
                    mzrange = unlist(peakgroups[j, c("mzmin", "mzmax")]),
                    rtrange = peakgroups[j, c("rtmin", "rtmax")]
                )
                scans <- which(ms_file@scantime %in% rawmat[, "time"])
                scmin <- which.min(abs(ms_file@scantime -
                                           peakgroups[j, "rtmin"]))
                scmax <- which.min(abs(ms_file@scantime -
                                           peakgroups[j, "rtmax"]))
                missing_scans <- scmin:scmax
                missing_scans <- missing_scans[!missing_scans %in% scans]
                if (length(missing_scans) > 0) {
                    rawmat <- rbind(
                        rawmat[, c("time", "intensity", "mz")],
                        data.frame(
                            time = ms_file@scantime[missing_scans],
                            intensity = NA,
                            mz = NA
                        )
                    )
                }
                db_write_table(
                    tmp_db,
                    "eic",
                    cbind(
                        group_id = peakgroups[j, "group_id"],
                        sample = i,
                        aggregate(
                            intensity ~ time,
                            data = rawmat,
                            FUN = sum,
                            na.action = NULL
                        )
                    ),
                    append = TRUE
                )
                db_write_table(
                    tmp_db,
                    "mzmat",
                    cbind(
                        group_id = peakgroups[j, "group_id"],
                        sample = i,
                        aggregate(
                            mz ~ time,
                            data = rawmat,
                            FUN = function(x) {
                                if (all(is.na(x))) NA
                                else x[which.min(abs(x - peakgroups[j, "mz"]))]
                            },
                            na.action = NULL
                        )
                    ),
                    append = TRUE
                )
            })
            RSQLite::dbDisconnect(tmp_db)
        }
    )))

    # stop the parallelization if wasn't initialized
    if (!parallelization) {
        BiocParallel::bpstop()
    }

    # now for each group id align the EICs & mzmat
    db_execute(db, "DROP TABLE IF EXISTS eic")
    db_execute(db, "DROP TABLE IF EXISTS mzmat")
    tmp_db <- db_connect(tmp_db_file)
    for (i in seq(nrow(peakgroups))) {
        eics <- db_get_query(
            tmp_db,
            sprintf(
                "SELECT * FROM eic WHERE group_id == %s",
                peakgroups[i, "group_id"])
        )
        mzmat <- db_get_query(
            tmp_db,
            sprintf(
                "SELECT * FROM mzmat WHERE group_id == %s",
                peakgroups[i, "group_id"]
            )
        )

        rts <- eics[eics$sample == which.min(table(eics$sample)), "time"]
        # case of one sample has an rt more than the referent sample
        if (!all(table(eics$sample) == length(which(eics$sample == 1)))) {
            #### should happen only if user remove scans in middle of the files
            # get the sample with the less scan
            eics <- do.call(cbind, setNames(
                lapply(
                    split(eics[, c("time", "intensity")], eics$sample),
                    function(x)
                        sapply(rts, function(rt)
                            x[which.min(abs(x$time - rt)), "intensity"]
                        )
                ),
                sample_names)
            )
            mzmat <- do.call(cbind, setNames(
                lapply(split(mzmat[, c("time", "mz")], mzmat$sample),
                       function(x)
                           sapply(rts, function(rt)
                               x[which.min(abs(x$time - rt)), "mz"]
                           )
                   ),
                sample_names)
            )
        } else {
            eics <- do.call(
                cbind,
                setNames(split(eics$int, eics$sample), sample_names)
            )
            mzmat <- do.call(
                cbind,
                setNames(split(mzmat$mz, mzmat$sample), sample_names)
            )

        }
        db_write_table(
            db,
            "eic",
            cbind.data.frame(
                group_id = peakgroups[i, "group_id"],
                rt = rts,
                eics
            ),
            append = TRUE
        )

        db_write_table(
            db,
            "mzmat",
            cbind.data.frame(
                group_id = peakgroups[i, "group_id"],
                rt = rts,
                mzmat
            ),
            append = TRUE
        )
    }
    RSQLite::dbDisconnect(tmp_db)
}

#' @title Get EIC data
#'
#' @description
#' Get EIC recorded in the database for peaks with the same group ID
#'
#' @param db `SQLiteConnection`
#' @param group_id `numeric(1)` group ID
#'
#' @return `DataFrame` with a column rT & multiple columns, each of them
#' corresponding to a sample with the intensity
db_get_eic <- function(db, group_id) {
    eic <- db_get_query(db, sprintf(
        "SELECT * FROM eic WHERE group_id == %s",
        group_id
    ))[, -1]
    eic[is.na(eic)] <- 0
    eic
}

#' @title Get m/z matrix
#'
#' @description
#' Get m/z deviations recorded in the database for peaks with the same group ID
#'
#' @param db `SQLiteConnection`
#' @param group_id `numeric(1)` group ID
#'
#' @return `DataFrame` with a column rT & multiple columns, each of them
#' corresponding to a sample with the intensity
db_get_mzmat <- function(db, group_id) {
    db_get_query(db, sprintf(
        "SELECT * FROM mzmat WHERE group_id == %s",
        group_id
    ))[, -1]
}

#' @title Get Group ID of ion
#'
#' @description
#' Get group ID of an ion. If only the name is given it will return the group ID
#'  of the referent ion
#'
#' @param db `SQLiteConnection`
#' @param name `character(1)` compound name, optional
#' @param row_id `numeric(1)` row ID of the ann DataFrame
#'
#' @return `numeric(1)` group ID
db_get_group_id <- function(db, name = NULL, row_id = NULL) {
    query <- "SELECT basepeak_group_id FROM ann WHERE"
    if (!is.null(name)) {
        query <- paste(query, sprintf(
            "name == \"%s\" AND referent_adduct == adduct",
            name
        ), sep = " ")
    } else if (!is.null(row_id)) {
        query <- paste(query, sprintf("ROWID == %s", row_id), sep = " ")
    } else {
        return(NULL)
    }

    db_get_query(db, query)[1, 1]
}

#' @title Get all group IDs
#'
#' @description
#' Get all group IDs for a batch of spectra IDs
#'
#' @param db `SQLiteConnection`
#' @param spectra_ids `numeric` spectra IDs
#'
#' @return `numeric` group IDs
db_get_group_ids <- function(db, spectra_ids) {
    db_get_query(db, sprintf(
        "SELECT DISTINCT(group_id) FROM spectras WHERE spectra_id IN (%s) AND
            group_id IS NOT NULL",
        paste(spectra_ids, collapse = ", ")
    ))[, 1]
}

#' @title Get the highest IDs
#'
#' @description
#' Get the highest ID for each table in database
#'
#' @param db `SQLiteConnection`
#'
#' @return `numeric(5)` named vector with the highest ID :
#' \itemize{
#'    \item feature
#'    \item group
#'    \item spectra
#'    \item pcgroup
#'    \item cluster
#' }
db_get_max_ids <- function(db) {
    c(
        feature = db_get_query(db, "SELECT COUNT(*) from peaks")[1, 1],
        group = db_get_query(db, "SELECT COUNT(*) from peakgroups")[1, 1],
        spectra = db_get_query(
            db,
            "SELECT COUNT(*) FROM spectra_infos"
        )[1, 1],
        pcgroup = db_get_query(db, "SELECT MAX(pcgroup_id) FROM ann")[1, 1],
        cluster = db_get_query(
            db,
            "SELECT MAX(cluster_id) FROM peakgroups"
        )[1, 1]
    )
}

#' @title Duplicate EICs & m/z mat
#'
#' @description
#' Duplicate EICs & m/z matrix & assign them new group IDs
#'
#' @param db `SQLiteConnection`
#' @param samples `character` sample names (corresponding column names to copy)
#' @param old_group_ids `numeric` the old group IDs of the EICs & m/z mat to
#'  copy
#' @param new_group_ids `numeric` the group IDs to assign to the copied EICs &
#'  m/z mat
db_copy_eic_mzmat <- function(db, samples, old_group_ids, new_group_ids) {
    lapply(seq(length(old_group_ids)), function(i) {
        db_execute(db, sprintf(
            "INSERT INTO eic
                SELECT %s AS group_id, rt, %s
                FROM eic
                WHERE group_id == %s",
            new_group_ids[i],
            paste("`", samples, "`", sep = "", collapse = ", "),
            old_group_ids[i]
        ))
        db_execute(db, sprintf(
            "INSERT INTO mzmat
                SELECT %s AS group_id, rt, %s
                FROM mzmat
                WHERE group_id IN (%s)",
            new_group_ids[i],
            paste("`", samples, "`", sep = "", collapse = ", "),
            old_group_ids[i]
        ))
    })
}

#' @title Replace an annotation
#'
#' @description
#' Replace annotations for a compound
#'
#' @param db `SQLiteConnection`
#' @param cpd_name `character(1)` compound name associated to the annotation to
#'  replace
#' @param ann `DataFrame` each line correspond to a compound found with the
#'  columns:
#' \itemize{
#'     \item pcgroup_id `integer` group ID
#'     \item basepeak_group_id `integer` EIC ID
#'     \item class `character` cpd class
#'     \item name `character` name
#'     \item formula `character` chemical formula
#'     \item adduct `character` adduct form
#'     \item ion_formula `character` ion chemical formula
#'     \item rtdiff `numeric` retention time difference between the measured
#'      & the expected
#'     \item rt `numeric` retention time measured meanned accross the
#'      samples
#'     \item rtmin `numeric` born min of retention time measured accross the
#'      samples
#'     \item rtmax `numeric` born max of the retention time measured accross
#'      the samples
#'     \item nsamples `integer` number of samples where the compound was
#'      found
#'     \item best_score `numeric` best isotopic score seen
#'     \item best_deviation_mz `numeric` best m/z deviation seen
#'     \item best_npeak `integer` best number of isotopologues found
#'     \item ... `integer` a column for each sample which contain the
#'      spectra ID
#' }
#' @param spectras `DataFrame`, each line correspond to a peak annotated with
#'  its corresponding theoretical peak or the theoretical peak missed,
#'  with the columns :
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
#'  annotated, with the columns :
#' \itemize{
#'     \item spectra_id `integer` spectra ID
#'     \item score `numeric` isotopic score observed
#'     \item deviation_mz `numeric` m/z deviation observed
#'     \item npeak `integer` number of isotopologue annotated
#'     \item basepeak_mz `numeric` m/z of the basepeak annotated
#'     \item basepeak_int `numeric` area of the basepeak annotated
#'     \item sum_int `numeric` cumulative sum off all the area of the
#'      isotopologues annotated
#'     \item rt `numeric` retention time
#' }
#' @param peakgroups `DataFrame`, each line correspond to a group annotated
#'  by XCMS and CAMERA
#' \itemize{
#'     \item group_id `integer` group ID
#'     \item pcgroup_id `integer` pcgroup ID
#'     \item adduct `character` adduct annotation by CAMERA (NULL if absent)
#'     \item cluster_id `integer` cluster ID
#'     \item iso `character` could be "M" or "M+*"
#'     \item mzmed `float` m/z median computed by XCMS
#'     \item mzmin `float` m/z median born min (not the m/z born min !!)
#'     \item mzmax `float` m/z median born max (not the m/z born max !!)
#'     \item rtmed `float` rT median computed by XCMS
#'     \item rtmin `float` rT median born min (not the rT born min !!)
#'     \item rtmax `float` rT median born max (not the rT born max !!)
#'     \item npeaks `integer` number of peaks grouped accross samples
#'     \item ... `integer` a column for each sample which contain the
#'      feature ID (row ID from the peaktable)
#' }
#' @param peaks `DataFrame`, peaktable from XCMS
#' \itemize{
#'     \item mz `float` m/z
#'     \item mzmin `float` m/z born min
#'     \item mzmax `float` m/z born max
#'     \item rt `float` rT
#'     \item rtmin `float` rT born min
#'     \item rtmax `float` rT born max
#'     \item into `float` area of the peak
#'     \item intb `float` area of the peak above baseline
#'     \item maxo `float` maximum intensity
#'     \item sn `float` signal/noise
#'     \item egauss `float` ignore
#'     \item mu `float` ignore
#'     \item sigma `float` ignore
#'     \item h `float` ignore
#'     \item f `integer` ID of the ROI
#'     \item dppm `float` ppm deviation
#'     \item scale `integer` width of the wave used for the peak detection
#'     \item scpos `integer` scan ID
#'     \item scmin `integer` scan ID born min of the wave detection
#'     \item scmax `integer` scan ID born max of the wave detection
#'     \item lmin `integer` scan ID after extension of the scmin
#'     \item lmax `integer` scan ID after extension of the scmax
#' }
db_replace_ann <- function(db,
                           cpd_name,
                           ann,
                           spectra_infos,
                           spectras,
                           peaks,
                           peakgroups) {
    nsamples <- db_get_nsamples(db)
    invisible(lapply(seq(nrow(ann)), function(i)
        db_execute(db, sprintf(
            "UPDATE ann SET
                pcgroup_id = %s,
                basepeak_group_id = %s,
                rtdiff = %s,
                rt = %s,
                rtmin = %s,
                rtmax = %s,
                nsamples = %s,
                best_score = %s,
                best_deviation_mz = %s,
                best_npeak = %s,
                %s
            WHERE ROWID == %s;",
            ann[i, "pcgroup_id"],
            ann[i, "basepeak_group_id"],
            ann[i, "rtdiff"],
            ann[i, "rt"],
            ann[i, "rtmin"],
            ann[i, "rtmax"],
            ann[i, "nsamples"],
            ann[i, "best_score"],
            ann[i, "best_deviation_mz"],
            ann[i, "best_npeak"],
            paste(sprintf(
                "\"%s\" = %s",
                colnames(ann)[(ncol(ann) - nsamples + 1):ncol(ann)],
                ann[i, (ncol(ann) - nsamples + 1):ncol(ann)]
            ), collapse = ", "),
            ann[i, "rowid"]
        ))
    ))
    db_write_table(db, "spectra_infos", spectra_infos, append = TRUE)
    db_write_table(db, "spectras", spectras, append = TRUE)
    db_write_table(db, "peaks", peaks, append = TRUE)
    db_write_table(db, "peakgroups", peakgroups, append = TRUE)
}
