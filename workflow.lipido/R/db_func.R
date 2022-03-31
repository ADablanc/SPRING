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
    dbExecute(db, "PRAGMA temp_store = memory;")
    dbExecute(db, "PRAGMA synchronous = normal;")
    dbExecute(db, "PRAGMA locking_mode = normal;")
    dbExecute(db, "PRAGMA cache_size = 1000000;")
    dbExecute(db, "PRAGMA journal_mode = wal;")
    dbExecute(db, "PRAGMA auto_vacuum = FULL;")
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
dbWriteTable <- function(db,
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
dbExecute <- function(db, query, ...) {
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
dbGetQuery <- function(db, query, ...) {
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
dbReadTable <- function(db, table_name, ...) {
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
    dbWriteTable(
        db,
        "sample",
        data.frame(
            sample = sample_names,
            ms_file_positive = NA,
            ms_file_negative = NA,
            profile_positive = NA,
            profile_negative = NA,
            xset_positive = NA,
            xset_negative = NA
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
        # fst::compress_fst(
            serialize(obj, NULL)#,
            # compression = 100
        # )
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
        # fst::decompress_fst(
            obj[[1]]
        # )
    )
}

#' @title Record ms file
#'
#' @description
#' Record a `xcmsRaw` file and its corresponding profile matrix in the database
#' this two object are compress into a blob object before inserting in the
#' database.
#' These object are recorded in their corresponding "polarity" column (a sample
#' could contain a `xcmsRaw` in positive AND a `xcmsRaw` in negative !)
#' Before doing it, we must first use the function `db_record_samples` in order
#' to prepopulate the database with the sample name (which is the primary key)
#'
#' @param db `SQLiteConnection`
#' @param sample_name `character(1)` sample name (primary key)
#' @param polarity `character(1)` "positive" or "negative"
#' @param ms_file `xcmsRaw` object
#' @param bin_size `numeric(1)` bin size to use for the generation of the
#' profile matrix. cannot be 0 !!!!
db_record_ms_file <- function(db, sample_name, polarity, ms_file, bin_size) {
    suppressMessages(profile <- compress(
        xcms::profMat(ms_file, step = bin_size)
    ))
    ms_file <- compress(ms_file)
    query <- sprintf(
        "UPDATE sample
        SET
            ms_file_%s = :a,
            profile_%s = :b
        WHERE sample == \"%s\";",
        polarity,
        polarity,
        sample_name
    )
    dbExecute(db, query, params = list(a = ms_file, b = profile))
    rm(ms_file)
    rm(profile)
    gc()
}

#' @title Get ms file
#'
#' @description
#' Get the `xcmsRaw` of corresponding sample in the polarity desired
#'
#' @param db `SQLiteConnection`
#' @param sample_name `character(1)` sample name
#' @param polarity `character(1)` "positive" or "negative"
#'
#' @return an `xcmsRaw` object
db_read_ms_file <- function(db, sample_name, polarity) {
    query <- sprintf(
        "SELECT ms_file_%s
        FROM sample
        WHERE sample == \"%s\";",
        polarity,
        sample_name
    )
    ms_file <- dbGetQuery(db, query)[1, 1]
    if (is.null(ms_file)) {
        NULL
    } else if (is.na(ms_file[1])) {
        NULL
    } else {
        decompress(ms_file)
    }
}

#' @title Get profile matrix
#'
#' @description
#' Get the profile matrix of corresponding sample in the polarity desired
#'
#' @param db `SQLiteConnection`
#' @param sample_name `character(1)` sample name
#' @param polarity `character(1)` "positive" or "negative"
#'
#' @return a profile matrix
db_get_profile <- function(db, sample_name, polarity) {
    query <- sprintf(
        "SELECT profile_%s
        FROM sample
        WHERE sample == \"%s\";",
        polarity,
        sample_name
    )
    profile <- dbGetQuery(db, query)[1, 1]
    if (is.na(profile[1])) {
        NULL
    } else {
        decompress(profile)
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
#' the `xcmsRaw` object and its corresponding profile matrix are compress into a
#' `blob` object before inserting in the database.
#' These object are recorded in their corresponding "polarity" column (a sample
#' could contain a `xcmsRaw` in positive AND a `xcmsRaw` in negative !)
#' Before doing it, we must first use the function `db_record_samples` in order
#' to prepopulate the database with the sample name (which is the primary key)
#'
#' @param db `SQLiteConnection`
#' @param sample_name `character(1)` sample name (primary key)
#' @param raw_file `character(1)` filepath to the raw file to convert
#' @param converter `character(1)` filepath to the msconvert.exe
#' @param polarity `character(1)` "positive" or "negative"
#' @param filter_params `FilterParam` object
#' @param bin_size `numeric(1)` bin size to use for the generation of the
#' profile matrix. cannot be 0 !!!!
#'
#' @seealso `convert_file`, `record_ms_file`
import_ms_file <- function(db,
                           sample_name,
                           raw_file,
                           converter,
                           polarity,
                           filter_params,
                           bin_size = .1) {
    ms_file <- tryCatch({
            convert_file(raw_file, converter, polarity, filter_params)
        },
        error = function(e) {
            e$message
        }
    )
    if (class(ms_file) == "xcmsRaw") {
        ms_file <- filter_ms_file(ms_file, filter_params)
        db_record_ms_file(db, sample_name, polarity, ms_file, bin_size)
        "success"
    } else {
        ms_file
    }
}

#' @title Record `xcmsSet` in the database
#'
#' @description
#' Record an `xcmsSet` with the positive results and another with the negative
#' obtained by XCMS in the sample table
#' This record is only use to debugging the `annotate_peaks` function to see why
#' some peaks were not annotated
#'
#' @param db `SQLiteConnection`
#' @param xset_pos `xcmsSet` object with only the positive result, can be NULL
#' @param xset_neg `xcmsSet` object with only the negative result, can be NULL
#' @param sample_name `character(1)` sample name
db_record_xset <- function(db, xset_pos, xset_neg, sample_name) {
    if (!is.null(xset_pos)) {
        query <- sprintf(
            "UPDATE sample
            SET xset_positive = :a
            WHERE sample == \"%s\";",
            sample_name
        )
        dbExecute(db, query, params = list(a = compress(xset_pos)))
    }
    if (!is.null(xset_neg)) {
        query <- sprintf(
            "UPDATE sample
            SET xset_negative = :a
            WHERE sample == \"%s\";",
            sample_name
        )
        dbExecute(db, query, params = list(a = compress(xset_neg)))
    }
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
#'     \item basepeak_int `numeric` area of the basepeak annotated
#'     \item sum_int `numeric` cumulative sum off all the area of the
#'     isotopologues annotated
#'     \item rt `numeric` retention time
#' }
#' @param peaks `DataFrame` obtained from XCMS, it contains all peaks peak
#' picked, it contains the columns :
#' \itemize{
#'     \item feature_id `integer` feature ID
#'     \item mz `numeric` m/z
#'     \item mz_min `numeric` m/z born min
#'     \item mz_max `numeric` m/z born max
#'     \item rt `numeric` retention time
#'     \item rtmin `numeric` retention time born min
#'     \item rtmax `numeric` retention time born max
#'     \item int `numeric` area integrated
#'     \item intb `numeric` area integrated with baseline substracted
#'     \item maxo `numeric` maximum of intensity
#'     \item sn `numeric` signal / noise
#'     \item egauss `numeric` ignore
#'     \item mu `numeric` ignore
#'     \item sigma `numeric` ignore,
#'     \item h `numeric` ignore
#'     \item f `integer` ID of the ROI in the ROI list constructed by XCMS,
#'     no use
#'     \item dppm `numeric` m/z deviation of the peak in ppm
#'     \item scale `integer` centwave scale used for the integration
#'     (in scans & not in sec !!!)
#'     \item scpos `integer` scan position
#'     \item scmin `integer` scan born min before the optimization of the area
#'     integrated
#'     \item scmax `integer` scan born max before the optimization of the area
#'     integrated
#'     \item lmin `integer` scan born min after the optimization of the area
#'     integrated
#'     \item lmax `integer` scan born max after the optimization of the area
#'     integrated
#'     \item sample `character` sample name
#'     \item polarity `character` polarity
#' }
#' @param peak_groups `DataFrame`, each line correspond to a group from XCMS,
#' the columns are :
#' \itemize{
#'     \item group_id `integer` group ID
#'     \item polarity `character` polarity
#'     \item mzmed `numeric` m/z median
#'     \item mzmin `numeric` m/z born min
#'     (not the mzmin column from the peaklist !)
#'     \item mzmax `numeric` m/z born max
#'     (not the mzmax column from the peaklist !)
#'     \item rtmed `numeric` rt median
#'     \item rtmin `numeric` rt born min
#'     (not the rtmin column from the peaklist !)
#'     \item rtmax `numeric` rt born max
#'     (not the rtmax column from the peaklist !)
#'     \item npeaks `integer` number of peaks grouped
#'     \item ... `integer` a column for each sample which contain the feature ID
#' }
db_record_ann <- function(db,
                          ann,
                          spectras,
                          spectra_infos,
                          peaks,
                          peak_groups) {
    dbWriteTable(db, "ann", ann, overwrite = TRUE)
    dbWriteTable(db, "spectras", spectras, overwrite = TRUE)
    dbWriteTable(db, "spectra_infos", spectra_infos, overwrite = TRUE)
    dbWriteTable(db, "peaks", peaks, overwrite = TRUE)
    dbWriteTable(db, "peak_groups", peak_groups, overwrite = TRUE)
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
#' @param ann_params `AnnParam`
db_record_params <- function(db,
                             filter_params,
                             cwt_params,
                             obw_params,
                             pd_params,
                             ann_params) {
    filter_params <- params_to_dataframe(filter_params)
    cwt_params <- params_to_dataframe(cwt_params)
    obw_params <- params_to_dataframe(obw_params)
    pd_params <- params_to_dataframe(pd_params)
    ann_params <- params_to_dataframe(ann_params)
    dbWriteTable(db, "filter_params", filter_params, overwrite = TRUE)
    dbWriteTable(db, "cwt_params", cwt_params, overwrite = TRUE)
    dbWriteTable(db, "obw_params", obw_params, overwrite = TRUE)
    dbWriteTable(db, "pd_params", pd_params, overwrite = TRUE)
    dbWriteTable(db, "ann_params", ann_params, overwrite = TRUE)
}

#' @title Get Annotations
#'
#' @description
#' Get annotations from database
#'
#' @param db `SQLiteConnection`
#' @param names `character vector` the compound names
#'
#' @return `DataFrame` each line correspond to a compound found
#' with the columns:
#' \itemize{
#'     \item group_id `integer` group ID
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
db_get_annotations <- function(db, names = NULL) {
    if (is.null(names)) {
        dbReadTable(db, "ann")
    } else {
        dbGetQuery(db, sprintf(
            "SELECT *
            FROM ann
            WHERE name IN (%s)",
            paste("\"", names, "\"", sep = "", collapse = ", ")
        ))
    }
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
#'     \item basepeak_int `numeric` area of the basepeak annotated
#'     \item sum_int `numeric` cumulative sum off all the area of the
#'     isotopologues annotated
#'     \item rt `numeric` retention time
#' }
db_get_spectra_infos <- function(db, spectra_ids = NULL) {
    if (is.null(spectra_ids)) {
        dbReadTable(db, "spectra_infos")
    } else {
        dbGetQuery(db, sprintf(
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
db_get_spectras <- function(db, spectra_ids = NULL) {
    if (is.null(spectra_ids)) {
        dbReadTable(db, "spectras")
    } else {
        dbGetQuery(
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
#'     \item ann `DataFrame` of one line with columns :
#'     \itemize{
#'         \item da_tol `numeric` m/z tolerance in Dalton
#'         \item rt_tol `numeric` rT tolerance in sec
#'         \item abd_tol `numeric` relative abundance tolerance, each peak which
#'         have an higher difference of relative abundance with its
#'         corresponding theoretical peak will be discarded
#'         \item adduct_names `character` adduct names from the enviPat package
#'         collapsed with the character ";"
#'         \item instrument `character` instrument names from the enviPat
#'         package
#'     }
#' }
db_get_params <- function(db) {
    list(
        filter = dbReadTable(db, "filter_params")[1, ],
        cwt = dbReadTable(db, "cwt_params")[1, ],
        obw = dbReadTable(db, "obw_params")[1, ],
        pd = dbReadTable(db, "pd_params")[1, ],
        ann = dbReadTable(db, "ann_params")[1, ]
    )
}

#' @title Get peaks
#'
#' @description
#' Get peaks from database
#'
#' @param db `SQLiteConnection`
#' @param feature_ids `integer vector` feature IDs
#'
#' @return `DataFrame` obtained from XCMS, it contains all peaks peak
#' picked, it contains the columns :
#' \itemize{
#'     \item feature_id `integer` feature ID
#'     \item mz `numeric` m/z
#'     \item mz_min `numeric` m/z born min
#'     \item mz_max `numeric` m/z born max
#'     \item rt `numeric` retention time
#'     \item rtmin `numeric` retention time born min
#'     \item rtmax `numeric` retention time born max
#'     \item int `numeric` area integrated
#'     \item intb `numeric` area integrated with baseline substracted
#'     \item maxo `numeric` maximum of intensity
#'     \item sn `numeric` signal / noise
#'     \item egauss `numeric` ignore
#'     \item mu `numeric` ignore
#'     \item sigma `numeric` ignore,
#'     \item h `numeric` ignore
#'     \item f `integer` ID of the ROI in the ROI list constructed by XCMS,
#'     no use
#'     \item dppm `numeric` m/z deviation of the peak in ppm
#'     \item scale `integer` centwave scale used for the integration
#'     (in scans & not in sec !!!)
#'     \item scpos `integer` scan position
#'     \item scmin `integer` scan born min before the optimization of the area
#'     integrated
#'     \item scmax `integer` scan born max before the optimization of the area
#'     integrated
#'     \item lmin `integer` scan born min after the optimization of the area
#'     integrated
#'     \item lmax `integer` scan born max after the optimization of the area
#'     integrated
#'     \item sample `character` sample name
#'     \item polarity `character` polarity
#' }
db_get_peaks <- function(db, feature_ids = NULL) {
    if (is.null(feature_ids)) {
        dbReadTable(db, "peaks")
    } else {
        query <- sprintf(
            "SELECT *
                FROM peaks
                WHERE feature_id IN (%s);",
            paste(feature_ids, collapse = ", ")
        )
        dbGetQuery(db, query)
    }
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
    dbExecute(db, sprintf(
        "DELETE FROM ann
            WHERE group_id == %s
                AND name != \"%s\";",
        group_id,
        name
    ))
}
