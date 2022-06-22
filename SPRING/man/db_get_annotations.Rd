% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db_func.R
\name{db_get_annotations}
\alias{db_get_annotations}
\title{Get Annotations}
\usage{
db_get_annotations(db, names = NULL, group_ids = NULL)
}
\arguments{
\item{db}{`SQLiteConnection`}

\item{names}{`character vector` the compound names, not mandatory}

\item{group_ids}{`numeric vector` the group IDs, not mandatory}
}
\value{
`DataFrame` each line correspond to a compound found
with the columns:
\itemize{
        \item group_id `integer` group ID
        \item class `character` cpd class
        \item name `character` name
        \item major_adduct `character` majoritary adduct for the compound
        \item formula `character` chemical formula
        \item adduct `character` adduct form
        \item ion_formula `character` ion chemical formula
        \item rtdiff `numeric` retention time difference between the measured
         & the expected
        \item rt `numeric` retention time measured meanned accross the
        samples
        \item rtmin `numeric` born min of retention time measured accross the
        samples
        \item rtmax `numeric` born max of the retention time measured accross
         the samples
        \item nsamples `integer` number of samples where the compound was
        found
        \item best_score `numeric` best isotopic score seen
        \item best_deviation_mz `numeric` best m/z deviation seen
        \item best_npeak `integer` best number of isotopologues found
        \item ... `integer` a column for each sample which contain the
        spectra ID
}
}
\description{
Get annotations from database
}