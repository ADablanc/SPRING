# `character(1) `contains the path to the database
sqlite_path <- shiny::reactiveVal(NULL)

# `SQLiteConnection`
db <- shiny::reactiveVal(NULL)

# `list` of two items :
# \itemize{
#     \item no_conflicts : `DataFrame` each line correspond to a compound found
#     with the columns:
#     \itemize{
#         \item group_id `integer` group ID
#         \item name `character` name
#         \item formula `character` chemical formula
#         \item adduct `character` adduct form
#         \item ion_formula `character` ion chemical formula
#         \item rtdiff `numeric` retention time difference between the measured
#          & the expected
#         \item rt `numeric` retention time measured meanned accross the
#         samples
#         \item rtmin `numeric` born min of retention time measured accross the
#         samples
#         \item rtmax `numeric` born max of the retention time measured accross
#          the samples
#         \item nsamples `integer` number of samples where the compound was
#         found
#         \item best_score `numeric` best isotopic score seen
#         \item best_deviation_mz `numeric` best m/z deviation seen
#         \item best_npeak `integer` best number of isotopologues found
#         \item ... `integer` a column for each sample which contain the
#         spectra ID
#     }
#     \item conflicts : `DataFrame list` each item correspond to the same group
#      of peaks where multiple annotations is possible. each dataframe has the
#     columns :
#     \itemize{
#         \item group_id `integer` group ID
#         \item name `character` name
#         \item formula `character` chemical formula
#         \item adduct `character` adduct form
#         \item ion_formula `character` ion chemical formula
#         \item rtdiff `numeric` retention time difference between the measured
#          & the expected
#         \item rt `numeric` retention time measured meanned accross the
#         samples
#         \item rtmin `numeric` born min of retention time measured accross the
#         samples
#         \item rtmax `numeric` born max of the retention time measured accross
#          the samples
#         \item nsamples `integer` number of samples where the compound was
#         found
#         \item best_score `numeric` best isotopic score seen
#         \item best_deviation_mz `numeric` best m/z deviation seen
#         \item best_npeak `integer` best number of isotopologues found
#         \item ... `integer` a column for each sample which contain the
#         spectra ID
#     }
# }
ann <- shiny::reactiveVal(list(conflicts = list(), no_conflicts = data.frame()))

# `DataFrame`, each line correspond to a spectra
# annotated, with the columns :
# \itemize{
#     \item spectra_id `integer` spectra ID
#     \item score `numeric` isotopic score observed
#     \item deviation_mz `numeric` m/z deviation observed
#     \item npeak `integer` number of isotopologue annotated
#     \item basepeak_int `numeric` area of the basepeak annotated
#     \item sum_int `numeric` cumulative sum off all the area of the
#     isotopologues annotated
#     \item rt `numeric` retention time
# }
spectra_infos <- shiny::reactiveVal(data.frame())

# `numeric(1)` contain the ID of the conflict to browse in the conflict tab
conflict_id <- shiny::reactiveVal(0)
