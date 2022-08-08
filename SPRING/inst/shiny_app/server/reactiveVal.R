# `character(1) `contains the path to the database
sqlite_path <- shiny::reactiveVal(NULL)

# `SQLiteConnection`
db <- shiny::reactiveVal(NULL)

# `numeric vector` contains only group IDs where a conflict was detected
conflicts <- shiny::reactiveVal(c())

# `numeric(1)` contain the ID of the conflict to browse in the conflict tab
conflict_id <- shiny::reactiveVal(0)

# `numeric(1)` only here to force reactualization of specific graph after update
    # or actualization of data in database
actualize <- shiny::reactiveValues(
    peak_spot = NULL
)
