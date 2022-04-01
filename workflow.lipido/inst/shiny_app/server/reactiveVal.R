# `character(1) `contains the path to the database
sqlite_path <- shiny::reactiveVal(NULL)

# `SQLiteConnection`
db <- shiny::reactiveVal(NULL)

# `numeric vector` contains only peak group IDs where a conflict was detected
conflicts <- shiny::reactiveVal(c())

# `numeric(1)` contain the ID of the conflict to browse in the conflict tab
conflict_id <- shiny::reactiveVal(0)
