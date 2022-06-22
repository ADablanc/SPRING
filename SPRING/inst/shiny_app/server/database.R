# shiny::updateSelectizeInput(
shinyWidgets::updatePickerInput(
  session = session,
  inputId = "database_name",
  label = "Database",
  choices = get_available_database()
)

#' @title Database table
#'
#' @description
#' Display the database in a datatable
output$database_table <- DT::renderDataTable({
  default_table <- matrix(, nrow = 0, ncol = 4, dimnames = list(
    c(), c("class", "name", "formula", "rt")
  ))
  params <- list(
    database = input$database_name
  )
  tryCatch({
    if (input$database_name == "") {
      custom_stop("invalid", "no database selected")
    }
    data <- utils::read.csv(system.file(
      "extdata",
      "database",
      paste(input$database_name, "csv", sep = "."),
      package = "SPRING"
    ))
    data$class <- as.factor(data$class)
    data
  }, invalid = function(i) {
    print("########## DATABASE_TABLE")
    print(params)
    print(i)
    default_table
  }, error = function(e) {
    print("########## DATABASE_TABLE")
    print(params)
    print(e)
    shinyWidgets::closeSweetAlert()
    sweet_alert_error(e$message)
    default_table
  })
}, server = isFALSE(getOption("shiny.testmode")),
  rownames = FALSE,
  selection = "none",
  filter = "top",
  extensions = c("Scroller", "FixedColumns"),
  options = list(
    dom = "frtip",
    paging = TRUE,
    scroller = TRUE,
    scrollY = "65vh",
    scrollX = TRUE,
    scrollCollapse = TRUE,
    fixedColumns = 1,
    columnDefs = list(
      list(
        className = "dt-head-center dt-center",
        targets = "_all",
        width = 80
      )
    ),
    language = list(
      emptyTable = "no lipids in database"
    ),
    initComplete = htmlwidgets::JS("
            function(settings, json) {
                settings.oInstance.api().columns.adjust();
            }
        ")
  )
)
