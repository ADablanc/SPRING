output$database_table <- DT::renderDataTable(utils::read.csv(
  system.file("extdata", "database.csv", package = "workflow.lipido")),
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
            function(settings, json){
                settings.oInstance.api().columns.adjust();
            }
        ")
  )
)
