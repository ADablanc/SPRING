shinydashboard::tabItem(
    tabName = "ms_map",
    shinydashboard::box(
        title = "Filters",
        width = 12,
        shiny::tags$table(class = "table-filters",
            shiny::tags$tr(
                shiny::tags$td(
                    bsplus::shinyInput_label_embed(
                        tag = shinyWidgets::radioGroupButtons(
                            inputId = "ms_map_type",
                            label = "Type",
                            choices = c("MS map", "Kendrick plot"),
                            checkIcon = list(
                                yes = shiny::tags$i(
                                    class = "fa fa-check-square",
                                    style = "color: steelblue"
                                ),
                                no = shiny::tags$i(
                                    class = "fa fa-square-o",
                                    style = "color: steelblue"
                                )
                            )
                        ),
                        element = bsplus::bs_embed_tooltip(
                            tag = bsplus::shiny_iconlink(),
                            title = "Which type of plot ? (MS map = m/z fct(rT))
                            "
                        )
                    )
                ),
                shiny::tags$td(
                    bsplus::shinyInput_label_embed(
                        tag = shinyWidgets::radioGroupButtons(
                            inputId = "ms_map_annotation_filter",
                            label = "Annotation filter",
                            choices = c("all", "no annotated", "annotated"),
                            checkIcon = list(
                                yes = shiny::tags$i(
                                    class = "fa fa-check-square",
                                    style = "color: steelblue"
                                ),
                                no = shiny::tags$i(
                                    class = "fa fa-square-o",
                                    style = "color: steelblue"
                                )
                            )
                        ),
                        element = bsplus::bs_embed_tooltip(
                        tag = bsplus::shiny_iconlink(),
                        title = "Filter data according if they are annotated or
                            not ?"
                        )
                    )
                ),
                shiny::tags$td(
                    bsplus::shinyInput_label_embed(
                        tag = shiny::sliderInput(
                            inputId = "ms_map_int_threshold",
                            label = "Intensity threshold",
                            min = 0,
                            max = 0,
                            value = 0
                        ),
                        element = bsplus::bs_embed_tooltip(
                        tag = bsplus::shiny_iconlink(),
                        title = "Filter points according the maximum intensity
                            recorded"
                        )
                    )
                )
            )
        )
    ),

    shinydashboard::box(
        width = 12,
        shiny::column(width = 9,
            shinycssloaders::withSpinner(
                plotly::plotlyOutput(
                    "ms_map_plot",
                    height = "65vh"
                )
            )
        ),
        shiny::column(width = 3,
            shinycssloaders::withSpinner(
                plotly::plotlyOutput(
                    "ms_map_eic",
                    height = "65vh"
                )
            )
        )
    )
)
