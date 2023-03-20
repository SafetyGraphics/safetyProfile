#' Safety Profile Module - UI
#'
#' @param id module id
#'
#' @return returns shiny module UI
#'
#' @import shiny
#' @importFrom plotly plotlyOutput
#' @importFrom DT DTOutput
#' @importFrom reactable reactableOutput
#'
#' @export
#'

profile_ui <- function(id) {
    ns <- NS(id)

    ## bring components together as complete ui
    ui <- fluidPage(
        selectizeInput(
            ns("idSelect"),
            label="Select Participant",
            choices=c()
        ),

        # Call Modules
        tabsetPanel(id=ns("tableWrap"), type = "tabs",
            tabPanel("Overview",OverviewUI(ns("overview"))),
            tabPanel("AE Plot", ae_plot_UI(ns("ae_plot"))),
            tabPanel("LB Plot", safety_lineplot_UI(ns("safety_line_plot"))),
            tabPanel("LB Reactable", react_UI(ns("react")))
        )
    )

    return(ui)
}

