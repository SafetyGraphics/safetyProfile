#' Safety Profile Module - UI
#'
#' @param id module id
#'
#' @return returns shiny module UI
#'
#' @import shiny
#' @importFrom plotly plotlyOutput
#' @importFrom DT DTOutput
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

        # TODO Make this dynamic for any domain provided (use a sub-module?)
        tabsetPanel(id=ns("tableWrap"), type = "tabs",
            tabPanel("Overview",
                     div(
                         selectizeInput(
                             ns("domainSelect"),
                             label="Select Data Domain",
                             choices=c()
                         ),
                         DTOutput(ns("overview")))
            ),
            tabPanel("AE Plot",
                 div(
                     h5(htmlOutput(ns("AE Plot"))),
                     plotOutput(ns("AEplot")),
                     DTOutput(ns("AEtable"))
                 ),
                )
            )
        )
    # )

    return(ui)
}

