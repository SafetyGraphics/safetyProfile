#' UI that facilitates the AE plot tab that includes AE plot and AE table outputs
#'
#' @param id module id
#'
#' @return AE plot tab UI
#'
#'
ae_plot_UI <- function(id) {
  ns <- NS(id)

  fluidPage(

    #h5(htmlOutput(ns("AE Plot"))),
    # plotlyOutput(ns("AEplot")),
    textOutput("text1"),
    uiOutput(ns("AEplot")),
    DTOutput(ns("AEtable"))
  )
}
