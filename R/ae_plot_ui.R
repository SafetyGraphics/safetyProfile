#' UI that facilitates the AE plot tab that includes AE plot and AE table outputs
#'
#' @param id module id
#'
#' @return AE plot tab UI
#'
#'
ae_plot_UI <- function(id) {
  ns <- NS(id)

  div(
    # h3("AE and CM Timelines"),
    plotOutput(ns("AEplot"),height="100%"),
    DTOutput(ns("AEtable"))
  )
}
