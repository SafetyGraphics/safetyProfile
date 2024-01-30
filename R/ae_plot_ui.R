#' Event Timeline Module: UI
#'
#' @param id module id
#'
#' @return AE plot tab UI
#'
#' @import shiny
#'
#' @export

ae_plot_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    textOutput(ns("text1")),
    uiOutput(ns("AEplot")),
    DTOutput(ns("AEtable"))
  )
}
