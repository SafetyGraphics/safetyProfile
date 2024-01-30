#' Event Timeline Module: UI
#'
#' @param id `character` Module ID
#'
#' @return `list` Shiny tag list
#'
#' @importFrom DT DTOutput
#' @import shiny
#'
#' @export

ae_plot_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    textOutput(ns("text1")),
    uiOutput(ns("AEplot")),
    DT::DTOutput(ns("AEtable"))
  )
}
