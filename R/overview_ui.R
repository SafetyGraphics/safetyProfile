#' @title UI that facilitates the mapping of a single data element (column or field) with a simple select UI
#'
#' @param id unique id for the UI
#' @param label label associated with the control
#' @param choices a list of options for the control
#' @param default default value for the control
#'
#' @return returns the selected value wrapped in a \code{reactive()}.
#'
#' @import shiny
#' @importFrom DT DTOutput
#'
#' @export

overview_ui <- function(id) {
  ns <- NS(id)

  div(
    # h3("Demographics Summary"),
    # htmlOutput(ns("demogList")),
    h3("Participant Data Listings"),
    selectizeInput(
      ns("domainSelect"),
      label = "Select Data Domain",
      choices = c()
    ),
    DT::DTOutput(ns("overview"))
  )
}
