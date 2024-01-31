#' Data Domain Listing Module: UI
#'
#' @param id `character` Module ID
#'
#' @return `list` Shiny tag list
#'
#' @import shiny
#' @importFrom DT DTOutput
#'
#' @export

overview_ui <- function(id) {
  ns <- NS(id)

  div(
    h3("Participant Data Listings"),
    selectizeInput(
      ns("domainSelect"),
      label = "Select Data Domain",
      choices = c()
    ),
    DT::DTOutput(ns("overview"))
  )
}
