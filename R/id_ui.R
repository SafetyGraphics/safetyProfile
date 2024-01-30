#' ID Select Module: UI
#'
#' @param id module id
#'
#' @return returns shiny module UI
#'
#' @import shiny
#'
#' @export
#'

id_ui <- function(id) {
  ns <- NS(id)

  ## bring components together as complete ui
  ui <- div(
    class="profile-header chart-header",
    div(
        tags$small("ID"),
        selectInput(
          ns("idSelect"),
          label='',
          selectize=FALSE,
          choices = c()
        )
    ),
    htmlOutput(ns("demogList"))
  )
  return(ui)
}
