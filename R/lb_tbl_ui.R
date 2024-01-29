#' UI that facilitates the Safety reactable tab that includes safety reactable
#'
#' @param id module id
#'
#' @return LB reactable tab UI
#'
#' @importFrom DT DTOutput
#' @importFrom shiny NS
#'
#' @export

lb_tbl_ui <- function(id){
  ns <- shiny::NS(id)

  DT::DTOutput(ns("lb_tbl"))
}
