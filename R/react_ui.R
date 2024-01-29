#' UI that facilitates the Safety reactable tab that includes safety reactable
#'
#' @param id module id
#'
#' @return LB reactable tab UI
#'
#' @importFrom shiny dataTableOutput NS
#'
#' @export

lb_tbl_UI <- function(id){
  ns <- shiny::NS(id)

  shiny::dataTableOutput(ns("lb_tbl"))
}
