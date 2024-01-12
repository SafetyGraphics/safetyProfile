#' UI that facilitates the Safety reactable tab that includes safety reactable
#'
#' @param id module id
#'
#' @return LB reactable tab UI
#'
#'
#'
lb_tbl_UI <- function(id){
  ns <- NS(id)

  # div(
    # reactableOutput(ns("react"))
    DT::dataTableOutput(ns("lb_tbl"))
  # )
}
