#' Lab Table Module: UI
#'
#' @param id `character` Module ID
#'
#' @return `list` Shiny tag list
#'
#' @importFrom DT DTOutput
#' @import shiny
#'
#' @export

lb_tbl_ui <- function(id){
  ns <- NS(id)

  DT::DTOutput(ns("lb_tbl"))
}
