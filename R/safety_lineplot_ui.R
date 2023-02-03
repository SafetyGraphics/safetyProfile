#' UI that facilitates the Safety line plot tab that includes safety line plot and corresponding table outputs
#'
#' @param id module id
#'
#' @return LB plot tab UI
#'
#'
safety_lineplot_UI <- function(id){
  ns <- NS(id)

  div(
       h5(htmlOutput(ns("LB Plot"))),
       plotOutput(ns("safety_lineplot")),
       DTOutput(ns("LBtable"))
  )
}
