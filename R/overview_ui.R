#' @title UI that facilitates the mapping of a single data element (column or field) with a simple select UI
#' 
#' @param id unique id for the UI
#' @param label label associated with the control  
#' @param choices a list of options for the control
#' @param default default value for the control
#'
#' @return returns the selected value wrapped in a \code{reactive()}.
#'
#' @export

OverviewUI <- function(id, label){  
    ns <- NS(id)
    div(
        selectizeInput(
            ns("domainSelect"),
            label="Select Data Domain",
            choices=c()
        ),
        DTOutput(ns("overview"))
    )
}

