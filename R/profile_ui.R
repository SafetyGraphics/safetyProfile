#' Safety Profile Module - UI
#'
#' @param id module id
#'
#' @return returns shiny module UI
#'
#' @import shiny
#' @importFrom plotly plotlyOutput
#' @importFrom DT DTOutput
#'
#' @export
#'

profile_ui <- function(id) {
    ns <- NS(id)
    
    ## bring components together as complete ui
    ui <- fluidPage(
        selectizeInput(
            ns("idSelect"),
            label="Select Participant",
            choices=c()       
        ),  
        # TODO Make this dynamic for any domain provided (use a sub-module?)
        tabsetPanel(id=ns("tableWrap"), type = "tabs",
            tabPanel("AEs", 
                div(
                    h5("AE"),
                    DTOutput(ns("aeListing")))
                ),
            tabPanel("Labs", 
                div(
                    h5(htmlOutput(ns("Lab Listing"))),
                    DTOutput(ns("labListing"))
                ),
            ),
            tabPanel("DM", 
                div(
                    h5(htmlOutput(ns("DM Listing"))),
                    DTOutput(ns("dmListing"))
                ),
            )
        )  
    )
    
    return(ui)
}

