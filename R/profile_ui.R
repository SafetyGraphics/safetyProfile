#' Safety Profile Module - UI
#'
#' @param id module id
#'
#' @return returns shiny module UI
#'
#' @import shiny
#' @import shinyjs
#' @importFrom plotly plotlyOutput
#' @importFrom DT DTOutput
#' @importFrom reactable reactableOutput
#'
#' @export
#'

profile_ui <- function(id) {
  ns <- NS(id)
  #read css from package
  css_path <- system.file("www","index.css", package = "safetyProfile")
  app_css <-  HTML(readLines(css_path))

  ## bring components together as complete ui
  ui <- tagList(
    shinyjs::useShinyjs(),
    tags$head(
      tags$style(app_css)    
    ),

    # ID select
    id_ui(id=ns('id')),

    # Call Modules
    tabsetPanel(id=ns("tableWrap"), type = "tabs",
      tabPanel("Overview",OverviewUI(ns("overview"))),
      tabPanel("AE Plot", ae_plot_UI(ns("ae_plot"))),
      tabPanel("LB Plot", safety_lineplot_UI(ns("safety_line_plot"))),
      tabPanel("LB Reactable", react_UI(ns("react")))
    )
  )
  
  return(ui)
}
