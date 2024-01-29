#' Safety Profile Module - UI
#'
#' @param id module id
#'
#' @return returns shiny module UI
#'
#' @importFrom htmltools HTML
#' @import shiny
#' @importFrom shinyjs useShinyjs
#'
#' @export

profile_ui <- function(id) {
  ns <- NS(id)
  #read css from package
  css_path <- system.file("www","index.css", package = "safetyProfile")
  app_css <-  htmltools::HTML(readLines(css_path))

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
      tabPanel("Overview", overview_ui(ns("overview"))),
      tabPanel("Event Timeline", ae_plot_UI(ns("ae_plot"))),
      tabPanel("Measures Over Time", lb_tbl_UI(ns("lb_tbl")))
    )
  )

  return(ui)
}
