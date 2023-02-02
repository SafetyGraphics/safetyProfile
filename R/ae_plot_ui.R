ae_plot_UI <- function(id){
  ns <- NS(id)

  div(
    h5(htmlOutput(ns("AE Plot"))),
    plotOutput(ns("AEplot")),
    DTOutput(ns("AEtable"))
  )
}
