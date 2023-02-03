#' Safety Profile Module - Server
#'
#' @param input module input
#' @param output module output
#' @param session module session
#' @param params parameters object with `data` and `settings` options.
#'
#' @return returns shiny module Server function
#'
#' @import shiny
#' @import ggplot2
#' @import dplyr
#' @importFrom DT renderDT
#' @importFrom purrr map
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @importFrom rlang sym !!
#' @export

# profile_server <- function(input, output, session, params) {

profile_server <-  function(id, params) {

    moduleServer(id, function(input, output, session){
    ns <- session$ns
    cat('starting server')

    ## set up some basic reactives for convenience
    id_col<-reactive({
        params()$settings$dm$id_col
    })

    ids <- reactive({
        req(params()$data$dm)
        unique(params()$data$dm[[id_col()]])
    })

    ## Update ID Select
    observe({
        updateSelectizeInput(
            session,
            inputId = 'idSelect',
            choices = ids()
        )
    })

    current_id <- reactive({
        input$idSelect
    })

    ## Overview Module
    # callModule(OverviewServer, "overview", params, current_id)
    # callModule(ae_plot_server, "ae_plot", params, current_id)
    ae_plot_server("ae_plot", params, current_id)
    safety_lineplot_server("safety_line_plot", params, current_id)
    OverviewServer("overview", params, current_id)
    # callModule(safety_lineplot_server, "safety_line_plot", params, current_id)
})

}

