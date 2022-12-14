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
#' @importFrom DT renderDT
#' @importFrom purrr map
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @importFrom rlang sym !!
#' @export

profile_server <- function(input, output, session, params) {
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
            #selected = current
        )
    })


    ## Show data for the selected
    # TODO Make this dynamic for any domain provided (use a sub-module?)
    output$aeListing <- renderDT({params()$data$aes %>% filter(!!sym(id_col()) == input$idSelect)})
    output$labListing <- renderDT({params()$data$labs %>% filter(!!sym(id_col()) == input$idSelect)})
    output$dmListing <- renderDT({params()$data$dm %>% filter(!!sym(id_col()) == input$idSelect)})
}
