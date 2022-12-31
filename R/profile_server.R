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

    domain <- reactive({
        req(params()$data)
        names(params()$data)
    })

    domain_choice<-reactive({
        req(params()$data)
        req(input$domainSelect)

        params()$data[[input$domainSelect]]
    })

    aes_sub <-  reactive({
        req(params()$data$aes)
        params()$data$aes %>% select(params()$settings$dm$id_col,
                                     params()$settings$dm$siteid_col,
                                     params()$settings$aes$trarm_col,
                                     params()$settings$aes$stdy_col,
                                     params()$settings$aes$endy_col,
                                     params()$settings$aes$bodsys_col,
                                     params()$settings$aes$aeterm_col)
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
    observe({
        updateSelectizeInput(
            session,
            inputId = 'domainSelect',
            choices = domain()
        )
    })



    # TODO Make this dynamic for any domain provided (use a sub-module?)
    output$overview <- renderDT({domain_choice() %>% filter(!!sym(id_col()) == input$idSelect)})

    output$AEplot <- renderPlot({
        AEplot(
            data=params()$data$aes %>% filter(!!sym(id_col()) == input$idSelect),
            paramVar = !!sym(params()$settings$aes$aeterm_col),
            aeStartVar = !!sym(params()$settings$aes$stdy_col),
            aeEndVar = !!sym(params()$settings$aes$endy_col),
            colorVar = !!sym(params()$settings$aes$severity_col)
        )

    })
    output$AEtable <- renderDT({
        aes_sub() %>% filter(!!sym(id_col()) == input$idSelect)
    })

}
