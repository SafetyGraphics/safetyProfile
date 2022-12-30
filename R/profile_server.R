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
                                     SITEID,
                                     TRTA,
                                     AENDT,
                                     AENDY,
                                     params()$settings$bodsys_col,
                                     AETERM)
    })

    observe({
        print(aes_sub())
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



    # domainTable <- observeEvent({
    #    .data %>% filter(!!sym(id_col()) == input$idSelect)
    # })

    ## Show data for the selected
    # params()$data %>% map(domainTable())
    # params()$data %>% map(., filter(!!sym(id_col()) == input$idSelect))
    # TODO Make this dynamic for any domain provided (use a sub-module?)
    output$overview <- renderDT({domain_choice() %>% filter(!!sym(id_col()) == input$idSelect)})

    # output$aeListing <- renderDT({params()$data$aes %>% filter(!!sym(id_col()) == input$idSelect)})
    # output$labListing <- renderDT({params()$data$labs %>% filter(!!sym(id_col()) == input$idSelect)})
    # output$dmListing <- renderDT({params()$data$dm %>% filter(!!sym(id_col()) == input$idSelect)})

    output$AEplot <- renderPlot({
        AEplot(
            data=params()$data$aes %>% filter(!!sym(id_col()) == input$idSelect)
        )
    })
    output$AEtable <- renderDT({
            # params()$data$aes %>% filter(!!sym(id_col()) == input$idSelect)
        aes_sub() %>% filter(!!sym(id_col()) == input$idSelect)
    })

}
