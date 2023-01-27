#' @title Server that facilitates the mapping of a single data element (column or field) with a simple select UI
#'
#' @param input Shiny input object
#' @param output  Shiny output object
#' @param session Shiny session object
#' 
#' @return A reactive containing the selected column
#'
#' @export

OverviewServer <- function(input, output, session, params, id){
    ns <- session$ns

    domains <- reactive({
        req(params()$data)
        names(params()$data)
        print(names(params()$data))
    })

    observe({
        updateSelectizeInput(
            session,
            inputId = 'domainSelect',
            choices = domains()
        )
        print("updated")
        print(ns('domainSelect'))
    })

    id_col<-reactive({
        params()$settings$dm$id_col
    })

    domain_choice<-reactive({
        req(params()$data)
        req(input$domainSelect)

        params()$data[[input$domainSelect]]
    })

    output$overview <- renderDT({domain_choice() %>% filter(!!sym(id_col()) == id())})

}
