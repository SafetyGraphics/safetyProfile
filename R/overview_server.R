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

    # Populate list of domains in select
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
    })

    # Make a simple Demographics summary
    demogData <- reactive({
        params()$data$dm %>% filter(!!sym(id_col()) == id())
    })

    demogHTML <- reactive({
        names <- names(params()$settings$dm)
        vals <- list(params()$settings$dm %>% map_chr(~as.character(demogData()[1,.x]))) %>% unlist
        names(vals) <- names
        lis <- vals %>% imap(function(val,name){
            tags$li(
                tags$small(name,class='dlabel'),
                tags$strong(val,class="dvalue")
            )
        })
        return(tags$ul(lis, class="dlist"))
    })

    output$demogList <- renderUI({demogHTML()})

    # Render table for selected domain/ID 
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
