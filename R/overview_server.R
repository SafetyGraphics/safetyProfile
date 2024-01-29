#' @title Server that facilitates the mapping of a single data element (column or field) with a simple select UI
#'
#' @param input Shiny input object
#' @param output  Shiny output object
#' @param session Shiny session object
#' @param params parameters object with `data` and `settings` options.
#' @param id Shiny module id
#' @param current_id current selected id
#'
#' @import purrr
#'
#' @return A reactive containing the selected column
#'
#' @export

overview_server <- function(id, params, current_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Populate list of domains in select
    domains <- reactive({
      req(params()$data)
      names(params()$data)
    })

    observe({
      updateSelectizeInput(
        session,
        inputId = "domainSelect",
        choices = domains()
      )
    })


    # Render table for selected domain/ID
    id_col <- reactive({
      params()$settings$dm$id_col
    })

    domain_choice <- reactive({
      req(params()$data)
      req(input$domainSelect)

      params()$data[[input$domainSelect]]
    })
    output$overview <- renderDT({
      domain_choice() %>% filter(!!sym(id_col()) == current_id())
    })
  })
}
