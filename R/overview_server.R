#' Data Domain Listing Module: Server
#'
#' @param id `character` Shiny module ID
#' @param params `list` Named list with `data` and `settings` (reactive).
#' @param current_id `character` Current participant ID (reactive).
#'
#' @importFrom dplyr filter
#' @import shiny
#'
#' @return `function` Module server
#'
#' @export

overview_server <- function(id, params, current_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      updateSelectizeInput(
        session,
        inputId = "domainSelect",
        choices = names(params()$data)
      )
    })

    domain_choice <- reactive({
      req(
        params()$data,
        input$domainSelect
      )

      params()$data[[
        input$domainSelect
      ]]
    })

    output$overview <- renderDT({
      domain_choice() %>%
          dplyr::filter(
              .data[[ params()$settings$dm$id_col ]] == current_id()
          )
    })
  })
}
