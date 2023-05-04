#' AE Plot Module - Server
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param params parameters object with `data` and `settings` options.
#' @param id Shiny module id
#' @param current_id current selected id
#'
#'
#' @return Reactive containing AE plot and listing
#'

ae_plot_server <- function(id, params, current_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## set up some basic reactives for convenience
    id_col <- reactive({
      params()$settings$dm$id_col
    })

    combined <- reactive({
      req(params()$data$aes)
      req(params()$data$cm)

      combine_domains(aes_data = params()$data$aes,
                    cm_data = params()$data$cm,
                    settings = params()$settings)
    })

    observe({
      print(combined() %>% filter(!!sym(id_col()) == current_id()))

    })

    output$AEplot <- renderPlot({

        if (!nrow(combined() %>% filter(!!sym(id_col()) == current_id())) == 0) {
          AEplot(
            dataCombined = combined() %>% filter(!!sym(id_col()) == current_id()),
            eventVar = EVENT,
            startDayVar = STDY,
            endDayVar = ENDY,
            colorVar = DOMAIN
          )
      }  else {
        showNotification("There are no Adverse Events for this subject", type = "warning")
      }
    })

    output$AEtable <- renderDT({
      combined() %>% filter(!!sym(id_col()) == current_id())
    })
  })
}
