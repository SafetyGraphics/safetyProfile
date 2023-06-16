#' AE Plot Module - Server
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param params parameters object with `data` and `settings` options.
#' @param id Shiny module id
#' @param current_id current selected id
#'
#' @import safetyCharts
#' @return Reactive containing AE plot and listing
#'

ae_plot_server <- function(id, params, current_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## set up some basic reactives for convenience
    id_col <- reactive({
      params()$settings$dm$id_col
    })

    data <- reactive({
      req(params()$data)
      req(params()$settings)

      safetyCharts::stack_events(
        data = params()$data,
        settings = params()$settings
      ) %>%
      filter(id == current_id())
    })
    sub <- reactive({
      data() %>%
        filter(!(is.na(stdy) & is.na(endy))) %>%
        mutate(seq = row_number())
    })

    footnote <- reactive({
      dropped <- nrow(data()) - nrow(sub())
      ifelse(
        dropped > 0,
        paste("Dropped",dropped,"rows with missing start and end dates."),
        ""
      )
    })

    output$AEplot <- renderPlot(
      width = 600,
      height = function(){(nrow(sub()) * 10) +50},
      {
      if(!nrow(sub()) == 0) {
        AEplot(sub(), footnote())
      } else {
        showNotification("No events with valid dates for this subject", type = "warning")
      }
    })

    output$AEtable <- renderDT({
      data()
    })
  })
}
