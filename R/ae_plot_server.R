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

    ## AE Stuff (to be moved to module)
    # aes_sub <- reactive({
    #   req(params()$data$aes)
    #   aes_dat <- params()$settings$aes
    #   params()$data$aes %>% select(
    #     aes_dat$id_col,
    #     aes_dat$siteid_col,
    #     aes_dat$trarm_col,
    #     aes_dat$stdy_col,
    #     aes_dat$endy_col,
    #     aes_dat$bodsys_col,
    #     aes_dat$term_col,
    #     aes_dat$severity_col
    #   )
    # })

    combined <- reactive({
      req(params()$data$aes)
      req(params()$data$cm)

      combine_ae_cm(aes_data = params()$data$aes,
                    cm_data = params()$data$cm,
                    settings = params()$settings) # %>%
        # na.omit(ENDY)
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
# browser()
      }  else {
        showNotification("There are no Adverse Events for this subject", type = "warning")
      }
    })

    output$AEtable <- renderDT({
      combined() %>% filter(!!sym(id_col()) == current_id())
    })
  })
}
