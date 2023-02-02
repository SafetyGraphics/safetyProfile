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


    labs_sub <-  reactive({
      req(params()$data$labs)
      labs_dat <- params()$settings$labs
      params()$data$labs %>% select(labs_dat$id_col,
                                   labs_dat$siteid_col,
                                   labs_dat$trarm_col,
                                   labs_dat$term_col,
                                   labs_dat$stdy_col,
                                   labs_dat$aval_col)
    })

    ## Update ID Select
    observe({
        updateSelectizeInput(
            session,
            inputId = 'idSelect',
            choices = ids()
        )
    })

    current_id <- reactive({
        input$idSelect
    })

    ## Overview Module
    callModule(OverviewServer, "overview", params, current_id)
    callModule(ae_plot_server, "ae_plot", params, current_id)

    # ## AE Stuff (to be moved to module)
    # aes_sub <-  reactive({
    #     req(params()$data$aes)
    #     aes_dat <- params()$settings$aes
    #     params()$data$aes %>% select(
    #         aes_dat$id_col,
    #         aes_dat$siteid_col,
    #         aes_dat$trarm_col,
    #         aes_dat$stdy_col,
    #         aes_dat$endy_col,
    #         aes_dat$bodsys_col,
    #         aes_dat$aeterm_col,
    #         aes_dat$severity_col
    #     )
    # })
    #
    # output$AEplot <- renderPlot({
    #     if(!nrow(params()$data$aes %>% filter(!!sym(id_col()) == input$idSelect)) == 0){
    #     AEplot(
    #         data=params()$data$aes %>% filter(!!sym(id_col()) == input$idSelect),
    #         paramVar = params()$settings$aes$aeterm_col,
    #         aeStartVar = params()$settings$aes$stdy_col,
    #         aeEndVar = params()$settings$aes$endy_col,
    #         colorVar = params()$settings$aes$severity_col
    #     )}else{
    #         showNotification("There are no Adverse Events for this subject", type = "warning")
    #     }
    # })
    #
    # output$AEtable <- renderDT({
    #     aes_sub() %>% filter(!!sym(id_col()) == input$idSelect)
    # })


    # TODO Make this dynamic for any domain provided (use a sub-module?)
    output$safety_lineplot <- renderPlot({
      if(!nrow(params()$data$labs %>% filter(!!sym(id_col()) == input$idSelect)) == 0){
        safety_lineplot(
          data=params()$data$labs %>% filter(!!sym(id_col()) == input$idSelect),
          paramVar = params()$settings$labs$term_col,
          adyVar = params()$settings$labs$stdy_col,
          avalVar = params()$settings$labs$aval_col
        )}else{
          showNotification("There are no Laboratories for this subject", type = "warning")
        }

    })
    output$LBtable <- renderDT({
      labs_sub() %>% filter(!!sym(id_col()) == input$idSelect)
    })
}

