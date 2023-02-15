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

ae_plot_server <-  function(id, params, current_id) {

  moduleServer(id, function(input, output, session){
  ns <- session$ns

  ## set up some basic reactives for convenience
  id_col<-reactive({
    params()$settings$dm$id_col
  })

  ## AE Stuff (to be moved to module)
  aes_sub <-  reactive({
    req(params()$data$aes)
    aes_dat <- params()$settings$aes
    params()$data$aes %>% select(
      aes_dat$id_col,
      aes_dat$siteid_col,
      aes_dat$trarm_col,
      aes_dat$stdy_col,
      aes_dat$endy_col,
      aes_dat$bodsys_col,
      aes_dat$term_col,
      aes_dat$severity_col
    )
  })

  output$AEplot <- renderPlot({
    if(!nrow(params()$data$aes %>% filter(!!sym(id_col()) == current_id())) == 0){
      AEplot(
        data=params()$data$aes %>% filter(!!sym(id_col()) == current_id()),
        paramVar = params()$settings$aes$term_col,
        aeStartVar = params()$settings$aes$stdy_col,
        aeEndVar = params()$settings$aes$endy_col,
        colorVar = params()$settings$aes$severity_col
      )}else{
        showNotification("There are no Adverse Events for this subject", type = "warning")
      }
  })

  output$AEtable <- renderDT({
    aes_sub() %>% filter(!!sym(id_col()) == current_id())
  })
  })
}
