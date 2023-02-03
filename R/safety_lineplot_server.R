#' Safety Line plot Module - Server
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param params parameters object with `data` and `settings` options.
#' @param id Shiny output object
#'
#'
#' @return Reactive containing AE plot and listing
#'
safety_lineplot_server <-  function(input, output, session, params, id){
  ns <- session$ns

  ## set up some basic reactives for convenience
  id_col<-reactive({
    params()$settings$dm$id_col
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

  # TODO Make this dynamic for any domain provided (use a sub-module?)
  output$safety_lineplot <- renderPlot({
    if(!nrow(params()$data$labs %>% filter(!!sym(id_col()) == id())) == 0){
      safety_lineplot(
        data=params()$data$labs %>% filter(!!sym(id_col()) == id()),
        paramVar = params()$settings$labs$term_col,
        adyVar = params()$settings$labs$stdy_col,
        avalVar = params()$settings$labs$aval_col
      )}else{
        showNotification("There are no Laboratories for this subject", type = "warning")
      }

  })
  output$LBtable <- renderDT({
    labs_sub() %>% filter(!!sym(id_col()) == id())
  })

}
