#' Safety Reactable Module - Server
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param params parameters object with `data` and `settings` options.
#' @param id Shiny module id
#' @param current_id current selected id
#'
#'
#' @return Reactive containing AE reactable
#'

react_server <-  function(id, params, current_id) {

  moduleServer(id, function(input, output, session){
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
                                    labs_dat$aval_col,
                                    labs_dat$lo_col,
                                    labs_dat$hi_col,
                                    labs_dat$base_col,
                                    labs_dat$vis_col)
    })

    # TODO Make this dynamic for any domain provided (use a sub-module?)
    output$react <- renderReactable({
      if(!nrow(params()$data$labs %>% filter(!!sym(id_col()) == current_id())) == 0){
        lb_react(
          data=params()$data$labs %>% filter(!!sym(id_col()) == current_id()),
          paramVar = params()$settings$labs$term_col,
          visVar = params()$settings$labs$vis_col,
          adyVar = params()$settings$labs$stdy_col,
          baseVar = params()$settings$labs$base_col,
          avalVar = params()$settings$labs$aval_col,
          lowVar = params()$settings$labs$lo_col,
          highVar = params()$settings$labs$hi_col

        )}else{
          showNotification("There are no Laboratories for this subject", type = "warning")
        }

    })
    # output$LBtable <- renderDT({
    #   labs_sub() %>% filter(!!sym(id_col()) == current_id())
    # })

  })

}
