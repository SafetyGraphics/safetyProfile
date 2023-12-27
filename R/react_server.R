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

lb_tbl_server <-  function(id, params, current_id) {

  moduleServer(id, function(input, output, session){
    ns <- session$ns

    ## set up some basic reactives for convenience
    id_col<-reactive({
      params()$settings$dm$id_col
    })

    labs_sub <- reactive({
      req(params()$data$labs)
      params()$data$labs %>% select(
        params()$settings$labs$site_col,
        params()$settings$labs$measure_col,
        params()$settings$labs$studyday_col,
        params()$settings$labs$value_col,
        params()$settings$labs$normal_col_low,
        params()$settings$labs$normal_col_high,
        params()$settings$labs$visit_col
      )
    })

    # TODO Make this dynamic for any domain provided (use a sub-module?)
    # output$react <- renderReactable({
    output$lb_tbl <- DT::renderDataTable({
      if(!nrow(params()$data$labs %>% filter(!!sym(id_col()) == current_id())) == 0){
        lb_tbl(
          data=params()$data$labs %>% filter(!!sym(id_col()) == current_id()),
          paramVar = params()$settings$labs$measure_col,
          visVar = params()$settings$labs$visit_col,
          adyVar = params()$settings$labs$studyday_col,
          avalVar = params()$settings$labs$value_col,
          lowVar = params()$settings$labs$normal_col_low,
          highVar = params()$settings$labs$normal_col_high
        )}else{
          showNotification("There are no Laboratories for this subject", type = "warning")
        }
    })
    # output$LBtable <- renderDT({
    #   labs_sub() %>% filter(!!sym(id_col()) == current_id())
    # })

  })

}
