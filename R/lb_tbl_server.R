#' Lab Table Module: Server
#'
#' @param id `character` Shiny module ID
#' @param params `list` Named list with `data` and `settings` (reactive).
#' @param current_id `character` Current participant ID (reactive).
#'
#' @return Reactive containing AE reactable
#'
#' @importFrom DT renderDT
#' @importFrom dplyr filter select
#' @import shiny
#'
#' @export

lb_tbl_server <-  function(id, params, current_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    labs_sub <- reactive({
      req(params()$data$labs)

      params()$data$labs %>%
        dplyr::select(
          params()$settings$labs$site_col,
          params()$settings$labs$measure_col,
          params()$settings$labs$studyday_col,
          params()$settings$labs$value_col,
          params()$settings$labs$normal_col_low,
          params()$settings$labs$normal_col_high,
          params()$settings$labs$visit_col
        )
    })

    # TODO: Make this dynamic for any domain provided (use a sub-module?)
    output$lb_tbl <- DT::renderDT({
      req(current_id())

      data <- params()$data$labs %>%
        dplyr::filter(
          .data[[ params()$settings$dm$id_col ]] == current_id()
        )

      if (nrow(data) == 0) {
        showNotification(
            glue::glue(
                "No lab data found for subject {current_id()}."
            ),
            type = "warning"
        )

        return(NULL)
      }

      table <- data %>%
        lb_tbl(
          measureVar = params()$settings$labs$measure_col,
          visitVar = params()$settings$labs$visit_col,
          studyDayVar = params()$settings$labs$studyday_col,
          resultVar = params()$settings$labs$value_col,
          llnVar = params()$settings$labs$normal_col_low,
          ulnVar = params()$settings$labs$normal_col_high
        )

      return(table)
    })
  })
}
