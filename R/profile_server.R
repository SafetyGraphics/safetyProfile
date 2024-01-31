#' Safety Profile Module: Server
#'
#' @param id `character` Shiny module ID
#' @param params `list` Named list with `data` and `settings` (reactive).
#' @param ptid `character` Current participant ID (reactive).
#'
#' @return `function` Module server
#'
#' @import shiny
#'
#' @export

profile_server <- function(id, params, ptid = reactive({NULL})) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## ID select
    current_id <- id_server("id", params, ptid)

    ## Call  Modules
    overview_server("overview", params, current_id)
    ae_plot_server("ae_plot_ui", params, current_id)
    lb_tbl_server("lb_tbl_ui", params, current_id)

    return(current_id)
  })
}
