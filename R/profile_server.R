#' Safety Profile Module - Server
#'
#' @param input module input
#' @param output module output
#' @param session module session
#' @param id Shiny module id
#' @param params parameters object with `data` and `settings` options. {reactive}
#' @param ptid ID to select when module is initialized {reactive}
#'
#' @return returns shiny module Server function
#'
#' @import shiny
#' @import ggplot2
#' @import dplyr
#' @importFrom DT renderDT
#' @importFrom reactable renderReactable
#' @importFrom purrr map
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @importFrom rlang sym !!
#' @export


profile_server <- function(id, params, ptid = reactive({NULL})) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    cat("starting server")

    ## ID select
    current_id <- id_server("id", params, ptid)

    ## Call  Modules
    OverviewServer("overview", params, current_id)
    ae_plot_server("ae_plot", params, current_id)
    safety_lineplot_server("safety_line_plot", params, current_id)
    react_server("react", params, current_id)

    return(current_id)
  })
}
