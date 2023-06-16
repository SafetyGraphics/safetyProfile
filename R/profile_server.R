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


profile_server <- function(id, params, ptid = reactive({
  NULL
})) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    cat("starting server")

    ## set up some basic reactives for convenience
    id_col <- reactive({
      params()$settings$dm$id_col
    })

    ids <- reactive({
      req(params()$data$dm)
      unique(params()$data$dm[[id_col()]])
    })

    ## Update ID Select
    observe({
      updateSelectizeInput(
        session,
        inputId = "idSelect",
        choices = ids(),
        selected = ptid()
      )
    })

    current_id <- reactive({
      input$idSelect
    })

    #------------------------------------------
    # Make a simple Demographics summary
    demogData <- reactive({
      params()$data$dm %>% filter(!!sym(id_col()) == current_id())
    })

    demogHTML <- reactive({
      demogCols <- params()$settings$dm[grep("_col", names(params()$settings$dm))]
      names <- names(demogCols)
      vals <- list(demogCols %>% map_chr(~ as.character(demogData()[1, .x]))) %>% unlist()
      names(vals) <- names
      lis <- vals %>% imap(function(val, name) {
        tags$li(
          tags$small(name, class = "dlabel"),
          tags$strong(val, class = "dvalue")
        )
      })
      return(tags$ul(lis, class = "dlist"))
    })

    output$demogList <- renderUI({
      demogHTML()
    })

    #------------------------------------------

    ## Call  Modules
    ae_plot_server("ae_plot", params, current_id)
    safety_lineplot_server("safety_line_plot", params, current_id)
    OverviewServer("overview", params, current_id)
    react_server("react", params, current_id)

    return(current_id)
  })
}
