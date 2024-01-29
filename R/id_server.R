#' ID Select Module - Server
#'
#' @param id Shiny module id
#' @param params parameters object with `data` and `settings` options. {reactive}
#' @param ptid ID to select when module is initialized {reactive}
#'
#' @return returns shiny module Server function
#'
#' @importFrom dplyr filter
#' @importFrom purrr imap map_chr
#' @importFrom rlang sym !!
#' @import shiny
#' @import shinyjs
#'
#' @export

id_server <- function(
    id,
    params,
    ptid = reactive({NULL}))
{
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

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
      updateSelectInput(
        session,
        inputId = "idSelect",
        choices = ids(),
        selected = ptid()
      )

      # Remove standard bootstrap classes from inline input in header
      shinyjs::removeClass(class="form-group", selector = ".profile-header div ")
      shinyjs::removeClass(class="shiny-input-container", selector = ".profile-header div")
      shinyjs::removeClass(class="form-control", selector = ".profile-header select")
      shinyjs::hide(selector = ".profile-header label")

    })

    current_id <- reactive({
      input$idSelect
    })

    #------------------------------------------
    # Make a simple Demographics summary
    demogData <- reactive({
      req(current_id())

      params()$data$dm %>%
          dplyr::filter(
            !!rlang::sym(id_col()) == current_id()
          )
    })

    demogHTML <- reactive({
      req(current_id())

      demogCols <- params()$settings$dm[grep("_col", names(params()$settings$dm))]
      names <- names(demogCols)
      vals <- list(demogCols %>% purrr::map_chr(~ as.character(demogData()[1, .x]))) %>% unlist()
      names(vals) <- names
      vals$id_col <- NULL
      divs <- vals %>% purrr::imap(function(val, name) {
        div(
          tags$small(name),
          val
        )
      })

      return(divs)
    })

    output$demogList <- renderUI({
      demogHTML()
    })

    return(current_id)
  })
}
