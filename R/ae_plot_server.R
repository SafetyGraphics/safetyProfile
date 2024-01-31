#' Event Timeline Module: Server
#'
#' @param id `character` Module ID
#' @param params `list` Named list with `data` and `settings` (reactive)
#' @param current_id `character` Current participant ID (reactive)
#'
#' @return `function` Module server
#'
#' @importFrom dplyr filter mutate rename select
#' @importFrom DT renderDT
#' @importFrom safetyCharts stack_events
#' @import shiny
#' @importFrom stringr str_replace_all
#'
#' @export

ae_plot_server <- function(id, params, current_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## set up some basic reactives for convenience
    id_col <- reactive({
      params()$settings$dm$id_col
    })

    data <- reactive({
      req(
        params()$data,
        params()$settings,
        current_id()
      )

      params()$data %>%
        safetyCharts::stack_events(
          settings = params()$settings
        ) %>%
        dplyr::filter(
            .data$id == current_id()
        )
    })

    ae_table_dat <- reactive({
      req(data())

      data() %>%
        dplyr::mutate(
          seq = row_number()
        )
    })

    sub <- reactive({
      req(data())

      data() %>%
        dplyr::filter(
          !is.na(.data$stdy),
          !is.na(.data$endy)
        ) %>%
        dplyr::mutate(
          seq = row_number()
        )
    })

    footnote <- reactive({
      dropped <- nrow(data()) - nrow(sub())
      ifelse(
        dropped > 0,
        paste("Dropped",dropped,"rows with missing start and end dates."),
        ""
      )
    })

    output$AEplot <- renderUI({
        if(nrow(sub()) == 0) {
          output$text1 <- renderText({
              paste("No events with valid dates for this subject")
          })

          return(NULL)
        }

        ae_plot(sub(), footnote())
    })

    output$AEtable <- DT::renderDT({
        if(!nrow(ae_table_dat()) == 0) {
            ae_table_dat() %>%
                dplyr::mutate(
                  details = stringr::str_replace_all(.data$details, "\n", "<br>")
                ) %>%
                dplyr::rename(
                  `Subject ID` = .data$id,
                  `Start Day` = .data$stdy,
                  `End Day` = .data$endy,
                  `Event Details` = .data$details,
                  `Domain` = .data$domain
                ) %>%
                dplyr::select(
                  -seq
                )
            }
        },
        options = list(
            paging = FALSE
        ),
        escape = FALSE,
        rownames = FALSE
    )
  })
}
