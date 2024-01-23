#' AE Plot Module - Server
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param params parameters object with `data` and `settings` options.
#' @param id Shiny module id
#' @param current_id current selected id
#'
#' @import safetyCharts
#' @import plotly
#' @return Reactive containing AE plot and listing
#'

ae_plot_server <- function(id, params, current_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## set up some basic reactives for convenience
    id_col <- reactive({
      params()$settings$dm$id_col
    })

    data <- reactive({

      req(params()$data)
      req(params()$settings)
      req(current_id())

      safetyCharts::stack_events(
        data = params()$data,
        settings = params()$settings
      ) %>%
        filter(id == current_id())
    })

    ae_table_dat <- reactive({
      req(data())
      data()%>%
      mutate(seq = row_number())
    })

    sub <- reactive({
      req(data())
      data() %>%
        filter(!(is.na(stdy) & is.na(endy))) %>%
        mutate(seq = row_number())
    })

    footnote <- reactive({
      dropped <- nrow(data()) - nrow(sub())
      ifelse(
        dropped > 0,
        paste("Dropped",dropped,"rows with missing start and end dates."),
        ""
      )
    })
    output$AEplot <- renderUI(
      {
        if(!nrow(sub()) == 0) {
         AEplot(sub(), footnote())
        } else {
          output$text1 <- renderText({paste("No events with valid dates for this subject")})
        }
      })

    output$AEtable <- DT::renderDT({
      if(!nrow(ae_table_dat()) == 0){
      ae_table_dat() %>%
        mutate(details = stringr::str_replace_all(details, "\n", "<br>")) %>%
        rename(`Subject ID` = id,
               `Start Day` = stdy,
               `End Day` = endy,
               `Event Details` = details,
               `Domain` = domain) %>%
        select(-seq)
      }
    },
    options = list(paging = FALSE),
    escape = FALSE,
    rownames = FALSE)

  })

}
