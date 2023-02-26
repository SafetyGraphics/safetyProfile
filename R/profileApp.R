#' Safety Profile App
#'
#' @param data `list` Named list of data domains
#' @param settings `list` Named list of data mappings
#' @param ptid `character` Initially selected participant ID
#' @param standard `character` Name of data standard
#' - "adam" (default)
#' - "sdtm"
#' - "custom"
#' @param runNow `logical` Run app?
#'
#' @return `shiny.appobj` Shiny app
#'
#' @importFrom dplyr filter
#' @import shiny
#'
#' @export

profileApp <- function(
    data = list(
      aes = safetyData::adam_adae,
      dm = safetyData::adam_adsl,
      labs = safetyData::adam_adlbc %>%
          filter(PARAMCD %in% c("PHOS", "GLUC", "PROT"))
    ),
    settings = NULL,
    ptid = NULL,
    standard = 'adam',
    runNow = TRUE
) {
    stopifnot(
        '[ standard ] must be a character-classed variable.' =
            class(standard) == 'character',
        '[ standard ] must be one of "sdtm", "adam", "custom".' =
            tolower(standard) %in% c('sdtm', 'adam', 'custom')
    )

    standard <- tolower(standard)

    # TODO: use default data given standard
    if (is.null(data))
        settings <- safetyProfile::example_data[[ standard ]]

    # TODO: make sure settings work
    if (is.null(settings))
        settings <- safetyProfile::mapping[[ standard ]]

  ## create default settings when settings is not defined by default
  if (is.null(settings)) {
    settings <- list(
      labs = list(
        id_col = "USUBJID",
        measure_col = "PARAM",
        value_col = "AVAL",
        studyday_col = "ADY"
      ),
      aes = list(
        id_col = "USUBJID",
        siteid_col = "SITEID",
        bodsys_col = "AEBODSYS",
        term_col = "AEDECOD",
        term_col = "AETERM",
        severity_col = "AESEV",
        stdy_col = "ASTDY",
        endy_col = "AENDY"
      ),
      dm = list(
        id_col = "USUBJID",
        treatment_col = "ARM",
        sex_col = "SEX",
        race_col = "RACE",
        age_col = "AGE"
      )
    )
  }

  ## Create reactive list of data and settings to pass to server
  params <- reactive({
    list(
      data = data,
      settings = settings,
      standard = standard
    )
  })

  ## Create app with ui and server
  app <- shinyApp(
    ui = profile_ui("profile"),
    server = function(input, output, session) {
      id <- profile_server(
        "profile",
        params = params,
        ptid = reactive({
          ptid
        })
      )
      observe({
        print(paste0("id=", id()))
      })
    }
  )

  if (runNow)
    runApp(app)

  return(app)
}
