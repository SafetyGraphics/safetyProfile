#' Safety Profile App
#'
#' @param data `list` Named list of data domains
#' @param settings `list` Named list of data domain metadata
#' @param ptid `character` Initial participant ID; defaults to first ID when sorted alphanumerically
#' @param runNow `logical` Run app immediately? Default: `TRUE`
#'
#' @import shiny
#' @import safetyData
#'
#' @export

profileApp <- function(
  data = list(
    dm = safetyData::adam_adsl,
    aes = safetyData::adam_adae,
    labs = safetyData::adam_adlbc,
    cm = safetyData::sdtm_cm
  ),
  settings = NULL,
  ptid = NULL,
  runNow = TRUE
) {

  ## create default settings when settings is not defined by default
  if (is.null(settings)) {
    settings <- list(
      dm = list(
        id_col = "USUBJID",
        treatment_col = "ARM",
        sex_col = "SEX",
        race_col = "RACE",
        age_col = "AGE"
      ),
      aes = list(
        id_col = "USUBJID",
        stdy_col = "ASTDY",
        endy_col = "AENDY",
        term_col = "AEDECOD",
        bodsys_col = "AEBODSYS",
        severity_col = "AESEV"
      ),
      labs = list(
        id_col = "USUBJID",
        visit_col = "AVISIT",
        studyday_col = "ADY",
        measure_col = "PARAM",
        value_col = "AVAL",
        normal_col_low = "A1LO", 
        normal_col_high = "A1HI"
      ),
      cm = list(
        id_col = "USUBJID",
        stdy_col = "CMSTDY",
        endy_col = "CMENDY",
        cmtrt_col = "CMTRT",
        class_col = "CMCLAS",
        desc_col = "CMINDC"
      ),
      ex = list(
        id_col = 'USUBJID',
        stdy_col = "EXSTDY",
        endy_col = "EXENDY",
        extrt_col = 'EXTRT'
      )
    )
  }

  ## create object containing data and setting to pass to server
  params <- reactive({
    list(
      data = data,
      settings = settings
    )
  })

  ## Create app with ui and server
  app <- shinyApp(
    ui = fluidPage(profile_ui("profile")),
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
  else
    app
}
