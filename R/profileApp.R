#' Safety Profile App
#'
#' @param dfAE AE Data
#' @param dfDemog demog data
#' @param settings safetyGraphics settings
#' @param ptid participant ID to select when app is initialized
#'
#' @import shiny
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
        siteid_col = "SITEID",
        bodsys_col = "AEBODSYS",
        term_col = "AEDECOD",
        #term_col = "AETERM",
        severity_col = "AESEV",
        stdy_col = "ASTDY",
        endy_col = "AENDY"
      ),
      labs = list(
        id_col = "USUBJID",
        measure_col = "PARAM",
        value_col = "AVAL",
        studyday_col = "ADY",
        normal_col_low="A1LO", 
        normal_col_high="A1HI",
        visit_col="AVISIT"
      ),
      cm = list(
        id_col = "USUBJID",
        cmtrt_col = "CMTRT",
        stdy_col = "CMSTDY",
        endy_col = "CMENDY",
        class_col = "CMCLAS",
        desc_col = "CMINDC"
      ),
      ex = list(
        id_col = 'USUBJID',
        extrt_col = 'EXTRT',
        stdy_col = "EXSTDY",
        endy_col = "EXENDY"
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

  # if(runNow)
  runApp(app)
  # else
  # app
}
