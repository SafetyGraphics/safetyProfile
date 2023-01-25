#' Safety Profile App
#'
#' @param dfAE AE Data
#' @param dfDemog demog data
#' @param settings safetyGraphics settings
#'
#' @import shiny
#'
#' @export

profileApp <- function(
    data = list(
        aes=safetyData::adam_adae,
        dm = safetyData::adam_adsl,
        labs=safetyData::adam_adlbc %>% filter(PARAMCD %in% c('PHOS', 'GLUC', 'PROT'))
    ),
    settings=NULL,
    runNow=TRUE
){

    ## create default settings when settings is not defined by default
    if(is.null(settings)){
        settings<-list(
            labs=list(id_col="USUBJID", trarm_col="TRTA",
                      term_col='PARAM', aval_col="AVAL", stdy_col="ADY"),
            aes=list(id_col="USUBJID", siteid_col="SITEID", trarm_col="TRTA",
                     bodsys_col="AEBODSYS", term_col = 'AEDECOD',
                     aeterm_col="AETERM", severity_col="AESEV",
                     stdy_col="ASTDY", endy_col="AENDY"),
            dm=list(id_col="USUBJID", treatment_col="ARM")
        )
    }

    ## create object containing data and setting to pass to server
    params <- reactive({
        list(
            data=data,
            settings=settings
        )
    })

    ## Create app with ui and server
    app <- shinyApp(
        ui =  profile_ui("profile"),
        server = function(input,output,session){
            callModule(profile_server, "profile", params)
        }
    )

    #if(runNow)
        runApp(app)
    #else
    #app
}
