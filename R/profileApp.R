#' Safety Profile App
#'
#' @param data `list` Named list of data domains
#' @param settings `list` Named list of data mappings
#' @param standard `character` Name of data standard
#' @param runNow `logical` Run app?
#'
#' @return `shiny.appobj` Shiny app
#'
#' @importFrom shiny shinyApp callModule
#'
#' @export

profileApp <- function(
    data = NULL,
    settings = NULL,
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

    ## create list of default data when undefined
    if (is.null(data))
        data <- getDefaultData(standard)

    ## create list of default settings when undefined
    if (is.null(settings))
        settings <- getDefaultSettings(standard)
browser()
    ## create reactive list of data and settings to pass to server
    params <- reactive({
        list(
            data = data,
            settings = settings,
            standard = standard
        )
    })

    ## Create app with UI and server
    app <- shinyApp(
        ui =  profile_ui("profile"),
        server = function(input,output,session) {
            callModule(profile_server, "profile", params)
        }
    )

    if (runNow)
        runApp(app)
    else
        app
}
