getSDTMTiming <- function() {
    stopifnot(
        '[ standard ] must be a character-classed variable.' =
            class(standard) == 'character',
        '[ standard ] must be one of "sdtm", "adam", "custom".' =
            tolower(standard) %in% c('sdtm', 'adam', 'custom')
    )

    standard <- tolower(standard)

    ## Create list of default data when undefined.
    if (is.null(data))
        data <- getDefaultData(standard)

    ## Create list of default settings when undefined.
    if (is.null(settings))
        settings <- getDefaultSettings(standard)

    ## Calculate study timing.
    if (standard == 'sdtm') {
        data$aes <- getTiming(
            params = list(
                data = data,
                settings = settings,
                standard = standard
            ),
            'aes',
            domainStartDate = settings$aes$stdt_col,
            domainEndDate = settings$aes$endt_col
        )
        settings$aes$stdy_col <- paste0(settings$aes$stdt_col, '_dy')
        settings$aes$endy_col <- paste0(settings$aes$endt_col, '_dy')

        data$labs <- getTiming(
            params = list(
                data = data,
                settings = settings,
                standard = standard
            ),
            'labs',
            domainDate = settings$labs$dt_col
        )
        settings$labs$dy_col <- paste0(settings$labs$dt_col, '_dy')
    }
}
