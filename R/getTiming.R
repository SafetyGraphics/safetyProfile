#' Calculate reference timepoint.
#'
#' @param params `list` Reactive list of input data and mappings
#' @param domain `character` Name of data domain with which to calculate reference timepoint
#' @param domainDate `character` Name of date column with which to calculate reference timepoint
#' @param reference `character` Name of data reference with which to calculate reference timepoint
#' @param referenceDate `character` Name of date column with which to calculate reference timepoint

getTiming <- function(
    params,
    domain,
    domainDate = NULL,
    refDomain = 'dm',
    refDate = 'RFSTDTC',
    domainStartDate = NULL,
    domainEndDate = NULL
) {
    # TODO: add stopifnot() logic

    id_col <- params$settings[[ refDomain ]]$id_col

    ref_data <- params$data[[ refDomain ]] %>%
        select(
            id_col, refDate
        ) %>%
        mutate(
            # TODO: add logic around date format... lubridate::ymd?
            refDate = as.Date(.data[[ refDate ]])
        )

    domain_data <- params$data[[ domain ]] %>%
        left_join(
            ref_data,
            id_col
        )

    getStudyDay <- function(data, date_col) {
        data[[ date_col ]] <- as.Date(data[[ date_col ]])

        data[[ paste0(date_col, '_dy') ]] <- as.numeric(
            data[[ date_col ]] - data$refDate
        ) + (
            data[[ date_col ]] >= data$refDate
        )

        data
    }

    if (!is.null(domainDate))
        domain_data <- getStudyDay(domain_data, domainDate)

    if (!is.null(domainStartDate))
        domain_data <- getStudyDay(domain_data, domainStartDate)

    if (!is.null(domainEndDate))
        domain_data <- getStudyDay(domain_data, domainEndDate)

    domain_data
}
