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

    id_col <- reactive({
        params()$settings[[ refDomain ]]$id_col
    })

    ref_data <- params()$data[[ refDomain ]] %>%
        select(
            id_col, refDate
        ) %>%
        mutate(
            refDate = as.Date(.data$refDate)
        )

    domain_data <- params()$data[[ domain ]] %>%
        left_join(
            ref_data,
            id_col
        )

    if (!is.null(domainDate)) {
        domain_date[[ domainDate ]] <- as.Date(domain_data[[ domainDate ]])

        domain_data[[ paste0(domainDate, '_dy') ]] <- as.numeric(domain_data[[ domainDate ]] -
            ref_data[[ refDate ]]) + (domain_data[[ domainDate ]] >= ref_data[[ refDate ]])
    }

    if (!is.null(domainStartDate)) {
        domain_date[[ domainStartDate ]] <- as.Date(domain_data[[ domainStartDate ]])

        domain_data[[ paste0(domainDate, '_stdy') ]] <- as.numeric(domain_data[[ domainStartDate ]] -
            ref_data[[ refDate ]]) + (domain_data[[ domainStartDate ]] >= ref_data[[ refDate ]])
    }

    if (!is.null(domainEndDate)) {
        domain_date[[ domainEndDate ]] <- as.Date(domain_data[[ domainEndDate ]])

        domain_data[[ paste0(domainDate, '_endy') ]] <- as.numeric(domain_data[[ domainEndDate ]] -
            ref_data[[ refDate ]]) + (domain_data[[ domainEndDate ]] >= ref_data[[ refDate ]])
    }

    domain_data
}
