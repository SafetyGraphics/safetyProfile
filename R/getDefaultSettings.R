getDefaultSettings <- function(standard) {
    settings <- list()

    if (standard == 'sdtm') {
        settings$aes <- list(
            id_col = "USUBJID",
            stdt_col = "AESTDTC",
            endt_col = "AEENDTC",
            stdy_col = NULL,
            endy_col = NULL,
            aeterm_col = "AETERM",
            decod_col = 'AEDECOD',
            bodsys_col = "AEBODSYS",
            severity_col = "AESEV"
        )

        settings$dm <- list(
            id_col = "USUBJID",
            reference_date_col = 'RFSTDTC',
            treatment_col = "ARM"
        )

        settings$labs <- list(
            id_col = "USUBJID",
            visit_col = 'VISIT',
            visit_order_col = 'VISITNUM',
            dt_col = 'LBDTC',
            dy_col = NULL,
            result_col = 'LBSTRESN'
        )
    }

    if (standard == 'adam') {
        settings$aes <- list(
            id_col = "USUBJID",
            stdt_col = 'ASTDT',
            endt_col = 'AENDT',
            stdy_col = "ASTDY",
            endy_col = "AENDY",
            aeterm_col = "AETERM",
            decod_col = 'AEDECOD',
            bodsys_col = "AEBODSYS",
            severity_col = "AESEV"
        )

        settings$dm <- list(
            id_col = "USUBJID",
            reference_date_col = 'TRTSDT',
            treatment_col = "TRT01P"
        )

        settings$labs <- list(
            id_col = "USUBJID",
            visit_col = 'AVISIT',
            visit_order_col = 'AVISITN',
            dt_col = 'ADT',
            dy_col = 'ADY',
            result_col = 'AVAL'
        )
    }

    settings
}
