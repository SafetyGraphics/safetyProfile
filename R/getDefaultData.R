getDefaultData <- function(standard) {
    data <- list()

    if (standard == 'sdtm') {
        data$aes <- safetyData::sdtm_ae
        data$dm <- safetyData::sdtm_dm
        data$labs <- safetyData::sdtm_lb
    }

    if (standard == 'adam') {
        data$aes <- safetyData::adam_adae
        data$dm <- safetyData::adam_adsl
        data$labs <- safetyData::adam_adlbc
    }

    data
}
