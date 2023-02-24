library(dplyr)
library(purrr)

mapping_sdtm <- read.csv("data-raw/mapping_sdtm.csv")
mapping_adam <- read.csv("data-raw/mapping_adam.csv")

usethis::use_data(mapping_sdtm, overwrite = TRUE)
usethis::use_data(mapping_adam, overwrite = TRUE)

mapping <- bind_rows(mapping_sdtm, mapping_adam) %>%
    split(.$standard) %>%
    map(function(standard) {
        standard %>%
            split(.$domain) %>%
            map(function(domain) {
                as.list(domain$value) %>%
                    setNames(domain$key)
            })
    })

usethis::use_data(mapping, overwrite = TRUE)
