
#' Combine AE and CM data
#'
#' @param aes_data AE data
#' @param cm_data CM data
#' @param settings safetyGraphics settings
#'
#' @return combined dataset with stacked AE and CM data

#'
#' @examples
#' combine_domains(
#'      aes_data = params()$data$aes,
#'      cm_data = params()$data$cm,
#'      settings = params()$settings
#'      )

combine_domains <- function(aes_data, cm_data, settings){

  aes_settings <- settings$aes
  cm_settings <- settings$cm

  aes_data <- aes_data %>% select(aes_settings[["id_col"]],
                              STDY = aes_settings[["stdy_col"]],
                              ENDY = aes_settings[["endy_col"]],
                              EVENT = aes_settings[["term_col"]],
                              DETAILS = aes_settings[["severity_col"]]
                                ) %>% mutate(DOMAIN = "AE")

  cm_data <- cm_data %>% select(cm_settings[["id_col"]],
                                EVENT = cm_settings[["cmtrt_col"]],
                                DETAILS = cm_settings[["class_col"]],
                                STDY = cm_settings[["stdy_col"]],
                                ENDY = cm_settings[["endy_col"]],
                                # DETAILS = settings$cm[["desc_col"]]
  ) %>% mutate(DOMAIN = "CM")

  combined <- aes_data %>% rbind(cm_data)
  return(combined)
}
