#' Create AEplot
#'
#' @param data AE data frame
#' @param paramVar AE Preferred Term Column AETERM
#' @param aeStartVar AE Start day column
#' @param aeEndVar AE End day column
#' @param colorVar 	AE Severity
#'
#' @import ggplot2
#' @return an AE  plot created with ggplot
#' @export
#'
#' @examples
#' AEplot(
#'   data = safetyData::adam_adae,
#'   paramVar = "AETERM",
#'   aeStartVar = "ASTDY",
#'   aeEndVar = "AENDY",
#'   colorVar = "AENDY"
#' )
#'
AEplot <- function(dataCombined, eventVar, startDayVar, colorVar, endDayVar) {

  startDay <- deparse(substitute(startDayVar))
  endDay <- deparse(substitute(endDayVar))

  dataCombined  <- dataCombined[!(is.na(dataCombined[[startDay]])) | !(is.na(dataCombined[[endDay]])),]

  if(nrow(dataCombined) == 0){
    showNotification("The data do not have any start and end dates of the events. Refer to the listing.", type = "warning")
  }

    p <- ggplot(dataCombined) +
      geom_point(aes(x = {{startDayVar}}, y = {{eventVar}})) +
      geom_segment(aes(
        x = {{startDayVar}},
        xend = {{endDayVar}},
        y = {{eventVar}},
        yend = {{eventVar}},
        color = {{colorVar}}
        ), linetype = 1, size = 2) +

        scale_colour_brewer(palette = "Pastel1") +
        xlab("Study Day Start/End") +
        ylab("") +
        # scale_x_continuous(limits = c(x_lower_limit, x_upper_limit)) +
        theme_bw()

    p + theme(legend.position = "none")

  return(p)
}
