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
#'     data=safetyData::adam_adae,
#'     paramVar = "AETERM",
#'     aeStartVar= "ASTDY",
#'     aeEndVar="AENDY",
#'     colorVar="AENDY"
#'    )
#'
#'
AEplot <- function(data, paramVar, aeStartVar, aeEndVar, colorVar) {

  x_lower_limit <- min(na.omit(data[[aeStartVar]]))
  x_upper_limit <- max(na.omit(data[[aeEndVar]]))


  p <- ggplot(data) +
    geom_segment(aes(x=.data[[aeStartVar]],
                     xend=.data[[aeEndVar]],
                     y=.data[[paramVar]],
                     yend=.data[[paramVar]],
                     color=.data[[colorVar]]), linetype=1, size=2) +
    geom_point(x=data[[aeStartVar]], y=data[[paramVar]]) +
    scale_colour_brewer(palette = "Pastel1")+
    xlab("Study Day Start/End of AE")+
    ylab("") +
    scale_x_continuous(limits=c(x_lower_limit, x_upper_limit)) +
    theme_bw()
  p + theme(legend.position="none")

  return(p)
  }
