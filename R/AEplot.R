#' Create AEplot
#'
#' @param data AE data frame
#' @param paramVar AE Preferred Term Column AETERM
#' @param aeStartVar AE Start day column
#' @param aeEndVar AE End day column
#' @param colorVar 	AE Severity
#' @param ...
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
AEplot <- function(data, paramVar, aeStartVar, aeEndVar, colorVar, ...) {
  p <- ggplot(data) +
    geom_segment(aes(x={{aeStartVar}}, xend={{aeEndVar}}, y={{paramVar}}, yend={{paramVar}}, color={{colorVar}}), linetype=1, size=2) +
    scale_colour_brewer(palette = "Pastel1")+
    xlab("Study Day Start/End of AE")+
    ylab("") +
    scale_x_continuous(limits=c(min(na.omit(data$ASTDY)),max(na.omit(data$AENDY)))) +
    theme_bw()
  p + theme(legend.position="none")

  return(p)
  }
