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
#'     data=safetyData::adam_adae
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
    ylab("AE Preferred Term") +
    scale_x_continuous(limits=c(0,max(na.omit(data$AENDY)))) #+
    # theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major =   element_blank()) + theme(aspect.ratio = .2)
  p + theme(legend.position="none")

  return(p)
  }
