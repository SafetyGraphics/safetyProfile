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
AEplot <- function(data, footnote) {
if(nrow(data) == 0){
    showNotification("No records with start/end date found", type = "warning")
  }

  p <- ggplot(data) +
    geom_point(
      aes(
        x = stdy,
        y = seq,
        color=domain
      )
    ) +
    geom_segment(
      aes(
        x = stdy,
        xend = endy,
        y = seq,
        yend = seq,
        color = domain,
      ),
      linetype = 1,
      size = 2
    ) +
    scale_colour_brewer(palette = "Pastel1") +
    xlab("Study Day Start/End") +
    ylab("") +
    labs(caption = footnote)+
    # scale_x_continuous(limits = c(x_lower_limit, x_upper_limit)) +
    theme_bw()

  p + theme(legend.position = "none")

  return(p)
}
