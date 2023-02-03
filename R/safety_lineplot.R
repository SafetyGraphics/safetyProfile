#' Create safety line plot
#'
#' @param data long data frame such as LB or VS
#' @param paramVar term column
#' @param adyVar analysis day column
#' @param avalVar analysis value column
#' @param colorVar optional color variable
#'
#' @import ggplot2
#' @return an lineplot created with ggplot
#' @export
#'
#' @examples
#' safety_lineplot(
#'     data=safetyData::adam_advs,
#'     paramVar = "PARAM",
#'     adyVar= "ADY",
#'     avalVar="AVAL",
#'     colorVar="ATPT"
#'    )
#'
#'
safety_lineplot <- function(data, paramVar, adyVar, avalVar) {

  x_lower_limit <- min(data[[adyVar]])
  x_upper_limit <- max(data[[adyVar]])


  p <- ggplot(data) +

    geom_point(aes(x=.data[[adyVar]], y=.data[[avalVar]])) +
    geom_line(aes(x=.data[[adyVar]], y=.data[[avalVar]])) +

    facet_wrap(~ data[[paramVar]],
               ncol = 1,
               scales = "free_y") +

    #scale_colour_brewer(palette = "Pastel1")+
    xlab("Analysis Relative Day")+
    ylab("") +
    scale_x_continuous(limits=c(x_lower_limit, x_upper_limit)) +
    theme_bw() +
    theme(legend.position="none")

  return(p)
  }
