#' Create AEplot
#'
#' @param data AE data frame
#' @param paramVar AE Preferred Term Column AETERM
#' @param aeStartVar AE Start day column
#' @param aeEndVar AE End day column
#' @param colorVar 	AE Severity
#'
#' @import ggplot2
#' @import plotly
#' @import glue
#' @return an AE  plot created with ggplot
#' @export
#'
#' @examples
#'  AEplot(
#'   data = safetyCharts::stack_events() %>%
#'     mutate(seq = dplyr::row_number()) %>%
#'     filter(id == "01-717-1004"),
#'   footnote = "fff"
#' )



AEplot <- function(data, footnote) {
if(nrow(data) == 0){
    showNotification("No records with start/end date found", type = "warning")
  }

  p <- ggplot2::ggplot(data %>%
                mutate(seq = forcats::fct_reorder(as.character(seq), stdy) %>% forcats::fct_rev()),
              aes(x = stdy,
                  y = seq,
                  text = glue::glue("{details}"))) +
    geom_point(
      aes(
        color = domain
      )
    ) +
    geom_segment(
      aes(
        xend = endy,
        yend = seq,
        color = domain
      ),
      linetype = 1,
      linewidth = 2
    ) +
    scale_colour_brewer(palette = "Dark2") +
    xlab("Study Day Start/End") +
    ylab("") +
    theme_bw() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank())

    plotly::ggplotly(p, tooltip = c("text"), source = "AEsource") %>%
    layout(legend = list(orientation = "h", x = 0, y = -0.2),
           title = list(text = glue::glue("Study Event Timeline
                                    <sup>{nrow(data %>% filter(domain == 'aes'))} Adverse Events, {nrow(data %>% filter(domain == 'cm'))} Concomitent Medications</sup>")
                        ),
           annotations =
             list(x = 1, y = -.3, text = glue("{footnote}"),
                  showarrow = F, xref='paper', yref='paper',
                  xanchor='right', yanchor='auto', xshift=0, yshift=0),
           margin = list(t = 50))


}

