#' Create AE Plot
#'
#' @param data AE data frame
#' @param paramVar AE Preferred Term Column AETERM
#' @param aeStartVar AE Start day column
#' @param aeEndVar AE End day column
#' @param colorVar 	AE Severity
#'
#' @return an AE  plot created with ggplot
#'
#' @examples
#'  ae_plot(
#'   data = safetyCharts::stack_events() %>%
#'     mutate(seq = dplyr::row_number()) %>%
#'     filter(id == "01-717-1004"),
#'   footnote = "fff"
#' )
#'
#' @importFrom dplyr filter mutate
#' @importFrom forcats fct_reorder fct_rev
#' @import ggplot2
#' @importFrom purrr map_chr
#' @importFrom glue glue
#' @importFrom plotly ggplotly layout
#'
#' @export

ae_plot <- function(data, footnote) {
    if (nrow(data) == 0) {
        showNotification("No records with start/end date found", type = "warning")
    }

    p <- data %>%
        dplyr::mutate(
            seq = forcats::fct_reorder(as.character(seq), .data$stdy) %>%
                forcats::fct_rev()
        ) %>%
        ggplot2::ggplot(ggplot2::aes_string(
            x = 'stdy',
            y = 'seq',
            color = 'domain',
            text = 'details'
        )) +
        ggplot2::geom_point() +
        ggplot2::geom_segment(
            ggplot2::aes_string(
                xend = 'endy',
                yend = 'seq'
            ),
            linetype = 1,
            linewidth = 2
        ) +
        ggplot2::scale_colour_brewer(palette = "Dark2") +
        ggplot2::xlab("Study Day Start/End") +
        ggplot2::ylab("") +
        ggplot2::theme_bw() +
        ggplot2::theme(
            axis.text.y = ggplot2::element_blank(),
            axis.ticks.y = ggplot2::element_blank()
        )

    n_events_by_domain <- purrr::map_chr(
        unique(data$domain),
        function(domain) {
            n_events <- data %>%
                dplyr::filter(.data$domain == !!domain) %>%
                nrow()

            s <- ifelse(
                n_events != 1,
                's',
                ''
            )

            event_type <- switch(
                domain,
                aes = 'Adverse Event',
                cm = 'Concomitant Medication',
                ex = 'Treatment'
            )

            return(glue::glue(
                '{n_events} {event_type}{s}'
            ))
        }
    )

    p %>%
        plotly::ggplotly(
            height = ifelse(
                nrow(data) < 5,
                300,
                as.numeric(nrow(data)*35)
            ),
            source = "AEsource",
            tooltip = c("text")
        ) %>%
        plotly::layout(
            legend = list(
                orientation = "h",
                x = 0,
                y = -0.2
            ),
            title = list(
                text = glue::glue(
                    "Study Event Timeline\n<sup>{paste(n_events_by_domain, collapse = ', ')}</sup>"
                )
            ),
            annotations = list(
                x = 1,
                y = -.3,
                text = footnote,
                showarrow = F,
                xref='paper',
                yref='paper',
                xanchor='right',
                yanchor='auto',
                xshift=0,
                yshift=0
            ),
            margin = list(
                t = 50
            )
        )

}

