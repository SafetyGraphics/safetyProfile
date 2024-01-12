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
    if (nrow(data) == 0) {
        showNotification("No records with start/end date found", type = "warning")
    }

    p <- data %>%
        mutate(
            seq = forcats::fct_reorder(as.character(seq), stdy) %>% forcats::fct_rev()
        ) %>%
        ggplot2::ggplot(aes(
            x = stdy,
            y = seq,
            color = domain,
            text = glue::glue("{details}")
        )) +
        geom_point() +
        geom_segment(
            aes(
                xend = endy,
                yend = seq
            ),
            linetype = 1,
            linewidth = 2
        ) +
        scale_colour_brewer(palette = "Dark2") +
        xlab("Study Day Start/End") +
        ylab("") +
        theme_bw() +
        theme(
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()
        )

    n_events_by_domain <- map_chr(
        unique(data$domain),
        function(domain) {
            n_events <- data %>%
                filter(.data$domain == !!domain) %>%
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
            height = ifelse(nrow(data) < 5, 300, as.numeric(nrow(data)*35)),
            tooltip = c("text"),
            source = "AEsource"
        ) %>%
        layout(
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
                text = glue("{footnote}"),
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

