#' Event Timeline
#'
#' @param data `data.frame` Adverse events data domain
#' @param footnote `character` Footnote text
#'
#' @return `htmlwidget` Event timeline `plotly` object
#'
#' @examples
#' data <- safetyCharts::stack_events() %>%
#'   dplyr::filter(
#'     !(is.na(.data$stdy) & is.na(.data$endy))
#'   ) %>%
#'   dplyr::arrange(
#'     .data$id, .data$stdy, .data$endy
#'   ) %>%
#'   dplyr::group_by(.data$id) %>%
#'   dplyr::mutate(
#'     seq = dplyr::row_number()
#'   ) %>%
#'   ungroup()
#' 
#' data %>%
#'   dplyr::filter(
#'     .data$id == sample(unique(.data$id), 1)
#'   ) %>%
#'   ae_plot()
#'
#' @importFrom dplyr filter mutate
#' @importFrom forcats fct_reorder fct_rev
#' @import ggplot2
#' @importFrom purrr map_chr
#' @importFrom glue glue
#' @importFrom plotly ggplotly layout
#' @importFrom rlang !!
#'
#' @export

ae_plot <- function(data, footnote = '') {
    data_cleaned <- data %>%
        dplyr::filter(
            !(is.na(.data$stdy) & is.na(.data$endy))
        ) %>%
        dplyr::arrange(
            .data$stdy, .data$endy, .data$domain
        ) %>%
        dplyr::mutate(
            seq = forcats::fct_reorder(as.character(seq), .data$stdy) %>%
                forcats::fct_rev()
        )

    if (nrow(data_cleaned) == 0) {
        showNotification("No records with start/end date found.", type = "warning")
    }

    p <- data_cleaned %>%
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
        unique(data_cleaned$domain),
        function(domain) {
            n_events <- data_cleaned %>%
                dplyr::filter(
                  .data$domain == !!domain
                ) %>%
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
                nrow(data_cleaned) < 5,
                300,
                as.numeric(nrow(data_cleaned)*35)
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

