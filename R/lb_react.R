#' Create safety line plot
#'
#' @param data long data frame such as LB or VS
#' @param paramVar term column
#' @param visVar analysis visit column
#' @param adyVar analysis day column
#' @param avalVar analysis value column
#' @param lowVar lower limit column
#' @param highVar upper limit column
#'
#' @import ggplot2
#' @import reactable
#' @importFrom reactablefmtr fivethirtyeight
#' @importFrom DT datatable
#'
#' @return an lineplot created with ggplot
#' @export
#'
#' @examples
#' lb_react(
#'     data=safetyData::adam_adlb,
#'     paramVar = "PARAM",
#'     visVar = "AVISIT",
#'     adyVar = "ADY",
#'     avalVar = "AVAL",
#'     lowVar = "A1LO",
#'     highVar = "A1HI"
#'    )
#'
#'
lb_react <- function(data, paramVar, visVar, adyVar, avalVar, lowVar, highVar) {

  p <- data %>%
    select(Parameter = {{paramVar}}, A1LO = {{lowVar}}, A1HI = {{highVar}}, AVISIT = {{visVar}}, ADY = {{adyVar}}, AVAL = {{avalVar}}) %>%
    nest_by(Parameter, A1LO, A1HI, .key = 'table')  %>%
    rowwise() %>%
      mutate(gg = list( ggplot(data = table,
                            aes(x = ADY,
                                y = AVAL,
                                label = AVISIT) ) +
                        # annotate('rect', xmin = -Inf, xmax = Inf,
                        #          ymin = A1LO, ymax = A1HI,
                        #          alpha = 0.15) +
                        geom_point_interactive(
                           aes(tooltip = str_glue("Analysis Value: {AVAL} <br> Analysis Day (Visit): {ADY} <small>({AVISIT})</small>"))
                        ) +
                        geom_line() +
                        theme_void() ),
          gi = list( girafe(ggobj = gg,
                            fonts = list(sans = "Roboto"),
                            width_svg = 4, height_svg = 0.65,
                            options = list(
                               opts_sizing(rescale = FALSE),
                               opts_toolbar(saveaspng = FALSE),
                               opts_tooltip(opacity = 0.5)
                            ))
          )) %>%
    relocate(table) %>%
    select(-gg) %>%
    reactable(.,
              bordered = TRUE,
              highlight = FALSE,
              searchable = TRUE,
              filterable = TRUE,
              pagination = FALSE,
              theme = fivethirtyeight(),
              columns = list(
                table = colDef(
                  maxWidth = 25,
                  name = 'TABLE',
                  header= function () "",
                  cell = function() "",
                  details = function(index){
                    reactable(
                      .$table[[index]],
                      pagination = FALSE,
                      theme = fivethirtyeight(),
                      defaultColDef = colDef(format = colFormat(digits = 2))
                    )
                  }
                ),
                Parameter = colDef(
                  maxWidth = 300
                ),
                A1LO = colDef(
                  name = 'Lower Limit',
                  maxWidth = 65
                ),
                A1HI = colDef(
                  name = 'Upper Limit',
                  maxWidth = 65
                ) ,
                gi = colDef(
                  name = '',
                  width = 500,
                  html = TRUE,
                  cell  = function(x){return(htmltools::div(x))}
                )
              )
    )

  return(p)
}


#lb_react(data = adam_adlbh, paramVar = "PARAM", visVar = "AVISIT", adyVar = "ADY", avalVar = 'AVAL', lowVar = "A1LO", highVar = "A1HI")
