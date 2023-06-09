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
#' @import sparkline
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
    select(Parameter = {{paramVar}}, A1LO = {{lowVar}}, A1HI = {{highVar}}, {{visVar}}, {{adyVar}}, {{avalVar}}) %>%
    nest_by(Parameter, A1LO, A1HI, .key = 'table')  %>%
    rowwise() %>%
    mutate(spk = list(
      sparkline(
        table[[avalVar]],
        height = 25,
        width = 400,
        fillColor= FALSE,              # NO fill color
        lineColor = '#404040',          # LINE color (gray 25)
        minSpotColor= 'red',            # MIN value color
        maxSpotColor= 'blue',           # MAX value color
        spotColor   = '#404040',        # value color
        highlightSpotColor= '#404040',
        highlightLineColor= '#404040',
        spotRadius = 3,                # SIZE pixels circles
        normalRangeMin= A1LO,      ## turn these into inputs
        normalRangeMax= A1HI,
        normalRangeColor= '#e5e5e5'
      )
    ))%>%
    relocate(table)%>%
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
                      defaultColDef = colDef( format = colFormat(digits = 2))
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
                ),
                spk = colDef(
                  name = '',
                  width = 500,
                  cell = function(value, index) .$spk[[index]]
                )
              )
    )

  return(p)
}