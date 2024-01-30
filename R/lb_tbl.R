#' Create safety line plot
#'
#' @param data long data frame such as LB or VS
#' @param measureVar term column
#' @param visitVar analysis visit column
#' @param studyDayVar analysis day column
#' @param resultVar analysis value column
#' @param llnVar lower limit column
#' @param ulnVar upper limit column
#'
#' @return an lineplot created with ggplot
#'
#' @examples
#' lb_tbl(
#'     data=safetyData::adam_adlbh,
#'     measureVar = "PARAM",
#'     visitVar = "AVISIT",
#'     studyDayVar = "ADY",
#'     resultVar = "AVAL",
#'     llnVar = "A1LO",
#'     ulnVar = "A1HI"
#'  )
#'
#' @import dplyr
#' @importFrom DT datatable
#' @importFrom htmlwidgets JS
#' @importFrom purrr transpose
#' @importFrom sparkline spk_add_deps spk_chr
#' @importFrom tibble as_tibble
#' @importFrom tidyselect everything
#'
#' @export

lb_tbl <- function(data, measureVar, visitVar, studyDayVar, resultVar, llnVar, ulnVar) {
  # select and rename data columns
  data <- data %>%
    dplyr::select(
      Measure = {{measureVar}},
      LLN = {{llnVar}},
      ULN = {{ulnVar}},
      Visit = {{visitVar}},
      `Study Day` = {{studyDayVar}},
      Result = {{resultVar}}
    ) %>%
    tibble::as_tibble() %>%
    dplyr::arrange(
      .data$Measure, .data$`Study Day`
    )

  # select min and max for each measure
  min_max <- data %>%
    dplyr::group_by(.data$Measure) %>%
    dplyr::summarize(LLN = format(min(.data$LLN),digits = 1),
              ULN = format(max(.data$ULN),digits = 1))

  # identify distinct measure values
  par_dt <- data %>%
      dplyr::distinct(.data$Measure)

  # Create details column that hold column values in the list
  transposed_data <- data %>%
    dplyr::group_by(.data$Measure) %>%
    dplyr::summarize(
        `_details` = list(
            purrr::transpose(
                dplyr::across(.data$Visit:.data$Result)
            )
        )
    ) %>%
    dplyr::mutate(
        ' ' = '&oplus;'
    )

  # Merge data
  merged_data <- par_dt %>%
      dplyr::left_join(
          transposed_data,
          by = "Measure"
      )

  # add sparkline
  sparkline <- data %>%
    dplyr::select(.data$Measure, .data$Result) %>%
    dplyr::group_by(.data$Measure) %>%
    dplyr::summarize(
      Trend = sparkline::spk_chr(
        .data$Result,
        type = "line",
        chartRangeMin = min(.data$Result),
        chartRangeMax = max(.data$Result)
      ))

  # Reorder columns
  reordered_dt <- merged_data %>%
    dplyr::select(
        length(merged_data),
        tidyselect::everything()
    ) %>%
    dplyr::left_join(
        min_max,
        by = "Measure"
    ) %>%
    dplyr::left_join(
        sparkline,
        by = 'Measure'
    ) %>%
    dplyr::select(tidyselect::all_of(c(
        ' ',
        'Measure',
        'LLN',
        'ULN',
        'Trend',
        '_details'
    ))) # this order is important for JS code

## the callback
callback <- htmlwidgets::JS(
  "table.column(1).nodes().to$().css({cursor: 'pointer'});",
  "",
  "// make the table header of the nested table",
  "var format = function(d, childId){",
  "  if(d != null){",
  "    var html = ",
  "      '<table class=\"display compact hover\" id=\"' + childId + '\"><thead><tr>';",
  "    for (var key in d[d.length-1][0]) {",
  "      html += '<th>' + key + '</th>';",
  "    }",
  "    html += '</tr></thead></table>'",
  "    return html;",
  "  } else {",
  "    return '';",
  "  }",
  "};",

  "// make the datatable",
  "var format_datatable = function(d, childId){",
  "  var dataset = [];",
  "  var n = d.length - 1;",
  "  for(var i = 0; i < d[n].length; i++){",
  "    var datarow = $.map(d[n][i], function (value, index) {",
  "      return [value];",
  "    });",

  "    dataset.push(datarow);",
  "  }",
  "  var id = 'table#' + childId;",
  "  if (Object.keys(d[n][0]).indexOf('_details') === -1) {",
  "    var subtable = $(id).DataTable({",
  "                 'data': dataset,",
  "                 'autoWidth': true,",
  "                 'deferRender': true,",
  "                 'info': false,",
  "                 'lengthChange': false,",
  "                 'ordering': d[n].length > 1,",
  "                 'order': [],",
  "                 'paging': false,",
  "                 'scrollX': false,",
  "                 'scrollY': false,",
  "                 'searching': false,",
  "                 'sortClasses': false,",
  # "                 'rowCallback': rowCallback,",
  # "                 'headerCallback': headerCallback,",
  "                 'columnDefs': [{targets: '_all', className: 'dt-right'},",
  "]",
  "               });",
  "  } else {",
  "    var subtable = $(id).DataTable({",
  "            'data': dataset,",
  "            'autoWidth': true,",
  "            'deferRender': true,",
  "            'info': false,",
  "            'lengthChange': false,",
  "            'ordering': d[n].length > 1,",
  "            'order': [],",
  "            'paging': false,",
  "            'scrollX': false,",
  "            'scrollY': false,",
  "            'searching': false,",
  "            'sortClasses': false,",
  "            'rowCallback': rowCallback,",
  "            'headerCallback': headerCallback,",
  "            'columnDefs': [",
  "              {targets: -1, visible: false},",
  "              {targets: 0, orderable: false, className: 'details-control'},",
  "              {targets: '_all', className: 'dt-center'}",
  "             ]",
  "          }).column(0).nodes().to$().css({cursor: 'pointer'});",
  "  }",
  "};",
  "",
  "// display the child table on click",
  "table.on('click', 'td.details-control', function(){",
  "  var tbl = $(this).closest('table'),",
  "      tblId = tbl.attr('id'),",
  "      td = $(this),",
  "      row = $(tbl).DataTable().row(td.closest('tr')),",
  "      rowIdx = row.index();",
  "  if(row.child.isShown()){",
  "    row.child.hide();",
  "    td.html('&oplus;');",
  "  } else {",
  "    var childId = tblId + '-child-' + rowIdx;",
  "    row.child(format(row.data(), childId)).show();",
  "    td.html('&CircleMinus;');",
  "    format_datatable(row.data(), childId);",
  "  }",
  "});")

  reordered_dt %>%
    DT::datatable(
      callback = callback,
      escape = FALSE, #-2
      options = list(
        columnDefs = list(
          list(visible = FALSE, targets = ncol(reordered_dt)),
          list(orderable = FALSE, className = 'details-control', targets = 1),
          # list(className = "dt-left", targets = "_all")
          list(className = "dt-left", targets = 2),
          list(className = "dt-right", targets = 3:4)
        ),
        fnDrawCallback = htmlwidgets::JS('
          function() {
            HTMLWidgets.staticRender();
          }
        '),
        paging = FALSE
      )
      # TODO: figure out why rownames can't be turned off
      #rownames = FALSE
    ) %>%
    sparkline::spk_add_deps()
}
