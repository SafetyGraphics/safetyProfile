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
#' @import ggplot2
#' @import DT
#' @import htmlwidgets
#' @import sparkline
#' @importFrom stringr str_glue
#' @importFrom reactablefmtr fivethirtyeight
#' @importFrom DT datatable
#'
#' @return an lineplot created with ggplot
#' @export
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

lb_tbl <- function(data, measureVar, visitVar, studyDayVar, resultVar, llnVar, ulnVar) {

  #select and rename data columns
  data <- data %>%
    select(
      Measure = {{measureVar}},
      LLN = {{llnVar}},
      ULN = {{ulnVar}},
      Visit = {{visitVar}},
      `Study Day` = {{studyDayVar}},
      Result = {{resultVar}}
    ) %>%
    as_tibble() %>%
    arrange(
      Measure, `Study Day`
    )

  #select min and max for each measure
  min_max <- data %>%
    group_by(Measure) %>%
    summarize(LLN = format(min(LLN),digits = 1),
              ULN = format(max(ULN),digits = 1))

  #identify distinct measure values
  par_dt <- data %>%
      distinct(Measure)

  # Create details column that hold column values in the list
  transposed_data <- data %>%
    group_by(Measure) %>%
    summarise(
        `_details` = list(
            purrr::transpose(
                across(Visit:Result)
            )
        )
    ) %>%
    mutate(' ' = '&oplus;')

  # Merge data
  merged_data <- left_join(par_dt, transposed_data, by = "Measure")

  # add sparkline
  sparkline <- data %>%
    select(Measure, Result) %>%
    group_by(Measure) %>%
    summarize(
      Trend = spk_chr(
        Result, type ="line",
        chartRangeMin = min(Result), chartRangeMax = max(Result)
      ))

  # Reorder columns
  reordered_dt <- merged_data %>%
    select(length(merged_data), everything()) %>%
    left_join(min_max, by = "Measure") %>%
    left_join(sparkline) %>%
    select(' ',
        Measure,
        LLN,
        ULN,
        Trend,
        '_details'
    ) # this order is important for JS code

## the callback
callback = JS(
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
  "                 'columnDefs': [{targets: '_all', className: 'dt-center'}]",
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
    datatable(
      callback = callback,
      escape = FALSE, #-2
      options = list(
        columnDefs = list(
          list(visible = FALSE, targets = ncol(reordered_dt)),
          list(orderable = FALSE, className = 'details-control', targets = 1),
          list(className = "dt-center", targets = "_all")
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
    spk_add_deps()
}
