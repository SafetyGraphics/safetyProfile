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
#' @import DT
#' @import ggiraph
#' @importFrom stringr str_glue
#' @importFrom reactablefmtr fivethirtyeight
#' @importFrom DT datatable
#'
#' @return an lineplot created with ggplot
#' @export
#'
#' @examples
#' lb_react(
#'     data=safetyData::adam_adlbh,
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

  data <- data %>%
    select(parameter = {{paramVar}},
           A1LO = {{lowVar}},
           A1HI = {{highVar}},
           AVISIT = {{visVar}},
           ADY = {{adyVar}},
           AVAL = {{avalVar}})%>%
  # data <- data %>%
  #   select({{paramVar}},
  #         {{lowVar}},
  #          {{highVar}},
  #          {{visVar}},
  #          {{adyVar}},
  #          {{avalVar}})%>%
    as_tibble()
  # Get Unique cyl
  par_dt <- data %>%
    distinct(parameter)

  # Create details column
  data <- data %>%
    group_by(parameter) %>%
    summarise("_details" = list(purrr::transpose(across(everything())))) %>%
    mutate(' ' = '&oplus;')

  # Merge data
  par_dt <- left_join(par_dt, data, by = "parameter")

  # Reorder columns
  par_dt <- par_dt %>%
    select(length(par_dt), everything())

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
    "",
    "// row callback to style the rows of the child tables",
    "var rowCallback = function(row, dat, displayNum, index){",
    "  if($(row).hasClass('odd')){",
    "    $(row).css('background-color', 'papayawhip');",
    "    $(row).hover(function(){",
    "      $(this).css('background-color', '#E6FF99');",
    "    }, function() {",
    "      $(this).css('background-color', 'papayawhip');",
    "    });",
    "  } else {",
    "    $(row).css('background-color', 'lemonchiffon');",
    "    $(row).hover(function(){",
    "      $(this).css('background-color', '#DDFF75');",
    "    }, function() {",
    "      $(this).css('background-color', 'lemonchiffon');",
    "    });",
    "  }",
    "};",
    "",
    "// header callback to style the header of the child tables",
    "var headerCallback = function(thead, data, start, end, display){",
    "  $('th', thead).css({",
    "    'border-top': '3px solid indigo',",
    "    'color': 'indigo',",
    "    'background-color': '#fadadd'",
    "  });",
    "};",
    "",
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
    "                 'rowCallback': rowCallback,",
    "                 'headerCallback': headerCallback,",
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

  datatable(par_dt, callback = callback, escape = -2,
            options = list(
              columnDefs = list(
                list(visible = FALSE, targets = ncol(par_dt)),
                list(orderable = FALSE, className = 'details-control', targets = 1),
                list(className = "dt-center", targets = "_all")
              )
            ))
}
