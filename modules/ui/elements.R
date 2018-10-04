enterButtonJS <- '
  $(function() {
    var $els = $("[data-proxy-click]");
    $.each( $els, function(idx, el) {
      var $el = $(el);
      var $proxy = $("#" + $el.data("proxyClick"));
      $el.keydown(function (e) {
        if (e.keyCode == 13) { $proxy.click(); }
      });
    });
  });
'
tags$head(tags$script(HTML(enterButtonJS)))

actionEnterButton <- function(id, label) {
  ns <- NS(id)

  tags$head(tags$script(HTML(enterButtonJS)))
  actionButton(id, label)
}

dimensionSelection <- function(id, label, default = 'V1') {
  ns <- NS(id)

  column(
    2,
    selectInput(
      id,
      label = label,
      choice = c('V1', 'V2', 'V3'),
      selected = default
    )
  )
}
