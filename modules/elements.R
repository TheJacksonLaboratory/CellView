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

actionEnterButton <- function(id, label) {
  ns <- NS(id)

  tags$head(tags$script(HTML(enterButtonJS)))
  actionButton(id, label)
}
