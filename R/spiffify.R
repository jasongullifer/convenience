#' \code{spiffify} adds  some formatting to a ggplot to make it easier to read.
#'
#' @param basesize The basesize of for ggplot. Defaults to 17.
#' @param lineheight  The lineheight for ggplot. Defaults to .8.
#' @param face Font face. Defaults to bold.
#' @keywords ggplot
#' @export
#' @examples
#' ggplot() + spiffify()

spiffify <- function(base_size = 17, lineheight=.8, face="bold"){
ggplot2::theme_grey(base_size = base_size) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = rel(1), lineheight=lineheight, face=face))
}
