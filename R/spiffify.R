#' \code{spiffify} adds  some formatting to a ggplot to make it easier to read.
#'
#' @param base_size The base size of for ggplot. Defaults to 17.
#' @param lineheight  The lineheight for ggplot. Defaults to .8.
#' @param face Font face. Defaults to bold.
#' @keywords ggplot
#' @import ggplot2
#' @export
#' @examples
#' library(languageR)
#' library(ggplot2)
#'
#' lexdec.rt <- lexdec[lexdec$Correct=="correct",]
#'
#' ggplot(lexdec.rt, aes(x=Frequency, y=RT)) + geom_smooth() +
#'   geom_point(alpha=.1) + ggtitle("Freqyency by RT")
#'
#' ggplot(lexdec.rt, aes(x=Frequency, y=RT)) + geom_smooth() +
#'   geom_point(alpha=.1) + ggtitle("Freqyency by RT") + spiffify()

spiffify <- function(base_size = 17, lineheight=.8, face="bold",angle=45, vjust, hjust=.9){
  ggplot2::theme_grey(base_size = base_size) +
    ggplot2::theme(plot.title = element_text(size = rel(1),  lineheight=lineheight, face=face, hjust = 0),
          axis.text.x = element_text(angle = angle, vjust = vjust,  hjust = hjust, size = rel(1)))
}
