#' @import htmlwidgets
#' @export
brainbrowser <- function(f){
  htmlwidgets::createWidget("brainbrowser", list(f = f, dir = getwd()))
}
