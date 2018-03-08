#' @import htmlwidgets
#' @export
brainbrowser <- function(obj, intensities, color_map, width = "100%", height = "500px"){
  htmlwidgets::createWidget("bb", list(data = obj, intens = intensities, color_map = color_map)
                          , package = "brainbrowser"
                          , width = width, height = height)
}

#' @export
brainbrowserOutput <- function(outputId, width = "100%", height = "400px") {
  shinyWidgetOutput(outputId, "sigma", width, height, package = "sigma")
}

#' @export
renderBrainbrowser <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, sigmaOutput, env, quoted = TRUE)
}
