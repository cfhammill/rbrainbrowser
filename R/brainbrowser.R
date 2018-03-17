#' @import htmlwidgets
#' @export
brainbrowser <-
  function(obj
         , intensities = NULL
         , color_map = NULL
         , min = NULL
         , max = NULL
         , zoom = 1
         , rotation = NULL
         , matrix = NULL
         , debug = FALSE
         , width = "100%"
         , height = "400px"){
    
    data <-
      list(data = obj
         , intens = intensities
         , color_map = color_map
         , min = min
         , max = max
         , zoom = zoom
         , debug = debug
         , rotation = rotation
         , matrix = matrix
           )

    data <- Filter(function(d) !is.null(d), data)
    
    htmlwidgets::createWidget("bb"
                            , data
                            , package = "rbrainbrowser"
                            , width = width, height = height)
}

#' @export
brainbrowserOutput <- function(outputId
                             , width = "100%"
                             , height = "400px") {

  shinyWidgetOutput(outputId, "bb"
                  , width
                  , height
                  , package = "rbrainbrowser")
}

#' @export
renderBrainbrowser <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, brainbrowserOutput, env, quoted = TRUE)
}
