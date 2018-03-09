#' @import htmlwidgets
#' @export
brainbrowser <-
  function(obj
         , intensities = NULL
         , color_map = NULL
         , min = NULL
         , max = NULL
         , width = "100%"
         , height = "400px"){
    
    data <-
      list(data = obj
         , intens = intensities
         , color_map = color_map
         , min = min
         , max = max
           )

    data <- Filter(function(d) !is.null(d), data)
    
    htmlwidgets::createWidget("bb"
                            , data
                            , package = "rbrainbrowser"
                            , width = width, height = height)
}

#' @export
brainbrowserOutput <- function(outputId
                             , intensities = NULL
                             , color_map = NULL
                             , width = "100%"
                             , height = "400px") {

  data <-
    list(data = obj
       , intens = intensities
       , color_map = color_map
       , min = min
       , max = max
         )

  data <- Filter(function(d) !is.null(d), data)

  shinyWidgetOutput(outputId, "bb"
                  , intensities = intensities
                  , color_map = color_map
                  , width
                  , height
                  , package = "rbrainbrowser")
}

#' @export
renderBrainbrowser <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, sigmaOutput, env, quoted = TRUE)
}
