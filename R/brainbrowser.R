#' @import htmlwidgets lenses htmltools
#' @importFrom purrr keep discard map_lgl map
#' @importFrom svglite stringSVG
#' @importFrom rlang eval_tidy enquo
#' @include utils-pipe.R
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
         , bg_colour = ""
         , bg_plot = NULL
         , width = "100%"
         , height = "400px"){

    bg_plot <- enquo(bg_plot)
    bg_plot <- plot_to_url(bg_plot, width = width, height = height)    
  
    data <-
      list(data = obj
         , intens = intensities
         , color_map = color_map
         , min = min
         , max = max
         , zoom = zoom
         , debug = debug
         , bg_colour = bg_colour
         , rotation = rotation
         , bg_plot = bg_plot
         , matrix = matrix
           )

    data <- Filter(function(d) !is.null(d), data)
    
    htmlwidgets::createWidget("bb"
                            , data
                            , package = "rbrainbrowser"
                            , width = width, height = height)
  }

plot_to_url <- function(plot_cmd, height, width, scale = 4){
  width <- as.numeric(gsub("[^0-9]", "", width)) 
  height <- as.numeric(gsub("[^0-9]", "", height))
  aspect <- height / (width + height)
  new_height <- aspect * (scale * 2)
  new_width  <- (1 - aspect) * (scale * 2)
  
  plot_cmd <- enquo(plot_cmd)
  
  recursive_eval <- function(x){
    if(!is.call(x)) return(x)
    recursive_eval(eval_tidy(x))
  }
    
  rendered_plot <-
    stringSVG(recursive_eval(plot_cmd)
            , height = new_height, width = new_width)
  
  rendered_plot %>%
    openssl::base64_encode() %>%
    paste0("url('data:image/svg+xml;base64,", ., "')")
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


bb <- function(obj = NULL
             , intensities = NULL
             , color_map = NULL
             , min = NULL
             , max = NULL
             , zoom = NULL
             , rotation = NULL
             , matrix = NULL
             , debug = FALSE
             , bg_colour = ""
             , bg_plot = NULL
             , width = 1
             , height = 1){

  bg_plot <- enquo(bg_plot)
  mc <- match.call()[-1]
  args <- formals()
  args[names(mc)] <- mc
  args$bg_plot <- bg_plot
  
  structure(args, class = "bb")
}

bbrow <- function(old, ..., height = 1, width = 1, bg_plot = NULL, data = NULL){  
  new <- list(...)
  bg_plot <- enquo(bg_plot)
  
  if(inherits(old, "colfig")){
    old[[length(old) + seq_along(new)]] <- new
    if(is.null(attr(old, "bg_plot")))
      attr(old, "bg_plot") <- bg_plot
    over_map(old, indexes_l(), ~ unify_data(., data) )
  } else {
    structure(c(list(old), new), bg_plot = bg_plot
            , height = height, width = width, class = "bbrow") %>%
      over_map(indexes_l(), ~ unify_data(., data) )
  }         
}

bbcol <- function(old, ..., height = 1, width = 1, bg_plot = NULL, data = NULL){
  new <- list(...)
  bg_plot <- enquo(bg_plot)
  
  if(inherits(old, "bbcol")){
    old[[length(old) + seq_along(new)]] <- new
    if(is.null(attr(old, "bg_plot")))
      attr(old, "bg_plot") <- bg_plot
    
    over_map(old, indexes_l(), ~ unify_data(., data) )
  } else {
    structure(c(list(old), new), bg_plot = bg_plot
            , height = height, width = width, class = "bbcol") %>%
      over_map(indexes_l(), ~ unify_data(., data) )
  }         
}

`%cb%` <- bbrow
`%rb%` <- bbcol

width_l <-
    lenses::lens(view =
             function(x){
                 if(inherits(x, "bb")) return(x$width)
                 if(inherits(x, "bbcol")) return(attr(x, "width"))
                 if(inherits(x, "bbrow")) return(attr(x, "width"))
                 stop("width_l is not valid for object of class ", class(x))
             }
       , set =
             function(x, d){
                 if(inherits(x, "bb")) return(set(x, c_l("width"), d))
                 if(inherits(x, "bbcol")) return(set(x, attr_l("width"), d))
                 if(inherits(x, "bbrow")) return(set(x, attr_l("width"), d))
                 stop("width_l is not valid for object of class ", class(x))
             })

spread_widths <- function(fig, width) UseMethod("spread_widths")
spread_widths.bb <- function(fig, width) set(fig, c_l("width"), width)
spread_widths.bbrow <- function(fig, width){
    widths_l <- map_l(width_l) %.% unlist_l

    fig_updated <-
        set(fig, width_l, width) %>%
        over(widths_l, function(ws){
            (ws / sum(ws)) * width
        }) %>%
        over_map(indexes_l(), function(ch){
            spread_widths(ch, view(ch, width_l))
        })
    
    fig_updated
}
spread_widths.bbcol <- function(fig, width){
    widths_l <- map_l(width_l) %.% unlist_l

    fig_updated <-
        set(fig, width_l, width) %>%
        set(widths_l, rep(width, length(fig))) %>%    
        over_map(indexes_l(), function(ch){
            spread_widths(ch, view(ch, width_l))
        })

    fig_updated    
}

height_l <-
    lenses::lens(view =
             function(x){
                 if(inherits(x, "bb")) return(x$height)
                 if(inherits(x, "bbcol")) return(attr(x, "height"))
                 if(inherits(x, "bbrow")) return(attr(x, "height"))
                 stop("height_l is not valid for object of class ", class(x))
             }
       , set =
             function(x, d){
                 if(inherits(x, "bb")) return(set(x, c_l("height"), d))
                 if(inherits(x, "bbcol")) return(set(x, attr_l("height"), d))
                 if(inherits(x, "bbrow")) return(set(x, attr_l("height"), d))
                 stop("height_l is not valid for object of class ", class(x))
             })

spread_heights <- function(fig, height) UseMethod("spread_heights")
spread_heights.bb <- function(fig, height) set(fig, c_l("height"), height)
spread_heights.bbrow <- function(fig, height){
    heights_l <- map_l(height_l) %.% unlist_l

    fig_updated <-
        set(fig, height_l, height) %>%        
        set(heights_l, rep(height, length(fig))) %>%
        over_map(indexes_l(), function(ch){
            spread_heights(ch, view(ch, height_l))
        })

    fig_updated    
}
spread_heights.bbcol <- function(fig, height){
    heights_l <- map_l(height_l) %.% unlist_l

    fig_updated <-
        set(fig, height_l, height) %>%
        over(heights_l, function(ws){
            (ws / sum(ws)) * height
        }) %>%
        over_map(indexes_l(), function(ch){
            spread_heights(ch, view(ch, height_l))
        })
    
    
}

unify_data <- function(fig, seed = NULL) UseMethod("unify_data")
unify_data.bb <- function(fig, data){
    if(!is.null(data)){
        data <- discard(data, is.null)
        missing_names <- names(fig)[map_lgl(fig, is.null)]
        updates <- intersect(missing_names, names(data))
        fig[updates] <- data[match(updates, names(data))]
    }
    
    fig
}
unify_data.bbcol <- function(fig, data){
    over_map(fig, indexes_l(), ~ unify_data(., data) )
}
unify_data.bbrow <- function(fig, data){
    unify_data.bbcol(fig, data)
}
        
reify_fig <- function(fig, height = 400, width = 600){
    fig %>%
        spread_widths(width) %>%
        spread_heights(height)
    
}

table_style <-
    css("border-collapse" = "collapse"
      , "border" = "0px solid black"
      , "padding-top" = "0px"
      , "padding-bottom" = "0px"
      , "padding-right" = "0px"
      , "padding-left" = "0px")

fig_to_html <- function(fig, unit_w = "px", unit_h = "px"){
  scene <- fig_to_html_helper(fig, unit_w = unit_w, unit_h = unit_h)

  ## add_style <- function(tag){
  ##   if(!is.null(tag$name) && tag$name %in% c("table", "tr", "td", "div"))
  ##     tag$attribs$style <- table_style

  ##   if(is.null(tag$children) || length(tag$children) == 0)
  ##     return(tag)
    
  ##   return(over_map(tag, c_l("children"), add_style))
  ## }

  ## add_style(scene)
  scene
}
    

fig_to_html_helper <- function(fig, unit_w = "px", unit_h = "px"){
    UseMethod("fig_to_html_helper")
}
fig_to_html_helper.bb <- function(fig, unit_w = "px", unit_h = "px"){
  fig$width <- paste0(fig$width, unit_w)
  fig$height <- paste0(fig$height, unit_h)
  tags$table(style = table_style
           , tags$tr(style = table_style
                   , tags$td(style = table_style
                           , htmlwidgets:::toHTML(do.call(brainbrowser, fig)))))
}
fig_to_html_helper.bbcol <- function(fig, unit_w = "px", unit_h = "px"){
  new_width <- paste0(view(fig, width_l), unit_w)
  new_height <- paste0(view(fig, height_l), unit_h)
  
  if(!is.null(attr(fig, "bg_plot"))){
    toplevel_style <-
      paste0(table_style, "background-image: "
           , plot_to_url(attr(fig, "bg_plot")
                       , new_height
                       , new_width)
           , ";"
             )
  } else {
    toplevel_style <- table_style
  }
  do.call(tags$table
        , c(height = new_height
          , width = new_width
          , style = toplevel_style
          , map(fig, function(row){
            tags$tr(class = "row"
                  , width = paste0(view(row, width_l), unit_w)
                  , height = paste0(view(row, height_l), unit_h)
                  , style = table_style
                  , tags$td(fig_to_html(row, unit_w = unit_w, unit_h = unit_h)
                          , style = table_style))
          })))
}

fig_to_html_helper.bbrow <- function(fig, unit_w = "px", unit_h = "px"){
  new_width <- paste0(view(fig, width_l), unit_w)
  new_height <- paste0(view(fig, height_l), unit_h)
  if(!is.null(attr(fig, "bg_plot"))){
    toplevel_style <-
      paste0(table_style, "background-image: "
           , plot_to_url(attr(fig, "bg_plot")
                       , new_height
                       , new_width)
           , ";"
             )
  } else {
    toplevel_style <- table_style
  }
  tags$table(height = new_height
           , width = new_width
           , style = toplevel_style
           , do.call(tags$tr
                   , c(style = table_style
                     , map(fig, function(col){
                         tags$td(fig_to_html(col, unit_w = unit_w, unit_h = unit_h)
                               , style = table_style)
                       }))))
}

## x <- 
## fig_to_html(
##     reify_fig(
##       (bb(obj_file) %rb% bb() %cb% bb(obj_file)))) %>%
##   identity ##htmltools::browsable()

## fig_to_html(
##     reify_fig(
##       bb(obj_file
##        , bg_colour = "#00FF00"
##        , bg_plot = plot(1:20)
##          ))) %>%
##   htmltools::browsable()

fig_to_html(
  reify_fig(
    bbrow(
      bbcol(
        bb()
      , bb(obj_file)
      , bb()
      , bg_plot = plot(1:15)
      , data = list(obj = obj2, zoom = 3)
      )
    , bb(bg_plot = plot(1:5))
    , data = list(obj = obj_file, zoom = 4)
    )
  )) %>%
  htmltools::html_print()

eCaps <- list(
  chromeOptions = list(
    args = c('--headless', '--window-size=1280,800')))

snap_page <- function(site, file = "test.png", port = 4869L){
  drv <- wdman::chrome(port = port, version = "73.0.3683.68")
  sel <- remoteDriver(port = port, browser = "chrome",
                    , extraCapabilities = list(
                        chromeOptions = list(
                          args = c('--headless', '--window-size=1280,800')))
)
  sel$open()
  sel$navigate(paste0("file://", site))
  sel$screenshot(file = file)
  invisible(sel)
}
