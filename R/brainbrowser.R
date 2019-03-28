#' Generate a brainbrowser html widget of a given obj
#'
#' Sets up a simple htmlwidget displaying a mesh with
#' brainbrowser
#'
#' @param obj The object to display
#' @param intensities Intensity values, one per mesh vertex
#' @param color_map color values, one per mesh vertex
#' @param min minimum for the displayed intensities
#' @param max maximum for the displayed intensities
#' @param A zoom level for the mesh
#' @param debug whether to append the viewer object to
#' the html window (no longer necessary, the viewer is attached
#' to the widget dom element).
#' @param bg_colour The background colour for the widget, an empty
#' string has a clear background.
#' @param bg_plot A command to produce a plot in the background of a widget
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


#' An abstract representation of a brainbrowser widget
#'
#' This is used to construct layouts containing multiple widgets.
#' Primarily just stores the arguments to evaluate at render time.
#' @inheritParams brainbrowser
#' @export
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

#' Abstract row layout for brainbrowser widgets.
#'
#' Takes one or more layout objects (bb, bbrow, and bbcols) and assembles them
#' into a row. Data can be passed in via the `data` argument, this can be used to
#' fill missing data into the child layout objects. The width is
#' divided amongst the children by their relative widths.
#'
#' @param old A layout object `bb`, `bbrow`, or `bbcol`
#' @param ... More unnamed layout objects
#' @param height A height for the row
#' @param width A width for the row
#' @param data A list containing arguments to `bb` to be applied where child layout
#' objects are missing arguments. This can be used to set the same mesh for the whole
#' row for example.
#' @export
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

#' Abstract column layout for brainbrowser widgets.
#'
#' Takes one or more layout objects (bb, bbrow, and bbcols) and assembles them
#' into a column. Data can be passed in via the `data` argument, this can be used to
#' fill missing data into the child layout objects.
#'
#' @param old A layout object `bb`, `bbrow`, or `bbcol`
#' @param ... More unnamed layout objects
#' @param height A height for the column
#' @param width A width for the column
#' @param data A list containing arguments to `bb` to be applied where child layout
#' objects are missing arguments. This can be used to set the same mesh for the whole
#' column for example.
#' @export
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

#' A lens into the width of a layout object
#' @export
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

#' Spread a width across a layout
#'
#' Expand layout objects proportionality to fit a fixed width.
#'
#' @param fig A layout
#' @param width The width to fill
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

#' A lens into the height of a layout object
#' @export
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

#' Spread a height across a layout
#'
#' Expand layout objects proportionality to fit a fixed height.
#'
#' @param fig A layout
#' @param height The height to fill
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

#' Unify data
#'
#' Fill in missing layout data from a reference list of arguments
#' to `bb`
#'
#' @param fig a layout
#' @param data the reference data
#' @export
unify_data <- function(fig, data = NULL) UseMethod("unify_data")
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

#' Materialize a figure
#'
#' Materialize the figure by recursively spreading the layout to
#' fit a fixed width and height
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

#' Figure to html
#'
#' Convert a layout to an html page full of widgets.
#' 
#' @param fig the layout
#' @param unit_w the css unit to use for the widths
#' @param unit_h the css unit to use for the heights
#' @export
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

#' Take a screenshot
#'
#' Take a snapshot of an html page using the chrome drive
#'
#' @param site the url to image
#' @param file a png file to save the output
#' @param port an integer port number for the chrome driver
#' @param height a height in pixels for the page viewport
#' @param width a width in pixels for the page viewport
#' @param version the version of the chrome driver to use
#' @export
snap_page <- function(site, file = "test.png", port = 4869L
                    , height = 800, width = 1280
                    , version = NULL){
  drv <- wdman::chrome(port = port, version = version) #"73.0.3683.68"
  sel <-
    RSelenium::remoteDriver(
                 port = port, browser = "chrome",
               , extraCapabilities = list(
                   chromeOptions = list(
                     args = c("--headless"
                            , paste0("--window-size=", width, ",", height))
                   )))
  
  sel$open()
  sel$navigate(paste0("file://", site))
  sel$screenshot(file = file)
  invisible(sel)
}



## obj1 <-
##   fromJSON(
##     gzcon(
##       url("https://github.com/aces/brainbrowser/raw/master/examples/models/dbs-vat.json.gz")), simplifyDataFrame = FALSE)

## obj2 <-
##   fromJSON(
##     gzcon(
##       url("https://github.com/aces/brainbrowser/raw/master/examples/models/dbs.json.gz")), simplifyDataFrame = FALSE)

## fig_to_html(
##   reify_fig(
##     bbrow(
##       bbcol(
##         bb()
##       , bb(obj1)
##       , bb()
##       , bg_plot = {
##         (iris %>%
##           ggplot(aes(x = Petal.Width, y = Sepal.Length)) +
##           geom_point() ) %>% print
##       }
##       , data = list(obj = obj2, zoom = 3)
##       )
##     , bb(bg_plot = plot(1:5))
##     , data = list(obj = obj1, zoom = 4)
##     )
##   , height = 90, width = 180)
##  , unit_w = "vmin", unit_h = "vmin"
## ) %>%
##   htmltools::html_print()
