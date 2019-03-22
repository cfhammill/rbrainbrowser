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
         , bg_colour = "#FFFFFF"
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

plot_to_url <- function(plot_cmd, height, width){

  print(height)
  print(width)
  plot_cmd <- enquo(plot_cmd)
  
  recursive_eval <- function(x){
    print(x)
    if(!is.call(x)) return(x)
    recursive_eval(eval_tidy(x))
  }
    
  rendered_plot <-
    stringSVG(recursive_eval(plot_cmd)
            , height = height / 96, width = width / 96)
  
  rendered_plot %>%
    openssl::base64_encode() %>%
    paste0("url('data:image/svg+xml;base64,", ., "');")
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
             , bg_colour = "#FFFFFF"
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

bbrow <- function(old, ..., height = 1, width = 1, bg_plot = NULL){  
  new <- list(...)
  bg_plot <- enquo(bg_plot)
  
  if(inherits(old, "colfig")){
    old[[length(old) + seq_along(new)]] <- new
    if(is.null(old$bg_plot)) old$bg_plot <- bg_plot
    old
  } else {
    structure(c(list(old), new), bg_plot = bg_plot, height = height, width = width, class = "bbrow")
  }         
}

bbcol <- function(old, ..., height = 1, width = 1, bg_plot = NULL){
  new <- list(...)
  bg_plot <- enquo(bg_plot)
  
  if(inherits(old, "bbcol")){
    old[[length(old) + seq_along(new)]] <- new
    if(is.null(old$bg_plot)) old$bg_plot <- bg_plot
    old
  } else {
    structure(c(list(old), new), bg_plot = bg_plot, height = height, width = width, class = "bbcol")
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

propagate_data <- function(fig, seed = NULL) UseMethod("propagate_data")
propagate_data.bb <- function(fig, seed = NULL){
    if(!is.null(seed)){
        seed <- discard(seed, is.null)
        missing_names <- names(fig)[map_lgl(fig, is.null)]
        updates <- intersect(missing_names, names(seed))
        fig[updates] <- seed[match(updates, names(seed))]
    }
    
    fig
}
propagate_data.bbcol <- function(fig, seed = NULL){
    bbs <- keep(fig, ~ inherits(., "bb"))
    if(length(bbs) > 0){
        new_seed <- discard(bbs[[1]], is.null)
        seed[names(new_seed)] <- new_seed
    }

    for(i in 1:length(fig)){
        fig[[i]] <- propagate_data(fig[[i]], seed)
        if(inherits(fig[[i]], "bb")){
            new_seed <- discard(fig[[i]], is.null)            
            seed[names(new_seed)] <- new_seed
        } 
    }

    fig
}
propagate_data.bbrow <- function(fig, seed = NULL){
    propagate_data.bbcol(fig, seed)
}
        
reify_fig <- function(fig, height = 400, width = 600, seed = NULL){
    fig %>%
        spread_widths(width) %>%
        spread_heights(height) %>%
        propagate_data(seed)
    
}

table_style <-
    css("border-collapse" = "collapse"
      , "border" = "0px solid black"
      , "padding-top" = "0px"
      , "padding-bottom" = "0px"
      , "padding-right" = "0px"
      , "padding-left" = "0px")

fig_to_html <- function(fig){
  scene <- fig_to_html_helper(fig)

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
    

fig_to_html_helper <- function(fig){
    UseMethod("fig_to_html_helper")
}
fig_to_html_helper.bb <- function(fig){
  tags$table(style = table_style
           , tags$tr(style = table_style
                   , tags$td(style = table_style
                           , htmlwidgets:::toHTML(do.call(brainbrowser, fig)))))
}
fig_to_html_helper.bbcol <- function(fig){
  if(!is.null(attr(fig, "bg_plot"))){
    toplevel_style <-
      paste0(table_style, "background-image: "
           , plot_to_url(attr(fig, "bg_plot")
                       , view(fig, height_l)
                       , view(fig, width_l)))
  } else {
    toplevel_style <- table_style
  }
  do.call(tags$table
        , c(height = view(fig, height_l)
          , width = view(fig, width_l)
          , style = toplevel_style
          , map(fig, function(row){
            tags$tr(class = "row"
                  , width = view(row, width_l)
                  , height = view(row, height_l)
                  , style = table_style
                  , tags$td(fig_to_html(row)
                          , style = table_style))
          })))
}

fig_to_html_helper.bbrow <- function(fig){
  if(!is.null(attr(fig, "bg_plot"))){
    print("in")
    toplevel_style <-
      paste0(table_style, "background-image: "
           , plot_to_url(attr(fig, "bg_plot")
                       , view(fig, height_l)
                       , view(fig, width_l)))
  } else {
    toplevel_style <- table_style
  }
  tags$table(height = view(fig, height_l)
           , width = view(fig, width_l)
           , style = toplevel_style
           , do.call(tags$tr
                   , c(style = table_style
                     , map(fig, function(col){
                         tags$td(fig_to_html(col)
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
      bb(obj_file)
    , bb(obj_file)
    , bb(obj_file)
    , bg_plot = {print("plotting at last"); plot(1:15) }
    ))) %>%
  htmltools::browsable()
