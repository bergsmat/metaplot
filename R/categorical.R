globalVariables(c(
'group_by','g','summarize', 'n','tf',
'ff','mutate','sf','n','frac','prev','ungroup',
'at',  'arrange','at','fd','ff','fh','fl','lag','left_join','n',
'select','sf','sh','sl','summarize','tf','th','tl','ungroup','width',
'left','right','top','bottom',
'tot', 'y', 'f1', 'f2'
))

#' Categorical Plot
#'
#' Categorical Plot. Generic, with method for 'data.frame'.
#' @export
#' @param x object of dispatch
#' @param ... passed arugments
#' @family generic functions
#' @family categorical
categorical <- function(x, ...)UseMethod('categorical')

#' Categorical Method for Data Frame
#'
#' Categorical method for 'data.frame'.
#'
#' @param x data.frame
#' @param fun function to draw the plot
#' @param verbose generate messages describing process
#' @param ... other arguments
#' @family categorical
#' @family methods
#' @return character
#' @export
#' @importFrom rlang quos
#'
categorical.data.frame <- function(
  x,
  ...,
  fun = metOption('categorical','categorical_data_frame'),
  verbose = metOption('verbose_categorical_data_frame',FALSE)
){
  args <- quos(...)
  args <- lapply(args,f_rhs)
  vars <- args[names(args) == '']
  other <- args[names(args) != '']
  vars <- sapply(vars, as.character)

  # this function needs to explicitly assign xvar, yvar, groups, and facets
  # prime is all yvar, if present, and xvar
  # all prime are categorical by definition
  # all groups and facets, if any, are also categorical
  # therefore all vars are categorical
  # one vars is treated as xvar
  # two vars is yvar, xvar
  # three vars is yvar, xvar, groups
  # all other vars are facets
  # groups may be skipped
  # prime is defined as all vars before groups or facets, if present
  # non-prime start with the first missing or categorical in position 2 or later
  # since groups may be missing, checking properties may fail
  # isolate non-prime
  missing <- match('',vars)
  if(is.defined(missing)){
    vars <- vars[-missing]
  }
  # now we have protected vars from missingness, but preserved info from missing group, if any

  # test presence
  stopifnot(all(vars %in% names(x)))

  # if groups was not passed as missing, prime etc can be defined in terms of vars
  # if length one: xvar
  # if length two: yvar xvar
  # if length three: yvar xvar grp
  # if length four: yvar xvar grp facet
  # must reserve at least one xvar.
  # find first categorical in position 3 or later
  pos <- seq_along(vars)
  can <- pos > 2
  grp <- match(TRUE, can)

  # we now have var, giving the names of all real variables
  # missing is NA, or one greater than the last prime
  # grp is NA, or the position of the first (remaining) non-prime
  # x is last position in var not greater than missing or grp
  xlim <- min(na.rm = T, missing, grp, length(vars) + 1)
  xpos <- xlim - 1
  xvar <- vars[xpos]
  yvar <- vars[seq_len(xpos -1)]
  if(length(yvar) == 0) yvar <- NULL
  if(length(yvar) > 1) stop('only one yvar supported')
  groups <- NULL
  facets <- NULL
  more <- character(0)
  if(length(vars) > xpos) more <- vars[(xpos+1):length(vars)]
  # first additional is groups if missing:NA
  if(length(more) & is.na(missing)){
    groups <- more[[1]]
    more <- more[-1]
  }
  # any remaining are facets
  if(length(more)) facets <- more

  formal <- list(
    x = x,
    yvar = yvar,
    xvar = xvar,
    groups = groups,
    facets = facets
  )
  args <- c(formal, other)
  if(verbose){
    if(is.character(fun))message('calling ', fun) else message('calling fun')
  }
  do.call(match.fun(fun), args)
}

#' Categorical Function for Data Frame
#'
#' Categorical function for class 'data.frame'. Implements a simple mosaic plot.
#'
#'
#' @param x data.frame
#' @param yvar character: y variable (optional)
#' @param xvar character: x variable
#' @param groups optional grouping variable (can be missing)
#' @param facets optional conditioning variables
#' @param ylab y axis label; can be function(x = x, var = yvar, ..)
#' @param xlab x axis label; can be function(x = x, var = xvar, ..)
#' @param na.rm whether to remove data points with one or more missing coordinates
#' @param aspect passed to \code{\link[lattice]{bwplot}} or ggplot; use 'fill', NA, or NULL to calculate automatically
#' @param space location of key (right, left, top, bottom)
#' @param key list: passed to \code{\link[lattice]{xyplot}} as \code{auto.key} or to \code{\link[ggplot2]{theme}}; can be a function groups name, groups levels, fill, lines, space, gg, type ('categorical'), and \dots .  See \code{\link{metaplot_key}}.
#' @param as.table passed to \code{\link[lattice]{xyplot}}
#' @param prepanel passed to \code{\link[lattice]{xyplot}} (guessed if NULL)
#' @param scales passed to \code{\link[lattice]{xyplot}} or \code{\link[ggplot2]{facet_grid}} or \code{\link[ggplot2]{facet_wrap}} (guessed if NULL)
#' @param panel name or definition of panel function for lattice
#' @param colors replacements for default colors in group order; can be length one integer to auto-select that many colors
#' @param fill whether to fill rectangles for each group: logical, or alpha values between 0 and 1
#' @param lines whether to plot borders for each group: logical, or alpha values between 0 and 1
#' @param main character, or a function of x, yvar, xvar, groups, facets
#' @param sub character, or a function of x, yvar, xvar, groups, facets
#' @param subscripts passed to \code{\link[lattice]{xyplot}}
#' @param settings default parameter settings: a list from which matching elements are passed to lattice (as par.settings) or  to ggplot theme()  and facet_wrap() or facet_grid().  \code{ncol} and \code{nrow} are used as layout indices for lattice (for homology with facet_wrap).
#' @param padding numeric (will be recycled to length 4) giving plot margins in default units: top, right, bottom, left (in multiples of 5.5 points for ggplot)
#' @param tex tile expansion: scale factor for reducing each tile size relative to full size (<= 1)
#' @param rot rotation for axis labels; can be length 2 for y and x axes, respectively
#' @param loc where to print statistics in a tile
#' @param msg a function of x and y to print text in a tile
#' @param cex expansion for msg text
#' @param gg logical: whether to generate \code{ggplot} instead of \code{trellis}
#' @param verbose generate messages describing process
#' @param ... passed to \code{\link{region}}
#' @seealso \code{\link{categorical_panel}}
#' @export
#' @import lattice
#' @importFrom tidyr gather
#' @importFrom scales hue_pal
#' @family categorical
#' @family metaplot
#' @examples
#'
#' library(magrittr)
#' library(dplyr)
#' library(csv)
#' x <- as.csv(system.file(package = 'metaplot', 'extdata/theoph.csv'))
#' x %<>% pack
#' x %>% metaplot(site)
#' x %>% metaplot(site, gg = T)
#' x %>% metaplot(arm, site)
#' x %>% metaplot(arm, site, gg = T)
#' x %>% metaplot(arm, site, cohort)
#' x %>% metaplot(arm, site, cohort, gg = T)
#' x %>% metaplot(arm, site, cohort, space = 'top')
#' x %>% metaplot(arm, site, , cohort)
#' x %>% metaplot(arm, site, , cohort, gg = T)
#' \donttest{
#' x %>% metaplot(arm, site, , cohort, rot = c(0,90))
#' x %>% metaplot(arm, site, , cohort, rot = c(0,90), gg = T)
#' x %>% metaplot(arm, site, , cohort, rot = c(45, 45))
#' x %>% metaplot(subject,cohort,arm, site, lines = F, rot = c(45,45))
#' x %>% metaplot(subject,cohort,arm, site, lines = F, rot = c(45,45), gg=T)
#' # panel-specific axis not well-supported for gg version
#' x %>% metaplot(subject,cohort,,arm, site)
#' x %>% metaplot(subject,cohort,,arm, site, gg=T)
#' }



categorical_data_frame <- function(
  x,
  yvar = NULL,
  xvar,
  groups = NULL,
  facets = NULL,
  ylab = metOption('xlab_categorical','axislabel'),
  xlab = metOption('ylab_categorical','axislabel'),
  na.rm = metOption('na.rm_categorical',TRUE),
  aspect = metOption('aspect_categorical',1),
  space = metOption('space_categorical','right'),
  key = metOption('key_categorical','metaplot_key'),
  as.table = metOption('as.table_categorical',TRUE),
  prepanel = metOption('prepanel_categorical', function(...)list(xlim=0:1,ylim=0:1)),
  scales = metOption('scales_categorical',NULL),
  panel = metOption('panel_categorical','categorical_panel'),
  colors = metOption('colors_categorical',NULL),
  fill = metOption('fill_categorical',0.5),
  lines = metOption('lines_categorical',TRUE),
  main = metOption('main_categorical',NULL),
  sub = metOption('sub_categorical',NULL),
  tex = metOption('tex_categorical', 0.9),
  rot = metOption('rot_categorical',c(90,0)),
  subscripts = metOption('subscripts_categorical',TRUE),
  settings = metOption('settings_categorical',NULL),
  padding = metOption('padding_categorical', 1),
  loc = metOption('loc_categorical',5),
  msg = metOption('msg_categorical','tilestats'),
  cex = metOption('cex_categorical',1),
  gg = metOption('gg_categorical',FALSE),
  verbose = metOption('verbose_categorical', FALSE),
  ...
){
  if(verbose)cat('this is categorical_data_frame')
  settings <- as.list(settings)
  if(is.null(names(settings))) names(settings) <- character(0)
  aspect <- metaplot_aspect(aspect, gg)
  stopifnot(inherits(x, 'data.frame'))
  stopifnot(length(groups) <= 1)
  stopifnot(is.character(yvar) | length(yvar) == 0)
  stopifnot(is.character(xvar))
  stopifnot(length(xvar) == 1, length(yvar) <= 1)
  stopifnot(length(tex) == 1, is.numeric(tex), tex <= 1)
  stopifnot(is.numeric(padding))
  padding <- rep(padding, length.out = 4)
  par.settings <- settings[names(settings) %in% names(trellis.par.get())]
  par.settings <- parintegrate(par.settings, padding)
  if(gg)padding <- unit(padding * 5.5, 'pt')
  stopifnot(is.numeric(rot))
  rot <- rep(rot, length.out = 2)

  if(!is.null(facets))stopifnot(is.character(facets))
  #x <- x
   if(any(c('metaplot_groups','metaplot_values') %in% names(x)))
    stop('metaplot_groups and metaplot_values are reserved and cannot be column names')
 stopifnot(all(c(xvar,yvar,groups,facets) %in% names(x)))
  if(is.null(groups)){
    x$metaplot_groups <- TRUE
    groups <- 'metaplot_groups'
    #key = if(gg)'none' else FALSE
  }
  bivariate <- TRUE
  if(is.null(yvar)){
    x$metaplot_values <- TRUE
    yvar <- 'metaplot_values'
    bivariate <- FALSE
  }
  # groups now assigned, and yvar is singular
   x[[yvar]] <- as_factor(x[[yvar]])
   x[[xvar]] <- as_factor(x[[xvar]])

  #if(is.null(keycols))keycols <- min(3, length(unique(x[[groups]])))
  #if(is.character(key))if(length(key == 1))if(key %in% c('left','right','top','bottom'))if(!gg)key <- list(space = key, pch = 22, points=any(as.logical(tiles)),lines=any(as.logical(lines)))
  if(na.rm) {
    foo <- x
    x <- x[is.defined(x[[yvar]]) & is.defined(x[[xvar]]),]

    for(col in names(foo))attributes(x[[col]]) <- attributes(foo[[col]])
    at <- attributes(foo)
    at$row.names <- NULL
    for(a in names(at)) attr(x,a) <- attr(foo,a)
  }
  ff <- character(0)
  if(!is.null(facets))ff <- paste(facets, collapse = ' + ')
  if(!is.null(facets))ff <- paste0('|',ff)
  formula <- as.formula(yvar %>% paste(sep = '~', xvar) %>% paste(ff))
  ylev <- as.character(sort(unique(x[[yvar]])))
  if(yvar == 'metaplot_values') ylev <- ''
  xlev <- as.character(sort(unique(x[[xvar]])))
  if(is.null(scales) && gg) scales <- 'free'
  if(is.null(scales)) scales <- list(
    relation = 'free',
    draw = TRUE,
    tck = c(0,0),
    alternating = FALSE,
    col = 'transparent',
    x = list(
      at = seq(from = 0, to = 1, length.out = length(xlev)),
      labels = xlev,
      rot = rot[[2]]
    ),
    y = list(
      at = seq(from = 0, to = 1, length.out = length(ylev)),
      labels = ylev,
      rot = rot[[1]]
    )
  )
  if(is.character(ylab)) ylab <- tryCatch(match.fun(ylab), error = function(e)ylab)
  if(is.function(ylab)) ylab <- ylab(x, var = yvar, ...)
  ylab <- base::sub('metaplot_values','',ylab)

  if(is.character(xlab)) xlab <- tryCatch(match.fun(xlab), error = function(e)xlab)
  if(is.function(xlab)) xlab <- xlab(x, var = xvar, ...)

  # if (is.null(groups)) # cannot be null at this point
  x[[groups]] <- as_factor(x[[groups]])
  if(!is.null(main))if(is.function(main)) main <- main(x = x,yvar = yvar, xvar = xvar, groups = groups, facets = facets, ...)
  if(!is.null(sub))if(is.function(sub)) sub <- sub(x = x, yvar = yvar, xvar = xvar, groups = groups, facets = facets, ...)

  #if(!gg)groups <- as.formula(paste('~',groups))
  if(!is.null(facets)){
    for (i in seq_along(facets)) x[[facets[[i]]]] <- as_factor(x[[ facets[[i]] ]])
  }
  nlev <- length(levels(x[[groups]]))
  levs <- levels(x[[groups]])
  if(is.null(colors)) {
    if(gg){
      colors <- hue_pal()(nlev)
      if(nlev == 1) colors <- '#0080ff'
    } else {
      colors <- trellis.par.get()$superpose.symbol$col
    }
  }
  if(is.numeric(colors))colors <- hue_pal()(colors[[1]])
  if(is.null(fill)) fill <- 0.5 # same as default
  if(is.null(lines)) lines <- TRUE # same as default
  fill <- as.numeric(fill)
  lines <- as.numeric(lines)
  colors <- rep(colors, length.out = nlev)
  fill <- rep(fill, length.out = nlev)
  fill[fill == 0] <- 0.000000001 # key borders are not drawn if fill == 0
  lines <- rep(lines, length.out = nlev)
  # par.settings is defined
  poly <- trellis.par.get()$superpose.polygon
  poly$col <- alpha(colors, fill)
  poly$alpha <- 1
  poly$border <- alpha(colors, fill)
  # poly <- list(
  #   col = alpha(colors, fill),
  #   alpha = 1,
  #   border = alpha(colors,lines)
  # )
  if(is.null(par.settings$superpose.polygon)) par.settings$superpose.polygon <- poly
  if(gg)if(length(facets) > 2) facets <- facets[1:2]
  if(is.character(key)) key <- match.fun(key)
  if(is.function(key)) key <- key(
    groups = groups,
    levels = levs,
    fill = fill,
    lines = lines,
    space = space,
    gg = gg,
    type = 'categorical',
    verbose = verbose,
    ...
  )

  if(gg){
    nms <- c(xvar, yvar, groups, facets)
    # dat <- tiles(x=x[,xvar], y = x[,yvar], g = x[,groups], tex = tex, msg = msg)
    dat <- x[,nms, drop = F]
    if(ncol(dat) == 3) names(dat) <- c('x','y','g')
    if(ncol(dat) == 4) names(dat) <- c('x','y','g', 'f1')
    if(ncol(dat) == 5) names(dat) <- c('x','y','g', 'f1', 'f2')
    if(verbose)cat('calling tiles')
    dat <- tiles(dat, tex = tex, msg = msg, verbose = verbose)
    if(length(facets) >= 1) names(dat)[names(dat) == 'f1'] <- facets[[1]]
    if(length(facets) == 2) names(dat)[names(dat) == 'f2'] <- facets[[2]]
    yax <- cax(x[[yvar]])
    xax <- cax(x[[xvar]])
    plot <- ggplot(data = dat) +
    geom_rect(
      mapping = aes(
        xmin = left,
        xmax = right,
        ymin = bottom,
        ymax = top,
        color = g,
        fill = g
      )
    ) +
    ggtitle(main, subtitle = sub) + theme_bw()
    plot <- plot + scale_x_continuous(name = xlab, labels = xax$val, breaks = xax$at)
    plot <- plot + labs(color = groups, fill = groups)
    plot <- plot + geom_text(mapping = aes(x = xpos(loc, lo = left, hi = right), y = ypos(loc, lo = bottom, hi = top), label = msg))
    theme_settings <- list(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      aspect.ratio = aspect,
      plot.margin = padding,
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_text(angle = rot[[2]]),
      legend.title = element_blank()
    )
    theme_settings <- merge(theme_settings, key)
    theme_extra <- settings[names(settings) %in% names(formals(theme))]
    theme_settings <- merge(theme_settings, theme_extra)
    plot <- plot + do.call(theme, theme_settings)
    if(bivariate) {
      plot <- plot + scale_y_continuous(name = ylab, labels = yax$val, breaks = yax$at)
      plot <- plot + theme(axis.text.y = element_text(angle = rot[[1]]))
    } else {
      plot <- plot + theme(axis.title.y = element_blank(), axis.text.y = element_blank())
    }
    levs <- length(unique(dat$g))
    if(is.null(colors)) {
      colors <- hue_pal()(levs)
      if(levs == 1) colors <- 'black'
    }
    # fill <- rep(fill, length.out = levs) # see above
    # lines <- rep(lines, length.out = levs) # see above
    plot <- plot +
      scale_color_manual(values = alpha(colors, lines)) + # i.e. "border"
      scale_fill_manual(values = alpha(colors, fill))
  facet_args <- list()
  if(length(facets) == 1) facet_args[[1]] <- facets[[1]] #list(facets[[1]], scales = scales)
  if(length(facets) > 1)  facet_args[[1]] <- as.formula(
    paste(
      sep='~',
      facets[[1]],
      facets[[2]]
    )
  )
  facet_args$scales <- scales
  facet_extra <- list()
  if(length(facets) == 1) facet_extra <- settings[names(settings) %in% names(formals(facet_wrap))]
  if(length(facets) >  1) facet_extra <- settings[names(settings) %in% names(formals(facet_grid))]
  facet_args <- merge(facet_args, facet_extra)
  if(length(facets) == 1) plot <- plot + do.call(facet_wrap, facet_args)
  if(length(facets) >  1) plot <- plot + do.call(facet_grid, facet_args)
  return(plot)
  }

  args <- list(
    formula,
    data = x,
    groups = as.formula(paste('~',groups)),
    auto.key = key,
    as.table = as.table,
    aspect = aspect,
    scales = scales,
    prepanel = prepanel,
    ylab = ylab,
    xlab = xlab,
    panel = panel,
    subscripts = subscripts,
    par.settings = par.settings,
    main = main,
    sub = sub,
    .data = x,
    tex = tex,
    rot = rot,
    bivariate = bivariate,
    loc = loc,
    msg = msg,
    cex = cex,
    verbose = verbose
  )
 args <- c(args, list(...))
 if(all(c('ncol','nrow') %in% names(settings))){
   layout <- c(settings$ncol, settings$nrow)
   args <- c(args, list(layout = layout))
 }
 if(verbose)cat('calling xyplot')
 do.call(xyplot, args)
}

#' Panel Function for Metaplot Categorical Plot
#'
#' Default panel function for categorical_data_frame. Implements a simple mosaic plot.
#' Global options are supported but typically are supplied by the calling function and
#' may therefore be unreachable.
#'
#' @export
#' @param x x values
#' @param y y values
#' @param groups optional grouping item
#' @param bivariate whether to create y axis
#' @param loc where to print statistics in a tile
#' @param msg a function of x and y to print text in a tile
#' @param tex tile expansion: scale factor for reducing each tile size relative to full size (<= 1)
#' @param cex expansion for msg text
#' @param rot rotation for axis labels; can be length 2 for y and x axes, respectively
#' @param subscripts subscripts of the original data for this panel
#' @param verbose generate messages describing process
#' @param ... passed to \code{\link[lattice]{panel.superpose}}
#' @family panel functions
#' @family categorical
#' @import grid
#' @seealso \code{\link{tilestats}}
#' @seealso \code{\link{categorical.data.frame}}
#'
categorical_panel <- function(
  x,
  y,
  groups,
  bivariate = TRUE,
  loc = metOption('loc_categorical_panel',5),
  msg = metOption('msg_categorical_panel','tilestats'),
  tex = metOption('tex_categorical_panel', 0.9),
  cex = metOption('cex_categorical_panel',1),
  rot = metOption('rot_categorical_panel',c(90,0)),
  subscripts,
  verbose = metOption('verbose_categorical_panel',FALSE),
  ...
)
{
  if(verbose)cat('this is categorical_panel')
  if(is.null(rot)) rot <- c(90,0) # same as default
  if(length(rot) == 1) rot <- c(rot,rot)
  if(is.na(msg)) msg <- function(x,y)''
  if(is.character(msg)) msg <- match.fun(msg)
  if(verbose)cat('calling tiles')
  tiles <- tiles(data.frame(x = x, y = y, g = groups[subscripts]), tex = tex, msg = msg, verbose = verbose)

  superpose.polygon <- trellis.par.get()$superpose.polygon
  if(verbose)cat('calling panel.superpose')
  panel.superpose(
    x = tiles$x,
    y = tiles$y,
    groups = tiles$g,
    .src = tiles,
    panel.groups = panel_tile,
    col = superpose.polygon$col, # superpose.symbol$col,
    alpha = superpose.polygon$alpha,
    border = superpose.polygon$border,
    # line.alpha = 1, #superpose.$alpha,
    #border = superpose.line$col,
    # col.symbol = superpose.symbol$col,
    # col.line = superpose.line$col,
    loc = loc,
    msg = msg,
    #tex = 1,  tex used to be passed to panel_tile
    cex = cex,
    #pch = pch,
    rot = rot,
    subscripts = seq_along(tiles$x),
    verbose = verbose,
    ...
  )
  yax <- cax(y)
  xax <- cax(x)
  xscale <- current.viewport()$xscale
  yscale <- current.viewport()$yscale
  pushViewport(viewport(width=2, height=2, clip=TRUE))
  pushViewport(viewport(width=.5, height=.5,xscale=xscale, yscale=yscale))
  if(verbose)cat('calling panel.axis')
  if(bivariate) panel.axis(
    side = 'left',
    at = yax$at,
    labels = yax$val,
    outside = TRUE,
    half = FALSE,
    ticks = FALSE,
    rot = c(rot[[1]],rot[[1]])
  )

panel.axis(
    side = 'bottom',
    at = xax$at,
    labels = xax$val,
    outside = TRUE,
    half = FALSE,
    ticks = FALSE,
    rot = c(rot[[2]],rot[[2]])
  )
  popViewport(2)

}

#' Draw a Tile
#'
#' Draws a tile in a mosaic.
#'
#' @export
#' @param x x values
#' @param y y values
#' @param subscripts subscripts
#' @param group.number group number
#' @param group.value group value
#' @param col fill color
#' @param alpha alpha transparency for fill
#' @param border border color
#' @param loc location for output of msg
#' @param msg ignored
#' @param .src data source for which subscripts give x, y, msg,  and tile limits
#' @param cex expansion for msg text; passed to msg
#' @param verbose generate messages describing process
#' @param ... passed arguments
#' @family panel functions
#' @family categorical
#'
#'
panel_tile <- function(
  x, y, subscripts, group.number, group.value,
  col,
  alpha, #fill.alpha,
  border, #line.alpha,
  # col.line,
  loc, msg, .src,cex,verbose,...){ #  tex
  # next two lines still necessary?
  # line.alpha <- rep(line.alpha, length.out = group.number) # maybe not distributed by superpose
  # line.alpha <- rev(line.alpha)[[1]] # take group.numberth value
  if(verbose)cat('this is panel_tile')
  for(i in subscripts)one_rect(
    x = x[i],
    y = y[i],
    left=.src$left[i],
    right=.src$right[i],
    bottom=.src$bottom[i],
    top=.src$top[i],
    col = col,
    alpha = alpha, # fill.alpha = fill.alpha,
    border = border[[group.number]], # line.alpha = line.alpha,
    loc = loc,
    msg = .src$msg[i],
    cex = cex,
    verbose = verbose,
   # tex = tex,
    ...
  )
}


one_rect <- function(
  x, y, left, right, bottom, top, # tex
#  fill.alpha, line.alpha, col,
  col, alpha, border,
  msg=NA, loc, cex, verbose = FALSE,
  ...
){
  # coords <- smaller(left = left, right = right, top = top, bottom = bottom, tex = tex)
  # left <- coords$left
  # right <- coords$right
  # bottom <- coords$bottom
  # top <- coords$top
  if(verbose)cat('this is one_rect')
  panel.rect(
    xleft = left,
    xright = right,
    ybottom = bottom,
    ytop = top,
    alpha = alpha,
    border = border, #'transparent',
    col = col
  )
 #  poly <- data.frame(
 #    x = c(left, right, right,left,left),
 #    y= c(bottom, bottom, top, top, bottom)
 #  )
 # panel.lines(x = poly$x, y = poly$y, alpha = line.alpha, col = col)
 if(!is.na(msg))if(verbose)cat('calling panel.text')
 if(!is.na(msg)) panel.text(
   x = xpos(loc, lo = left, hi = right),
   y = ypos(loc, lo = bottom, hi = top),
   labels = msg,
   cex = cex
  )
}

smaller <- function(left, right, bottom, top, tex,...){
  stopifnot(tex <= 1)
  width = right - left
  height = top - bottom
  area = width * height * tex
  aspect = height/width
  w = sqrt(area/aspect)
  h = aspect * w
  # now h/w gives same aspect as height/width
  # now h*w gives area width * height * tex
  l = left
  b = bottom
  r = l + w
  t = b + h
  # now tiles are downsized proportional
  # now tiles are positioned as close to origin as possible
  dh = height - h
  dw = width - w
  # but we have dh and dw as margins to play with
  # we want to add some fraction of dh to t and b
  # we want to add some fraction of dw to l and r
  # we want to add half of dw if mid(left, right) was 0.5
  # in general, we want to add dw * mid(left, right)
  run <- left/2 + right/2
  rise <- bottom/2 + top/2
  l = l + dw * run
  r = r + dw * run
  b = b + dh * rise
  t = t + dh * rise
  # each tile will have the same aspect ratio as original
  # each tile will have the same area reduction as all others
  # each tile will be plotted somewhere within its original area
  # (since dw and dh are maximum displacements and run, rise <= 1)

  data.frame(
    left = l,
    right = r,
    top = t,
    bottom = b
  )
}

#' Format Tile Statistics
#'
#' Formats statistics for a mosaic tile.
#'
#' @export
#' @param x x values
#' @param y y values
#' @family categorical family
#' @param ... other arguments
#' @return character
#' @seealso \code{\link{categorical_panel}}
#'
tilestats <- function(x, y, ...)as.character(length(x))

#' Calculate Tile Limits
#'
#' Calculates limits for mosaic tiles
#'
#' @export
#' @param x a data.frame with at least columns x, y, and g, possibly f1 and f2 (facets)
#' @param tex tile shrinkage <= 1
#' @param msg a function of x and y to create a tile message
#' @param verbose generate messages describing process
#' @param ... other arguments
#' @family categorical family
#' @return data.frame
#' @importFrom dplyr bind_cols
#' @seealso \code{\link{categorical_panel}}
#'
tiles <- function(x, ...,  tex = 0.9, msg = 'tilestats', verbose = FALSE){ # y f1, f2, tot
  if(verbose)cat('this is tiles')
  stopifnot(all(c('x','y','g') %in% names(x)))
  if(!'f1' %in% names(x)) x$f1 <- 1
  if(!'f2' %in% names(x)) x$f2 <- 1
  x %<>% select(x, y, g, f1, f2)
  x %<>% group_by(f1, f2)
  x %<>% mutate(tot = n())
  tiles  <- x %>% group_by(f1, f2, x, y, g) %>% summarize(n = n(), tf = n()/unique(tot), msg = match.fun(msg)(x,y)) # tile fraction
  frames <- tiles %>% group_by(f1, f2, x, y) %>% summarize(ff = sum(tf)) # frame fraction
  spars <- frames %>% group_by(f1, f2, x) %>% summarize(sf = sum(ff)) # spar fraction
  spars %<>% mutate(sh = cumsum(sf)) # spar high
  spars %<>% mutate(sl = lag(sh,default = 0)) # spar low
  suppressMessages(frames %<>% left_join(spars) %>% arrange(f1, f2, x, y))
  # sh and sl are spar high and low (x dimension)
  # frames are nested within spars, sharing their x limits
  # from these we can calculate frame y limits
  frames %<>% mutate(fd = ff / (sh - sl)) # sh - sl happens to be sf
  # frame delta y is fractional area div by delta x
  # each frame is stacked in a fixed-width spar
  # within spar (defined by x) cumsum fd gives frame high
  frames %<>% group_by(f1, f2, x) %>% mutate(fh = cumsum(fd))
  frames %<>% group_by(f1, f2, x) %>% mutate(fl = lag(fh, default = 0))
  # now we know the limits of each frame
  # fd * sf should be ff
  # frame defined by x and y, and bounded by sh, sl, fh, fl
  suppressMessages(tiles %<>% left_join(frames %>% select(f1, f2, x, y, sl, sh, fl, fh)) %>% arrange(f1, f2, x, y, g))
  # tiles have the same high and low as corresponding frame.
  # tiles in a frame (x, y group) split delta sl, sh proportionally
  tiles %<>% group_by(f1, f2, x, y) %>% mutate(width = (sh - sl) / sum(tf) * tf)
  # tile high is spar low plus cumsum width (within frame)
  tiles %<>% group_by(f1, f2, x, y) %>% mutate(th = sl + cumsum(width))
  # tile low is high of previous tile within frame (defined by x, y)
  tiles %<>% group_by(f1, f2, x, y) %>% mutate(tl = lag(th))
  # if no previous, default to spar low
  tiles %<>% mutate(tl = ifelse(is.na(tl), sl, tl))
  # now the limits of each tile are completely defined by th, tl, fh, fl
  # draw tiles one group at a time
  tiles %<>% select(f1, f2, x, y, g, n, tl, th, fl, fh, msg)
  # now we can shrink each tile a bit
  reduced <- with(tiles, smaller(left = tl, right = th, top = fh, bottom = fl, tex = tex))
  tiles <- bind_cols(tiles, reduced)
  tiles
}

#' Calculate Categorical Axis Labels and Positions
#'
#' Calculates axis labels and positions for categorical values.
#'
#' @export
#' @param x x values
#' @family categorical family
#' @param ... other arguments
#' @return data.frame
#' @seealso \code{\link{categorical_panel}}
#'
cax <- function(x, ...)data.frame(x = x) %>%
    arrange(x) %>%
    mutate(tot = n()) %>%
    group_by(tot, x) %>%
    summarize(frac = n()/unique(tot)) %>%
    mutate(max = cumsum(frac)) %>%
    mutate(prev = lag(max, default = 0)) %>%
    mutate(at = (max + prev)/2) %>%
    ungroup %>%
    select(val=x,at)


