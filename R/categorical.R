
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
  fun = getOption('metaplot_categorical','categorical_data_frame')
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
  do.call(match.fun(fun), args)
}

#' Categorical Function for Data Frame
#'
#' Categorical function for class 'data.frame'. Default panel function implements a simple mosaic plot.
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
#' @param aspect passed to \code{\link[lattice]{xyplot}}
#' @param auto.key passed to \code{\link[lattice]{xyplot}}
#' @param keycols number of auto.key columns
#' @param as.table passed to \code{\link[lattice]{xyplot}}
#' @param prepanel passed to \code{\link[lattice]{xyplot}} (guessed if NULL)
#' @param scales passed to \code{\link[lattice]{xyplot}} (guessed if NULL)
#' @param panel name or definition of panel function
#' @param colors replacements for default colors in group order
#' @param tiles whether to fill rectangles for each group: logical, or alpha values between 0 and 1
#' @param lines whether to plot borders for each group: logical, or alpha values between 0 and 1
#' @param main character, or a function of x, yvar, xvar, groups, facets
#' @param sub character, or a function of x, yvar, xvar, groups, facets
#' @param subscripts passed to \code{\link[lattice]{xyplot}}
#' @param par.settings passed to \code{\link[lattice]{xyplot}} (calculated if null)
#' @param tex tile expansion: scale factor for reducing each tile size relative to full size (<= 1)
#' @param pch symbol character for legend
#' @param ... passed to \code{\link{region}}
#' @seealso \code{\link{categorical_panel}}
#' @export
#' @import lattice
#' @importFrom tidyr gather
#' @family categorical
#' @family metaplot
#' @examples
#'
#' library(magrittr)
#' library(dplyr)
#' library(csv)
#' x <- as.csv(system.file(package = 'metaplot', 'data/theoph.csv'))
#' x %<>% pack
#' x %>% metaplot(site)
#' x %>% metaplot(arm, site)
#' x %>% metaplot(arm, site, cohort)
#' x %>% metaplot(arm, site, , cohort)

categorical_data_frame <- function(
  x,
  yvar = NULL,
  xvar,
  groups = NULL,
  facets = NULL,
  ylab = getOption('metaplot_lab',axislabel),
  xlab = getOption('metaplot_lab',axislabel),
  na.rm = getOption('metaplot_na.rm',TRUE),
  aspect = getOption('metaplot_aspect',1),
  auto.key = getOption('metaplot_auto.key',NULL),
  keycols = getOption('metaplot_keycols',NULL),
  as.table = getOption('metaplot_categorical_as.table',TRUE),
  prepanel = getOption('metaplot_categorical_prepanel', function(...)list(xlim=0:1,ylim=0:1)),
  scales = getOption('metaplot_categorical_scales',NULL),
  panel = getOption('metaplot_categorical_panel',categorical_panel),
  colors = getOption('metaplot_colors',NULL),
  tiles = getOption('metaplot_tiles',0.5),
  lines = getOption('metaplot_lines',TRUE),
  main = getOption('metaplot_main',NULL),
  sub = getOption('metaplot_sub',NULL),
  tex = getOption('metaplot_tex', 0.9),
  pch = getOption('metaplot_categorical_pch',22),
  subscripts = TRUE,
  par.settings = NULL,
  ...
){
  stopifnot(inherits(x, 'data.frame'))
  stopifnot(length(groups) <= 1)
  stopifnot(is.character(yvar) | length(yvar) == 0)
  stopifnot(is.character(xvar))
  stopifnot(length(xvar) == 1, length(yvar) <= 1)
  stopifnot(length(tex) == 1, is.numeric(tex), tex <= 1)

  if(!is.null(facets))stopifnot(is.character(facets))
  y <- x
   if(any(c('metaplot_groups','metaplot_values') %in% names(y)))
    stop('metaplot_groups and metaplot_values are reserved and cannot be column names')
 stopifnot(all(c(xvar,yvar,groups,facets) %in% names(y)))
  if(is.null(groups)){
    y$metaplot_groups <- TRUE
    groups <- 'metaplot_groups'
  }
  if(is.null(yvar)){
    y$metaplot_values <- TRUE
    yvar <- 'metaplot_values'
  }
  # groups now assigned, and yvar is singular
  if(is.null(keycols))keycols <- min(3, length(unique(y[[groups]])))
  if(is.null(auto.key))if(length(unique(y[[groups]])) > 1) auto.key <- list(columns = keycols, pch = 22, points=any(as.logical(tiles)),lines=any(as.logical(lines)))
  if(na.rm) {
    foo <- y
    y <- y[is.defined(y[[yvar]]) & is.defined(y[[xvar]]),]

    for(col in names(foo))attributes(y[[col]]) <- attributes(foo[[col]])
    at <- attributes(foo)
    at$row.names <- NULL
    for(a in names(at)) attr(y,a) <- attr(foo,a)
  }
  ff <- character(0)
  if(!is.null(facets))ff <- paste(facets, collapse = ' + ')
  if(!is.null(facets))ff <- paste0('|',ff)
  formula <- as.formula(yvar %>% paste(sep = '~', xvar) %>% paste(ff))
  if(is.null(scales)) scales <- list(relation = 'free', draw = TRUE, tck = c(1,0), alternating = FALSE, col = 'transparent')
  if(is.character(ylab)) ylab <- tryCatch(match.fun(ylab), error = function(e)ylab)
  if(is.function(ylab)) ylab <- ylab(y, var = yvar, ...)
  ylab <- base::sub('metaplot_values','',ylab)

  if(is.character(xlab)) xlab <- tryCatch(match.fun(xlab), error = function(e)xlab)
  if(is.function(xlab)) xlab <- xlab(y, var = xvar, ...)

  # if (is.null(groups)) # cannot be null at this point
  y[[groups]] <- ifcoded(y, groups)
  if(!is.null(main))if(is.function(main)) main <- main(x = y,yvar = yvar, xvar = xvar, groups = groups, facets = facets, ...)
  if(!is.null(sub))if(is.function(sub)) sub <- sub(x = y, yvar = yvar, xvar = xvar, groups = groups, facets = facets, ...)

  groups <- as.formula(paste('~',groups))
  if(!is.null(facets)){
    for (i in seq_along(facets)) y[[facets[[i]]]] <- ifcoded(y, facets[[i]])
  }

  if(!is.null(tiles)) tiles <- as.numeric(tiles)
  if(!is.null(lines)) lines <- as.numeric(lines)
  sym <- list(
    col = colors,
    alpha = tiles,
    pch = pch
  )
  line <- list(
    col = colors,
    alpha = lines
  )
  sym <- sym[sapply(sym,function(i)!is.null(i))]
  line <- line[sapply(line,function(i)!is.null(i))]
  pars <- list(
    superpose.symbol = sym,
    superpose.line = line
  )
  pars <- pars[sapply(pars, function(i)length(i) > 0 )]

  xyplot(
    formula,
    data = y,
    groups = groups,
    auto.key = auto.key,
    as.table = as.table,
    aspect = aspect,
    scales = scales,
    prepanel = prepanel,
    ylab = ylab,
    xlab = xlab,
    panel = panel,
    subscripts = subscripts,
    par.settings = if(is.null(par.settings)) pars else par.settings,
    main = main,
    sub = sub,
    .data = y,
    tex = tex,
    pch = pch,
    ...
  )
}

#' Panel Function for Metaplot Categorical Plot
#'
#' Default panel function for categorical_data_frame. Implements a simple mosaic plot.
#'
#' @export
#' @param x x values
#' @param y y values
#' @param groups optional grouping item
#' @param loc where to print statistics in a tile
#' @param msg a function of x and y to print text in a tile
#' @param tex tile expansion: scale factor for reducing each tile size relative to full size (<= 1)
#' @param cex expansion for msg text
#' @param subscripts subscripts of the original data for this panel
#' @param ... passed to \code{link[lattice]{panel.superpose}}
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
  loc = getOption('metaplot_loc',5),
  msg = getOption('metaplot_msg','tilestats'),
  tex = getOption('metaplot_tex', 0.9),
  cex = getOption('metaplot_categorical_panel_cex',1),
  subscripts,
  ...
)
{

  if(is.na(msg)) msg <- function(x,y)''
  if(is.character(msg)) msg <- match.fun(msg)

  d <- data.frame(
    x = x,
    y = y,
    g = groups[subscripts]
  )
  tot <- nrow(d)
  tiles  <- d %>% group_by(x,y,g) %>% summarize(n = n(), tf = n()/tot, msg = msg(x,y)) # tile fraction
  frames <- tiles %>% group_by(x,y) %>% summarize(ff = sum(tf)) # frame fraction
  spars <- frames %>% group_by(x) %>% summarize(sf = sum(ff)) # spar fraction
  spars %<>% mutate(sh = cumsum(sf)) # spar high
  spars %<>% mutate(sl = lag(sh,default = 0)) # spar low
  suppressMessages(frames %<>% left_join(spars) %>% arrange(x,y))
  # sh and sl are spar high and low (x dimension)
  # frames are nested within spars, sharing their x limits
  # from these we can calculate frame y limits
  frames %<>% mutate(fd = ff / (sh - sl)) # sh - sl happens to be sf
  # frame delta y is fractional area div by delta x
  # each frame is stacked in a fixed-width spar
  # within spar (defined by x) cumsum fd gives frame high
  frames %<>% group_by(x) %>% mutate(fh = cumsum(fd))
  frames %<>% group_by(x) %>% mutate(fl = lag(fh, default = 0))
  # now we know the limits of each frame
  # fd * sf should be ff
  # frame defined by x and y, and bounded by sh, sl, fh, fl
  suppressMessages(tiles %<>% left_join(frames %>% select(x,y,sl,sh, fl,fh)) %>% arrange(x,y,g))
  # tiles have the same high and low as corresponding frame.
  # tiles in a frame (x, y group) split delta sl, sh proportionally
  tiles %<>% group_by(x,y) %>% mutate(width = (sh - sl) / sum(tf) * tf)
  # tile high is spar low plus cumsum width (within frame)
  tiles %<>% group_by(x,y) %>% mutate(th = sl + cumsum(width))
  # tile low is high of previous tile within frame (defined by x, y)
  tiles %<>% group_by(x,y) %>% mutate(tl = lag(th))
  # if no previous, default to spar low
  tiles %<>% mutate(tl = ifelse(is.na(tl), sl, tl))
  # now the limits of each tile are completely defined by th, tl, fh, fl
  # draw tiles one group at a time
  tiles %<>% select(x,y,g,n,tl,th,fl,fh,msg)

  superpose.line <- trellis.par.get()$superpose.line
  superpose.symbol <- trellis.par.get()$superpose.symbol
  panel.superpose(
    x = tiles$x,
    y = tiles$y,
    groups = tiles$g,
    .src = tiles,
    panel.groups = panel_tile,
    fill.alpha = superpose.symbol$alpha,
    line.alpha = superpose.line$alpha,
    #border = superpose.line$col,
    col = superpose.symbol$col,
    col.symbol = superpose.symbol$col,
    col.line = superpose.line$col,
    loc = loc,
    msg = msg,
    tex = tex,
    cex = cex,
    subscripts = seq_along(tiles$x),
    ...
  )
  yax <-
    data.frame(y = y) %>%
    arrange(y) %>%
    mutate(tot = n()) %>%
    group_by(tot, y) %>%
    summarize(frac = n()/unique(tot)) %>%
    mutate(max = cumsum(frac)) %>%
    mutate(prev = lag(max, default = 0)) %>%
    mutate(at = (max + prev)/2) %>%
    ungroup %>%
    select(y,at)

  xax <-
    data.frame(x = x) %>%
    arrange(x) %>%
    mutate(tot = n()) %>%
    group_by(tot, x) %>%
    summarize(frac = n()/unique(tot)) %>%
    mutate(max = cumsum(frac)) %>%
    mutate(prev = lag(max, default = 0)) %>%
    mutate(at = (max + prev)/2) %>%
    ungroup %>%
    select(x,at)

  xscale <- current.viewport()$xscale
  yscale <- current.viewport()$yscale
  pushViewport(viewport(width=2, height=2, clip=TRUE))
  pushViewport(viewport(width=.5, height=.5,
                         xscale=xscale, yscale=yscale))



  if(nrow(yax) > 1) panel.axis(
    side = 'left',
    at = yax$at,
    labels = yax$y,
    outside = TRUE,
    half = FALSE,
    ticks = FALSE,
    rot = c(90,90)
  )


  if(nrow(xax) > 1) panel.axis(
    side = 'bottom',
    at = xax$at,
    labels = xax$x,
    outside = TRUE,
    half = FALSE,
    ticks = FALSE,
    rot = c(0,0)
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
#' @param fill.alpha alpha transparency for fill
#' @param line.alpha alpha transparency for line
#' @param col.line border color
#' @param col fill color
#' @param loc location for output of msg
#' @param msg ignored
#' @param .src data source for which subscripts give x, y, msg,  and tile limits
#' @param alpha ignored
#' @param tex tile expansion: scale factor for reducing each tile size relative to full size (<= 1)
#' @param cex expansion for msg text; passed to msg
#' @family panel functions
#' @family categorical
#'
#'
panel_tile <- function(x, y, subscripts, group.number, group.value, fill.alpha, line.alpha, col, col.line, loc, msg, .src, alpha,cex, tex, ...){
  line.alpha <- rep(line.alpha, length.out = group.number) # maybe not distributed by superpose
  line.alpha <- rev(line.alpha)[[1]]
  for(i in subscripts)one_rect(
    x = x[i],
    y = y[i],
    xleft=.src$tl[i],
    xright=.src$th[i],
    ybottom=.src$fl[i],
    ytop=.src$fh[i],
    fill.alpha = fill.alpha,
    line.alpha = line.alpha,
    col = col,
    loc = loc,
    msg = .src$msg[i],
    cex = cex,
    tex = tex,
    ...
  )
}


one_rect <- function(
  x, y, xleft, xright, ybottom, ytop,
  tex, fill.alpha, line.alpha, col,
  msg=NA, loc, cex,
  ...
){
  coords <- smaller(xleft, xright, ybottom, ytop, tex)
  xleft <- coords$xleft
  xright <- coords$xright
  ybottom <- coords$ybottom
  ytop <- coords$ytop

  panel.rect(
    xleft = xleft,
    xright = xright,
    ybottom = ybottom,
    ytop = ytop,
    alpha = fill.alpha,
    border = 'transparent',
    col = col
  )
  poly <- data.frame(
    x = c(xleft, xright, xright,xleft,xleft),
    y= c(ybottom, ybottom, ytop, ytop, ybottom)
  )
 panel.lines(x = poly$x, y = poly$y, alpha = line.alpha, col = col)
 if(!is.na(msg)) panel.text(
   x = xpos(loc, lo = xleft, hi = xright),
   y = ypos(loc, lo = ybottom, hi = ytop),
   labels = msg,
   cex = cex
  )
}

smaller <- function(xleft, xright, ybottom, ytop, tex,...){
  stopifnot(tex <= 1)
  width = xright - xleft
  height = ytop - ybottom
  area = width * height * tex
  aspect = height/width
  w = sqrt(area/aspect)
  h = aspect * w
  dh = height - h
  dw = width - w
  ymid = (ytop + ybottom)/2 # midpoint of y
  # anchor point is proportional to throw
  # throw is ymid relative to possible ymid
  # possible ymid range from height/2 to 1 - height/2
  ymin = height/2
  ymax = 1 - height/2
  yrange = ymax - ymin
  ythrow = 0
  if(yrange > 0) ythrow = (ymid - ymin)/yrange

  xmid = (xright + xleft)/2
  xmin = width/2
  xmax = 1 - width/2
  xrange = xmax - xmin
  xthrow = 0
  if(xrange > 0) xthrow = (xmid - xmin)/xrange
  # if throw is zero, anchor bottom of box
  # if throw is 1, anchor top of box
  # pro-rate other throws
  list(
    ybottom = ybottom + dh * ythrow,
    ytop = ytop - dh * (1 - ythrow),
    xleft = xleft + dw * xthrow,
    xright = xright - dw * (1 - xthrow)
  )
}

#' Format Tile Statistics
#'
#' Formats statistics for a mosaic tile.
#'
#' @export
#' @param x x values
#' @param y y values
#' @param family categorical family
#' @param ... other arguments
#' @return character
#' @seealso \code{\link{categorical_panel}}
#'
tilestats <- function(x, y, ...)as.character(length(x))

