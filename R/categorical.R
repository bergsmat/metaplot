
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

  # if groups was not passed as missing, prime etc can be defined in terms of num
  # must reserve at least one xvar.
  # find first categorical in position 2 or later
  pos <- seq_along(var)
  can <- pos > 1
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


############################

#' Categorical Function for Data Frame
#'
#' Categorical function for class 'data.frame'.
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
#' @param ... passed to \code{\link{region}}
#' @seealso \code{\link{categorical_panel}}
#' @export
#' @import lattice
#' @importFrom tidyr gather
#' @family categorical
#' @family metaplot

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
  lines = getOption('metaplot_lines',FALSE),
  main = getOption('metaplot_main',NULL),
  sub = getOption('metaplot_sub',NULL),
  subscripts = TRUE,
  par.settings = NULL,
  ...
){
  stopifnot(inherits(x, 'data.frame'))
  stopifnot(length(groups) <= 1)
  stopifnot(is.character(yvar) | length(yvar) == 0)
  stopifnot(is.character(xvar))
  stopifnot(length(xvar) == 1, length(yvar) <= 1)

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
    y$yvar <- 'metaplot_values'
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
  if(is.null(scales)) scales <- list(relation = 'free', tck = c(1,0), alternating = FALSE, col = 'transparent')
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
    alpha = tiles
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
#' @param msg a function to print text in a tile: called with x values, y values, and \dots.
#' @param ... passed to msg
#' @family panel functions
#' @family categorical
#' @seealso \code{\link{tilestats}}
#' @seealso \code{\link{categorical.data.frame}}
#'
categorical_panel <- function(
  x,
  y,
  groups,
  loc = getOption('metaplot_loc',5),
  msg = getOption('metaplot_msg','tilestats'),
  ...
)
{
  d <- data.frame(
    x = x,
    y = y,
    g = groups
  )
  tot <- nrow(d)
  tiles  <- d %>% group_by(x,y,g) %>% summarize(n = n(), tf = n()/tot) # tile fraction
  frames <- tiles %>% group_by(x,y) %>% summarize(ff = sum(tf)) # frame fraction
  spars <- frames %>% group_by(x) %>% summarize(sf = sum(ff)) # spar fraction
  spars %<>% mutate(sh = cumsum(sf)) # spar high
  spars %<>% mutate(sl = lag(sh,default = 0)) # spar low
  frames %<>% left_join(spars) %>% arrange(x,y)
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
  tiles %<>% left_join(frames %>% select(x,y,sl,sh, fl,fh)) %>% arrange(x,y,g)
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
  tiles %<>% select(x,y,g,n,tl,th,fl,fh)


  superpose.line <- trellis.par.get()$superpose.line
  superpose.symbol <- trellis.par.get()$superpose.symbol
  panel.superpose(
    x = tiles$x,
    y = tiles$y,
    groups = tiles$g,
    .src = tiles,
    panel.groups = panel.tile,
    alpha = superpose.symbol$alpha,
    border = superpose.line$col,
    col = superpose.symbol$col,
    loc = loc,
    msg = msg,
    ...
  )
}

#' Draw a Tile
#'
#' Draws a tile in a mosaic.
#'
#' @export
#' @param x x values
#' @param y y values
#' @param subscripts
#' @param group.number group number
#' @param group.value group value
#' @param alpha alpha transparency
#' @param border border color
#' @param col fill color
#' @param loc location for output of msg
#' @param msg text message as a function of x and y
#' @param .src data source for which subscripts give x, y, and tile limits
#' @family panel functions
#' @family categorical
#'
#'
panel.tile <- function(x, y, subscripts, group.number, group.value, alpha, border, col, loc, msg, .src,...){
  panel.rect(
    xleft = .src$tl[subscripts],
    xright = .src$th[subscripts],
    ybottom = .src$fl[subscripts],
    ytop = .src$fh[subscripts],
    alpha = alpha,
    border = border,
    col = col,
    ...
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

