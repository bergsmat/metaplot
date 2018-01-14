globalVariables('metaplot_groups')
globalVariables('metaplot_values')

#' Scatterplot
#'
#' Scatterplot.
#'
#' @param x object
#' @param ... passed arguments
#' @export
#' @family generic functions
#' @family scatter
#' @family bivariate plots
#' @family metaplot
scatter <- function(x,...)UseMethod('scatter')

#' Scatterplot Function for Data Frame
#'
#' Scatterplot function for class 'data.frame'.
#'
#'
#' @param x data.frame
#' @param yvar character: y variable(s)
#' @param xvar character: x variable
#' @param groups optional grouping variable; ignored if more than one \code{yvar}
#' @param facets optional conditioning variables
#' @param log a default shared by \code{ylog} and \code{xlog}
#' @param ylog log transform y axis (guessed if NULL)
#' @param xlog log transform x axis (guessed if NULL)
#' @param yref reference line from y axis; can be function(x = x, var = yvar, ...){...}
#' @param xref reference line from x axis; can be function(x = x, var = xvar, ...){...}
#' @param ysmooth supply loess smooth of y on x
#' @param xsmooth supply loess smmoth of x on y
#' @param ylab y axis label; can be function(x = x, var = yvar, log = ylog, ..){...}
#' @param xlab x axis label; can be function(x = x, var = xvar, log = xlog, ..){...}
#' @param iso plot line of unity
#' @param crit if ylog or xlog missing, log transform if mean/median ratio for non-missing values is greater than crit
#' @param na.rm whether to remove data points with one or more missing coordinates
#' @param aspect passed to \code{\link[lattice]{xyplot}}
#' @param auto.key passed to \code{\link[lattice]{xyplot}}
#' @param keycols number of auto.key columns
#' @param as.table passed to \code{\link[lattice]{xyplot}}
#' @param prepanel passed to \code{\link[lattice]{xyplot}} (guessed if NULL)
#' @param scales passed to \code{\link[lattice]{xyplot}} (guessed if NULL)
#' @param panel name or definition of panel function
#' @param colors replacements for default colors in group order
#' @param symbols replacements for default symbols in group order
#' @param points whether to plot points for each group: logical, or alpha values between 0 and 1
#' @param lines whether to plot lines for each group: logical, or alpha values between 0 and 1
#' @param main character, or a function of x, yvar, xvar, groups, facets, and log
#' @param sub character, or a function of x, yvar, xvar, groups, facets, and log

#' @param ... passed to \code{\link{region}}
#' @seealso \code{\link{metapanel}}
#' @export
#' @import lattice
#' @importFrom tidyr gather
#' @family bivariate plots
#' @family scatter
#' @examples
#' library(magrittr)
#' library(dplyr)
#' attr(Theoph$conc,'label') <- 'theophylline concentration'
#' attr(Theoph$conc,'guide') <- 'mg/L'
#' attr(Theoph$Time,'label') <- 'time'
#' attr(Theoph$Time,'guide') <- 'h'
#' attr(Theoph$Subject,'guide') <- '////'
#' scatter_data_frame(Theoph, 'conc', 'Time')
#' scatter_data_frame(Theoph, 'conc','Time', 'Subject')
#' scatter_data_frame(Theoph, 'conc','Time', facets = 'Subject')
#' scatter_data_frame(Theoph %>% filter(conc > 0), 'conc','Time', 'Subject',ylog = TRUE, yref = 5)
#' scatter_data_frame(Theoph, 'conc','Time', 'Subject',ysmooth = TRUE)
#' scatter_data_frame(Theoph, 'conc','Time', 'Subject',ysmooth = TRUE,global = TRUE)
#' scatter_data_frame(Theoph, 'conc','Time', conf = TRUE, loc = 3, yref = 6)
#' scatter_data_frame(Theoph, 'conc','Time', conf = TRUE, loc = 3, yref = 6)


scatter_data_frame <- function(
  x,
  yvar,
  xvar,
  groups = NULL,
  facets = NULL,
  log = getOption('metaplot_log',FALSE),
  ylog = getOption('metaplot_ylog',log),
  xlog = getOption('metaplot_xlog',log),
  yref = getOption('metaplot_ref',NULL),
  xref = getOption('metaplot_ref',NULL),
  ylab = getOption('metaplot_lab',axislabel),
  xlab = getOption('metaplot_lab',axislabel),
  ysmooth = getOption('metaplot_ysmooth',FALSE),
  xsmooth = getOption('metaplot_xsmooth',FALSE),
  iso = getOption('metaplot_iso',FALSE),
  crit = getOption('metaplot_crit',1.3),
  na.rm = getOption('metaplot_na.rm',TRUE),
  aspect = getOption('metaplot_aspect',1),
  auto.key = getOption('metaplot_auto.key',NULL),
  keycols = getOption('metaplot_keycols',NULL),
  as.table = getOption('metaplot_scatter_as.table',TRUE),
  prepanel = getOption('metaplot_scatter_prepanel',NULL),
  scales = getOption('metaplot_scatter_scales',NULL),
  panel = getOption('metaplot_scatter_panel',metapanel),
  colors = getOption('metaplot_colors',NULL),
  symbols = getOption('metaplot_symbols',NULL),
  points = getOption('metaplot_points',TRUE),
  lines = getOption('metaplot_lines',FALSE),
  main = getOption('metaplot_main',NULL),
  sub = getOption('metaplot_sub',NULL),
  ...
){
  # yref
  if(is.character(yref)) yref <- match.fun(yref)
  if(is.function(yref)) yref <- yref(x = x, var = yvar,...)
  yref <- as.numeric(yref)
  yref <- yref[is.defined(yref)]
  # xref
  if(is.character(xref)) xref <- match.fun(xref)
  if(is.function(xref)) xref <- xref(x = x, var = yvar,...)
  xref <- as.numeric(xref)
  xref <- xref[is.defined(xref)]

  stopifnot(inherits(x, 'data.frame'))
  stopifnot(length(groups) <= 1)
  stopifnot(is.character(yvar))
  stopifnot(is.character(xvar))
  stopifnot(length(xvar) == 1)

  if(!is.null(facets))stopifnot(is.character(facets))
  y <- x
  stopifnot(all(c(xvar,yvar,groups,facets) %in% names(y)))
  if(any(c('metaplot_groups','metaplot_values') %in% names(y)))
      stop('metaplot_groups and metaplot_values are reserved and cannot be column names')
  if(length(yvar) > 1){
    if(is.null(keycols))if(length(yvar) > 1)keycols <- 1
    suppressWarnings(y %<>% gather(metaplot_groups, metaplot_values, !!!yvar))
    groups <- 'metaplot_groups'
    labs <- sapply(yvar, function(col){
      a <- attr(x[[col]], 'label')
      if(is.null(a)) a <- ''
      a
    })
    if(!any(labs == '')){
      attr(y[['metaplot_groups']],'guide') <- encode(yvar,labs)
    } else {
      attr(y[['metaplot_groups']],'guide') <- encode(yvar)
    }
    gui <- sapply(yvar, function(col){
      a <- attr(x[[col]], 'guide')
      if(is.null(a)) a <- ''
      a
    })
    gui <- unique(gui)
    if(length(gui) == 1) attr(y$metaplot_values, 'guide') <- gui
    yvar <- 'metaplot_values'
  }
  if(is.null(groups)){
    y$metaplot_groups <- TRUE
    groups <- 'metaplot_groups'
  }
  if(is.null(keycols))keycols <- min(3, length(unique(y[[groups]])))
  if(is.null(auto.key))if(length(unique(y[[groups]])) > 1) auto.key <- list(columns = keycols,points=any(points),lines=any(lines))
  if(na.rm) {
    #y %<>% filter(is.defined(UQ(yvar)) & is.defined(UQ(xvar))) # preserves attributes
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
  default_log <- function(x,crit){
    x <- x[!is.na(x)]
    all(x > 0) && (mean(x)/median(x) > crit)
  }
  if(ylog %>% is.null) ylog <- default_log(y[[yvar]], crit)
  if(xlog %>% is.null) xlog <- default_log(y[[xvar]], crit)

  if(ylog)if(any(y[[yvar]] <= 0, na.rm = TRUE)){
    warning(yvar, ' must be positive for log scale')
    ylog <- FALSE
  }
  if(xlog)if(any(y[[xvar]] <= 0, na.rm = TRUE)){
    warning(xvar, ' must be positive for log scale')
    xlog <- FALSE
  }

  if(ylog) yref <- log10(yref[yref > 0])
  if(xlog) xref <- log10(xref[xref > 0])

  yscale = list(log = ylog,equispaced.log = FALSE)
  xscale = list(log = xlog,equispaced.log = FALSE)

  if(is.null(scales)) scales <- list(y = yscale,x = xscale,tck = c(1,0),alternating = FALSE)

  if(is.character(ylab)) ylab <- tryCatch(match.fun(ylab), error = function(e)ylab)
  if(is.function(ylab)) ylab <- ylab(y, var = yvar, log = ylog, ...)
  ylab <- base::sub('metaplot_values','',ylab)

  if(is.character(xlab)) xlab <- tryCatch(match.fun(xlab), error = function(e)xlab)
  if(is.function(xlab)) xlab <- xlab(y, var = xvar, log = xlog, ...)

  # if (is.null(groups)) # cannot be null at this point
  y[[groups]] <- ifcoded(y, groups)
  if(!is.null(main))if(is.function(main)) main <- main(x = y,yvar = yvar, xvar = xvar, groups = groups, facets = facets, log = log, ...)
  if(!is.null(sub))if(is.function(sub)) sub <- sub(x = y, yvar = yvar, xvar = xvar, groups = groups, facets = facets, log = log, ...)

  groups <- as.formula(paste('~',groups))
  if(!is.null(facets)){
    for (i in seq_along(facets)) y[[facets[[i]]]] <- ifcoded(y, facets[[i]])
  }
  if(is.null(prepanel))if(iso) prepanel <- function(x,y,...){
    lim = c(min(x,y,na.rm = T),max(x,y,na.rm = T))
    list(
      xlim = lim,
      ylim = lim
    )
  }

  if(!is.null(points)) points <- as.numeric(points)
  if(!is.null(lines)) lines <- as.numeric(lines)
  sym <- list(
    col = colors,
    alpha = points,
    pch = symbols
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
    yref = yref,
    xref = xref,
    ysmooth = ysmooth,
    xsmooth = xsmooth,
    ylab = ylab,
    xlab = xlab,
    iso = iso,
    panel = panel,
    subscripts = TRUE,
    par.settings = pars,
    main = main,
    sub = sub,
    .data = y,
    ...
  )
}

#' Scatterplot Method for Data Frame
#'
#' Scatterplot method for class 'data.frame'. Parses arguments and generates the call: fun(x, yvar, xvar, groups, facets, ...).
#' @param x data.frame
#' @param ... passed to fun
#' @param fun function to draw the plot
#' @seealso \code{\link{scatter_data_frame}}
#' @export
#' @import lattice
#' @importFrom rlang f_rhs quos
#' @family bivariate plots
#' @family scatter
#' @examples
#' library(magrittr)
#' library(dplyr)
#' attr(Theoph$conc,'label') <- 'theophylline concentration'
#' attr(Theoph$conc,'guide') <- 'mg/L'
#' attr(Theoph$Time,'label') <- 'time'
#' attr(Theoph$Time,'guide') <- 'h'
#' attr(Theoph$Subject,'guide') <- '////'
#' scatter(Theoph,conc, Time)
#' scatter(Theoph, conc, Time, Subject) # Subject as groups
#' scatter(Theoph, conc, Time, , Subject) # Subject as facet
#' scatter(Theoph %>% filter(conc > 0), conc, Time, Subject, ylog = TRUE, yref = 5)
#' scatter(Theoph, conc, Time, Subject, ysmooth = TRUE)
#' scatter(Theoph, conc, Time, conf = TRUE, loc = 3, yref = 6)
#' scatter(Theoph, conc, Time, conf = TRUE, loc = 3, yref = 6, global = TRUE)
#' \dontrun{
#' \dontshow{
#' attr(Theoph,'title') <- 'Theophylline'
#' scatter(Theoph, conc, Time, main = function(x,...)attr(x,'title'))
#' scatter(Theoph, conc, Time, sub= function(x,...)attr(x,'title'))
#' options(metaplot_main = function(x,...)attr(x,'title'))
#' scatter(Theoph, conc, Time)
#' }
#' }
scatter.data.frame <- function(
  x,
  ...,
  fun = getOption('metaplot_scatter','scatter_data_frame')
){
  args <- quos(...)
  args <- lapply(args,f_rhs)
  vars <- args[names(args) == '']
  other <- args[names(args) != '']
  vars <- sapply(vars, as.character)

  # this function needs to explicitly assign xvar, yvar, groups, and facets
  # prime is all yvar, if present, and xvar
  # prime is defined as all vars before groups or facets, if present
  # non-prime start with the first missing or categorical in position 3 or later
  # since groups may be missing, checking properties may fail
  # isolate non-prime
  missing <- match('',vars)
  if(is.defined(missing)){
    #prime <- vars[seq_len(missing - 1)]
    #if(length(vars) > missing) nonpr <- vars[(missing+1):length(vars)]
    vars <- vars[-missing]
  }
  # now we have protected vars from missingness, but preserved info from missing group, if any

  # test numeric
  stopifnot(all(vars %in% names(x)))
  num <- sapply(x[vars], is.numeric)

  # but the definition of numeric depends partly on guide.
  guide <- lapply(x[vars], attr, 'guide')
  guide[is.null(guide)] <- ''
  stopifnot(all(sapply(guide,length) <= 1))
  guide <- as.character(guide)

  encoded <- encoded(guide)
  num[encoded] <- FALSE # now num is fully defined

  # if groups was not passed as missing, prime etc can be defined in terms of num
  # must reserve at least one yvar and one xvar.
  # find first categorical in position 3 or later
  pos <- seq_along(num)
  can <- !num & pos > 2
  grp <- match(TRUE, can)

  # we now have var, giving the names of all real variables
  # missing is NA, or one greater than the last prime
  # grp is NA, or the position of the first (remaining) non-prime
  # x is last position in var not greater than missing or grp
  xlim <- min(na.rm = T, missing, grp, length(vars) + 1)
  xpos <- xlim - 1
  xvar <- vars[xpos]
  yvar <- vars[seq_len(xpos -1)]
  groups <- NULL
  facets <- NULL
  more <- character(0)
  if(length(vars) > xpos) more <- vars[(xpos+1):length(vars)]
  # first additional is groups if missing:NA and length(y) == 1
  if(length(more) & is.na(missing) & length(yvar) == 1){
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


#' Panel Function for Metaplot Scatterplot
#'
#' Panel function for metaplot::scatterplot.data.frame.
#'
#' @export
#' @param x x values
#' @param y y values
#' @param groups optional grouping item
#' @param yref reference line from y axis; can be function of x, y
#' @param xref reference line from x axis; can be function of x, y
#' @param ref.col reference line color
#' @param ref.lty reference line type
#' @param ref.alpha reference line alpha
#' @param ysmooth supply loess smooth of y on x
#' @param xsmooth supply loess smmoth of x on y
#' @param smooth.lty smooth line type
#' @param smooth.alpha smooth alpha
#' @param iso use isometric axes with line of unity
#' @param global if TRUE, xsmooth, ysmooth, fit, and conf are applied to all data rather than groupwise
#' @param global.col color for global aesthetics
#' @param fit draw a linear fit of y ~ x
#' @param fit.lty fit line type
#' @param fit.alpha fit alpha
#' @param conf logical, or width for a confidence region around a linear fit; passed to \code{\link{region}}; \code{TRUE} defaults to 95 percent confidence interval; may not make sense if xlog is TRUE
#' @param conf.alpha alpha transparency for confidence region
#' @param loc where to print statistics on a panel; suppressed for grouped plots
#' @param msg a function to print text on a panel: called with x values, y values, and \dots.
#' @param type overridden by metapanel
#' @param ... passed to panel.superpose, panel.xyplot, panel.polygon, region, panel.text
#' @family panel functions
#' @family scatter
#' @seealso \code{\link{metastats}}
#' @seealso \code{\link{scatter.data.frame}}
#'
metapanel <- function(
  x,
  y,
  groups,
  xref = getOption('metapanel_ref',NULL),
  yref = getOption('metapanel_ref',NULL),
  ref.col = getOption('metaplot_ref.col','grey'),
  ref.lty = getOption('metaplot_ref.lty','solid'),
  ref.alpha = getOption('metaplot_ref.alpha',1),
  ysmooth = getOption('metaplot_ysmooth',FALSE),
  xsmooth = getOption('metaplot_xsmooth',FALSE),
  smooth.lty = getOption('metaplot_smooth.lty','dashed'),
  smooth.alpha = getOption('metaplot_smooth.alpha',1),
  fit = getOption('metaplot_fit',conf),
  fit.lty = getOption('metaplot_fit.lty','solid'),
  fit.alpha = getOption('metaplot_fit.alpha',1),
  conf = getOption('metaplot_conf',FALSE),
  conf.alpha = getOption('metaplot_conf.alpha',1),
  loc = getOption('metaplot_loc',0),
  iso = getOption('metaplot_iso',FALSE),
  global = getOption('metaplot_global',FALSE),
  global.col = getOption('metaplot_global.col','grey'),
  msg = getOption('metaplot_msg','metastats'),
  type,
  ...
)
{
  stopifnot(length(global) == 1, is.logical(global))
  # if(is.null(groups)) groups <- rep(TRUE,length(x)) # cannot be NULL
  myxsmooth <- function(x,y,type,lty,col, col.symbol, col.line,...){
    bar <- try(silent = TRUE, suppressWarnings(loess.smooth(y,x, family = 'gaussian')))
    if(xsmooth && !inherits(bar,'try-error'))try(panel.xyplot(bar$y,bar$x,lty = smooth.lty, alpha = smooth.alpha,type = 'l',col = col.line,...))
  }
  myysmooth <- function(x,y,type,lty,col, col.symbol, col.line,alpha,...){
    foo <- try(silent = TRUE, suppressWarnings(loess.smooth(x,y, family = 'gaussian')))
    if(ysmooth && !inherits(foo,'try-error'))try(panel.xyplot(foo$x,foo$y,lty = smooth.lty, alpha = smooth.alpha,type = 'l',col = col.line,...))
  }
  myfit <- function(x,y,type,lty,col, col.symbol, col.line,alpha,...){
    f <- data.frame()
    f <- region(x, y, conf = conf, ...)
    try(panel.xyplot(x=f$x, y=f$y, col= col.line, type='l',lty = fit.lty,alpha= fit.alpha, ...))
  }
  myconf <- function(x,y,type,lty,col, col.symbol, col.line,alpha,...){
    f <- region(x, y, conf = conf, ...)
    try(panel.polygon(
      x = c(f$x, rev(f$x)),
      y = c(f$lo, rev(f$hi)),
      border = FALSE,
      alpha=conf.alpha,
      col=col.symbol
    ))
  }
  superpose.line <- trellis.par.get()$superpose.line
  superpose.symbol <- trellis.par.get()$superpose.symbol
  panel.superpose(x = x,y = y,groups = groups,panel.groups = panel.lines,type='l',alpha = superpose.line$alpha, ...)
  panel.superpose(x = x,y = y,groups = groups,panel.groups = panel.points,type='p',alpha = superpose.symbol$alpha,...)
  if(conf){
    if(global){
      myconf(x,y, col = global.col, col.symbol = global.col, col.line = global.col,...)
    }else{
      panel.superpose(x = x, y = y, groups = groups, panel.groups = myconf, ...)
    }
  }
  if(fit){
    if(global){
      myfit(x,y, col = global.col, col.symbol = global.col, col.line = global.col,...)
    }else{
      panel.superpose(x = x, y = y, groups = groups, panel.groups = myfit, ...)
    }
  }
  if(ysmooth){
    if(global){
      myysmooth(x,y, col = global.col, col.symbol = global.col, col.line = global.col,...)
    }else{
      panel.superpose(x = x, y = y, groups = groups, panel.groups = myysmooth, ...)
    }
  }
  if(xsmooth){
    if(global){
      myxsmooth(x,y, col = global.col, col.symbol = global.col, col.line = global.col,...)
    }else{
      panel.superpose(x = x, y = y, groups = groups, panel.groups = myxsmooth, ...)
    }
  }
  if(sum(loc))panel = panel.text(
    x = xpos(loc),
    y = ypos(loc),
    label = match.fun(msg)(x = x, y = y, ...)
  )
  if(is.character(yref)) yref <- match.fun(yref)
  if(is.function(yref)) yref <- yref(y, x, ...)
  yref <- as.numeric(yref)
  yref <- yref[is.defined(yref)]

  if(is.character(xref)) xref <- match.fun(xref)
  if(is.function(xref)) xref <- xref(x, y, ...)
  xref <- as.numeric(xref)
  xref <- xref[is.defined(xref)]

  if(length(yref))panel.abline(h = yref, col = ref.col, lty = ref.lty, alpha = ref.alpha)
  if(length(xref))panel.abline(v = xref, col = ref.col, lty = ref.lty, alpha = ref.alpha)

  if(iso)panel.abline(0,1)
}

xpos <- function(loc){
  stopifnot(length(loc) %in% 1:2)
  if(length(loc) == 1) stopifnot(loc == as.integer(loc), loc < 10)
  l <- rep(c(.2,.5,.8),3)
  x <- if(length(loc) == 1) l[[loc]] else loc[[1]]
  stopifnot(x <= 1, x >= 0)
  lo <- current.panel.limits()$xlim[[1]]
  hi <- current.panel.limits()$xlim[[2]]
  xpos <- lo + x * (hi - lo)
  xpos
}

ypos <- function(loc){
  stopifnot(length(loc) %in% 1:2)
  if(length(loc) == 1) stopifnot(loc == as.integer(loc), loc < 10)
  l <- rep(c(.8,.5,.2),each = 3)
  y <- if(length(loc) == 1) l[[loc]] else loc[[2]]
  stopifnot(y <= 1, y >= 0)
  lo <- current.panel.limits()$ylim[[1]]
  hi <- current.panel.limits()$ylim[[2]]
  ypos <- lo + y * (hi - lo)
  ypos
}

