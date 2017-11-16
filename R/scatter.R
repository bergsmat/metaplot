#' Scatterplot
#'
#' Scatterplot.
#'
#' @param x object
#' @param ... passed arguments
#' @export
#' @family generic functions
scatter <- function(x,...)UseMethod('scatter')

#' Scatterplot Function for Data Frame
#'
#' Scatterplot function for class 'data.frame'. Uses standard evaluation.
#' @param x data.frame
#' @param var character vector of variables to plot (expecting length: 2)
#' @param groups optional grouping item
#' @param ylog log transform y axis (guessed if NULL)
#' @param xlog log transform x axis (guessed if NULL)
#' @param yref reference line from y axis
#' @param xref reference line from x axis
#' @param ysmooth supply loess smooth of y on x
#' @param xsmooth supply loess smmoth of x on y
#' @param ylab y axis label, constructed from attributes \code{label} and \code{guide} if available
#' @param xlab x axis label, constructed from attributes \code{label} and \code{guide} if available
#' @param cols suggested columns for auto.key
#' @param density plot point density instead of points (ignored if \code{groups} is supplied)
#' @param iso use isometric axes with line of unity
#' @param main logical: whether to construct a default title; or a substitute title or NULL
#' @param corr append Pearson correlation coefficient to default title (only if main is \code{TRUE})
#' @param crit if ylog or xlog missing, log transform if mean/median ratio for non-missing values is greater than crit
#' @param na.rm whether to remove data points with one or more missing coordinates
#' @param fit draw a linear fit of y ~ x
#' @param conf logical, or width for a confidence region around a linear fit; passed to \code{\link{region}}; \code{TRUE} defaults to 95 percent confidence interval
#' @param msg a function to print text on a panel: called with x values, y values, and \dots.
#' @param loc where to print statistics on a panel
#' @param aspectpassed to \code{\link[lattice]{xyplot}}
#' @param auto.key passed to \code{\link[lattice]{xyplot}}
#' @param panel name or definition of panel function
#' @param ... passed to \code{\link{region}}
#' @seealso \code{\link{metapanel}}
#' @export
#' @import lattice
#' @family bivariate plots
#' @family scatter
#' @examples
#' attr(Theoph$conc,'label') <- 'theophylline concentration'
#' attr(Theoph$conc,'guide') <- 'mg/L'
#' attr(Theoph$Time,'label') <- 'time'
#' attr(Theoph$Time,'guide') <- 'h'
#' attr(Theoph$Subject,'guide') <- '////'
#' scatter_data_frame(Theoph, c('conc','Time'))
#' scatter_data_frame(Theoph, c('conc','Time'), 'Subject')
#' scatter_data_frame(Theoph %>% filter(conc > 0), c('conc','Time'), 'Subject',ylog = TRUE, yref = 5)
#' scatter_data_frame(Theoph, c('conc','Time'), 'Subject',ysmooth = TRUE)
#' scatter_data_frame(Theoph, c('conc','Time'),main = TRUE, corr = TRUE)
#' scatter_data_frame(Theoph, c('conc','Time'),conf = TRUE, loc = 3, yref = 6)
scatter_data_frame <- function(
  x,
  var,
  groups = NULL,
  ylog = NULL,
  xlog = NULL,
  yref = NULL,
  xref = NULL,
  ylab = NULL,
  xlab = NULL,
  ysmooth = FALSE,
  xsmooth = FALSE,
  cols = 3,
  density = FALSE,
  iso = FALSE,
  main = FALSE,
  corr = FALSE,
  crit = 1.3,
  na.rm = TRUE,
  fit = conf,
  conf = FALSE,
  loc = 0,
  aspect = 1,
  auto.key = list(columns = cols),
  msg = 'metastats',
  panel = metapanel,
  ...
){
  stopifnot(inherits(x, 'data.frame'))
  stopifnot(is.logical(corr))
  stopifnot(length(groups) <= 1)
  stopifnot(is.character(var))
  if(length(var) < 2) stop('need two items to make scatterplot')
  if(length(var) > 2) warning('only using first two of supplied items')
  .y <- var[[1]]
  .x <- var[[2]]
  y <- x
  stopifnot(all(c(.x,.y,groups) %in% names(y)))
  if(na.rm) y %<>% filter(is.defined(!!.y) & is.defined(!!.x)) # preserves attributes
  formula <- as.formula(paste(sep = '~',.y, .x))
  default_log <- function(x,crit){
    x <- x[!is.na(x)]
    all(x > 0) && (mean(x)/median(x) > crit)
  }
  if(ylog %>% is.null) ylog <- default_log(y[[.y]], crit)
  if(xlog %>% is.null) xlog <- default_log(y[[.x]], crit)
  if(ylog)if(any(y[[.y]] <= 0, na.rm = TRUE)){
    warning(.y, ' must be positive for log scale')
    ylog <- FALSE
  }
  if(xlog)if(any(y[[.x]] <= 0, na.rm = TRUE)){
    warning(.x, ' must be positive for log scale')
    ylog <- FALSE
  }
  if(length(yref) & ylog) yref <- log10(yref)
  if(length(xref) & xlog) xref <- log10(xref)
  yscale = list(log = ylog,equispaced.log = FALSE)
  xscale = list(log = xlog,equispaced.log = FALSE)
  scales = list(y = yscale,x = xscale,tck = c(1,0))
  if(is.null(ylab))ylab <- axislabel(y,var = .y, log = ylog)
  if(is.null(xlab))xlab <- axislabel(y,var = .x, log = xlog)

  if(!is.null(groups)){
    guide <- attr(y[[groups]], 'guide')
    if(encoded(guide)){
      decoded <- decode(y[[groups]], encoding = guide)
      if(!any(is.na(decoded))) y[[groups]] <- decoded
      if(all(is.na(decoded))) y[[groups]] <- decode(y[[groups]]) # values represent themselves as factor
      # mixed NA and decodable: do nothing
    }
  }

  prepanel = if(iso) function(x,y,...){
    lim = c(min(x,y,na.rm = T),max(x,y,na.rm = T))
    list(
      xlim = lim,
      ylim = lim
    )
  } else NULL
  cor <- cor(
    use = 'pair',
    if(ylog) log(y[[.y]]) else y[[.y]],
    if(xlog) log(y[[.x]]) else y[[.x]]
  )
  cor <- signif(cor,2)
  cor <- paste('R =',cor)
  cor <- parens(cor)
  mn <- paste(sep = ' ~ ',.y,.x)
  if(length(groups)) mn <- paste(mn,'by',groups)
  if(corr) mn <- paste(mn, cor)
  mn <- list(mn, cex = 1, fontface = 1)
  if(is.logical(main)){
    if(main){
      main <- mn
    }else{
      main <- NULL
    }
  }
  if(!is.null(groups)) groups <- as.formula(paste('~',groups))
  xyplot(
    formula,
    data = y,
    groups = groups,
    auto.key = auto.key,
    aspect = aspect,
    scales = scales,
    prepanel = prepanel,
    yref = yref,
    xref = xref,
    ysmooth = ysmooth,
    xsmooth = xsmooth,
    ylab = ylab,
    xlab = xlab,
    density = density,
    iso = iso,
    main = main,
    fit = fit,
    conf = conf,
    loc = loc,
    msg = msg,
    panel = panel,
    ...
  )
}

#' Panel Function for Metaplot Scatterplot
#'
#' Panel function for metaplot::scatterplot.data.frame.
#'
#' @export
#' @param x x values
#' @param y y values
#' @param groups optional grouping item
#' @param yref reference line from y axis
#' @param xref reference line from x axis
#' @param ysmooth supply loess smooth of y on x
#' @param xsmooth supply loess smmoth of x on y
#' @param density plot point density instead of points
#' @param iso use isometric axes with line of unity
#' @param fit draw a linear fit of y ~ x
#' @param conf logical, or width for a confidence region around a linear fit; passed to \code{\link{region}}; \code{TRUE} defaults to 95 percent confidence interval
#' @param loc where to print statistics on a panel
#' @param msg a function to print text on a panel: called with x values, y values, and \dots.
#' @param ... passed to panel.superpose, panel.smoothScatter, panel.xyplot, panel.polygon, region, panel.text
#' @family panel functions
#' @family metaplots
#' @seealso \code{\link{metastats}}
#' @seealso \code{\link{scatter.data.frame}}
#'
metapanel <- function(x, y, groups = NULL, xref = NULL, yref = NULL, ysmooth = FALSE, xsmooth = FALSE, density = FALSE, iso = FALSE, fit = conf, conf = FALSE, loc = 0, msg = 'metastats', ...)
{

  if( length(groups))panel.superpose(
    x = x,
    y = y,
    groups = groups,
    panel.groups = function(x,y,lty,type,...){
      panel.xyplot(x,y,...)
      foo <- try(silent = TRUE, suppressWarnings(loess.smooth(x,y, family = 'gaussian')))
      bar <- try(silent = TRUE, suppressWarnings(loess.smooth(y,x, family = 'gaussian')))
      if(ysmooth && !inherits(foo,'try-error'))try(panel.xyplot(foo$x,foo$y,lty = 'dashed',type = 'l',...))
      if(xsmooth && !inherits(bar,'try-error'))try(panel.xyplot(bar$y,bar$x,lty = 'dashed',type = 'l',...))
    },
    ...
  )
  if(!length(groups)){
    if( density)panel.smoothScatter(x,y,...)
    if(!density)panel.xyplot(x,y,...)
    foo <- try(silent = TRUE, suppressWarnings(loess.smooth(x,y, family = 'gaussian')))
    bar <- try(silent = TRUE, suppressWarnings(loess.smooth(y,x, family = 'gaussian')))
    if(ysmooth && !inherits(foo,'try-error'))try(panel.xyplot(foo$x,foo$y,col = 'black',lty = 'dashed',type = 'l'))
    if(xsmooth && !inherits(bar,'try-error'))try(panel.xyplot(bar$y,bar$x,col = 'black',lty = 'dashed',type = 'l'))
  }
  f <- data.frame()
  if(fit || conf) f <- region(x, y, conf = conf, ...)
  if(fit) try(panel.xyplot(x=f$x, y=f$y, col='black', type='l', ...))
  if(conf)try(panel.polygon(x = c(f$x, rev(f$x)),y = c(f$lo, rev(f$hi)),col='grey', border = FALSE, alpha=0.2, ...))
  if(sum(loc))panel = panel.text(x = xpos(loc), y = ypos(loc), label = match.fun(msg)(x = x, y = y, ...))
  if(length(yref))panel.abline(h = yref)
  if(length(xref))panel.abline(v = xref)
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

#' Scatterplot for Folded
#'
#' Scatterplot for class 'folded'.
#' @export
#' @import encode
#' @import lattice
#' @importFrom rlang UQS
#' @family bivariate plots
#' @describeIn scatter folded method
scatter.folded <- function(
  x,
  .y,
  .x,
  groups = NULL,
  ...,
  ylog = FALSE,
  xlog = FALSE,
  yref = NULL,
  xref = NULL,
  ysmooth = FALSE,
  xsmooth = FALSE,
  cols = 3,
  density = FALSE,
  iso = FALSE,
  main = FALSE,
  corr = FALSE,
  # group_codes = NULL,
  crit = 1.3,
  na.rm = TRUE,
  fit = conf,
  conf = FALSE,
  loc = 0,
  msg = 'metastats',
  panel = metapanel
){
  stopifnot(
    length(.x) == 1,
    length(.y) == 1,
    length(groups) <= 1
  )
  y <- x %>% unfold(UQS(c(.y,.x,groups)))
  stopifnot(all(c(.x,.y,groups) %in% names(y)))
  gc <- if(length(groups)) guide(x,groups) else NULL
  if(!is.null(gc))if(all(is.na(gc))) gc <- NULL
  ylab <- axislabel(x,.y,ylog)
  xlab <- axislabel(x,.x,xlog)
  scatter(
    y,
    .y = .y,
    .x = .x,
    ylab = ylab,
    xlab = xlab,
    groups = groups,
    ylog = ylog,
    xlog = xlog,
    yref = yref,
    xref = xref,
    ysmooth = ysmooth,
    xsmooth = xsmooth,
    cols = cols,
    density = density,
    iso = iso,
    main = main,
    corr = corr,
    group_codes = gc,
    crit = crit,
    na.rm = na.rm,
    fit = fit,
    conf = conf,
    loc = loc,
    msg = msg,
    panel = panel,
    ...
  )
}
