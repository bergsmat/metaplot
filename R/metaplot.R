globalVariables('groups_')
globalVariables('META')
globalVariables('VARIABLE')
globalVariables('VALUE')
globalVariables('collapse')
globalVariables('panel_')

#' Metaplot
#'
#' Creates a metaplot.
#' @param x object
#' @param ... passed arguments
#' @export
#' @family generic functions
metaplot <- function(x,...)UseMethod('metaplot')

#' Create Metaplot from Grouped_df
#'
#' Creates a metaplot from grouped_df.  Folds x and calls the method for folded. Dots arguments are passed only to metaplot.
#'
#' @family metaplots
#' @family univariate plots
#' @family bivariate plots
#' @family multivariate plots
#' @describeIn metaplot grouped_df method
#' @export
metaplot.grouped_df <- function(x,...){
  y <- fold(x)
  metaplot(y,...)
}

#' Create Metaplot from Data.frame.
#'
#' Creates a metaplot from data.frame.  Folds x and calls the method for folded. Dots arguments are passed only to metaplot.
#'
#' @family metaplots
#' @family univariate plots
#' @family bivariate plots
#' @family multivariate plots
#' @describeIn metaplot data.frame method
#' @export
metaplot.data.frame <- function(x,...){
  y <- fold(x)
  metaplot(y,...)
}

#' Create Metaplot from Folded
#'
#' Creates a plot from folded, using metadata as available.
#'
#' Metaplot creates univariate, bivariate, or multivariate plots depending on the
#'  number and type of items represented by the anonymous arguments.
#'
#'\itemize{
#' \item{CON:}{ A single argument representing a continuous variable (numeric, not having encoded GUIDE) is forwarded to \code{\link{dens.folded}} to give a density plot.}
#'
#' \item{CAT:}{ A single categorical argument is unexpected.}
#'
#' \item{CON, CAT:}{ Two arguments, types continuous and categorical, are forwarded to \code{\link{boxplot.folded}} to give a boxplot (vertical or horizontal, depending on order).}
#'
#' \item{CON, CON:}{ Two arguments representing continuous variables give a scatterplot (first vs. second) by means of \code{\link{scatter.folded}}.}
#'
#'\item{CAT, CAT:}{ Two anonymous categorical arguments are unexpected.}
#'
#'\item{CON, CAT, ARG:}{ A third anonymous argument is unexpected if a preceding argument is categorical.}
#'
#' \item{CON, CON, CAT:}{ A third, categorical argument following two continuous arguments is treated as a grouping variable.}
#'
#' \item{CON, CON, CON:}{ If there are three or more continuous arguments, a scatterplot matrix is created by means of \code{\link{corsplom.folded}}. However....}
#'
#' \item{CON, CON, CON, CAT (,CAT):}{ For three or more continuous arguments, if one or more categorical arguments are present, an overlay plot will be created by means of \code{\link{overlay.folded}}: other continuous items will be plotted vs. the last continuous item; up to two categorical items will be used as conditioning variables (facets).}
#'}
#'
#' Stratification, e.g. conditioning for trellis plots, is currently unimplemented.

#' @import lazyeval
#' @family metaplots
#' @family univariate plots
#' @family bivariate plots
#' @family multivariate plots
#' @describeIn metaplot folded method
#' @import lazyeval
#' @importFrom graphics boxplot
#' @importFrom stats as.formula cor density loess.smooth median
#' @importFrom dplyr filter
#' @import fold
#' @export
#' @examples
#' # quick example
#'
#'library(magrittr)
#' library(fold)
#' x <- as.folded(system.file(package='metaplot','extdata','drug1001.fld'))

#'x %>% metaplot(
#'  DV, IPRE, SEX,
#'  ylog = TRUE,
#'  xlog = TRUE,
#'  grid = TRUE, # passed to xyplot
#'  iso = TRUE,
#'  ysmooth = TRUE,
#'  xsmooth = TRUE,
#'  yref = 0.5,
#'  xref = 0.5,
#'  main = TRUE,
#'  corr = TRUE,
#'  fit = TRUE,
#'  conf = 1 - 1e-14,
#'  loc = 6
#')
#'
#' # extended examples
#'
#' \dontrun{
#' # load some packages
#' library(spec)
#' library(csv)
#' library(magrittr)
#' library(tidyr)
#' library(dplyr)
#' library(fold)
#'
#' # find paths to example data and specification
#' x <- system.file(package='metaplot','extdata','drug1001.csv')
#' spec <- system.file(package='metaplot','extdata','drug1001.spec')
#'
#'# verify agreement at file level
#' x %matches% spec
#'
#'# read and verify in memory
#' x %<>% as.csv
#' spec %<>% as.spec
#' x %matches% spec
#'
#' # convert specifaction to folded format
#' spec %<>%  as.folded
#'
#' # capture the most interesting parts of x
#' x %<>% filter(VISIBLE == 1) %>% filter(EVID == 0)
#'
#' # identify keys
#' attr(x, 'groups') <- c('ID','TIME')
#'
#' # fold x
#' x %<>% fold
#'
#' # combine with metadata
#' x %<>% bind_rows(spec)
#' x %<>% sort
#' # x %>% as.csv('drug1001.fld')
#'
#' # Now we have a plotting dataset with embedded metadata.
#' # We call metaplot with various numbers of continuous and
#' # categorical arguments, given as unquoted values from the
#' # VARIABLE column.
#'
#' x %>% metaplot(AGE) # one continuous
#' x %>% metaplot(PRED,DV) # two continuous
#' x %>% metaplot(AGE,SEX) # continuous and categorical
#' x %>% metaplot(SEX,AGE, main = TRUE) # categorical and continuous
#' x %>% metaplot(PRED,DV,SEX, main = TRUE) # two continous and categorical
#' x %>% metaplot(ETA1,ETA2,ETA3) # three or more continuous
#' x %>% metaplot(CWRES,TAD) # metadata
#' x %>% filter(META %>% is.na) %>% metaplot(CWRES,TAD) # no metadata
#' x %>% metaplot(PRED,DV, xlog = TRUE, ylog = TRUE, iso=TRUE, xsmooth = TRUE) # log-log
#' x %>% metaplot(CWRES, TAD, yref = 0, ysmooth = TRUE)
#' x %>% metaplot(ETA1, SEX, ref = 0)
#' x %>% metaplot(AGE,WEIGHT, ysmooth = TRUE, xsmooth = TRUE)
#' x %>% metaplot(AGE,WEIGHT, ysmooth = TRUE)
#' x %>% metaplot(AGE,WEIGHT, ysmooth = TRUE, fit = TRUE)
#' x %>% metaplot(AGE,WEIGHT, ysmooth = TRUE, conf = TRUE)
#' x %>% metaplot(AGE,WEIGHT, ysmooth = TRUE, conf = TRUE, loc = 9)
#' x %>% metaplot(AGE,WEIGHT, ysmooth = TRUE, conf = TRUE, loc = c(.2,.7),
#' main = TRUE, corr = TRUE)
#'
#' # FED ~ WEIGHT would normally invoke a boxplot.
#' # Here we force FED to be treated as numeric to illustrate logistic regression.
#' x %>% scatter('FED', 'WEIGHT', conf = TRUE)
#' # Alternatively:
#' x %>%
#' filter(is.na(META) | !(VARIABLE == 'FED' & META =='GUIDE')) %>%
#' metaplot(FED, WEIGHT, conf = TRUE)
#'
#' # Below, x is TAD,
#' # plot is conditioned by ID
#' # ID is categorical (encoded)
#'
#' x %>% metaplot(
#'   DV, PRED, IPRE, TAD, ID,
#'   color = 'black',
#'   points = c(TRUE, FALSE, FALSE),
#'   line   = c('none','dashed','solid'),
#'   ylab = 'plasma drug concentration (ng/mL)'
#' ) %>% `[[`(1)
#'
#' 2-way facetting
#'x %>% metaplot(DV, PRED, TIME, SEX, FED,
#'line = 'none', color = c('blue','magenta'))
#'}

metaplot.folded <- function(x, ...){
  args <- quos(...)
  args <- lapply(args,f_rhs)
  var <- args[names(args) == '']
  other <- args[names(args) != '']
  var <- sapply(var, as.character)
  x <- x[!is.na(x$VARIABLE),] # table-level metadata is unused
  x <- data.frame(x, stringsAsFactors = FALSE, fix.empty.names=FALSE, check.names=FALSE) # faster than grouped_df
  class(x) <- c('folded','data.frame')
  cont <- sapply(var,function(nm)continuous(x,nm))
  cat <-  !cont
  len <- length(var)
  args <- c(
    list(x = x),
    var,
    other
  )
  if(!any(cont))stop('metaplot requires at least one continuous variable')
  if(len == 1) return(do.call(dens, args))
  if(sum(cont) > 2 & sum(cat) == 0) return(do.call(corsplom,args))
  if(sum(cont) > 2) return(do.call('overlay',args)) # quoted to disambiguate function and argument
  # now have at least two var but no more than two continuous
  if(len >= 3 & any(cat[1:2])) stop('metaplot does not support a third variable following a categorical variable')
  if(!cont[[1]] && !cont[[2]]) stop('metaplot requires at least one continuous variable')
  if( cont[[1]] &&  cont[[2]]) return(do.call(scatter,args))
  if(!cont[[1]] &&  cont[[2]]) return(do.call(boxplot,args))
  if( cont[[1]] && !cont[[2]]) return(do.call(boxplot,args))
}

#' Density
#'
#' Creates a density metaplot.
#' @param x object
#' @param ... passed arguments
#' @export
#' @family generic functions
dens <- function(x,...)UseMethod('dens')

#' Plot Density for Data Frame
#'
#' Plot density for object of class 'data.frame'.
#' @param var item to plot
#' @param ref optional numeric
#' @param log whether to use log scale
#' @family univariate plots
#' @describeIn dens data.frame method
#' @export
dens.data.frame<- function(
  x,
  var,
  ref = NULL,
  log = FALSE,
  ...
){
  # d <- unfold(x,.y)
  v <- x[[var]]
  densityplot(
    v,
    aspect = 1,
    scales = list(tck = c(1,0),x = list(log = log,equispaced.log = FALSE)),
    panel = function(...){
      panel.densityplot(...)
      if(length(ref))panel.abline(v = ref)
    },
    ...
  )
}#' Plot Density for Folded
#'
#' Plot density for object of class 'folded'.
#' @describeIn dens folded method
#' @importFrom rlang UQS
#' @export
dens.folded <- function(
  x,
  var,
  ref = NULL,
  log = FALSE,
  ...
){
  d <- unfold(x,UQS(var))
  xlab = axislabel(x,var,log=log)
  dens(d, var = var, ref = ref, log = log, xlab=xlab, ...)
}

#' Scatterplot
#'
#' Scatterplot.
#'
#' @param x object
#' @param ... passed arguments
#' @export
#' @family generic functions
scatter <- function(x,...)UseMethod('scatter')

#' Scatterplot for Data Frame
#'
#' Scatterplot for class 'data.frame'. Extra arguments passed to \code{\link{region}}.
#' @param .y y axis item
#' @param .x x axis item
#' @param groups optional grouping item
#' @param ylog log transform y axis (guessed if missing)
#' @param xlog log transform x axis (guessed if missing)
#' @param yref reference line from y axis
#' @param xref reference line from x axis
#' @param ysmooth supply loess smooth of y on x
#' @param xsmooth supply loess smmoth of x on y
#' @param cols suggested columns for auto.key
#' @param density plot point density instead of points (ignored if \code{groups} is supplied)
#' @param iso use isometric axes with line of unity
#' @param main logical: whether to construct a default title; or a substitute title or NULL
#' @param corr append Pearson correlation coefficient to default title (only if main is \code{TRUE})
#' @param group_codes append these to group values for display purposes
#' @param crit if ylog or xlog missing, log transform if mean/median ratio for non-missing values is greater than crit
#' @param na.rm whether to remove data points with one or more missing coordinates
#' @param fit draw a linear fit of y ~ x
#' @param conf logical, or width for a confidence region around a linear fit; passed to \code{\link{region}}; \code{TRUE} defaults to 95 percent confidence interval
#' @param msg a function to print text on a panel: called with x values, y values, and \dots.
#' @param loc where to print statistics on a panel
#' @param panel name or definition of panel function
#' @seealso \code{\link{metapanel}}
#' @export
#' @import lattice
#' @family bivariate plots
#' @describeIn scatter data.frame method
scatter.data.frame <- function(
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
  group_codes = NULL,
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
    length(groups) <= 1,
    is.logical(corr)
  )
  y <- x # %>% unfold(c(.y,.x,groups))
  stopifnot(all(c(.x,.y,groups) %in% names(y)))
  names(y)[names(y) ==.y] <- 'y_'
  names(y)[names(y) ==.x] <- 'x_'
  if(na.rm){
    bad <- is.na(y$y_) | is.na(y$x_)
    y <- y[!bad,,drop = FALSE]
  }
  if(length(groups)){
    names(y)[names(y) == groups] <- 'groups_'
    gc <- group_codes
    if(length(gc) == 1){
      if(encoded(gc)){
        y$groups_ <- decode(y$groups_,gc)
      }else{
        y$groups_[is.defined(y$groups_)] <- paste(y$groups_[is.defined(y$groups_)],gc)
      }
    }
    cols = min(cols,length(unique(y$groups_[is.defined(y$groups_)])))
  }
  formula <- 'y_ ~ x_'
  formula <- as.formula(formula)
  auto.key <- if(length(groups)) list(columns = cols) else FALSE
  if(ylog %>% is.null) ylog <- all(na.rm = T,y$y_ > 0) && mean(y$y_,na.rm = T)/median(y$y_,na.rm = T) > crit
  if(xlog %>% is.null) xlog <- all(na.rm = T, y$x_ > 0) && mean(y$x_,na.rm = T)/median(y$x_,na.rm = T) > crit
  if(length(yref) & ylog) yref <- log(yref)
  if(length(xref) & xlog) xref <- log(xref)
  yscale = list(log = ylog,equispaced.log = FALSE)
  xscale = list(log = xlog,equispaced.log = FALSE)
  scales = list(y = yscale,x = xscale,tck = c(1,0))
  prepanel = if(iso) function(x,y,...){
    lim = c(min(x,y,na.rm = T),max(x,y,na.rm = T))
    list(
      xlim = lim,
      ylim = lim
    )
  } else NULL
  cor <- cor(use = 'pair',if(ylog) log(y$y_) else y$y_, if(xlog) log(y$x_) else y$x_)
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
      main = NULL
    }
  }
  xyplot(
    formula,
    data = y,
    groups = if(length(groups)) groups_ else NULL,
    auto.key = auto.key,
    aspect = 1,
    scales = scales,
    prepanel = prepanel,
    yref = yref,
    xref = xref,
    ysmooth = ysmooth,
    xsmooth = xsmooth,
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
  if( length(groups))panel.superpose(x = x, y = y, groups = groups, ...)
  if(!length(groups)){
    if( density)panel.smoothScatter(x,y,...)
    if(!density)panel.xyplot(x,y,...)
  }
  foo <- suppressWarnings(loess.smooth(x,y))
  bar <- suppressWarnings(loess.smooth(y,x))
  if(ysmooth)try(panel.xyplot(foo$x,foo$y,col = 'black',lty = 'dashed',type = 'l'))
  if(xsmooth)try(panel.xyplot(bar$y,bar$x,col = 'black',lty = 'dashed',type = 'l'))
  f <- data.frame()
  if(fit || conf) f <- region(x, y, conf = conf, ...)
# if(fit) try(panel.xyplot(x=x, y= y, col='black', type='r', ...))
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

#' Boxplots
#'
#' @name boxplot
NULL

#' Boxplot for Data Frame
#'
#' Boxplot for data.frame.
#' @param x data.frame
#' @param .y y axis item
#' @param .x x axis item
#' @param log whether to log transform continuous variable
#' @param horizontal whether box/whisker axis should be horizontal
#' @param main logical:whether to include title indicating x and y items; or a substitute title or NULL
#' @param crit if log is missing, log transform if mean/median ratio for non-missing x  is greater than this value
#' @param ref optional reference line on continuous axis
#' @param guide optional encoding for categories see \code{encode::encode}
#' @param nobs whether to include the number of observations under the category label
#' @param na.rm whether to remove data points with one or more missing coordinates
#' @param ... passed arguments
#' @export
#' @family bivariate functions
#' @rdname boxplot
boxplot.data.frame <- function(
  x,
  .y,
  .x,
  log,
  horizontal = TRUE,
  main = FALSE,
  crit = 1.3,
  ref = NULL,
  guide = NA_character_,
  nobs = FALSE,
  na.rm = TRUE,
  ...
){
  stopifnot(
    length(.x) == 1,
    length(.y) == 1
  )
  y <- x # %>% unfold(c(.y,.x))
  stopifnot(all(c(.x,.y) %in% names(y)))
  names(y)[names(y) ==.y] <- 'y_'
  names(y)[names(y) ==.x] <- 'x_'
  if(na.rm){
    bad <- is.na(y$y_) | is.na(y$x_)
    y <- y[!bad,,drop = FALSE]
  }
  formula <- 'y_ ~ x_'
  formula <- as.formula(formula)
  con <- if(horizontal) 'x_' else 'y_'
  cat <- if(horizontal) 'y_' else 'x_'
  if(missing(log)) log <- mean(y[[con]],na.rm = T)/median(y[[con]],na.rm = T) > crit
  if(any(y[[con]] <= 0,na.rm = T)) log <- FALSE
  #ylab <- axislabel(x,.y, log = FALSE )
  #xlab <- axislabel(x,.x, log = log )
  if(encoded(guide)){
    y[[cat]] <- decode(y[[cat]],encoding = guide)
    y[[cat]] <- factor(y[[cat]],levels = rev(levels(y[[cat]])))
  }
  if(!is.factor(y[[cat]])) y[[cat]] <- factor(y[[cat]],levels = unique(y[[cat]]))
  if(nobs){
    lev <- levels(y[[cat]])
    num <- sapply(lev,function(l)sum(na.rm = TRUE, y[[cat]] == l))
    levels(y[[cat]]) <- paste(lev,num,sep = '\n')
  }

  # if(is.null(ref)) if(ymeta$TYPEC %in% c('IIV','RESIDUAL') ) ref <- 0
  scales <- list(
    tck = c(1,0),
    x = list(log = log,equispaced.log = FALSE)
  )
  if(!horizontal)scales <- list(
    tck = c(1,0),
    y = list(log = log,equispaced.log = FALSE)
  )
  mn <- paste(sep = ' ~ ',.y,.x)
  mn <- list(mn, cex = 1, fontface = 1)
  if(is.logical(main)){
    if(main){
      main <- mn
    } else{
      main <- NULL
    }
  }
  bwplot(
    formula,
    data = y,
    aspect = 1,
    horizontal = horizontal,
    main = main,
    par.settings = standard.theme('pdf',color = FALSE),
    scales = scales,
    panel = function(...){
      panel.bwplot(...)
      if(length(ref)){
        if(horizontal) {
          panel.abline(v = ref)
        }else{
          panel.abline(h = ref)
        }
      }
    },
    ...
  )
}

#' Boxplot for Folded
#'
#' Boxplot for folded.
#' @import encode
#' @export
#' @family bivariate plots
#' @rdname boxplot
boxplot.folded <- function(
  x,
  .y,
  .x,
  log,
  horizontal,
  main = FALSE,
  crit = 1.3,
  ref = NULL,
  na.rm = TRUE,
  ...
){
  stopifnot(
    length(.x) == 1,
    length(.y) == 1
  )
  y <- x %>% unfold(UQS(c(.y,.x)))
  stopifnot(all(c(.x,.y) %in% names(y)))
  if(missing(horizontal)) horizontal <- continuous(x, .x)
  con <- if(horizontal) .x else .y
  cat <- if(horizontal) .y else .x
  if(missing(log)) log <- mean(y[[con]],na.rm = T)/median(y[[con]],na.rm = T) > crit
  if(any(y[[con]] <= 0,na.rm = T)) log <- FALSE
  ylab <- axislabel(x,.y, log = if(horizontal) FALSE else log )
  xlab <- axislabel(x,.x, log = if(horizontal) log else FALSE )
  guide <- guide(x,cat)
  boxplot(
    y,
    .y = .y,
    .x = .x,
    log = log,
    main = main,
    crit = crit,
    ref = ref,
    ylab = ylab,
    xlab = xlab,
    guide = guide,
    horizontal = horizontal,
    na.rm = na.rm,
    ...
  )
}

#' Correlated Splom
#'
#' Scatterplot matrix with correlations.
#' @param x object
#' @param ... passed arguments
#' @export
#' @family generic functions
corsplom <- function(x,...)UseMethod('corsplom')

#' Correlated Splom for Data.frame
#'
#' Creates a scatterplot matrix with correlations in lower panel, by default.
#' @param upper.panel passed to splom
#' @param lower.panel passed to splom
#' @param pscales passed to splom
#' @param xlab passed to splom
#' @param varname.cex passed to splom
#' @param diag.panel passed to splom
#' @param split break diagonal names on white space
#' @export
#' @family multivariate plots
#' @describeIn corsplom data.frame method
corsplom.data.frame <- function(
  x,
  upper.panel = u.p,
  lower.panel= l.p,
  pscales= 0,
  xlab = '',
  varname.cex = 1,
  diag.panel = my.diag.panel,
  split = TRUE,
  ...
){
  if(split) names(x) <- fracture(names(x))
  splom(
  x,
  upper.panel = upper.panel,
  lower.panel = lower.panel,
  pscales = pscales,
  xlab = xlab,
  varname.cex = varname.cex,
  diag.panel = diag.panel,
  ...
)
}

#' Correlated Splom for Folded
#'
#' Creates a scatterplot matrix with correlations for folded.
#' Categoricals in \dots are currently ignored. dots (\dots) are
#' names of items in VARIABLE to be plotted, or named arguments
#' to be passed to data.frame method.
#' @import lattice
#' @export
#' @family multivariate plots
#' @describeIn corsplom folded method
corsplom.folded <- function(x, ...){
  var <- quos(...)
  var <- lapply(var, f_rhs)
  item <- var[names(var) == '']
  item <- sapply(item,as.character)
  named <- var[names(var) != '']
  x %<>% filter(VARIABLE %in% item)
  cont <- sapply(item,function(nm)continuous(x,nm))
  item <- item[cont] # ignoring categoricals
  meta <- x %>% filter(!is.na(META))
  data <- x %>% filter(is.na(META))
  data %<>% unfold(UQS(item))
  data %<>% ungroup %>% select(UQS(item)) # ungroup should not be necessary
  for(nm in names(data)){
    y <- label(meta,nm)
    if(!is.na(y))names(data)[names(data) == nm] <- y
  }
  this <- list(x=data)
  out <- c(this,named)
  do.call(corsplom,out)
}

#' Generic Axis Label
#'
#' Generic axis label, with method for 'folded'.
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @family generic functions
#' @seealso \code{\link{axislabel.folded}}
axislabel <- function(x,...)UseMethod('axislabel')

#' Axis Label for Folded
#'
#' Axis label for folded.
#' @param x folded
#' @param var item of interest
#' @param log whether this is for a log scale
#' @param ... passed arguments
#' @keywords internal
#' @export
#' @import magrittr
#' @return character
axislabel.folded <- function(x, var, log = FALSE, ...){
  x <- x[x$VARIABLE == var & is.defined(x$META),,drop = FALSE]
  lab <- unique(x$VALUE[x$META =='LABEL'])
  guide <- unique(x$VALUE[x$META =='GUIDE'])
  res <- var
  if(length(lab) == 1){
    if(lab %>% is.defined){
      res <- lab
    }
  }
  if(length(guide) == 1){
    if(!encoded(guide)){
      if(guide %>% is.defined){
        guide <- paste0('(',guide,')')
        res <- paste(res,guide)
      }
    }
  }
  if(log) res <- paste0(res,'\n(log)')
  res
}

#' Check if Something is Continuous
#'
#' Checks if something is continuous.
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @family generic functions
continuous <- function(x,...)UseMethod('continuous')

#' Check if Folded is Continuous
#'
#' Checks if folded is continuous with respect to \code{var}
#' @param x folded
#' @param var length-one character
#' @param ... passed arguments
#' @import dplyr
#' @import magrittr
#' @export
#' @keywords internal
continuous.folded <- function(x,var, ...){
  stopifnot(length(var) == 1)
  is.number <- function(x)sum(is.defined(x)) == sum(is.defined(as.numeric(x)))
  val <- if(var %in% names(x)){
    x[[var]]
  } else{
    x %>% dplyr::filter(META %>% is.na) %>% dplyr::filter(VARIABLE == var) %$% VALUE
  }
  if(length(val) == 0)stop('no values found for ',var)
  guide_var <- guide(x,var)
  enc <- if(length(guide_var)) encoded(guide_var) else FALSE
  cont <- val %>% is.number && !enc
  cont
}

#' Extract Guide
#'
#' Extracts guide.
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @family generic functions
guide <- function(x,...)UseMethod('guide')

#' Extract Guide for Folded
#'
#' Extracts guide for class folded, given a variable.
#' @param x folded
#' @param var length-one character
#' @param ... ignored arguments
#' @return length-one character, possibly NA
#' @export
#' @keywords internal
guide.folded <- function(x,var,...){
  stopifnot(length(var) == 1)
  y <- x[is.defined(x$META) & x$META =='GUIDE' & x$VARIABLE == var,'VALUE']
  y <- unique(y) #
  if(length(y) > 1)stop('conflicting guides for ', var)
  if(length(y) == 0) y <- NA_character_
  y
}

#' Extract Label
#'
#' Extracts label.
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @family generic functions
label <- function(x,...)UseMethod('label')

#' Extract Label for Folded
#'
#' Extracts label for class folded, given a variable.
#' @param x folded
#' @param var length-one character
#' @param ... ignored arguments
#' @return length-one character, possibly NA
#' @export
#' @keywords internal
label.folded <- function(x,var, ...){
  stopifnot(length(var) == 1)
  y <- x[is.defined(x$META) & x$META =='LABEL' & x$VARIABLE == var,'VALUE']
  y <- unique(y)
  if(length(y) > 1)stop('conflicting guides for ', var)
  if(length(y) == 0) y <- NA_character_
  y
}

#' Upper Panel Function
#'
#' Upper panel function for corsplom(). Plots data with loess smooth.
#' @param x x values
#' @param y y values
#' @param col point color
#' @param loess loess color
#' @param ... passed arguments
#' @keywords internal
#' @export
#' @family panel functions
u.p = function(x,y,col = 'black', loess = 'red',...){
  panel.xyplot(x,y,col = col, ...)
  panel.loess(x,y,col = loess)
}

#' Lower Panel Function
#'
#' Lower panel function for corsplom(). Plots Pearson correlation coefficient.
#' @param x x values
#' @param y y values
#' @param ... passed arguments
#' @keywords internal
#' @export
#' @family panel functions
l.p = function(x, y, ...) {
  x1 <- range(x,na.rm = T)
  y1 <- range(y,na.rm = T)
  x0 <- min(x1)+(max(x1)-min(x1))/2
  y0 <- min(y1)+(max(y1)-min(y1))/2
  panel.text(x0 ,y0, labels = paste('r =',round(cor(x,y),2) ))
}

#' Diagonal Panel Function
#'
#' Diagonal panel function for corsplom(). Plots a density smooth against the corresponding axis from within the diagonal panel.  Plots a grey pin at each axis zero.
#' @param x numeric
#' @param denscale inflation factor for height of density smooth
#' @param ... passed arguments
#' @keywords internal
#' @export
#' @family panel functions
#' @seealso \code{\link{corsplom}}
my.diag.panel <- function(x, denscale = 0.2,...){
  d <- density(x)
  lim <- current.panel.limits()$x
  lo <- lim[[1]]
  hi <- lim[[2]]
  len <- hi - lo

  x1 <- d$x
  y1 <- d$y
  y1 <- y1 / max(y1,na.rm = TRUE)
  y1 <- y1 * len * denscale
  z1 <- hi - y1
  y1 <- y1 + lo
  lpolygon(
    x = x1,
    y = z1,
    col = 'gray',
    border = NA,
    alpha = 0.5
  )
  lpolygon(
    y = x1,
    x = y1,
    col = 'gray',
    border = NA,
    alpha = 0.5
  )
  lsegments(x0 = lo,x1 = lo + len * denscale,y0 = 0,y1 = 0,col = 'darkgray',alpha = 0.5)
  lsegments(y0 = hi,y1 = hi - len * denscale,x0 = 0,x1 = 0,col = 'darkgray',alpha = 0.5)
  diag.panel.splom(...)
}

is.defined <- function(x)!is.na(x)
parens <- function (x, ...)paste0("(", x, ")")
fracture <- function(x,sep='\n')gsub('\\s+',sep,x)

#' Execute Linear Model
#'
#' Executes a linear model, automatically choosing binomial family as necessary.
#' @param x x values
#' @param y y values
#' @param family gaussian by default, or binomial for all y either zero or 1
#' @param ... passed to \code{\link[stats]{glm}}
#' @return glm
#' @family regression functions
model <- function(x, y, family = if(all(y %in% 0:1,na.rm = TRUE)) 'binomial' else 'gaussian', ...){
  d <- data.frame(x=x,y=y)
  d <- d[order(d$x),]
  m <- glm(y~x,data=d,family=family) # elipses passed to glm.control, which chokes on unknown arguments, so not passing here.
  m
}

#' Calculate a Confidence Region
#'
#' Calculates a confidence region. \code{se.fit} from \code{\link[stats]{predict.glm}} is multiplied by \code{z} and added or subtracted from fits to give \code{hi} and \code{lo} columns in return value.  \code{z} is normal quantile for the one-tailed probablitity corresponding to \code{conf}, e.g. ~ 1.96 for \code{conf = 0.95}. If non-missing \code{y} is only 0 or 1, the model family is binomial and resulting confidence intervals are back-transformed using \code{\link[stats]{plogis}}.
#' @param x x values
#' @param y y values
#' @param family gaussian by default, or binomial for all y either zero or 1
#' @param length.out number of prediction points
#' @param conf width of confidence interval; logical TRUE defaults to 0.95
#' @importFrom stats qnorm predict glm plogis
#' @param ... passed to \code{\link{model}}
#' @return data.frame with x, y, hi, lo at 1000 points
#' @family regression functions
#' @seealso \url{https://stackoverflow.com/questions/14423325/confidence-intervals-for-predictions-from-logistic-regression}
#' @seealso \url{http://www.rnr.lsu.edu/bret/BretWebSiteDocs/GLMCI.pdf}
#' @seealso \url{https://stat.ethz.ch/pipermail/r-help/2010-September/254465.html}
#' @seealso \url{http://r.789695.n4.nabble.com/Confidence-Intervals-for-logistic-regression-td2315932.html}
region <- function(x, y, family = if(all(y %in% 0:1,na.rm = TRUE)) 'binomial' else 'gaussian', length.out = 1000, conf = 0.95, ...){
  if(is.logical(conf)){
    if(conf){
      conf <- 0.95
    } else{
      conf <- 0
    }
  }
  stopifnot(length(conf) == 1, is.numeric(conf), conf < 1, conf >= 0)
  tail <- 1 - conf  # e.g. 0.95 -> 0.05
  upper <- tail/2   # e.g. 0.025
  prob <- 1 - upper # e.g. 0.975
  z <- qnorm(prob)  # e.g. 1.96
  m <- model(x = x, y = y, family = family, ...)
  j <- seq(from=min(x),to=max(x),length.out=1000)
  f <- predict(m, se.fit=TRUE, newdata=data.frame(x=j),type='link')
  f <- data.frame(x=j,y=f$fit, se = f$se.fit)
  f$lo <- f$y - z * f$se
  f$hi <- f$y + z * f$se
  if(family=='binomial'){ # back-transform
    f <- within(f, y  <- plogis(y ))
    f <- within(f, lo <- plogis(lo))
    f <- within(f, hi <- plogis(hi))
  }
  f
}

#' Format GLM Statistics
#'
#' Formats GLM statistics.
#' @export
#' @param x x values
#' @param y y values
#' @param family regression family
#' @param ... other arguments
#' @importFrom stats coef glm plogis qnorm predict
#' @return character
#' @family regression functions
#' @seealso \code{\link{metapanel}}
#'
metastats <- function(x, y, family = if(all(y %in% 0:1,na.rm = TRUE)) 'binomial' else 'gaussian', ...){
  n <- paste('n =', length(x))
  m <- model(x, y, family, ...)
  p <- coef(summary(m))[,4]['x'] %>% signif(3)
  p <- paste('p =', p)
  r <- cor(x,y) %>% signif(3)
  r <- paste('r =',r)
  t <- paste(n,p,if(family=='gaussian') r else NULL,sep='\n')
  t
}


