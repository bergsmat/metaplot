globalVariables('groups_')
globalVariables('META')
globalVariables('VARIABLE')
globalVariables('VALUE')

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
#'  number and type of items represented by \code{...}.
#'
#' A single argument representing a continuous variable (numeric, not having encoded GUIDE) is forwarded to \code{\link{dens}} to give a density plot.  Same for a single categorical, but this is unexpected.
#'
#' Two arguments, types continuous and categorical, are forwarded to \code{\link{boxplot.folded}} to give a boxplot (vertical or horizontal, depending on order).
#'
#' Two arguments representing continuous variables give a scatterplot by means of \code{\link{scatter.folded}}.
#'
#' A third anonymous argument is unexpected if a preceding argument is categorical.
#'
#' A third, categorical argument following two continuous arguments is treated as a grouping variable.
#'
#' If there are three or more continuous arguments, a scatterplot matrix is created by means of \code{\link{corsplom.folded}}.  Additional categoricals will be ignored.
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
#'
#' # Now we have a plotting dataset with embedded metadata.
#' # We call metaplot with various numbers of continuous and
#' # categorical arguments, given as unquoted values from the
#'#  VARIABLE column.
#'
#' x %>% metaplot(AGE) # one continuous
#' x %>% metaplot(PRED,DV) # two continuous
#' x %>% metaplot(AGE,SEX) # continuous and categorical
#' x %>% metaplot(SEX,AGE) # categorical and continuous
#' x %>% metaplot(PRED,DV,SEX) # two continous and categorical
#' x %>% metaplot(ETA1,ETA2,ETA3) # three or more continuous
#' x %>% metaplot(CWRES,TAD) # metadata
#' x %>% filter(META %>% is.na) %>% metaplot(CWRES,TAD) # no metadata
#' x %>% metaplot(PRED,DV, xlog = TRUE, ylog = TRUE, iso=TRUE, xsmooth = TRUE) # log-log
#' x %>% metaplot(CWRES, TAD, yref = 0, ysmooth = TRUE)
#' x %>% metaplot(ETA1, SEX, ref = 0)
#' x %>% metaplot(AGE,WEIGHT, ysmooth = TRUE, xsmooth = TRUE)
metaplot.folded <- function(x,...){
  args <- quos(...)
  args <- lapply(args,f_rhs)
  var <- args[names(args) == '']
  other <- args[names(args) != '']
  var <- sapply(var, as.character)
{#   do.call(
#     metaplot_folded,
#     c(
#       list(
#         x = x,
#         var = var
#       ),
#       other
#     )
#   )
# }

# metaplot_folded = function(x, var, ...){
}
  x <- data.frame(x, stringsAsFactors = FALSE) # faster than grouped_df
  class(x) <- c('folded','data.frame')
  cont <- sapply(var,function(nm)continuous(x,nm))
  len <- length(var)
  args <- c(
    list(x = x),
    var,
    other
  )
  if(!any(cont))stop('metaplot requires at least one continuous variable')
  if(len == 1) return(do.call(dens, args))
  if(sum(cont) > 2) return(do.call(corsplom,args))
  # now have at least two var but no more than two continuous
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
#' Scatterplot for class 'data.frame'.
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
#' @param density plot point density instead of points
#' @param iso use isometric axes with line of unity
#' @param main default title
#' @param corr print Pearson correlation coefficient after title
#' @param group_codes append these to group values for display purposes
#' @param crit if ylog or xlog missing, log transform if mean/median ratio for non-missing values is greater than crit
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
  main = NULL,
  corr = FALSE,
  group_codes = NULL,
  crit = 1.3
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
  mn <- paste(mn,cor)
  if (!is.null(main)) mn <- paste(main, mn, sep = '\n')
  if(corr) main <- mn # which incorporates passed main if any
  metapanel <- function(x, y,...){
      if( length(groups))panel.superpose(x = x, y = y, ...)
      if(!length(groups)){
        if( density)panel.smoothScatter(x,y,...)
        if(!density)panel.xyplot(x,y,...)
      }
      foo <- loess.smooth(x,y)
      bar <- loess.smooth(y,x)
      if(ysmooth)try(panel.xyplot(foo$x,foo$y,col = 'black',lty = 'dashed',type = 'l'))
      if(xsmooth)try(panel.xyplot(bar$y,bar$x,col = 'black',lty = 'dashed',type = 'l'))
      if(length(yref))panel.abline(h = yref)
      if(length(xref))panel.abline(v = xref)
      if(iso)panel.abline(0,1)
    }
  xyplot(
    formula,
    data = y,
    groups = if(length(groups)) groups_ else NULL,
    auto.key = auto.key,
    aspect = 1,
    scales = scales,
    main = if(!is.null(main)) list(
      mn,
      cex = 1,
      fontface = 1
    ) else NULL,
    prepanel = prepanel,
    panel = metapanel,
    ...
  )
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
  crit = 1.3
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
    crit = crit,
    group_codes = gc,
    ylab = ylab,
    xlab = xlab,
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
#' @param main whether to include title indicating x and y items
#' @param crit if log is missing, log transform if mean/median ratio for non-missing x  is greater than this value
#' @param ref optional reference line on continuous axis
#' @param guide optional encoding for categories see \code{encode::encode}
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
  main = TRUE,
  crit = 1.3,
  ref = NULL,
  guide = NA_character_,
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
  # if(is.null(ref)) if(ymeta$TYPEC %in% c('IIV','RESIDUAL') ) ref <- 0
  scales <- list(
    tck = c(1,0),
    x = list(log = log,equispaced.log = FALSE)
  )
  mn <- paste(sep = ' ~ ',.y,.x)
  bwplot(
    formula,
    data = y,
    aspect = 1,
    horizontal = horizontal,
    main = if(main) list(
      mn,
      cex = 1,
      fontface = 1
    ) else NULL,
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
  main = TRUE,
  crit = 1.3,
  ref = NULL,
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
#' Generic axis label.
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @family generic functions
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
  val <- x %>% dplyr::filter(META %>% is.na) %>% dplyr::filter(VARIABLE == var) %$% VALUE
  if(length(val) ==0)stop('no values found for ',var)
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

