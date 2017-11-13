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
