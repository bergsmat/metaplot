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
#' @param ... item to plot
#' @param xlab x axis label
#' @param ref optional numeric
#' @param log whether to use log scale
#' @param aspect passed to \code{\link[lattice]{densityplot}}
#' @param scales  passed to \code{\link[lattice]{densityplot}}
#' @param panel  passed to \code{\link[lattice]{densityplot}}
#' @family univariate plots
#' @describeIn dens data.frame method
#' @importFrom rlang get_expr quo
#' @import lattice
#' @export
#' @examples
#' dens(Theoph, Wt, grid = T )
#' dens(Theoph, 'Wt')
dens.data.frame<- function(
  x,
  var,
  xlab = NULL,
  ref = NULL,
  log = FALSE,
  aspect = 1,
  scales = NULL,
  panel = NULL,
  ...
){
  #var <- quo(var)
  # args <- lapply(args,f_rhs)
  # var <- args[names(args) == '']
  # other <- args[names(args) != '']
  # var <- sapply(var, as.character)
  # if(length(var) < 1) stop('dens() requires an item to plot')
  # if(length(var) > 1)warning('only retaining the first item')
  # var <- var[[1]] # take first/only list member (quosure)
  if(is.null(scales)) scales <- list(tck = c(1,0),x = list(log = log,equispaced.log = FALSE))
  if(is.null(panel)) panel <- function(ref = NULL, ...){
    panel.densityplot(...)
    if(length(ref))panel.abline(v = ref)
  }
  if(is.null(xlab)) xlab <- as.character(f_rhs(var))
  densityplot(UQ(var), data = x, xlab = xlab, ref = ref, log = log, aspect = aspect, scales = scales, panel = panel, ...)
}

#' Plot Density for Folded
#'
#' Plot density for object of class 'folded'.
#' @describeIn dens folded method
#' @importFrom rlang UQS
#' @export
dens.folded <- function(
  x,
  ... ,
  xlab = NULL,
  ref = NULL,
  log = FALSE
){
  args <- quos(...)
  args <- lapply(args,f_rhs)
  var <- args[names(args) == '']
  other <- args[names(args) != '']
  var <- sapply(var, as.character)
  x <- x[!is.na(x$VARIABLE),] # table-level metadata is unused
  x <- data.frame(x, stringsAsFactors = FALSE, fix.empty.names=FALSE, check.names=FALSE) # faster than grouped_df
  class(x) <- c('folded','data.frame')
  if(length(var) < 1) stop('dens() requires an item to plot')
  if(length(var) > 1) {
    warning('only retaining the first item')
    var <- var[[1]]
  }
  d <- unfold(x,UQS(var))
  if(is.null(xlab)) xlab <- axislabel(x,var,log = log)
  args <- c(
    list(
      x = d,
      ref = ref,
      log = log,
      xlab = xlab
    ),
    var,
    other
  )
  # dens(d, var = var, ref = ref, log = log, xlab = xlab, ...)
  do.call(dens,args)
}
