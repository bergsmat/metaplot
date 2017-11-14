#' Density
#'
#' Creates a density metaplot.
#' @param x object
#' @param ... passed arguments
#' @export
#' @family generic functions
dens <- function(x,...)UseMethod('dens')

#' Plot Density for Data Frame, Standard Evaluation
#'
#' Plot density for object of class 'data.frame' using standard evaluation.
#' @param x data.frame
#' @param var item to plot, given as length-one character
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
#' @family dens
#' @examples
#' #dens(Theoph, Wt, grid = T )
#' dens_data_frame(Theoph, 'Wt', grid = TRUE)
dens_data_frame<- function(
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
  stopifnot(inherits(x, 'data.frame'))
  stopifnot(length(var) == 1)
  stopifnot(is.character(var))
  if(log)if(any(x[[var]] <= 0, na.rm = TRUE))stop('cannot take log of negative values')
  if(is.null(scales)) scales <- list(tck = c(1,0),x = list(log = log,equispaced.log = FALSE))
  if(is.null(panel)) panel <- function(ref = NULL, ...){
    panel.densityplot(...)
    if(length(ref))panel.abline(v = ref)
  }
  #if(is.null(xlab)) xlab <- as.character(f_rhs(var))
  default_xlab <- var
  xvarlab <- attr(x[[var]],'label')
  if(!is.null(xvarlab)) default_xlab <- xvarlab
  if(is.null(xlab)) xlab <- default_xlab
  densityplot(x[[var]], xlab = xlab, ref = ref, log = log, aspect = aspect, scales = scales, panel = panel, ...)
}
#' Plot Density for Data Frame, Nonstandard Evaluation
#'
#' Plot density for object of class 'data.frame'.
#' @param x data.frame
#' @param ... item to plot, given as unquoted column name
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
#' @importFrom rlang f_rhs
#' @family dens
#' @examples
#' dens(Theoph, Wt, grid = TRUE )
dens.data.frame<- function(
  x,
  ...,
  xlab = NULL,
  ref = NULL,
  log = FALSE,
  aspect = 1,
  scales = NULL,
  panel = NULL
){
  args <- quos(...)
  args <- lapply(args,f_rhs)
  var <- args[names(args) == '']
  other <- args[names(args) != '']
  var <- sapply(var, as.character)
  if(length(var) < 1) stop('dens() requires an item to plot')
  if(length(var) > 1)warning('only retaining the first item')
  var <- var[[1]] # take first (perh. only)
  main <- list(x = x, var = var)
  formal <- list(xlab = xlab, ref = ref, aspect = aspect, scales = scales, panel = panel)
  args <- c(main, formal, other)
  do.call(dens_data_frame, args)
}


