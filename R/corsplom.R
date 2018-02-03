#' Correlated Splom
#'
#' Scatterplot matrix with correlations.
#' @param x object
#' @param ... passed arguments
#' @export
#' @family generic functions
#' @family corsplom
corsplom <- function(x,...)UseMethod('corsplom')

#' Correlated Scatterplot Matrix Function for Data Frame
#'
#' Creates a scatterplot matrix with correlations in lower panel, by default.
#' @param x data.frame
#' @param xvar variables to plot
#' @param upper.panel passed to splom
#' @param lower.panel passed to splom
#' @param diag.panel passed to splom
#' @param pscales passed to splom
#' @param xlab passed to splom, can be function(x = x, var = xvar, ...)
#' @param varname.cex passed to splom
#' @param main character, or a function of x, xvar
#' @param sub character, or a function of x, xvar
#' @param ... extra arguments passed to \code{\link[lattice]{splom}}
#' @export
#' @importFrom rlang UQS
#' @family multivariate plots
#' @family corsplom
#' @family metaplot
corsplom_data_frame <- function(
  x,
  xvar = names(x),
  upper.panel = getOption('metaplot_upper.panel',corsplom_panel_scatter),
  lower.panel= getOption('metaplot_lower.panel',corsplom_panel_correlation),
  diag.panel = getOption('metaplot_diag.panel',corsplom_panel_diagonal),
  pscales= getOption('metaplot_pscales',0),
  xlab = getOption('metaplot_corsplom_xlab',NULL),
  varname.cex = getOption('metaplot_varname.cex',1),
  main = getOption('metaplot_main',NULL),
  sub = getOption('metaplot_sub',NULL),
  ...
){
  if(is.character(xlab)) xlab <- tryCatch(match.fun(xlab), error = function(e)xlab)
  if(is.function(xlab)) xlab <- xlab(x, xvar, ...)
  if(is.null(xlab)) xlab <- ''

  stopifnot(inherits(x, 'data.frame'))
  if(!is.null(main))if(is.function(main)) main <- main(x = x, xvar = xvar, ...)
  if(!is.null(sub))if(is.function(sub)) sub <- sub(x = x, xvar = xvar, ...)
  x <- x[,xvar,drop=FALSE]
  splom(
    x,
    upper.panel = upper.panel,
    lower.panel = lower.panel,
    diag.panel = diag.panel,
    pscales = pscales,
    xlab = xlab,
    varname.cex = varname.cex,
    main = main,
    sub = sub,
    .data = x,
    split = split,
    ...
  )
}
#' Correlated Scatterplot Matrix Method for Data Frame
#'
#' Creates a scatterplot matrix.  Parses arguments and generates the call: fun(x, xvar, ...).
#' @param x data.frame
#' @param ... passed to fun
#' @param fun function to do the actual plotting
#' @export
#' @importFrom rlang UQS quos
#' @family multivariate plots
#' @family corsplom
#' @family methods
corsplom.data.frame <- function(
  x,
  ...,
  fun = getOption('metaplot_corsplom','corsplom_data_frame')
){
  args <- quos(...)
  args <- lapply(args,f_rhs)
  vars <- args[names(args) == '']
  other <- args[names(args) != '']
  vars <- sapply(vars, as.character)
  prime <- list(x = x, xvar = vars)
  args <- c(prime, other)
  do.call(fun, args)
}

