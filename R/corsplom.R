#' Correlated Splom
#'
#' Scatterplot matrix with correlations.
#' @param x object
#' @param ... passed arguments
#' @export
#' @family generic functions
#' @family corsplom
#' @family metaplot
corsplom <- function(x,...)UseMethod('corsplom')

#' Correlated Scatterplot Matrix Function for Data Frame
#'
#' Creates a scatterplot matrix with correlations in lower panel, by default.
#' @param x data.frame
#' @param xvar variables to plot
#' @param upper.panel passed to splom
#' @param lower.panel passed to splom
#' @param pscales passed to splom
#' @param xlab passed to splom
#' @param varname.cex passed to splom
#' @param diag.panel passed to splom
#' @param split break diagonal names on white space
#' @param main character, or a function of x, xvar
#' @param sub character, or a function of x, xvar
#' @param ... extra arguments passed to \code{\link[lattice]{splom}}
#' @export
#' @importFrom rlang UQS
#' @family multivariate plots
#' @family corsplom
corsplom_data_frame <- function(
  x,
  xvar = names(x),
  upper.panel = u.p,
  lower.panel= l.p,
  pscales= 0,
  xlab = '',
  varname.cex = 1,
  diag.panel = my.diag.panel,
  split = TRUE,
  main = getOption('metaplot_main',NULL),
  sub = getOption('metaplot_sub',NULL),
  ...
){
  stopifnot(inherits(x, 'data.frame'))
  x <- x[,xvar,drop=FALSE]
  label <- lapply(x,attr,'label')
  label[sapply(label, is.null)] <- ''
  label <- as.character(label)
  stopifnot(all(sapply(label,length) <= 1))
  i <- is.defined(label) & label != ''
  names(x)[i] <- label[i]

  if(split) names(x) <- fracture(names(x))

  if(!is.null(main))if(is.function(main)) main <- main(x = x, xvar = xvar, ...)
  if(!is.null(sub))if(is.function(sub)) sub <- sub(x = x, xvar = xvar, ...)

  splom(
    x,
    upper.panel = upper.panel,
    lower.panel = lower.panel,
    pscales = pscales,
    xlab = xlab,
    varname.cex = varname.cex,
    diag.panel = diag.panel,
    main = main,
    sub = sub,
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

