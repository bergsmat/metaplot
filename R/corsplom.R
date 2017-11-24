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
#' @param var variables to plot
#' @param upper.panel passed to splom
#' @param lower.panel passed to splom
#' @param pscales passed to splom
#' @param xlab passed to splom
#' @param varname.cex passed to splom
#' @param diag.panel passed to splom
#' @param split break diagonal names on white space
#' @param ... extra arguments passed to \code{\link[lattice]{splom}}
#' @export
#' @importFrom rlang UQS
#' @family multivariate plots
#' @family corsplom
corsplom_data_frame <- function(
  x,
  var = names(x),
  upper.panel = u.p,
  lower.panel= l.p,
  pscales= 0,
  xlab = '',
  varname.cex = 1,
  diag.panel = my.diag.panel,
  split = TRUE,
  ...
){
  stopifnot(inherits(x, 'data.frame'))
  labels <- sapply(x,attr,'label')
  if(all(is.defined(labels))) names(x) <- labels
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
#' Correlated Scatterplot Matrix Method for Data Frame
#'
#' Creates a scatterplot matrix using nonstandard evaluation.
#' @param x data.frame
#' @param ... variables to plot as unquoted character strings
#' @param upper.panel passed to splom
#' @param lower.panel passed to splom
#' @param pscales passed to splom
#' @param xlab passed to splom
#' @param varname.cex passed to splom
#' @param diag.panel passed to splom
#' @param split break diagonal names on white space
#' @param fun function to do the actual plotting
#' @export
#' @importFrom rlang UQS
#' @family multivariate plots
#' @family corsplom
corsplom.data.frame <- function(
  x,
  ...,
  upper.panel = u.p,
  lower.panel= l.p,
  pscales= 0,
  xlab = '',
  varname.cex = 1,
  diag.panel = my.diag.panel,
  split = TRUE,
  fun = getOption('metaplot_corsplom','corsplom_data_frame')
){
  args <- quos(...)
  args <- lapply(args,f_rhs)
  vars <- args[names(args) == '']
  other <- args[names(args) != '']
  vars <- sapply(vars, as.character)
  prime <- list(x = x, vars = vars)
  formal <- list(
   upper.panel = upper.panel,
   lower.panel = lower.panel,
   xlab = '',
   varname.cex = varname.cex,
   split = split
  )
  args <- c(prime, formal, other)
  do.call(fun, args)
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
#' @family corsplom
#' @param x folded
#' @param ... unquoted names of variables to plot, or other named arguments
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
