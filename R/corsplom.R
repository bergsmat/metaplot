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
