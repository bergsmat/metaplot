#' Density Plot
#'
#' Creates a density plot.
#' @param x object
#' @param ... passed arguments
#' @export
#' @family generic functions
#' @family univariate plots
#' @family densplot
#' @family metaplot
densplot <- function(x,...)UseMethod('densplot')

#' Density Function for Data Frame
#'
#' Plot density for object of class 'data.frame' using standard evaluation.
#' @param x data.frame
#' @param xvar variable to plot
#' @param groups optional grouping variable
#' @param facets optional conditioning variables
#' @param xlab x axis label
#' @param ref optional numeric
#' @param log whether to use log scale
#' @param aspect passed to \code{\link[lattice]{densityplot}}
#' @param scales  passed to \code{\link[lattice]{densityplot}}
#' @param panel  passed to \code{\link[lattice]{densityplot}}
#' @param auto.key  passed to \code{\link[lattice]{densityplot}}
#' @param keycols number of auto.key columns
#' @param main character, or a function of x, xvar, groups, facets, and log
#' @param sub character, or a function of x, xvar, groups, facets, and log
#' @param ... passed to \code{\link[lattice]{densityplot}}
#' @family univariate plots
#' @family densplot
#' @import lattice
#' @export
#' @examples
#' densplot_data_frame(Theoph, 'conc', grid = TRUE)
#' densplot_data_frame(Theoph, 'conc', 'Subject')
#' densplot_data_frame(Theoph, 'conc', , 'Subject')
densplot_data_frame<- function(
  x,
  xvar,
  groups = NULL,
  facets = NULL,
  xlab = NULL,
  ref = NULL,
  log = FALSE,
  aspect = 1,
  scales = NULL,
  panel = NULL,
  auto.key = NULL,
  keycols = NULL,
  main = getOption('metaplot_main',NULL),
  sub = getOption('metaplot_sub',NULL),
  ...
){
  stopifnot(inherits(x, 'data.frame'))
  stopifnot(length(xvar) == 1)
  stopifnot(is.character(xvar))
  if(log)if(any(x[[xvar]] <= 0, na.rm = TRUE)){
    warning('cannot take log of negative values')
    log <- FALSE
  }
  if(is.null(scales)) scales <- list(tck = c(1,0),x = list(log = log,equispaced.log = FALSE))
  if(is.null(panel)) panel <- function(ref = NULL, ...){
    panel.densityplot(...)
    if(length(ref))panel.abline(v = ref)
  }
  default_xlab <- xvar
  xvarlab <- attr(x[[xvar]],'label')
  if(!is.null(xvarlab)) default_xlab <- xvarlab
  if(is.null(xlab)) xlab <- default_xlab

  if(is.null(keycols))if(!is.null(groups))keycols <- min(3, length(unique(x[[groups]])))
  if(is.null(auto.key))if(!is.null(groups))if(length(unique(x[[groups]])) > 1) auto.key = list(columns = keycols,lines=TRUE)

  ff <- character(0)
  if(!is.null(facets))ff <- paste(facets, collapse = ' + ')
  if(!is.null(facets))ff <- paste0('|',ff)
  formula <- as.formula(paste0('~', xvar) %>% paste(ff))
  if(!is.null(facets)){
    for (i in seq_along(facets)) x[[facets[[i]]]] <- ifcoded(x, facets[[i]])
  }
  if(!is.null(main))if(is.function(main)) main <- main(x = x, xvar = xvar, groups = groups, facets = facets, log = log, ...)
  if(!is.null(sub))if(is.function(sub)) sub <- sub(x = x, xvar = xvar, groups = groups, facets = facets, log = log, ...)
  if(!is.null(groups)) {
    x[[groups]] <- ifcoded(x, groups)
    groups <- as.formula(paste('~',groups))
  }
  densityplot(
    formula,
    data = x,
    groups = groups,
    xlab = xlab,
    ref = ref,
    log = log,
    aspect = aspect,
    scales = scales,
    panel = panel,
    auto.key = auto.key,
    main = main,
    sub = sub,
    ...
  )
}
#' Densplot Method for Data Frame
#'
#' Plot density for object of class 'data.frame'. Parses arguments and generates the call: fun(x, xvar, groups, facets,...).
#' @param x data.frame
#' @param ... passed to fun
#' @param fun plotting function
#' @import lattice
#' @export
#' @importFrom rlang f_rhs quos
#' @family univariate plots
#' @family densplot
#' @examples
#' densplot(Theoph, conc, grid = TRUE )
#' densplot(Theoph, conc, Subject )
#' densplot(Theoph, conc, , Subject )
#' attr(Theoph,'title') <- 'Theophylline'
#' densplot(Theoph, conc, main= function(x,...)attr(x,'title'))
#' densplot(Theoph, conc, sub= function(x,...)attr(x,'title'))

densplot.data.frame<- function(
  x,
  ...,
  fun = getOption('metaplot_dens','densplot_data_frame')
){
  args <- quos(...)
  args <- lapply(args,f_rhs)
  var <- args[names(args) == '']
  other <- args[names(args) != '']
  var <- sapply(var, as.character)

  # this function needs to explicitly assign xvar, groups, and facets
  xvar <- var[[1]]
  groups <- NULL
  if(length(var) > 1) groups <- var[[2]]
  if(!is.null(groups))if(groups == '') groups <- NULL
  facets <- NULL
  if(length(var) > 2) facets <- var[3:length(var)]

  prime <- list(x = x, xvar = xvar, groups = groups, facets = facets)
  args <- c(prime, other)
  do.call(fun, args)
}

