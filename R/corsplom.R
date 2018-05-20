# bug: splom labels are NA if symbol missing
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
#' @param col point color
#' @param loess.col loess color
#' @param loess.lty loess line type
#' @param loess.alpha loess alpha
#' @param density whether to plot density polygons
#' @param diag.label label for the diagonal; can be a function of x, varname, .data
#' @param pin location for a pin (reference line) in the density region; can be a function of x, varname, .data
#' @param pin.col color of pin, if any
#' @param pin.alpha alpha transparency of pin
#' @param dens.col color for density region
#' @param dens.scale inflation factor for height of density smooth
#' @param dens.alpha alpha transparency for density region
#' @param gg logical: whether to generate \code{ggplot} instead of \code{trellis}
#' @param ... extra arguments passed to \code{\link[lattice]{splom}}
#' @export
#' @importFrom rlang UQS
#' @importFrom GGally ggpairs
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
  col = getOption('metaplot_corsplom_point_col','blue'),
  loess.col = getOption('metaplot_loess.col',col),
  loess.lty = getOption('metaplot_loess.lty','solid'),
  loess.alpha = getOption('metaplot_loess.alpha',1),
  density = TRUE,
  diag.label = getOption('metaplot_diag.label',diag_label),
  pin = getOption('metaplot_pin',diag_pin),
  pin.col = getOption('metaplot_pin.col','darkgrey'),
  pin.alpha = getOption('metaplot_pin.alpha',1),
  dens.col = getOption('metaplot_dens.col','grey'),
  dens.scale = getOption('metaplot_dens.scale',0.2),
  dens.alpha = getOption('metaplot_dens.alpha',0.5),
  gg = getOption('metaplot_gg',FALSE),
  ...
){
  if(is.character(xlab)) xlab <- tryCatch(match.fun(xlab), error = function(e)xlab)
  if(is.function(xlab)) xlab <- xlab(x, xvar, ...)
  if(is.null(xlab)) xlab <- ''

  stopifnot(inherits(x, 'data.frame'))
  if(!is.null(main))if(is.function(main)) main <- main(x = x, xvar = xvar, ...)
  if(!is.null(sub))if(is.function(sub)) sub <- sub(x = x, xvar = xvar, ...)
  x <- x[,xvar,drop=FALSE]

  # https://stackoverflow.com/questions/35085261/how-to-use-loess-method-in-ggallyggpairs-using-wrap-function
  my_fn <- function(data, mapping, pts=list(), smt=list(), ...){
    ggplot(data = data, mapping = mapping, ...) +
      do.call(geom_point, pts) +
      do.call(geom_smooth, smt)
  }
# ggpairs(swiss[1:4],
#           lower = list(continuous =
#                          wrap(my_fn,
#                               pts=list(size=2, colour="red"),
#                               smt=list(method="lm", se=F, size=5, colour="blue"))))
  if(gg)return(
    ggpairs(
      head(x,nrow(x)), # head strips attributes
      lower = list(
        continuous = wrap(
          my_fn,
          smt = list(
            method = 'loess',
            se = F,
            colour = loess.col,
            alpha = loess.alpha,
            linetype = loess.lty
          )
        )
      )
    ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  )

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
    col = col,
    loess.col = loess.col,
    loess.lty = loess.lty,
    loess.alpha = loess.alpha,
    density = density,
    diag.label = diag.label,
    pin = pin,
    pin.col = pin.col,
    pin.alpha = pin.alpha,
    dens.col = dens.col,
    dens.scale = dens.scale,
    dens.alpha = dens.alpha,
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

