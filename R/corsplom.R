globalVariables(c(
  'z1'
))

#' Correlated Splom
#'
#' Scatterplot matrix with correlations.
#' @param x object
#' @param ... passed arguments
#' @export
#' @family generic functions
#' @family corsplom
#' @importFrom GGally ggpairs
#' @importFrom GGally wrap
#' @importFrom GGally ggally_cor
#' @importFrom utils head
corsplom <- function(x,...)UseMethod('corsplom')

#' Correlated Scatterplot Matrix Function for Data Frame
#'
#' Creates a scatterplot matrix with correlations in lower panel, by default.
#' @param x data.frame
#' @param xvar variables to plot
#' @param upper.panel passed to \code{\link[lattice]{splom}} or (if gg=T) \code{\link[GGally]{ggpairs}} (as 'upper')
#' @param lower.panel passed to \code{\link[lattice]{splom}} or (if gg=T) \code{\link[GGally]{ggpairs}} (as 'lower')
#' @param diag.panel passed to \code{\link[lattice]{splom}} or (if gg=T) \code{\link[GGally]{ggpairs}} (as 'diagonal')
#' @param pscales passed to \code{\link[lattice]{splom}}
#' @param xlab can be function(x = x, var = xvar, ...)
#' @param varname.cex text size multiplier
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
#' @param ... extra arguments passed to \code{\link[lattice]{splom}} and \code{\link[GGally]{ggpairs}} (including upper,lower, and diag)
#' @export
#' @importFrom rlang UQS
#' @importFrom GGally ggpairs
#' @family multivariate plots
#' @family corsplom
#' @family metaplot
#' @examples
#'
#' library(magrittr)
#' library(dplyr)
#' library(csv)

#' x <- as.csv(system.file(package = 'metaplot', 'extdata/theoph.csv'))
#' x %<>% pack

#' # options(metaplot_gg = TRUE)

#' x %>% metaplot(lKe, lKa, lCl)
#' x %>% metaplot(
#'   lKe, lKa, lCl,
#'   col = 'black',loess.col = 'red', pin.col = 'red',
#'   dens.col='blue',dens.alpha = 0.1
#' )

corsplom_data_frame <- function(
  x,
  xvar = names(x),
  upper.panel = getOption('metaplot_upper.panel',if(gg) corsplom_gg_scatter else corsplom_panel_scatter),
  lower.panel= getOption('metaplot_lower.panel',if(gg) corsplom_gg_correlation else corsplom_panel_correlation),
  diag.panel = getOption('metaplot_diag.panel',if(gg) corsplom_gg_diagonal else corsplom_panel_diagonal),
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

  if(gg)return(
    ggpairs(
      head(x,nrow(x)), # head strips attributes
      lower = list(
        continuous = wrap(
          match.fun(lower.panel),
          col = col,
          loess.col = loess.col,
          loess.alpha = loess.alpha,
          loess.lty = loess.lty,
          ...
        )
      ),
      upper = list(
        continuous = wrap(
          match.fun(upper.panel),
          col = col,
          loess.col = loess.col,
          loess.alpha = loess.alpha,
          loess.lty = loess.lty,
          ...
        )
      ),
      diag = list(
        continuous = wrap(
          match.fun(diag.panel),
          col = col,
          density = density,
          varname.cex = varname.cex,
          diag.label = diag.label,
          pin = pin,
          pin.col = pin.col,
          pin.alpha = pin.alpha,
          dens.col = dens.col,
          dens.scale = dens.scale,
          dens.alpha = dens.alpha,
          .data = x,
          varname.cex = varname.cex,
          ...
        )
      ),
      # showStrips = FALSE,
      axisLabels = 'none'
    )  + theme(aspect.ratio = 1)
       + theme(
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      strip.text.y = element_blank()
    )
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
    as.matrix = TRUE,
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

#' Correlation GG Function for GG Metaplot Corsplom
#'
#' Default lower panel function for corsplom_data_frame() with \code{gg = TRUE}. Plots Pearson correlation coefficient.
#' corsplom_data_frame() typically supplies loess aesthetics in case smooth is desired; loess arguments are defined to
#' suppress corresponding warnings.
#' @param data data
#' @param mapping mapping
#' @param col text color
#' @param loess.col ignored
#' @param loess.lty ignored
#' @param loess.alpha ignored
#' @param ... ignored
#' @keywords internal
#' @export
#' @family panel functions
corsplom_gg_correlation = function(
  data, mapping, col = 'blue',
  loess.col, loess.lty, loess.alpha, ...
)ggally_cor(
  data = data,
  mapping = mapping,
  colour = col,
  ...
) + theme(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_rect(fill = NA)
)

#' Scatter GG Function for Metaplot Corsplom
#'
#' Default upper panel function for corsplom_data_frame with \code{gg = TRUE}. Plots data with loess smooth.
#' @param data data
#' @param mapping mapping
#' @param col point color
#' @param loess.col loess color
#' @param loess.lty loess line type
#' @param loess.alpha loess alpha
#' @param ... passed arguments
#' @keywords internal
#' @export
#' @family panel functions
# https://stackoverflow.com/questions/35085261/how-to-use-loess-method-in-ggallyggpairs-using-wrap-function
corsplom_gg_scatter = function(
  data,
  mapping,
  method = 'loess',
  se = F,
  col = 'blue',
  loess.col = 'blue',
  loess.alpha = 1,
  loess.lty = 'solid',
  ...
){
  x <- as.character(mapping$x)
  y <- as.character(mapping$y)
  lim1 <- range(data[[x]],na.rm = TRUE)
  lim2 <- range(data[[y]],na.rm = TRUE)

  ggplot(data = data, mapping = mapping, ...) +
    xlim(lim1[[1]],lim1[[2]]) +
    ylim(lim2[[1]],lim2[[2]]) +
    geom_point(colour = col) +
    geom_smooth(
      method = method,
      se = se,
      colour = loess.col,
      alpha = loess.alpha,
      linetype = loess.lty,
      ...
    )
}

#' Diagonal GG Function for Metaplot Corsplom
#'
#' Default diagonal panel function for corsplom_data_frame with \code{gg = TRUE}. Plots a density smooth against the corresponding axis from within the diagonal panel.  Plots a grey pin at each axis zero. Formats and displays the variable name.

#' @param data data
#' @param mapping mapping
#' @param .data copy of original dataset
#' @param diag.label label for the diagonal; can be a function of x, varname, .data
#' @param pin location for a pin (reference line) in the density region; can be a function of x, varname, .data
#' @param pin.col color of pin, if any
#' @param pin.alpha alpha transparency of pin
#' @param density whether to plot density polygons
#' @param dens.col color for density region
#' @param dens.scale inflation factor for height of density smooth
#' @param dens.alpha alpha transparency for density region
#' @param varname.cex text size multiplier
#' @param ... passed arguments
#' @keywords internal
#' @export
#' @family panel functions
#' @seealso \code{\link{corsplom}}

corsplom_gg_diagonal <- function(
  data,
  mapping,
  .data,
  density = TRUE,
  diag.label = getOption('metaplot_diag.label',diag_label),
  pin = getOption('metaplot_pin',diag_pin),
  pin.col = getOption('metaplot_pin.col','darkgrey'),
  pin.alpha = getOption('metaplot_pin.alpha',1),
  dens.col = getOption('metaplot_dens.col','grey'),
  dens.scale = getOption('metaplot_dens.scale',0.2),
  dens.alpha = getOption('metaplot_dens.alpha',0.5),
  varname.cex = getOption('metaplot_corsplom_varname_cex', 1),
  ...
){
  if(density){
    x <- as.character(mapping$x)
    data$x <- data[[x]]

    lim <- range(data$x,na.rm = TRUE)
    lo <- lim[[1]]
    hi <- lim[[2]]
    len <- hi - lo
    me <- hi/2 + lo/2
    d <- density(data$x,na.rm=TRUE, from = lo, to = hi)
    d <- data.frame(x1 = d$x, y1 = d$y)
    # ensure start and end points are minima
    d <- rbind(
      data.frame(x1 = d$x1[[1]], y1 = min(d$y1)),
      d,
      data.frame(x1 = d$x1[[nrow(d)]], y1 = min(d$y1))
    )

    d$y1 <- d$y1 / max(d$y1,na.rm = TRUE)
    d$y1 <- d$y1 * len * dens.scale
    d$z1 <- hi - d$y1
    #d$y1 <- d$y1 + lo
    p <- ggplot(data = d)
    p <- p + xlim(lo,hi)
    p <- p + ylim(lo,hi)
    p <- p +
      geom_polygon(
        mapping = aes(x = x1, y = z1),
        col = NA,
        fill = dens.col,
        alpha = dens.alpha
      )
    p <- p +
      geom_polygon(
        mapping = aes(x = z1, y = x1),
        col = NA,
        fill = dens.col,
        alpha = dens.alpha
      )

    if(is.function(pin)) pin <- pin(x = x, varname = x, .data = .data, ...)
    ref <- pin
    ref <- as.numeric(ref)
    ref <- ref[is.defined(ref)]
    if(length(ref)){
      x0 = ref
      x1 = ref
      y0 = rep(hi, length(ref))
      y1 = rep(hi - len * dens.scale, length(ref))
      p <- p + geom_segment(
        data = data.frame(y0 = y0, y1 = y1, x0 = x0, x1 = x1),
        mapping = aes(x = x0, y = y0, xend = x1, yend = y1),
        col = pin.col, alpha = pin.alpha
      )
      y0 = ref
      y1 = ref
      x0 = rep(hi, length(ref))
      x1 = rep(hi - len * dens.scale, length(ref))
      p <- p + geom_segment(
        data = data.frame(y0 = y0, y1 = y1, x0 = x0, x1 = x1),
        mapping = aes(x = x0, y = y0, xend = x1, yend = y1),
        col = pin.col, alpha = pin.alpha
      )
    }

  if(is.character(diag.label))diag.label <- match.fun(diag.label)
  if(is.function(diag.label))diag.label <- diag.label(varname = x, .data = .data, ...)
  #if(is.expression(diag.label))diag.label <- as.character(diag.label)
  p <- p + annotate(
    'text',
    x = me,
    y = me,
    label = as.character(diag.label),
    parse = is.expression(diag.label),
    size = 4 * varname.cex
  )

  # diag.panel.splom(varname = diag.label, ...)
  p
}}
