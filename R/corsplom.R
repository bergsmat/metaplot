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
#' @param par.settings passed to \code{\link[lattice]{xyplot}} (calculated if NULL)
#' @param padding numeric (will be recycled to length 4) giving plot margins in default units: top, right, bottom, left (in multiples of 5.5 points for ggplot)
#' @param as.table diagonal arranged top-left to bottom-right
#' @param upper whether density plots in diagonal should face the upper triangle vs. lower
#' @param gg logical: whether to generate \code{ggplot} instead of \code{trellis}
#' @param ... extra arguments passed to \code{\link[lattice]{splom}} and \code{\link[GGally]{ggpairs}} (including upper,lower, and diag)
#' @export
#' @return trellis or grob
#' @importFrom rlang UQS
#' @importFrom gtable gtable_add_padding
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
  upper.panel = getOption('metaplot_corsplom_upper_panel',if(gg) corsplom_gg_scatter else corsplom_panel_scatter),
  lower.panel= getOption('metaplot_corsplom_lower_panel',if(gg) corsplom_gg_correlation else corsplom_panel_correlation),
  diag.panel = getOption('metaplot_corsplom_diag_panel',if(gg) corsplom_gg_diagonal else corsplom_panel_diagonal),
  pscales= getOption('metaplot_corsplom_pscales',0),
  xlab = getOption('metaplot_corsplom_xlab',NULL),
  varname.cex = getOption('metaplot_corsplom_varname_cex',1),
  main = getOption('metaplot_corsplom_main',NULL),
  sub = getOption('metaplot_corsplom_sub',NULL),
  col = getOption('metaplot_corsplom_point_col','blue'),
  loess.col = getOption('metaplot_corsplom_loess_col',col),
  loess.lty = getOption('metaplot_corsplom_loess_lty','solid'),
  loess.alpha = getOption('metaplot_corsplom_loess_alpha',1),
  density = getOption('metaplot_corsplom_density',TRUE),
  diag.label = getOption('metaplot_corsplom_diag_label',diag_label),
  pin = getOption('metaplot_corsplom_pin',diag_pin),
  pin.col = getOption('metaplot_corsplom_pin_col','darkgrey'),
  pin.alpha = getOption('metaplot_corsplom_pin_alpha',1),
  dens.col = getOption('metaplotcorsplom_dens_col','grey'),
  dens.scale = getOption('metaplot_corsplom_dens_scale',0.2),
  dens.alpha = getOption('metaplot_corsplom_dens_alpha',0.5),
  par.settings = getOption('metaplot_corsplom_par_settings',NULL),
  padding = getOption('metaplot_corsplom_padding', 1),
  as.table = getOption('metaplot_corsplom_as_table', FALSE),
  upper = getOption('metaplot_corsplom_upper', TRUE),
  gg = getOption('metaplot_corsplom_gg',FALSE),
  ...
){
  stopifnot(length(as.table) == 1, is.logical(as.table))
  if(is.character(xlab)) xlab <- tryCatch(match.fun(xlab), error = function(e)xlab)
  if(is.function(xlab)) xlab <- xlab(x, xvar, ...)
  if(is.null(xlab)) xlab <- ''

  stopifnot(is.numeric(padding))
  padding <- rep(padding, length.out = 4)
  par.settings = parintegrate(par.settings, padding)
  if(gg)padding <- unit(padding * 5.5, 'pt')
  stopifnot(inherits(x, 'data.frame'))
  if(!is.null(main))if(is.function(main)) main <- main(x = x, xvar = xvar, ...)
  if(!is.null(sub))if(is.function(sub)) sub <- sub(x = x, xvar = xvar, ...)
  x <- x[,xvar,drop=FALSE]

  if(gg){
    if(is.character(upper.panel)) upper.panel <- match.fun(upper.panel)
    if(is.character(lower.panel)) lower.panel <- match.fun(lower.panel)
    if(is.character(diag.panel)) diag.panel <- match.fun(diag.panel)
    ncol <- ncol(x)
    cols <- 1:ncol
    rows <- ncol:1
    if(as.table) rows <- 1:ncol
    res <- list()
    for(i in rows){
      for(j in cols){
        if(i == j){
          top <- FALSE
          right <- FALSE
          bottom <- FALSE
          left <- FALSE
          if(as.table & upper){
            top  <-  i != 1
            right <- i != ncol
          }
          if(!as.table & upper){
            top   <- i != ncol
            left  <- i != 1
          }
          if(as.table & !upper){
            bottom <- i != ncol
            left <- i != 1
          }
          if(!as.table & !upper){
            bottom <- i != 1
            right  <- i != ncol
          }
          p <- diag.panel(
            data = x,
            mapping = aes_string(x = names(x)[[i]]),
            col = col,
            density = density & c(top, right, bottom, left),
            varname.cex = varname.cex,
            diag.label = diag.label,
            pin = pin,
            pin.col = pin.col,
            pin.alpha = pin.alpha,
            dens.col = dens.col,
            dens.scale = dens.scale,
            dens.alpha = dens.alpha,
            .data = x,
            as.table = as.table,
            first = i == 1,
            last = i == ncol,
            ...
          )
        }
        if(i != j && xor(i < j, as.table)) p <- lower.panel(
          data = x,
          mapping = aes_string(x = names(x)[[j]], y = names(x)[[i]]),
          col = col,
          loess.col = loess.col,
          loess.alpha = loess.alpha,
          loess.lty = loess.lty,
          ...
        )
        if(i != j && xor(i > j, as.table)) p <- upper.panel(
          data = x,
          mapping = aes_string(x = names(x)[[j]], y = names(x)[[i]]),
          col = col,
          loess.col = loess.col,
          loess.alpha = loess.alpha,
          loess.lty = loess.lty,
          ...
        )

        p <- p + theme(aspect.ratio = 1)
        p <- p + theme(strip.background = element_blank())
        p <- p + theme(strip.text.x = element_blank())
        p <- p + theme(strip.text.y = element_blank())
        p <- p + theme(axis.title = element_blank())
        p <- p + theme(axis.ticks = element_blank())
        p <- p + theme(axis.line=element_blank())
        p <- p + theme(axis.text.x=element_blank())
        p <- p + theme(axis.text.y=element_blank())
        p <- p + theme(axis.ticks=element_blank())
        p <- p + theme(axis.title.x=element_blank())
        p <- p + theme(axis.title.y=element_blank())
        p <- p + theme(plot.margin = unit(c(0,0,0,0), 'pt'))
        res[[length(res) + 1]] <- p
      }
    }
    m <- do.call(
      arrangeGrob,
      c(
        res,
        ncol = ncol,
        nrow = ncol,
        respect = TRUE
      )
    )
    m <- gtable_add_padding(m, padding)
    class(m) <- c('metaplot_gtable', class(m))
    return(m)
  }

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
    as.matrix = as.table,
    par.settings = par.settings,
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
#' @param density whether to plot density polygons; can be a length 4 vector for top, right, bottom, left
#' @param dens.col color for density region
#' @param dens.scale inflation factor for height of density smooth
#' @param dens.alpha alpha transparency for density region
#' @param varname.cex text size multiplier
#' @param as.table diagonal arranged top-left to bottom-right
#' @param upper whether density plots should face the upper triangle (or lower, if FALSE)
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
  as.table = getOption('metaplot_corsplom_as_table', TRUE),
  upper = getOption('metaplot_corsplom_margin',TRUE),
  ...
){
  stopifnot(is.logical(density))
  density <- rep(density, length.out = 4)
  names(density) <- c('top','right','bottom','left')
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
  bottom <- rbind(
    data.frame(x1 = d$x1[[1]], y1 = min(d$y1)),
    d,
    data.frame(x1 = d$x1[[nrow(d)]], y1 = min(d$y1))
  )
  bottom %<>% mutate(y1 = y1 / max(y1, na.rm = TRUE)) # normalized
  bottom %<>% mutate(y1 = y1 * len * dens.scale)      # scaled
  bottom %<>% mutate(y1 = y1 + lo)                    # offset
  top <- bottom %>% mutate(y1 = hi - y1 + lo)              # inverted
  left <- bottom %>% mutate(z1 = y1, y1 = x1, x1 = z1)         # rotated
  right <- left %>% mutate(x1 = hi - x1 + lo)              # inverted
  p <- ggplot(mapping = aes(x = x1, y = y1)) + xlim(lo,hi) + ylim(lo,hi)

  if(density[['top']]) p <- p +
    geom_polygon(
      data = top,
      col = NA,
      fill = dens.col,
      alpha = dens.alpha
    )
  if(density[['right']]) p <- p +
    geom_polygon(
      data = right,
      col = NA,
      fill = dens.col,
      alpha = dens.alpha
    )
  if(density[['bottom']]) p <- p +
    geom_polygon(
      data = bottom,
      col = NA,
      fill = dens.col,
      alpha = dens.alpha
    )
  if(density[['left']]) p <- p +
    geom_polygon(
      data = left,
      col = NA,
      fill = dens.col,
      alpha = dens.alpha
    )

  if(is.function(pin)) pin <- pin(x = x, varname = x, .data = .data, ...)
  ref <- pin
  ref <- as.numeric(ref)
  ref <- ref[is.defined(ref)]
  if(length(ref)){
    bottom <- data.frame(
      x0 = ref,
      x1 = ref,
      y0 = rep(lo, length(ref)),
      y1 = rep(lo + len * dens.scale, length(ref))
    )
    top <- bottom %>% mutate(
      y0 = hi - y0 + lo,
      y1 = hi - y1 + lo
    )
    left <- bottom %>% mutate(
      z0 = x0, x0 = y0, z1 = x1, x1 = y1, y0 = z0, y1 = z1
    )
    right <- left %>% mutate(
      x0 = hi - x0 + lo,
      x1 = hi - x1 + lo
    )

    if(density[['top']]) p <- p + geom_segment(
      data = top,
      mapping = aes(x = x0, y = y0, xend = x1, yend = y1),
      col = pin.col, alpha = pin.alpha
    )
    if(density[['right']]) p <- p + geom_segment(
      data = right,
      mapping = aes(x = x0, y = y0, xend = x1, yend = y1),
      col = pin.col, alpha = pin.alpha
    )
    if(density[['bottom']]) p <- p + geom_segment(
      data = bottom,
      mapping = aes(x = x0, y = y0, xend = x1, yend = y1),
      col = pin.col, alpha = pin.alpha
    )
    if(density[['left']]) p <- p + geom_segment(
      data = left,
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
}

#' Plot a Metaplot GG Splom
#'
#' Plots a metaplot ggplot2-type splom. Underlying object type is 'gtable'.
#' Defaults for \code{link{corsplom_data_frame}} prepend class 'metaplot_gtable' to
#' modify plot and print functionality. This method invokes \code{\link[gridExtra]{grid.arrange}}.
#'
#' @param x metaplot_gtable
#' @param y ignored
#' @param \dots ignored
#' @keywords internal
#' @export
#' @return gtable
#' @family methods
#' @family corsplom
#' @seealso \code{\link{corsplom}}

plot.metaplot_gtable <- function(x, y, ...){
  class(x) <- setdiff(class(x), 'metaplot_gtable')
  grid.arrange(x)
}
#' Print a Metaplot GG Splom
#'
#' Prints a metaplot ggplot2-type splom. Underlying object type is 'gtable'.
#' Defaults for \code{link{corsplom_data_frame}} prepend class 'metaplot_gtable' to
#' modify plot and print functionality. Print method for 'gtable' summarizes the object.
#' This method invokes the plot method to give interactive behavior more like splom().
#'
#' @param x metaplot_gtable
#' @param \dots ignored
#' @keywords internal
#' @export
#' @return gtable
#' @family methods
#' @family corsplom
#' @seealso \code{\link{corsplom}}

print.metaplot_gtable <- function(x, ...)plot(x,...)
