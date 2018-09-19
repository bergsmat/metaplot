globalVariables(c(
  'z1','plot', 'x0', 'x1', 'y0', 'y1', 'z0'
))

#' Correlated Splom
#'
#' Scatterplot matrix with correlations.
#' @param x object
#' @param ... passed arguments
#' @export
#' @family generic functions
#' @family corsplom
#' @importFrom utils head
corsplom <- function(x,...)UseMethod('corsplom')

#' Correlated Scatterplot Matrix Function for Data Frame
#'
#' Creates a scatterplot matrix with correlations in lower panel, by default.
#' @param x data.frame
#' @param xvar variables to plot
#' @param upper.panel passed to \code{\link[lattice]{splom}} or ggplot
#' @param lower.panel passed to \code{\link[lattice]{splom}} or ggplot
#' @param diag.panel passed to \code{\link[lattice]{splom}} or ggplot
#' @param pscales passed to \code{\link[lattice]{splom}}
#' @param xlab can be function(x = x, var = xvar, ...)
#' @param varname.cex text size multiplier
#' @param main character, or a function of x, xvar
#' @param sub character, or a function of x, xvar
#' @param col point color
#' @param smooth.col smooth color
#' @param smooth.lty smooth line type
#' @param smooth.lwd smooth line size
#' @param smooth.alpha smooth alpha
#' @param density whether to plot density polygons
#' @param diag.label label for the diagonal; can be a function of x, varname, .data
#' @param pin location for a pin (reference line) in the density region; can be function(x, varname, .data) or NULL to suppress
#' @param pin.col color of pin, if any
#' @param pin.alpha alpha transparency of pin
#' @param dens.col color for density region
#' @param dens.scale inflation factor for height of density smooth
#' @param dens.alpha alpha transparency for density region
#' @param settings default parameter settings: a list from which matching elements are passed to lattice (as par.settings) or  to ggplot theme()
#' @param padding numeric (will be recycled to length 4) giving plot margins in default units: top, right, bottom, left (in multiples of 5.5 points for ggplot)
#' @param as.table diagonal arranged top-left to bottom-right
#' @param dens.up whether density plots in diagonal should face the upper triangle vs. lower
#' @param gg logical: whether to generate \code{ggplot} instead of \code{trellis}
#' @param ... extra arguments passed to \code{\link[lattice]{splom}} and ggplot
#' @export
#' @return trellis or grob
#' @importFrom rlang UQS
#' @importFrom gtable gtable_add_padding
#' @importFrom gridExtra arrangeGrob
#' @importFrom gridExtra grid.arrange
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

#' # setOption(gg = TRUE)

#' x %>% metaplot(lKe, lKa, lCl)
#' x %>% metaplot(
#'   lKe, lKa, lCl,
#'   col = 'black',smooth.col = 'red', pin.col = 'red',
#'   dens.col='blue',dens.alpha = 0.1
#' )

corsplom_data_frame <- function(
  x,
  xvar = names(x),
  upper.panel = metOption('upperpanel_corsplom',if(gg) corsplom_gg_scatter else corsplom_panel_scatter),
  lower.panel= metOption('lowerpanel_corsplom',if(gg) corsplom_gg_correlation else corsplom_panel_correlation),
  diag.panel = metOption('diagpanel_corsplom',if(gg) corsplom_gg_diagonal else corsplom_panel_diagonal),
  pscales= metOption('pscales_corsplom',0),
  xlab = metOption('xlab_corsplom',NULL),
  varname.cex = metOption('varname_cex_corsplom',1),
  main = metOption('main_corsplom',NULL),
  sub = metOption('sub_corsplom',NULL),
  col = metOption('point_col_corsplom','blue'),
  smooth.col = metOption('smooth_col_corsplom',col),
  smooth.lty = metOption('smooth_lty_corsplom','solid'),
  smooth.lwd = metOption('smooth_lwd_corsplom',1),
  smooth.alpha = metOption('smooth_alpha_corsplom',1),
  density = metOption('density_corsplom',TRUE),
  diag.label = metOption('diag_label_corsplom',diag_label),
  pin = metOption('pin_loc_corsplom',diag_pin),
  pin.col = metOption('pin_col_corsplom','darkgrey'),
  pin.alpha = metOption('pin_alpha_corsplom',1),
  dens.col = metOption('dens_col_corsplom','grey'),
  dens.scale = metOption('dens_scale_corsplom',0.2),
  dens.alpha = metOption('dens_alpha_corsplom',0.5),
  settings = metOption('settings_corsplom',NULL),
  padding = metOption('padding_corsplom', 1),
  as.table = metOption('astable_corsplom', FALSE),
  dens.up = metOption('updens_corsplom', TRUE), # must not partial match densplot or upper
  gg = metOption('gg_corsplom',FALSE),
  ...
){
  settings <- as.list(settings)
  if(is.null(names(settings))) names(settings) <- character(0)
  stopifnot(length(as.table) == 1, is.logical(as.table))
  if(is.character(xlab)) xlab <- tryCatch(match.fun(xlab), error = function(e)xlab)
  if(is.function(xlab)) xlab <- xlab(x, xvar, ...)
  if(is.null(xlab)) xlab <- ''

  stopifnot(is.numeric(padding))
  padding <- rep(padding, length.out = 4)
  par.settings <- settings[names(settings) %in% names(trellis.par.get())]
  par.settings <- parintegrate(par.settings, padding)
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
          if(as.table & dens.up){
            top  <-  i != 1
            right <- i != ncol
          }
          if(!as.table & dens.up){
            top   <- i != ncol
            left  <- i != 1
          }
          if(as.table & !dens.up){
            bottom <- i != ncol
            left <- i != 1
          }
          if(!as.table & !dens.up){
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
          smooth.col = smooth.col,
          smooth.alpha = smooth.alpha,
          smooth.lty = smooth.lty,
          smooth.lwd = smooth.lwd,
          ...
        )
        if(i != j && xor(i > j, as.table)) p <- upper.panel(
          data = x,
          mapping = aes_string(x = names(x)[[j]], y = names(x)[[i]]),
          col = col,
          smooth.col = smooth.col,
          smooth.alpha = smooth.alpha,
          smooth.lwd = smooth.lwd,
          ...
        )
        theme_settings <- list(
          aspect.ratio = 1,
          strip.background = element_blank(),
          strip.text.x = element_blank(),
          strip.text.y = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.margin = unit(c(0,0,0,0), 'pt')
        )
        theme_extra <- settings[names(settings) %in% names(formals(theme))]
        theme_settings <- merge(theme_settings, theme_extra)
        p <- p + do.call(theme, theme_settings)
        res[[length(res) + 1]] <- p
      }
    }
    m <- do.call(
      arrangeGrob,
      c(
        res,
        ncol = ncol,
        nrow = ncol,
        respect = TRUE,
        top = sub,
        bottom = xlab
      )
    )
    m <- do.call(
      arrangeGrob,
      list(
        m,
        top = textGrob(
          main,
          gp = gpar(
            fontface = 'bold',
            cex = 1.3
          )
        )
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
    smooth.col = smooth.col,
    smooth.lty = smooth.lty,
    smooth.lwd = smooth.lwd,
    smooth.alpha = smooth.alpha,
    density = density,
    diag.label = diag.label,
    pin = pin,
    pin.col = pin.col,
    pin.alpha = pin.alpha,
    dens.col = dens.col,
    dens.scale = dens.scale,
    dens.alpha = dens.alpha,
    as.matrix = as.table,
    as_table = as.table,
    par.settings = par.settings,
    dens.up = dens.up,
    ...
  )
}
#' Correlated Scatterplot Matrix Method for Data Frame
#'
#' Creates a scatterplot matrix.  Parses arguments and generates the call: fun(x, xvar, ...).
#' @param x data.frame
#' @param ... passed to fun
#' @param fun function to do the actual plotting
#' @param verbose generate messages describing process
#' @export
#' @importFrom rlang UQS quos
#' @family multivariate plots
#' @family corsplom
#' @family methods
corsplom.data.frame <- function(
  x,
  ...,
  fun = metOption('corsplom','corsplom_data_frame'),
  verbose = metOption('verbose_corsplom',FALSE)
){
  args <- quos(...)
  args <- lapply(args,f_rhs)
  vars <- args[names(args) == '']
  other <- args[names(args) != '']
  vars <- sapply(vars, as.character)
  prime <- list(x = x, xvar = vars)
  args <- c(prime, other)
  if(verbose){
    if(is.character(fun))message('calling ', fun) else message('calling fun')
  }
  do.call(fun, args)
}

#' Correlation GG Function for GG Metaplot Corsplom
#'
#' Default lower panel function for corsplom_data_frame() with \code{gg = TRUE}. Plots Pearson correlation coefficient.
#' corsplom_data_frame() typically supplies smooth aesthetics in case smooth is desired; smooth arguments are defined to
#' suppress corresponding warnings.
#' @param data data
#' @param mapping mapping
#' @param col text color
#' @param smooth.col ignored
#' @param smooth.lty ignored
#' @param smooth.lwd ignored
#' @param smooth.alpha ignored
#' @param use passed to \code{\link[stats]{cor}}
#' @param ... ignored
#' @keywords internal
#' @export
#' @family panel functions
#' @family corsplom
corsplom_gg_correlation = function(
  data, mapping, col = metOption('point_col_corsplom_gg','blue'),
  smooth.col, smooth.lty, smooth.lwd, smooth.alpha, use = 'pairwise.complete.obs', ...
){
  x <- as.character(mapping$x)
  x <- x[x != '~']
  x <- x[[1]]
  y <- as.character(mapping$y)
  y <- y[y != '~']
  y <- y[[1]]
  x <- data[[x]]
  y <- data[[y]]
  x1 <- range(x,na.rm = T)
  y1 <- range(y,na.rm = T)
  x0 <- min(x1)+(max(x1)-min(x1))/2
  y0 <- min(y1)+(max(y1)-min(y1))/2
  # panel.text(x0 ,y0, labels = paste('r =',round(cor(x,y, na.rm = T),3) ))
  p <- ggplot(
   data = data,
   mapping = mapping,
   colour = col
  )
  p <- p + xlim(x1[[1]], x1[[2]])
  p <- p + ylim(y1[[1]], y1[[2]])
  stat <- try(silent = TRUE, round(cor(x,y, use = use), 3))
  if(class(stat) == 'try-error') stat <- ''
  p <- p + annotate(
    'text',
    x = x0,
    y = y0,
    label = paste('r =',stat)
  )
  p <- p + theme(
    aspect.ratio = 1,
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank() #,
    # panel.border = element_rect(fill = NA)
  )
  p
}

#' Scatter GG Function for Metaplot Corsplom
#'
#' Default upper panel function for corsplom_data_frame with \code{gg = TRUE}. Plots data with loess smooth.
#' @param data data
#' @param mapping mapping
#' @param col point color
#' @param smooth.col smooth color
#' @param smooth.lty smooth line type
#' @param smooth.lwd smooth line size
#' @param smooth.alpha smooth alpha
#' @param ... passed arguments
#' @keywords internal
#' @export
#' @family panel functions
#' @family corsplom
corsplom_gg_scatter = function(
  data,
  mapping,
  method = 'loess',
  se = F,
  col = metOption('point_col_corsplom_gg','blue'),
  smooth.col = metOption('smooth_col_corsplom_gg','blue'),
  smooth.alpha = metOption('smooth_alpha_corsplom_gg',1),
  smooth.lty = metOption('smooth_lty_corsplom_gg','solid'),
  smooth.lwd = metOption('smooth_lwd_corsplom_gg',1),
  ...
){
  x <- as.character(mapping$x)
  x <- x[x != '~']
  x <- x[[1]]
  y <- as.character(mapping$y)
  y <- y[y != '~']
  y <- y[[1]]
  lim1 <- range(data[[x]],na.rm = TRUE)
  lim2 <- range(data[[y]],na.rm = TRUE)

  ggplot(data = data, mapping = mapping, ...) +
    xlim(lim1[[1]],lim1[[2]]) +
    ylim(lim2[[1]],lim2[[2]]) +
    geom_point(colour = col) +
    geom_smooth(
      method = method,
      se = se,
      colour = smooth.col,
      alpha = smooth.alpha,
      linetype = smooth.lty,
      size = smooth.lwd,
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
#' @param dens.up whether density plots should face the upper triangle (or lower, if FALSE)
#' @param ... passed arguments
#' @keywords internal
#' @export
#' @family panel functions
#' @family corsplom
#' @seealso \code{\link{corsplom}}

corsplom_gg_diagonal <- function(
  data,
  mapping,
  .data,
  density = TRUE,
  diag.label = metOption('diag_label_corsplom_gg',diag_label),
  pin = metOption('pin_loc_corsplom_gg',diag_pin),
  pin.col = metOption('pin_col_corsplom_gg','darkgrey'),
  pin.alpha = metOption('pin_alpha_corsplom_gg',1),
  dens.col = metOption('dens_col_corsplom_gg','grey'),
  dens.scale = metOption('dens_scale_corsplom_gg',0.2),
  dens.alpha = metOption('dens_alpha_corsplom_gg',0.5),
  varname.cex = metOption('varname_cex_corsplom_gg', 1),
  as.table = metOption('astable_corsplom_gg', TRUE),
  dens.up = metOption('densup_corsplom_gg',TRUE),
  ...
){
  stopifnot(is.logical(density))
  density <- rep(density, length.out = 4)
  names(density) <- c('top','right','bottom','left')
  x <- as.character(mapping$x)
  x <- x[x != '~']
  x <- x[[1]]
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
  if(length(ref) && any(density)){
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

#' Plot a Metaplot_gtable
#'
#' Plots a metaplot_gtable. Underlying object type is 'gtable'.
#' Defaults for \code{link{corsplom_data_frame}} and \code{link{multiplot}} prepend class 'metaplot_gtable' to
#' modify plot and print functionality.
#'
#' @param x metaplot_gtable
#' @param y ignored
#' @param \dots ignored
#' @keywords internal
#' @export
#' @return (invisible) gtable
#' @family methods
#' @family corsplom
#' @importFrom grid grid.newpage grid.draw
#' @seealso \code{\link{corsplom}}

plot.metaplot_gtable <- function(x, ...){
  grid.newpage()
  grid.draw(x)
}

#' Print a Metaplot_gtable
#'
#' Prints a metaplot_gtable. Underlying object type is 'gtable'.
#' Defaults for \code{link{corsplom_data_frame}} and \code{link{multiplot}} prepend class 'metaplot_gtable' to
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
#' @seealso \code{\link{corsplom}} \code{\link{multiplot}}

print.metaplot_gtable <- function(x, ...)plot(x,...)
