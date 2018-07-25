#' Density Plot
#'
#' Creates a density plot.
#' @param x object
#' @param ... passed arguments
#' @export
#' @family generic functions
#' @family univariate plots
#' @family densplot
densplot <- function(x,...)UseMethod('densplot')

#' Density Function for Data Frame
#'
#' Plot density for object of class 'data.frame' using \code{dens_panel} by default.
#' @param x data.frame
#' @param xvar variable to plot
#' @param groups optional grouping variable
#' @param facets optional conditioning variables
#' @param xlab x axis label; can be function(x = x, var = xvar, log = log, ...)
#' @param ref reference line; can be function(x = x, var = xvar, ...) or NULL to suppress
#' @param ref.col color for reference line(s)
#' @param ref.lty type for reference line(s)
#' @param ref.lwd size for reference line(s)
#' @param ref.alpha transparency for reference line(s)
#' @param log whether to log-transform x axis (auto-selected if NA)
#' @param crit if log is NA, log-transform if mean/median ratio for non-missing x is greater than this value (and no negative values)
#' @param aspect passed to \code{\link[lattice]{bwplot}} or ggplot; use 'fill', NA, or NULL to calculate automatically
#' @param scales passed to \code{\link[lattice]{xyplot}} or \code{\link[ggplot2]{facet_grid}} or \code{\link[ggplot2]{facet_wrap}} (guessed if NULL)
#' @param panel  passed to \code{\link[lattice]{densityplot}}
#' @param colors replacements for default colors in group order
#' @param symbols replacements for default symbols in group order
#' @param points whether to plot points: logical or alpha, same length as groups
#' @param lines whether to plot lines: logical or alpha, same length as groups
#' @param fill whether to fill curves: logical or alpha, same length as groups (symbol fill color is same as point color)
#' @param space location of key (right, left, top, bottom)
#' @param key list: passed to \code{\link[lattice]{xyplot}} as \code{auto.key} or to \code{\link[ggplot2]{theme}}; can be a function groups name, groups levels, points, lines, space, gg, and \dots .  See \code{\link{metaplot_key}}.
#' @param as.table passed to \code{\link[lattice]{xyplot}}
#' @param main character, or a function of x, xvar, groups, facets, and log
#' @param sub character, or a function of x, xvar, groups, facets, and log
#' @param settings default parameter settings: a list from which matching elements are passed to lattice (as par.settings) or  to ggplot theme()  and facet_wrap() or facet_grid().  \code{ncol} and \code{nrow} are used as layout indices for lattice (for homology with facet_wrap).
#' @param padding numeric (will be recycled to length 4) giving plot margins in default units: top, right, bottom, left (in multiples of 5.5 points for ggplot)
#' @param gg logical: whether to generate \code{ggplot} instead of \code{trellis}
#' @param ... passed to \code{\link[lattice]{densityplot}}
#' @family univariate plots
#' @family densplot
#' @family metaplot
#' @import lattice
#' @importFrom scales log_trans
#' @export
#' @examples
#' densplot_data_frame(Theoph, 'conc', grid = TRUE)
#' densplot_data_frame(Theoph, 'conc', 'Subject')
#' densplot_data_frame(Theoph, 'conc', 'Subject',
#' space = 'top', columns = 4, legend.direction = 'horizontal')
#' densplot_data_frame(Theoph, 'conc', 'Subject',
#' space = 'top', columns = 4, legend.direction = 'horizontal', gg = TRUE)
#' densplot_data_frame(Theoph, 'conc', , 'Subject')
densplot_data_frame<- function(
  x,
  xvar,
  groups = NULL,
  facets = NULL,
  xlab = metOption('metaplot_xlab_dens',axislabel),
  ref = metOption('metaplot_ref_x_dens',metaplot_ref),
  ref.col = metOption('metaplot_ref_col_dens','grey'),
  ref.lty = metOption('metaplot_ref_lty_dens','solid'),
  ref.lwd = metOption('metaplot_ref_lwd_dens',1),
  ref.alpha = metOption('metaplot_ref_alpha_dens',1),
  log = metOption('metaplot_log_dens',FALSE),
  crit = metOption('metaplot_crit_dens',1.3),
  aspect = metOption('metaplot_aspect_dens',1),
  scales = metOption('metaplot_scales_dens',NULL),
  panel = metOption('metaplot_panel_dens',dens_panel),
  colors = metOption('metaplot_colors_dens',NULL),
  symbols = metOption('metaplot_symbols_dens',NULL),
  points = metOption('metaplot_points_dens',TRUE),
  lines = metOption('metaplot_lines_dens',TRUE),
  fill = metOption('metaplot_fill_dens',FALSE),
  space = metOption('metaplot_space_dens','right'),
  key = metOption('metaplot_key_dens',metaplot_key),
  as.table = metOption('metaplot_astable_dens',TRUE),
  main = metOption('metaplot_main_dens',NULL),
  sub = metOption('metaplot_sub_dens',NULL),
  settings = metOption('metaplot_settings_dens',NULL),
  padding = metOption('metaplot_padding_dens', 1),
  gg = metOption('metaplot_gg_dens',FALSE),
  ...
){
  settings <- as.list(settings)
  if(is.null(names(settings))) names(settings) <- character(0)
  aspect <- metaplot_aspect(aspect, gg)
  stopifnot(inherits(x, 'data.frame'))
  stopifnot(length(xvar) == 1)
  stopifnot(is.character(xvar))
  stopifnot(is.numeric(padding))
  padding <- rep(padding, length.out = 4)
  par.settings <- list()
  par.settings <- settings[names(settings) %in% names(trellis.par.get())]
  par.settings <- parintegrate(par.settings, padding)
  if(gg)padding <- unit(padding * 5.5, 'pt')


  if(is.null(log))log <- FALSE # same as default
  if(is.na(log)){
    if(any(x[[xvar]] <= 0, na.rm = TRUE)){
      log <- FALSE
    } else{
      log <- mean(x[[xvar]],na.rm = TRUE)/median(x[[xvar]],na.rm = TRUE) > crit
    }
  }

  bad <- !is.na(x[[xvar]]) & x[[xvar]] <= 0
  bad[is.na(bad)] <- FALSE
  if(log && any(bad)){
    warning('dropping ',sum(bad), ' non-positive records for log scale')
    x <- x[!bad,]
  }

  if(is.null(scales) && gg) scales <- 'fixed'
  if(is.null(scales)) scales <- list(tck = c(1,0),alternating = FALSE, x = list(log = log,equispaced.log = FALSE))
  if(is.character(ref)) ref <- match.fun(ref)
  if(is.function(ref)) ref <- ref(x = x, var = xvar, log = log, ...)
  ref <- as.numeric(ref)
  ref <- ref[is.defined(ref)]
  if(log & !gg){  # ggplot handles reference rescaling implicitly
    ref <- ref[ref > 0]
    ref <- log(ref)
  }

  if(is.character(xlab)) xlab <- tryCatch(match.fun(xlab), error = function(e)xlab)
  if(is.function(xlab)) xlab <- xlab(x = x, var = xvar, log = log, ...)

  ff <- character(0)
  if(!is.null(facets))ff <- paste(facets, collapse = ' + ')
  if(!is.null(facets))ff <- paste0('|',ff)
  formula <- as.formula(paste0('~', xvar) %>% paste(ff))
  if(!is.null(facets)){
    for (i in seq_along(facets)) x[[facets[[i]]]] <- as_factor(x[[ facets[[i]] ]])
  }
  if(!is.null(main))if(is.function(main)) main <- main(x = x, xvar = xvar, groups = groups, facets = facets, log = log, ...)
  if(!is.null(sub))if(is.function(sub)) sub <- sub(x = x, xvar = xvar, groups = groups, facets = facets, log = log, ...)
  if(!is.null(groups)) x[[groups]] <- as_factor(x[[groups]])
  # if(!gg) groups <- as.formula(paste('~',groups))
  if(is.null(groups)){
    x$metaplot_groups <- factor(0)
    groups <- 'metaplot_groups'
  }
  # groups is factor if imputed
  # groups now assigned and is factor
  nlev <- length(levels(x[[groups]]))
  levs <- levels(x[[groups]])
  if(is.null(colors)){
    if(gg){
      colors <- hue_pal()(nlev)
      if(nlev == 1) colors <- 'black'
    } else{
      colors <- trellis.par.get()$superpose.symbol$col
    }
  }
  if(is.null(symbols)){
    if(gg){
      symbols <- 16
    } else {
      symbols <- trellis.par.get()$superpose.symbol$pch
    }
  }
  if(is.null(fill)) fill <- FALSE # same as default
  if(is.null(lines)) lines <- TRUE # same as default
  if(is.null(points)) points <- TRUE # same as default
  fill <- as.numeric(fill)
  lines <- as.numeric(lines)
  points <- as.numeric(points)
  colors <- rep(colors, length.out = nlev)
  symbols <- rep(symbols, length.out = nlev)
  fill <- rep(fill, length.out = nlev)
  fill[fill == 0] <- 0.000000001 # key borders are not drawn if fill == 0
  lines <- rep(lines, length.out = nlev)
  points <- rep(points, length.out = nlev)
  # par.settings is defined
  sym <- trellis.par.get()$superpose.symbol
  line <- trellis.par.get()$superpose.line
  poly <- trellis.par.get()$superpose.polygon
  sym$col <- alpha(colors, points)
  sym$alpha <- 1
  sym$pch <- symbols
  sym$fill <- alpha(colors, lines)
  line$col <- alpha(colors, lines)
  line$alpha <- 1
  poly$col <- alpha(colors, fill)
  poly$alpha <- 1
  poly$border <- alpha(colors, lines)
  # poly <- list(
  #   col = alpha(colors, fill),
  #   alpha = 1,
  #   border = alpha(colors,lines)
  # )
  # sym <- list(
  #   col = alpha(colors, points),
  #   alpha = 1,
  #   pch = symbols,
  #   fill = alpha(colors, points)
  # )
  # line <- list(
  #   col = alpha(colors, lines),
  #   alpha = 1
  # )
  if(is.null(par.settings$superpose.polygon)) par.settings$superpose.polygon <- poly
  if(is.null(par.settings$superpose.symbol)) par.settings$superpose.symbol <- sym
  if(is.null(par.settings$superpose.line)) par.settings$superpose.line <- line

  if(is.character(key)) key <- match.fun(key)
  if(is.function(key)) key <- key(groups = groups, levels = levs, points = points, lines = lines, fill = fill, space = space, gg = gg, type = 'density', ...)

if(gg){
  x$metaplot_points_alpha <- points[as.numeric(x[[groups]])]
  plot <- ggplot(data = x, aes_string(x = xvar))
  plot <- plot + geom_density(mapping = aes_string(color = groups, fill = groups))
  lim <- max(na.rm = TRUE, ggplot_build(plot)$data[[1]]$y)
  plot <- plot + geom_jitter( mapping = aes_string(y = 0, color = groups, shape = groups, alpha = 'metaplot_points_alpha'),height = 0.02 * lim)
  plot <- plot + scale_shape_manual(values = symbols)
  plot <- plot + scale_color_manual(values = alpha(colors, lines))
  plot <- plot + scale_fill_manual(values = alpha(colors, fill))
  plot <- plot + guides(alpha = FALSE)
  plot <- plot + scale_alpha_identity()

  plot <- plot + xlab(xlab)
  plot <- plot +  ggtitle(main, subtitle = sub)

  # scale aesthetics
  panels <- nrow(unique(x[facets]))
  if(!panels) panels <- 1

  ref.col <- rep(ref.col, length.out = length(ref))
  ref.lty <- rep(ref.lty, length.out = length(ref))
  ref.lwd <- rep(ref.lwd, length.out = length(ref))
  ref.alpha <- rep(ref.alpha, length.out = length(ref))

  ref.col <- rep(ref.col, times = panels)
  ref.lty <- rep(ref.lty, times = panels)
  ref.lwd <- rep(ref.lwd, times = panels)
  ref.alpha <- rep(ref.alpha, times = panels)

  if(length(ref)) plot <- plot +
  geom_vline(
    xintercept = ref,
    color = ref.col,
    linetype = ref.lty,
    size = ref.lwd,
    alpha = ref.alpha
  )
  theme_settings <- list(aspect.ratio = aspect, plot.margin = padding, legend.title = element_blank())
  theme_settings <- merge(theme_settings, key)
  theme_extra <- settings[names(settings) %in% names(formals(theme))]
  theme_settings <- merge(theme_settings, theme_extra)
  plot <- plot + do.call(theme, theme_settings)

  if(log) plot <- plot + scale_x_continuous(
   trans = log_trans(),
   breaks = base_breaks()
 )
  facet_args <- list()
  if(length(facets) == 1) facet_args[[1]] <- facets[[1]] #list(facets[[1]], scales = scales)
  if(length(facets) > 1)  facet_args[[1]] <- as.formula(
    paste(
      sep='~',
      facets[[1]],
      facets[[2]]
    )
  )
  facet_args$scales <- scales
  facet_extra <- list()
  if(length(facets) == 1)facet_extra <- settings[names(settings) %in% names(formals(facet_wrap))]
  if(length(facets) >  1)facet_extra <- settings[names(settings) %in% names(formals(facet_grid))]
  facet_args <- merge(facet_args, facet_extra)
  if(length(facets) == 1) plot <- plot + do.call(facet_wrap, facet_args)
  if(length(facets) >  1) plot <- plot + do.call(facet_grid, facet_args)
  return(plot)
}

vals <- x[[xvar]]
vals <- vals[!is.na(vals)]
if(log) vals <- vals[vals > 0]
if(log) vals <- log(vals)
range <- range(vals)

args <- list(
  formula,
  data = x,
  #cut = 0,
  groups = if(is.null(groups)) NULL else as.formula(paste('~',groups)),
  xlab = xlab,
  ref = ref,
  ref.col = ref.col,
  ref.lty = ref.lty,
  ref.lwd = ref.lwd,
  ref.alpha = ref.alpha,
  log = log,
  aspect = aspect,
  scales = scales,
  panel = panel,
  auto.key = key,
  as.table = as.table,
  main = main,
  sub = sub,
  par.settings = par.settings
)
args <- c(args, list(...))
if(all(c('ncol','nrow') %in% names(settings))){
  layout <- c(settings$ncol, settings$nrow)
  args <- c(args, list(layout = layout))
}
do.call(densityplot, args)
}

#' Panel Function for Metaplot Density Plot
#'
#' Default panel function for dens_data_frame.  Calls panel.densityplot, and plots reference lines if ref has length.
#' @export
#' @family panel functions
#' @family univariate plots
#' @keywords internal
#' @param ref numeric
#' @param ref.col passed to \code{\link[lattice]{panel.abline}} as col
#' @param ref.lty passed to \code{\link[lattice]{panel.abline}} as lty
#' @param ref.lwd passed to \code{\link[lattice]{panel.abline}} as lwd
#' @param ref.alpha passed to \code{\link[lattice]{panel.abline}} as alpha
dens_panel <- function(ref = NULL, ref.col, ref.lty, ref.lwd, ref.alpha, ...){
  panel.meta_densityplot(...)
  if(length(ref))panel.abline(v = ref, col=ref.col, lty = ref.lty, lwd = ref.lwd, alpha = ref.alpha)
}
#' Densplot Method for Data Frame
#'
#' Plot density for object of class 'data.frame'. Parses arguments and generates the call: fun(x, xvar, groups, facets,...).
#' @param x data.frame
#' @param ... passed to fun
#' @param fun plotting function
#' @param verbose generate messages describing process
#' @import lattice
#' @export
#' @importFrom rlang f_rhs quos
#' @family univariate plots
#' @family densplot
#' @family methods
#' @examples
#' densplot(Theoph, conc, grid = TRUE )
#' densplot(Theoph, conc, grid = TRUE, gg = TRUE )
#' densplot(Theoph, conc, Subject )
#' densplot(Theoph, conc, , Subject )
#' densplot(Theoph, conc, , Subject, gg = TRUE, scales = 'free_y' )
#' attr(Theoph,'title') <- 'Theophylline'
#' densplot(Theoph, conc, main= function(x,...)attr(x,'title'))
#' densplot(Theoph, conc, sub= function(x,...)attr(x,'title'))

densplot.data.frame<- function(
  x,
  ...,
  fun = getOption('metaplot_densplot','densplot_data_frame'),
  verbose = metOption('metaplot_verbose_densplot',FALSE)
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
  if(verbose){
    if(is.character(fun))message('calling ', fun) else message('calling fun')
  }
  do.call(fun, args)
}

#' Panel Function for Metaplot Density
#'
#' Variant of panel.densityplot that supports filled area and alpha points.
#'
#' @export
#' @family panel functions
#' @family univariate plots
#' @keywords internal
#' @param x see \code{link[lattice]{panel.densityplot}}
#' @param darg see \code{link[lattice]{panel.densityplot}}
#' @param plot.points see \code{link[lattice]{panel.densityplot}}
#' @param ref see \code{link[lattice]{panel.densityplot}}
#' @param groups see \code{link[lattice]{panel.densityplot}}
#' @param weights see \code{link[lattice]{panel.densityplot}}
#' @param jitter.amount see \code{link[lattice]{panel.densityplot}}
#' @param type see \code{link[lattice]{panel.densityplot}}
#' @param ... see \code{link[lattice]{panel.densityplot}}
#' @param identifier see \code{link[lattice]{panel.densityplot}}


panel.meta_densityplot <- function (
  x, darg = list(n = 512), plot.points = "jitter", ref = FALSE,
    groups = NULL, weights = NULL, jitter.amount = 0.01 * diff(current.panel.limits()$ylim),
    type = "p", ..., identifier = "density", group.number
)
{
    if (ref) {
        reference.line <- trellis.par.get("reference.line")
        panel.abline(h = 0, col = reference.line$col, lty = reference.line$lty,
            lwd = reference.line$lwd, identifier = paste(identifier,
                "abline"))
    }
    if (!is.null(groups)) {
        panel.superpose(x, darg = darg, plot.points = plot.points,
            ref = FALSE, groups = groups, weights = weights,
            panel.groups = panel.meta_densityplot, jitter.amount = jitter.amount,
            type = type, ...)
    }
    else {
        switch(
          as.character(plot.points),
          `TRUE` = panel.xyplot(
            x = x,
            y = rep(0, length(x)),
            type = type,
            ...,
            identifier = identifier
          ),
          rug = panel.rug(
            x = x,
            start = 0,
            end = 0,
            x.units = c("npc","native"),
            type = type,
            ...,
            identifier = paste(identifier,"rug")
          ),
          jitter = panel.xyplot(
            x = x,
            y = jitter(rep(0,length(x)), amount = jitter.amount),
            type = type,
            ...,
            identifier = identifier
          )
        )
        density.fun <- function(x, weights, subscripts = TRUE, darg, ...) {
            do.call("density", c(list(x = x, weights = weights[subscripts]),darg))
        }
        if (sum(!is.na(x)) > 1) {
            h <- density.fun(x = x, weights = weights, ..., darg = darg)
            lim <- current.panel.limits()$xlim
            id <- h$x > min(lim) & h$x < max(lim)
            panel.lines(x = h$x[id], y = h$y[id], ..., identifier = identifier)
            hx <- h$x[id]
            hy <- h$y[id]
            # for polygon, drop endpoints to axis
            hx <- c(min(hx), hx, max(hx))
            hy <- c(0, hy, 0)
            col <- trellis.par.get()$superpose.polygon$col
            col <- rep(col, length.out = group.number)
            col <- rev(col)[[1]]
            panel.polygon(border = NA, x = hx, y = hy, col = col, identifier = paste(identifier,'fill'))
        }
    }
}
