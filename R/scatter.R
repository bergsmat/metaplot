globalVariables('metaplot_groups')
globalVariables('metaplot_values')
globalVariables('metaplot_points_alpha')
globalVariables('metaplot_lines_alpha')
globalVariables('metaplot_points_sizes')
globalVariables('metaplot_lines_widths')
globalVariables('my_lpoints')
globalVariables('metaplot_lines_alpha')

#' Scatterplot
#'
#' Scatterplot.
#'
#' @param x object
#' @param ... passed arguments
#' @import ggplot2
#' @export
#' @family generic functions
#' @family scatter
#' @family bivariate plots

scatter <- function(x,...)UseMethod('scatter')

#' Scatterplot Function for Data Frame
#'
#' Scatterplot function for class 'data.frame'.
#'
#'
#' @param x data.frame
#' @param yvar character: y variable(s)
#' @param xvar character: x variable
#' @param groups optional grouping variable; ignored if more than one \code{yvar}
#' @param facets optional conditioning variables
#' @param log a default shared by \code{ylog} and \code{xlog}
#' @param ylog log transform y axis (auto-selected if NA)
#' @param xlog log transform x axis (auto-selected if NA)
#' @param crit if ylog or xlog missing, log transform if mean/median ratio for non-missing values is greater than crit
#' @param yref reference line from y axis; can be function(x = x, var = yvar, ...) or NULL to suppress
#' @param xref reference line from x axis; can be function(x = x, var = xvar, ...) or NULL to suppress
#' @param ysmooth supply loess smooth of y on x
#' @param xsmooth supply loess smmoth of x on y
#' @param ylab y axis label; can be function(x = x, var = yvar, log = ylog, ..)
#' @param xlab x axis label; can be function(x = x, var = xvar, log = xlog, ..)
#' @param iso logical: plot line of unity (auto-selected if NA); can be a (partial) list of aesthetics (col, lty, lwd, alpha)
#' @param na.rm whether to remove data points with one or more missing coordinates
#' @param aspect passed to \code{\link[lattice]{bwplot}} or ggplot; use 'fill', NA, or NULL to calculate automatically
#' @param space location of key (right, left, top, bottom)
#' @param key list: passed to \code{\link[lattice]{xyplot}} as \code{auto.key} or to \code{\link[ggplot2]{theme}}; can be a function groups name, groups levels, points, lines, space, gg, and \dots .  See \code{\link{metaplot_key}}.
#' @param as.table passed to \code{\link[lattice]{xyplot}}
#' @param prepanel passed to \code{\link[lattice]{xyplot}} (guessed if NULL)
#' @param isoprepanel passed to \code{\link[lattice]{xyplot}} if iso is TRUE
#' @param scales passed to \code{\link[lattice]{xyplot}} or \code{\link[ggplot2]{facet_grid}} or \code{\link[ggplot2]{facet_wrap}} (guessed if NULL)
#' @param panel name or definition of panel function
#' @param points whether to plot points and fill for each group: logical, or alpha values between 0 and 1
#' @param colors replacements for default colors in group order; can be length one integer to auto-select that many colors
#' @param fill replacements for default fill colors in group order (means something different
#' for \code{\link{densplot_data_frame}} and \code{\link{categorical_data_frame}}). Used for confidence
#' regions and for filling symbols (pch 21:25).
#' @param symbols replacements for default symbols in group order (i.e. values of pch)
#' @param sizes replacements for default symbol sizes in group order
#' @param lines whether to plot lines for each group: logical, or alpha values between 0 and 1
#' @param types replacements for default line types in group order
#' @param widths replacements for default line widths in group order
#' @param main character, or a function of x, yvar, xvar, groups, facets, and log
#' @param sub character, or a function of x, yvar, xvar, groups, facets, and log
#' @param subscripts passed to \code{\link[lattice]{xyplot}}
#' @param settings default parameter settings: a list from which matching elements are passed to lattice (as par.settings) or  to ggplot theme()  and facet_wrap() or facet_grid().  \code{ncol} and \code{nrow} are used as layout indices for lattice (for homology with facet_wrap). Also merged with \dots.
#' @param padding numeric (will be recycled to length 4) giving plot margins in default units: top, right, bottom, left (in multiples of 5.5 points for ggplot)
#' @param ref.col default shared by \code{xref.col} and \code{yref.col}; can be length one integer to auto-select that many colors
#' @param ref.lty default shared by \code{xref.lty} and \code{yref.lty}
#' @param ref.lwd default shared by \code{xref.lwd} and \code{yref.lwd}
#' @param ref.alpha default shared by \code{xref.alpha} and \code{yref.alpha}
#' @param xref.col x reference line color (recycled)
#' @param xref.lty x reference line type (recycled)
#' @param xref.lwd x reference line size (recycled)
#' @param xref.alpha x reference line alpha (recycled)
#' @param yref.col y reference line color (recycled)
#' @param yref.lty y reference line type (recycled)
#' @param yref.lwd y reference line size (recycled)
#' @param yref.alpha y reference line alpha (recycled)
#' @param smooth.lty smooth line type
#' @param smooth.lwd smooth line size
#' @param smooth.alpha smooth alpha
#' @param global if TRUE, xsmooth, ysmooth, fit, and conf are applied to all data rather than groupwise
#' @param global.col color for global aesthetics
#' @param global.fill fill color for global aesthetics
#' @param fit draw a linear fit of y ~ x
#' @param fit.lty fit line type
#' @param fit.lwd fit line size
#' @param fit.alpha fit alpha
#' @param conf logical, or width for a confidence region around a linear fit; passed to \code{\link{region}}; \code{TRUE} defaults to 95 percent confidence interval; may not make sense if xlog is TRUE
#' @param conf.alpha alpha transparency for confidence region
#' @param loc where to print statistics on a panel; suppressed for grouped plots an facetted ggplots
#' @param msg a function to print text on a panel: called with x values, y values, and \dots.
#' @param gg logical: whether to generate \code{ggplot} instead of \code{trellis}
#' @param verbose generate messages describing process
#' @param ... passed to called functions e.g.,  \code{\link{region}}
#' @seealso \code{\link{scatter_panel}}
#' @export
#' @import lattice
#' @importFrom tidyr gather
#' @importFrom scales alpha
#' @importFrom grDevices xy.coords
#' @family bivariate plots
#' @family metaplot
#' @family scatter
#' @examples
#' library(magrittr)
#' library(dplyr)
#' attr(Theoph$conc,'label') <- 'theophylline concentration'
#' attr(Theoph$conc,'guide') <- 'mg/L'
#' attr(Theoph$Time,'label') <- 'time'
#' attr(Theoph$Time,'guide') <- 'h'
#' attr(Theoph$Subject,'guide') <- '////'
#' scatter_data_frame(Theoph, 'conc', 'Time')
#' scatter_data_frame(Theoph, 'conc','Time', 'Subject')
#' scatter_data_frame(Theoph, 'conc','Time', facets = 'Subject')
#' scatter_data_frame(Theoph %>% filter(conc > 0), 'conc','Time', 'Subject',ylog = TRUE, yref = 5)
#' scatter_data_frame(Theoph, 'conc','Time', 'Subject',ylog = TRUE, yref = 5)
#' scatter_data_frame(Theoph, 'conc','Time', 'Subject',ysmooth = TRUE)
#' scatter_data_frame(Theoph, 'conc','Time', 'Subject',ysmooth = TRUE,global = TRUE)
#' scatter_data_frame(Theoph, 'conc','Time', conf = TRUE, loc = 3, yref = 6)
#' scatter_data_frame(Theoph, 'conc','Time', conf = TRUE, loc = 3, yref = 6)


scatter_data_frame <- function(
  x,
  yvar,
  xvar,
  groups = NULL,
  facets = NULL,
  log = metOption('log_scatter',FALSE),
  ylog = metOption('ylog_scatter',log),
  xlog = metOption('xlog_scatter',log),
  crit = metOption('crit_scatter',1.3),
  yref = metOption('yref_scatter','metaplot_ref'),
  xref = metOption('xref_scatter','metaplot_ref'),
  ylab = metOption('ylab_scatter','axislabel'),
  xlab = metOption('xlab_scatter','axislabel'),
  ysmooth = metOption('ysmooth_scatter',FALSE),
  xsmooth = metOption('xsmooth_scatter',FALSE),
  iso = metOption('iso_scatter',FALSE),
  na.rm = metOption('na.rm_scatter',TRUE),
  aspect = metOption('aspect_scatter',1),
  space = metOption('space_scatter','right'),
  key = metOption('key_scatter','metaplot_key'),
  as.table = metOption('as.table_scatter',TRUE),
  prepanel = metOption('prepanel_scatter', NULL),
  isoprepanel = metOption('isoprepanel_scatter', 'iso_prepanel'),
  scales = metOption('scales_scatter',NULL),
  panel = metOption('panel_scatter','scatter_panel'),
  points = metOption('points_scatter',TRUE),
  colors = metOption('colors_scatter',NULL),
  fill = metOption('fill_scatter',NULL),
  symbols = metOption('symbols_scatter',NULL),
  sizes = metOption('sizes_scatter',1),
  types = metOption('types_scatter','solid'),
  widths = metOption('widths_scatter', 1),
  lines = metOption('lines_scatter',FALSE),
  main = metOption('main_scatter',NULL),
  sub = metOption('sub_scatter',NULL),
  subscripts = metOption('subscripts_scatter',TRUE),
  settings = metOption('settings_scatter',NULL),
  padding = metOption('padding_scatter', 1),

  ref.col = metOption('ref.col_scatter','grey'),
  ref.lty = metOption('ref.lty_scatter','solid'),
  ref.lwd = metOption('ref.lwd_scatter',1),
  ref.alpha = metOption('ref.alpha_scatter',1),

  xref.col = metOption('xref.col_scatter',NULL),
  xref.lty = metOption('xref.lty_scatter',NULL),
  xref.lwd = metOption('xref.lwd_scatter',NULL),
  xref.alpha = metOption('xref.alpha_scatter',NULL),

  yref.col = metOption('yref.col_scatter',NULL),
  yref.lty = metOption('yref.lty_scatter',NULL),
  yref.lwd = metOption('yref.lwd_scatter',NULL),
  yref.alpha = metOption('yref.alpha_scatter',NULL),

  smooth.lty = metOption('smooth.lty_scatter','dashed'),
  smooth.lwd = metOption('smooth.lwd_scatter',1),
  smooth.alpha = metOption('smooth.alpha_scatter',1),
  fit = metOption('fit_scatter',conf),
  fit.lty = metOption('fit.lty_scatter','solid'),
  fit.lwd = metOption('fit.lwd_scatter',1),
  fit.alpha = metOption('fit.alpha_scatter',1),
  conf = metOption('conf_scatter',FALSE),
  conf.alpha = metOption('conf.alpha_scatter',0.3),
  loc = metOption('loc_scatter',0),
  global = metOption('global_scatter',FALSE),
  global.col = metOption('global.col_scatter','grey'),
  global.fill = metOption('global.fill_scatter','grey'),
  msg = metOption('msg_scatter','metastats'),
  gg = metOption('gg_scatter',FALSE),
  verbose = metOption('verbose',FALSE),
  ...
){
  if(verbose) cat('this is scatter_data_frame')
  if(is.null(ref.col)) ref.col <- 'grey'
  if(is.numeric(ref.col)) ref.col <- hue_pal()(ref.col[[1]])
  if(is.null(xref.col)) xref.col <- ref.col
  if(is.numeric(xref.col)) xref.col <- hue_pal()(xref.col[[1]])
  if(is.null(xref.lty)) xref.lty <- ref.lty
  if(is.null(xref.lwd)) xref.lwd <- ref.lwd
  if(is.null(xref.alpha)) xref.alpha <- ref.alpha

  if(is.null(yref.col)) yref.col <- ref.col
  if(is.numeric(yref.col)) yref.col <- hue_pal()(yref.col[[1]])
  if(is.null(yref.lty)) yref.lty <- ref.lty
  if(is.null(yref.lwd)) yref.lwd <- ref.lwd
  if(is.null(yref.alpha)) yref.alpha <- ref.alpha

  dots <- list(...)
  settings <- as.list(settings)
  if(is.null(names(settings))) names(settings) <- character(0)
  settings <- merge(settings, dots)
  aspect <- metaplot_aspect(aspect, gg)
  stopifnot(inherits(x, 'data.frame'))
  stopifnot(length(groups) <= 1)
  stopifnot(is.character(yvar))
  stopifnot(is.character(xvar))
  stopifnot(length(xvar) == 1)
  stopifnot(is.numeric(padding))
  padding <- rep(padding, length.out = 4)
  par.settings <- settings[names(settings) %in% names(trellis.par.get())]
  settings <- settings[!(names(settings) %in% names(trellis.par.get()))] ###
  par.settings <- parintegrate(par.settings, padding)
  if(gg)padding <- unit(padding * 5.5, 'pt')

  if(!is.null(facets))stopifnot(is.character(facets))
  y <- x
  stopifnot(all(c(xvar,yvar,groups,facets) %in% names(y)))
  if(!is.null(groups))if(!is.factor(y[[groups]])){
    y[[groups]] <- factor(y[[groups]])
    for(at in names(attributes(x[[groups]])))if(! at %in% c('levels','class'))attr(y[[groups]], at) <- attr(x[[groups]], at)
  }
  # now groups is factor if supplied
  if(any(c('metaplot_groups','metaplot_values') %in% names(y)))
      stop('metaplot_groups and metaplot_values are reserved and cannot be column names')
  if(length(yvar) > 1){
    suppressWarnings(y %<>% gather(metaplot_groups, metaplot_values, !!!yvar, factor_key = TRUE))
    groups <- 'metaplot_groups' # groups is factor if derived
    labs <- sapply(yvar, function(col){
      a <- attr(x[[col]], 'label')
      if(is.null(a)) a <- ''
      a
    })

    if(!any(labs == '')){
      attr(y[['metaplot_groups']],'guide') <- encode(yvar,labs)
    } else {
      attr(y[['metaplot_groups']],'guide') <- encode(yvar)
    }
    gui <- sapply(yvar, function(col){
      a <- attr(x[[col]], 'guide')
      if(is.null(a)) a <- ''
      a
    })
    gui <- unique(gui)
    if(length(gui) == 1) attr(y$metaplot_values, 'guide') <- gui
    yvar <- 'metaplot_values'
  }
  if(is.null(groups)){
    y$metaplot_groups <- factor(0)
    groups <- 'metaplot_groups'
  }
  # groups is factor if imputed

  # groups now assigned and is factor; and yvar is singular

  # yref
  yref
  if(is.character(yref)) yref <- match.fun(yref)
  if(is.function(yref)) yref <- do.call(yref, c(list(x = x, var = yvar),settings))
  yref <- as.numeric(yref)
  yref <- yref[is.defined(yref)]
  # xref
  xref
  if(is.character(xref)) xref <- match.fun(xref)
  if(is.function(xref)) xref <- do.call(xref, c(list(x = x, var = xvar),settings))
  xref <- as.numeric(xref)
  xref <- xref[is.defined(xref)]

  iso.aes <- list(col = 'black', lty = 'solid', lwd = 1, alpha = 1)
  if(is.list(iso)){
    iso.aes <- merge(iso.aes, iso)
    iso <- TRUE
  }
  if(is.null(iso)) iso <- FALSE # same as default
  if(is.na(iso)){
    left <- attr(y[[yvar]],'guide')
    right <- attr(y[[xvar]],'guide')
    if(is.character(left))
       if(!is.na(left))
          if(is.character(right))
            if(!is.na(right))
              if(left == right)iso <- TRUE
  }

  if(is.na(iso)) iso <- FALSE
  if(iso)prepanel <- isoprepanel

  if(na.rm) {
    #y %<>% filter(is.defined(UQ(yvar)) & is.defined(UQ(xvar))) # preserves attributes
    foo <- y
    y <- y[is.defined(y[[yvar]]) & is.defined(y[[xvar]]),]
    for(col in names(foo))attributes(y[[col]]) <- attributes(foo[[col]])
    at <- attributes(foo)
    at$row.names <- NULL
    for(a in names(at)) attr(y,a) <- attr(foo,a)
  }
  ff <- character(0)
  if(!is.null(facets))ff <- paste(facets, collapse = ' + ')
  if(!is.null(facets))ff <- paste0('|',ff)
  formula <- as.formula(yvar %>% paste(sep = '~', xvar) %>% paste(ff))
  default_log <- function(x,crit){
    x <- x[!is.na(x)]
    all(x > 0) && (mean(x)/median(x) > crit)
  }
  if(is.null(ylog)) ylog <- FALSE # same as default
  if(is.null(xlog)) xlog <- FALSE # same as default
  if(is.na(ylog)) ylog <- default_log(y[[yvar]], crit)
  if(is.na(xlog)) xlog <- default_log(y[[xvar]], crit)

  bady <- !is.na(y[[yvar]]) & y[[yvar]] <= 0
  bady[is.na(bady)] <- FALSE
  if(ylog && any(bady)){
    warning('dropping ',sum(bady), ' non-positive records for log y scale')
    # y <- y[!bady,]
    foo <- y
    #y <- y[is.defined(y[[yvar]]) & is.defined(y[[xvar]]),]
    y <- y[!bady,]
    for(col in names(foo))attributes(y[[col]]) <- attributes(foo[[col]])
    at <- attributes(foo)
    at$row.names <- NULL
    for(a in names(at)) attr(y,a) <- attr(foo,a)

  }

  badx <- !is.na(y[[xvar]]) & y[[xvar]] <= 0
  #badx[is.na(badx)] <- FALSE
  if(xlog && any(badx)){
    warning('dropping ',sum(badx), ' non-positive records for log x scale')
    foo <- y
    y <- y[!badx,]
    for(col in names(foo))attributes(y[[col]]) <- attributes(foo[[col]])
    at <- attributes(foo)
    at$row.names <- NULL
    for(a in names(at)) attr(y,a) <- attr(foo,a)
  }

  if(ylog & !gg) yref <- log10(yref[yref > 0])
  if(xlog & !gg) xref <- log10(xref[xref > 0])

  yscale = list(log = ylog,equispaced.log = FALSE)
  xscale = list(log = xlog,equispaced.log = FALSE)
  defaultscales <- list(y = yscale,x = xscale,tck = c(1,0),alternating = FALSE)
  if(gg) defaultscales <- 'fixed'
  if(is.null(scales)){
    scales <- defaultscales
  } else {
    if(!gg){
      scales <- modifyList(defaultscales, scales)
    }
  }

  #  if(is.null(scales) && gg) scales <- 'fixed'
  #  if(is.null(scales)) scales <- list(y = yscale,x = xscale,tck = c(1,0),alternating = FALSE)

  if(is.character(ylab)) ylab <- tryCatch(match.fun(ylab), error = function(e)ylab)
  if(is.function(ylab)) ylab <- do.call(ylab,c(list(y, var = yvar, log = ylog),settings))
  ylab <- base::sub('metaplot_values','',ylab)

  if(is.character(xlab)) xlab <- tryCatch(match.fun(xlab), error = function(e)xlab)
  if(is.function(xlab)) xlab <- do.call(xlab,c(list(y, var = xvar, log = xlog),settings))

  # if (is.null(groups)) # cannot be null at this point
  y[[groups]] <- as_factor(y[[groups]]) # blends with guide, if present
  if(!is.null(main))if(is.function(main)) main <- do.call(main, c(list(x = y,yvar = yvar, xvar = xvar, groups = groups, facets = facets, log = log),settings))
  if(!is.null(sub))if(is.function(sub))   sub <-  do.call(sub,  c(list(x = y, yvar = yvar, xvar = xvar, groups = groups, facets = facets, log = log), settings))

  #groups <- as.formula(paste('~',groups))
  if(!is.null(facets)){
    for (i in seq_along(facets)) y[[ facets[[i]] ]] <- as_factor(y[[ facets[[i]] ]])
  }

  nlev <- length(levels(y[[groups]]))
  levs <- levels(y[[groups]])
  if(is.null(symbols) &&  gg) symbols <- 16
  if(is.null(symbols) && !gg && nlev == 1) symbols <- trellis.par.get()$plot.symbol$pch
  if(is.null(symbols) && !gg && nlev != 1) symbols <- trellis.par.get()$superpose.symbol$pch
  if(is.null(sizes)) sizes <- 1 # same as default
  if(is.null(types)) types <- 'solid' # same as default
  if(is.null(widths)) widths <- 1 # same as default
  if(gg)widths <- widths * .5
  if(!gg)sizes <- sizes * .8
  if(is.null(points)) points <- TRUE # same as default
  points <- as.numeric(points)
  if(is.null(lines)) lines <- FALSE # same as default
  lines <- as.numeric(lines)
  if(is.null(colors) && nlev == 1 &  gg) colors <- 'black'
  if(is.null(colors) && nlev == 1 & !gg) colors <- trellis.par.get()$plot.symbol$col
  if(is.null(colors) && nlev != 1 &  gg) colors <- hue_pal()(nlev)
  if(is.null(colors) && nlev != 1 & !gg) colors <- trellis.par.get()$superpose.symbol$col
  if(is.numeric(colors)) colors <- hue_pal()(colors[[1]])
  if(is.null(fill)) fill <- colors
  symbols <- rep(symbols, length.out = nlev)
  sizes <- rep(sizes, length.out = nlev)
  types <- rep(types, length.out = nlev)
  widths <- rep(widths, length.out = nlev)
  points <- rep(points, length.out = nlev)
  lines <- rep(lines, length.out = nlev)
  colors <- rep(colors, length.out = nlev)
  fill <- rep(fill, length.out = nlev)
  sym <- trellis.par.get()$superpose.symbol
  line <- trellis.par.get()$superpose.line
  sym$col <- colors
  sym$fill <- fill
  sym$alpha <- points
  sym$pch <- symbols
  sym$cex <- sizes
  line$col <- colors
  line$alpha <- lines
  line$lty <- types
  line$lwd <- widths

    #par.settings is defined
  if(is.null(par.settings$superpose.symbol)) par.settings$superpose.symbol <- sym
  if(is.null(par.settings$superpose.line)) par.settings$superpose.line <- line
 # pars <- pars[sapply(pars, function(i)length(i) > 0 )]

  if(is.character(key)) key <- match.fun(key)
  if(is.function(key)) key <- do.call(key,c(list(
    groups = groups, levels = levs, points = points, lines = lines,
    space = space, gg = gg, type = 'scatter', verbose = verbose), settings)
  )
  # key$cex <- NULL # cex used for lattice point sizes

  if(gg){

    y$metaplot_points_alpha <- points[as.numeric(y[[groups]])]
    y$metaplot_points_sizes <- sizes[as.numeric(y[[groups]])]
    y$metaplot_lines_alpha <- lines[as.numeric(y[[groups]])]
    y$metaplot_lines_widths <- widths[as.numeric(y[[groups]])]

    xrange <- range(y[[xvar]], na.rm = TRUE)
    yrange <- range(y[[yvar]], na.rm = TRUE)
    lo <- min(xrange[[1]], yrange[[1]])
    hi <- max(xrange[[2]], yrange[[2]])
    isorange <- c(lo, hi)
    xpos <- if(sum(loc)) xpos(loc, xrange) else NA
    ypos <- if(sum(loc)) ypos(loc, yrange) else NA
    msg <- if(nlev == 1 & is.null(facets) & sum(loc)) do.call(match.fun(msg), c(list(x = y[[xvar]], y = y[[yvar]]), settings)) else ''
    p <- ggplot(
      data = y,
      mapping = aes_string(
        x = xvar,
        y = yvar,
        color = groups,
        fill = groups,
        shape = groups,
        linetype = groups
      )
    )
    p <- p + scale_alpha_identity()
    p <- p + guides(alpha = FALSE)
    p <- p + scale_size_identity()
    p <- p + guides(size = FALSE)
    p <- p +  scale_shape_manual(values = symbols)
    p <- p +  scale_linetype_manual(values = types)

    if(any(as.logical(points))) p <- p + geom_point(mapping = aes(alpha = metaplot_points_alpha, size = metaplot_points_sizes))
    if(any(as.logical(lines)))  p <- p + geom_line( mapping = aes(alpha = metaplot_lines_alpha,  size = metaplot_lines_widths))
    p <- p +  xlab(xlab)
    p <- p +  ylab(ylab)
    p <- p +  ggtitle(main, subtitle = sub)
    if(ysmooth & global) p <- p + geom_line(
      stat = 'smooth',
      alpha = smooth.alpha,
      linetype = smooth.lty,
      size = smooth.lwd,
      method = 'loess',
      se = FALSE,
      color = global.col,
      # fill = global.fill,
      inherit.aes = FALSE,
      mapping = aes_string(x = xvar,y = yvar),
      show.legend = FALSE
    )
    if(ysmooth & !global) p <- p + geom_line(
      stat = 'smooth',
      alpha = smooth.alpha,
      linetype = smooth.lty,
      size = smooth.lwd,
      method = 'loess',
      se = FALSE,
      # mapping = aes_string(x = xvar,y = yvar, color = groups),
      show.legend = FALSE
    )
    if(xsmooth & global) p <- p + geom_line(
      stat = 'smooth',
      alpha = smooth.alpha,
      linetype = smooth.lty,
      size = smooth.lwd,
      method = 'loess',
      se = FALSE,
      color = global.col,
      # fill = global.fill,
      inherit.aes = FALSE,
      mapping = aes_string(x = xvar,y = yvar),
      show.legend = FALSE,
      formula = x ~ y
    )
    if(xsmooth & !global) p <- p + geom_line(
      stat = 'smooth',
      alpha = smooth.alpha,
      linetype = smooth.lty,
      size = smooth.lwd,
      method = 'loess',
      se = FALSE,
      # mapping = aes_string(x = xvar,y = yvar, color = groups),
      show.legend = FALSE,
      formula = x ~ y
    )
    if(conf & global) p <- p + geom_smooth(
      alpha = conf.alpha,
      linetype = 'blank',
      method = 'lm',
      se = TRUE,
      color = global.col,
      #fill = global.fill,
      inherit.aes = FALSE,
      mapping = aes_string(x = xvar,y = yvar),
      show.legend = FALSE,
      level = if(is.logical(conf))0.95 else as.numeric(conf)
    )
    if(conf & !global) p <- p + geom_smooth(
      alpha = conf.alpha,
      linetype = 'blank',
      method = 'lm',
      se = TRUE,
      # mapping = aes_string(x = xvar,y = yvar, color = groups),
      show.legend = FALSE,
      level = if(is.logical(conf))0.95 else as.numeric(conf)
    )
    if(fit & global) p <- p + geom_line( # https://stackoverflow.com/questions/19474552/adjust-transparency-alpha-of-stat-smooth-lines-not-just-transparency-of-confi
      stat = 'smooth',
      alpha = fit.alpha,
      linetype = fit.lty,
      size = fit.lwd,
      method = 'lm',
      color = global.col,
      #fill = global.fill,
      inherit.aes = FALSE,
      mapping = aes_string(x = xvar,y = yvar),
      se = FALSE,
      show.legend = FALSE
    )
    if(fit & !global) p <- p + geom_line(
      stat = 'smooth',
      alpha = fit.alpha,
      linetype = fit.lty,
      size = fit.lwd,
      method = 'lm',
      # mapping = aes_string(x = xvar,y = yvar, color = groups),
      se = FALSE,
      show.legend = FALSE
    )

    nxref <- length(xref)
    nyref <- length(yref)

    xref.col <- rep(xref.col,  length.out = nxref)
    xref.lty <- rep(xref.lty,  length.out = nxref)
    xref.lwd <- rep(xref.lwd,  length.out = nxref)
    xref.alpha<-rep(xref.alpha,length.out = nxref)

    yref.col <- rep(yref.col,  length.out = nyref)
    yref.lty <- rep(yref.lty,  length.out = nyref)
    yref.lwd <- rep(yref.lwd,  length.out = nyref)
    yref.alpha<-rep(yref.alpha,length.out = nyref)

    panels <- 0
    if(length(facets))panels <- nrow(unique(x[facets]))
    if(!panels) panels <- 1

    xref.col <- rep(xref.col,  times = panels)
    xref.lty <- rep(xref.lty,  times = panels)
    xref.lwd <- rep(xref.lwd,  times = panels)
    xref.alpha<-rep(xref.alpha,times = panels)

    yref.col <- rep(yref.col,  times = panels)
    yref.lty <- rep(yref.lty,  times = panels)
    yref.lwd <- rep(yref.lwd,  times = panels)
    yref.alpha<-rep(yref.alpha,times = panels)


    if(length(yref)) p <- p + geom_hline(
        yintercept = yref,
        color = yref.col,
        linetype = yref.lty,
        size = yref.lwd,
        alpha = yref.alpha
      )
    if(length(xref)) p <- p + geom_vline(
        xintercept = xref,
        color = xref.col,
        linetype = xref.lty,
        size = xref.lwd,
        alpha = xref.alpha
      )
    if(iso){
      p <- p + geom_abline(
        slope = 1,
        intercept = 0,
        color = iso.aes$col,
        linetype = iso.aes$lty,
        size = iso.aes$lwd,
        alpha = iso.aes$alpha
      )
      lo <- min(min(y[[yvar]], na.rm=T), min(y[[xvar]], na.rm=T), na.rm=T)
      hi <- max(max(y[[yvar]], na.rm=T), max(y[[xvar]], na.rm=T), na.rm=T)
      p <- p + scale_y_continuous(limits = c(lo, hi))
      p <- p + scale_x_continuous(limits = c(lo, hi))
    }
    theme_settings <- list(aspect.ratio = aspect, plot.margin = padding)
    theme_settings <- merge(theme_settings, key)
    theme_extra <- settings[names(settings) %in% names(formals(theme))]
    theme_settings <- merge(theme_settings, theme_extra)
    p <- p + do.call(theme, theme_settings)
    #if(groups == 'metaplot_groups') p <- p + theme(legend.title=element_blank())
    p <- p + theme(legend.title=element_blank())

    if(xlog) p <- p + scale_x_continuous(
      trans = log_trans(),
      breaks = base_breaks(),
      limits = if(iso)c(lo,hi) else NULL
    )
    if(ylog) p <- p + scale_y_continuous(
      trans = log_trans(),
      breaks = base_breaks(),
      limits = if(iso)c(lo,hi) else NULL
    )

    p <- p +
      scale_color_manual(values = colors) +
      scale_fill_manual(values = fill)

    if(nlev == 1 & is.null(facets) & sum(loc)) p <- p + geom_text(
      x = xpos,
      y = ypos,
      label = msg
    )
    facet_args <- list()
    if(length(facets) ==1) facet_args[[1]] <- facets[[1]]
    if(length(facets) > 1) facet_args[[1]] <- as.formula(
      paste(
        sep='~',
        facets[[1]],
        facets[[2]]
      )
    )
    facet_args$scales <- scales
    facet_extra <- list()
    if(length(facets) == 1) facet_extra <- settings[names(settings) %in% names(formals(facet_wrap))]
    if(length(facets) >  1) facet_extra <- settings[names(settings) %in% names(formals(facet_grid))]
    facet_args <- merge(facet_args, facet_extra)
    if(length(facets) == 1) p <- p + do.call(facet_wrap, facet_args)
    if(length(facets) >  1) p <- p + do.call(facet_grid, facet_args)
    return(p)
  }

  args <- list(
    formula,
    data = y,
    groups = as.formula(paste('~',groups)),
    auto.key = key,
    as.table = as.table,
    aspect = aspect,
    scales = scales,
    prepanel = prepanel,
    yref = yref,
    xref = xref,
    ysmooth = ysmooth,
    xsmooth = xsmooth,
    ylab = ylab,
    xlab = xlab,
    iso = if(iso) iso.aes else FALSE,
    panel = panel,
    subscripts = subscripts,
    par.settings = par.settings,
    main = main,
    sub = sub,
    .data = y,
    xvar = xvar,
    yvar = yvar,
    groups_ = groups,
    facets = facets,
    ref.col = ref.col,
    ref.lty = ref.lty,
    ref.lwd = ref.lwd,
    ref.alpha = ref.alpha,
    xref.col = xref.col,
    xref.lty = xref.lty,
    xref.lwd = xref.lwd,
    xref.alpha = xref.alpha,
    yref.col = yref.col,
    yref.lty = yref.lty,
    yref.lwd = yref.lwd,
    yref.alpha = yref.alpha,
    smooth.lty = smooth.lty,
    smooth.lwd = smooth.lwd,
    smooth.alpha = smooth.alpha,
    global = global,
    global.col = global.col,
    global.fill = global.fill,
    fit = fit,
    fit.lty = fit.lty,
    fit.lwd = fit.lwd,
    fit.alpha = fit.alpha,
    conf = conf,
    conf.alpha = conf.alpha,
    loc = loc, # ?
    msg = msg,
    verbose = verbose
  )
  args <- c(args, settings)
 # args$cex <- NULL # regarding symbol sizes
  if(all(c('ncol','nrow') %in% names(settings))){
    layout <- c(settings$ncol, settings$nrow)
    args <- c(args, list(layout = layout))
  }
  if(verbose)cat('calling xyplot')
  do.call(xyplot, args)
}

#' Prepanel Function for Isometric Axes
#'
#' Prepanel function for isometric axes.  Returns join minimum and maximum for limits on both axes.
#' @export
#' @return list
#' @family panel functions
#' @family bivariate plots
#' @keywords internal
#' @param x numeric
#' @param y numeric
#'
iso_prepanel <- function(x,y,...){
  lim = c(min(x,y,na.rm = T),max(x,y,na.rm = T))
  list(
    xlim = lim,
    ylim = lim
  )
}
#' Scatterplot Method for Data Frame
#'
#' Scatterplot method for class 'data.frame'. Parses arguments and generates the call: fun(x, yvar, xvar, groups, facets, ...).
#' @param x data.frame
#' @param ... passed to fun
#' @param fun function to draw the plot
#' @param verbose generate messages describing process
#' @seealso \code{\link{scatter_data_frame}}
#' @export
#' @import lattice
#' @importFrom rlang f_rhs quos
#' @family bivariate plots
#' @family scatter
#' @family methods
#' @examples
#' library(magrittr)
#' library(dplyr)
#' attr(Theoph$conc,'label') <- 'theophylline concentration'
#' attr(Theoph$conc,'guide') <- 'mg/L'
#' attr(Theoph$Time,'label') <- 'time'
#' attr(Theoph$Time,'guide') <- 'h'
#' attr(Theoph$Subject,'guide') <- '////'
#' # setOption(gg = T)
#' scatter(Theoph,conc, Time)
#' scatter(Theoph, conc, Time, Subject) # Subject as groups
#' scatter(Theoph, conc, Time, , Subject) # Subject as facet
#' scatter(Theoph, conc, Time, , Subject, gg = TRUE, scales = 'free_y' )
#' scatter(Theoph %>% filter(conc > 0), conc, Time, Subject, ylog = TRUE, yref = 5)
#' scatter(Theoph, conc, Time, Subject, ysmooth = TRUE)
#' scatter(Theoph, conc, Time, conf = TRUE, loc = 3, yref = 6)
#' scatter(Theoph, conc, Time, conf = TRUE, loc = 3, yref = 6, global = TRUE)
#' \dontrun{
#' \dontshow{
#' attr(Theoph,'title') <- 'Theophylline'
#' scatter(Theoph, conc, Time, main = function(x,...)attr(x,'title'))
#' scatter(Theoph, conc, Time, sub= function(x,...)attr(x,'title'))
#' setOption(main = function(x,...)attr(x,'title'))
#' scatter(Theoph, conc, Time)
#' }
#' }
scatter.data.frame <- function(
  x,
  ...,
  fun = metOption('scatter','scatter_data_frame'),
  verbose = metOption('verbose_scatter',FALSE)
){
  args <- quos(...)
  args <- lapply(args,f_rhs)
  vars <- args[names(args) == '']
  other <- args[names(args) != '']
  vars <- sapply(vars, as.character)

  # this function needs to explicitly assign xvar, yvar, groups, and facets
  # prime is all yvar, if present, and xvar
  # prime is defined as all vars before groups or facets, if present
  # non-prime start with the first missing or categorical in position 3 or later
  # since groups may be missing, checking properties may fail
  # isolate non-prime
  missing <- match('',vars)
  if(is.defined(missing)){
    #prime <- vars[seq_len(missing - 1)]
    #if(length(vars) > missing) nonpr <- vars[(missing+1):length(vars)]
    vars <- vars[-missing]
  }
  # now we have protected vars from missingness, but preserved info from missing group, if any

  # test numeric
  stopifnot(all(vars %in% names(x)))
  num <- sapply(x[vars], is.numeric)

  # but the definition of numeric depends partly on guide.
  guide <- lapply(x[vars], attr, 'guide')
  guide[is.null(guide)] <- ''
  stopifnot(all(sapply(guide,length) <= 1))
  guide <- as.character(guide)

  encoded <- encoded(guide)
  num[encoded] <- FALSE # now num is fully defined

  # if groups was not passed as missing, prime etc can be defined in terms of num
  # must reserve at least one yvar and one xvar.
  # find first categorical in position 3 or later
  pos <- seq_along(num)
  can <- !num & pos > 2
  grp <- match(TRUE, can)

  # we now have var, giving the names of all real variables
  # missing is NA, or one greater than the last prime
  # grp is NA, or the position of the first (remaining) non-prime
  # x is last position in var not greater than missing or grp
  xlim <- min(na.rm = T, missing, grp, length(vars) + 1)
  xpos <- xlim - 1
  xvar <- vars[xpos]
  yvar <- vars[seq_len(xpos -1)]
  groups <- NULL
  facets <- NULL
  more <- character(0)
  if(length(vars) > xpos) more <- vars[(xpos+1):length(vars)]
  # first additional is groups if missing:NA and length(y) == 1
  if(length(more) & is.na(missing) & length(yvar) == 1){
    groups <- more[[1]]
    more <- more[-1]
  }
  # any remaining are facets
  if(length(more)) facets <- more

  formal <- list(
    x = x,
    yvar = yvar,
    xvar = xvar,
    groups = groups,
    facets = facets
  )
  args <- c(formal, other)
  if(verbose){
    if(is.character(fun))message('calling ', fun) else message('calling fun')
  }
  do.call(match.fun(fun), args)
}

#' Panel Function for Metaplot Scatterplot
#'
#' Default panel function for scatter_data_frame. Calls \code{\link[lattice]{panel.xyplot}}
#' and optionally plots linear fit, confidence region, reference lines, and statistics.
#' Note that, although global options are supported, typically these are unreachable
#' since the calling function supplies appropriate values.
#'
#' @export
#' @param x x values
#' @param y y values
#' @param groups optional grouping item
#' @param yref reference line from y axis; can be function(y, x, ...)
#' @param xref reference line from x axis; can be function(x, y, ...)
#' @param ref.col default shared by \code{xref.col} and \code{yref.col}
#' @param ref.lty default shared by \code{xref.lty} and \code{yref.lty}
#' @param ref.lwd default shared by \code{xref.lwd} and \code{yref.lwd}
#' @param ref.alpha default shared by \code{xref.alpha} and \code{yref.alpha}
#' @param xref.col x reference line color (recycled)
#' @param xref.lty x reference line type (recycled)
#' @param xref.lwd x reference line size (recycled)
#' @param xref.alpha x reference line alpha (recycled)
#' @param yref.col y reference line color (recycled)
#' @param yref.lty y reference line type (recycled)
#' @param yref.lwd y reference line size (recycled)
#' @param yref.alpha y reference line alpha (recycled)
#' @param ysmooth supply loess smooth of y on x
#' @param xsmooth supply loess smmoth of x on y
#' @param smooth.lty smooth line type
#' @param smooth.lwd smooth line size
#' @param smooth.alpha smooth alpha
#' @param iso logical: use isometric axes with line of unity (auto-selected if NA); can be a (partial) list of aesthetics (col, lty, lwd, alpha)
#' @param global if TRUE, xsmooth, ysmooth, fit, and conf are applied to all data rather than groupwise
#' @param global.col color for global aesthetics
#' @param global.fill fill color for global aesthetics
#' @param fit draw a linear fit of y ~ x; defaults to \code{as.logical(conf)}
#' @param fit.lty fit line type
#' @param fit.lwd fit line size
#' @param fit.alpha fit alpha
#' @param conf logical, or width for a confidence region around a linear fit; passed to \code{\link{region}}; \code{TRUE} defaults to 95 percent confidence interval; may not make sense if xlog is TRUE
#' @param conf.alpha alpha transparency for confidence region
#' @param loc where to print statistics on a panel; suppressed for grouped plots
#' @param msg a function to print text on a panel: called with x values, y values, and \dots.
#' @param type overridden by scatter_panel
#' @param verbose generate messages describing process
#' @param ... passed to panel.superpose, panel.xyplot, panel.polygon, region, panel.text
#' @family panel functions
#' @family scatter
#' @seealso \code{\link{metastats}}
#' @seealso \code{\link{scatter.data.frame}}
#'
scatter_panel <- function(
  x,
  y,
  groups,
  xref = metOption('xref_scatter_panel',scatter_panel_ref),
  yref = metOption('yref_scatter_panel',scatter_panel_ref),

  ref.col = metOption('ref.col_scatter_panel','grey'),
  ref.lty = metOption('ref.lty_scatter_panel','solid'),
  ref.lwd = metOption('ref.lwd_scatter_panel',1),
  ref.alpha = metOption('ref.alpha_scatter_panel',1),

  xref.col = metOption('xref.col_scatter_panel',NULL),
  xref.lty = metOption('xref.lty_scatter_panel',NULL),
  xref.lwd = metOption('xref_lwd_scatter_panel',NULL),
  xref.alpha = metOption('xref_alpha_scatter_panel',NULL),

  yref.col = metOption('yref_col_scatter_panel',NULL),
  yref.lty = metOption('yref_lty_scatter_panel',NULL),
  yref.lwd = metOption('yref_lwd_scatter_panel',NULL),
  yref.alpha = metOption('yref_alpha_scatter_panel',NULL),

  ysmooth = metOption('ysmooth_scatter_panel',FALSE),
  xsmooth = metOption('xsmooth_scatter_panel',FALSE),
  smooth.lty = metOption('smooth.lty_scatter_panel','dashed'),
  smooth.lwd = metOption('smooth.lwd_scatter_panel',1),
  smooth.alpha = metOption('smooth.alpha_scatter_panel',1),
  fit = metOption('fit_scatter_panel',NULL),
  fit.lty = metOption('fit.lty_scatter_panel','solid'),
  fit.lwd = metOption('fit.lwd_scatter_panel',1),
  fit.alpha = metOption('fit.alpha_scatter_panel',1),
  conf = metOption('conf_scatter_panel',FALSE),
  conf.alpha = metOption('conf.alpha_scatter_panel',0.3),
  loc = metOption('loc_scatter_panel',0),
  iso = metOption('iso_scatter_panel',FALSE),
  global = metOption('global_scatter_panel',FALSE),
  global.col = metOption('global.col_scatter_panel','grey'),
  global.fill = metOption('global.fill_scatter_panel','grey'),
  msg = metOption('msg_scatter_panel','metastats'),
  type,
  verbose = metOption('verbose_scatter_panel',FALSE),
  ...
)
{
  if(verbose)cat('this is scatter_panel')
  # panel.superpose extracts lwd from superpose.symbol$line and passes it
  # unconditionally to panel.groups as an element of args.
  # lpoints.default (panel.groups) does not have an lwd arg, and so
  # passes lwd to lplot.xy as ... .
  # lplot.xy has an lwd arg and passes same to grid.points as lwd .
  # grid.points has an lwd, and uses lwd originating from superpose.line to
  # draw borders of points.
  if(is.null(fit)) fit <- as.logical(conf)
  if(is.null(xref.col)) xref.col <- ref.col
  if(is.null(xref.lty)) xref.lty <- ref.lty
  if(is.null(xref.lwd)) xref.lwd <- ref.lwd
  if(is.null(xref.alpha)) xref.alpha <- ref.alpha

  if(is.null(yref.col)) yref.col <- ref.col
  if(is.null(yref.lty)) yref.lty <- ref.lty
  if(is.null(yref.lwd)) yref.lwd <- ref.lwd
  if(is.null(yref.alpha)) yref.alpha <- ref.alpha

  stopifnot(length(global) == 1, is.logical(global))
  # if(is.null(groups)) groups <- rep(TRUE,length(x)) # cannot be NULL
  myxsmooth <- function(x,y,type,lty,lwd,col, col.symbol, col.line,...){
    bar <- try(silent = TRUE, suppressWarnings(loess.smooth(y,x, family = 'gaussian')))
    if(xsmooth && !inherits(bar,'try-error'))try(panel.xyplot(bar$y,bar$x,lty = smooth.lty,lwd = smooth.lwd, alpha = smooth.alpha,type = 'l',col = col.line,...))
  }
  myysmooth <- function(x,y,type,lty,lwd,col, col.symbol, col.line,alpha,...){
    foo <- try(silent = TRUE, suppressWarnings(loess.smooth(x,y, family = 'gaussian')))
    if(ysmooth && !inherits(foo,'try-error'))try(panel.xyplot(foo$x,foo$y,lty = smooth.lty, lwd = smooth.lwd, alpha = smooth.alpha,type = 'l',col = col.line,...))
  }
  myfit <- function(x,y,type,lty,lwd,col, col.symbol, col.line,alpha,...){
    f <- data.frame()
    f <- region(x, y, conf = conf, ...)
    try(panel.xyplot(x=f$x, y=f$y, col= col.line, type='l',lty = fit.lty,lwd = fit.lwd, alpha= fit.alpha, ...))
  }
  myconf <- function(x,y,type,lty,lwd, col, col.symbol, col.line, fill, alpha, ...){
    f <- region(x, y, conf = conf, ...)
    try(panel.polygon(
      x = c(f$x, rev(f$x)),
      y = c(f$lo, rev(f$hi)),
      border = FALSE,
      alpha = conf.alpha,
      col=fill
    ))
  }
  my_lpoints <- function (x, y = NULL, type = "p", col = plot.symbol$col, pch = plot.symbol$pch,
            alpha = plot.symbol$alpha, fill = plot.symbol$fill, font = plot.symbol$font,
            fontfamily = plot.symbol$fontfamily, fontface = plot.symbol$fontface,
            cex = plot.symbol$cex, lwd, ..., identifier = NULL, name.type = "panel")
  {
    plot.symbol <- trellis.par.get("plot.symbol")
    lplot.xy(xy.coords(x, y, recycle = TRUE), type = type, col = col,
             pch = pch, alpha = alpha, fill = fill, font = font, fontfamily = fontfamily,
             fontface = fontface, cex = cex, ..., identifier = identifier,
             name.type = name.type)
  } # traps lwd to prevent passing to lplot.xy
  superpose.line <- trellis.par.get()$superpose.line
  superpose.symbol <- trellis.par.get()$superpose.symbol
  if(verbose)cat('calling panel.superpose')
  panel.superpose(x = x,y = y,groups = groups,panel.groups = panel.lines,type='l',alpha = superpose.line$alpha, ...)
  if(verbose)cat('calling panel.superpose')
  panel.superpose(x = x,y = y,groups = groups,panel.groups = my_lpoints,type='p',alpha = superpose.symbol$alpha, ...)
  if(conf){
    if(global){
      myconf(x, y, col = global.col, fill = global.fill, col.symbol = global.col, col.line = global.col, alpha = conf.alpha, ...)
    }else{
      panel.superpose(x = x, y = y, groups = groups, panel.groups = myconf, ...)
    }
  }
  if(fit){
    if(global){
      myfit(x,y, col = global.col, fill = global.fill, col.symbol = global.col, col.line = global.col,...)
    }else{
      panel.superpose(x = x, y = y, groups = groups, panel.groups = myfit, ...)
    }
  }
  if(ysmooth){
    if(global){
      myysmooth(x,y, col = global.col, fill = global.fill,  col.symbol = global.col, col.line = global.col,...)
    }else{
      panel.superpose(x = x, y = y, groups = groups, panel.groups = myysmooth, ...)
    }
  }
  if(xsmooth){
    if(global){
      myxsmooth(x,y, col = global.col, fill = global.fill, col.symbol = global.col, col.line = global.col,...)
    }else{
      panel.superpose(x = x, y = y, groups = groups, panel.groups = myxsmooth, ...)
    }
  }
  if(sum(loc))panel = panel.text(
    x = xpos(loc, range = current.panel.limits()$xlim),
    y = ypos(loc, range = current.panel.limits()$ylim),
    label = match.fun(msg)(x = x, y = y, ...)
  )
  if(is.character(yref)) yref <- match.fun(yref)
  if(is.function(yref)) yref <- yref(y, x, ...)
  yref <- as.numeric(yref)
  yref <- yref[is.defined(yref)]

  if(is.character(xref)) xref <- match.fun(xref)
  if(is.function(xref)) xref <- xref(x, y, ...)
  xref <- as.numeric(xref)
  xref <- xref[is.defined(xref)]

  nxref <- length(xref)
  nyref <- length(yref)

  xref.col <- rep(xref.col,  length.out = nxref)
  xref.lty <- rep(xref.lty,  length.out = nxref)
  xref.lwd <- rep(xref.lwd,  length.out = nxref)
  xref.alpha<-rep(xref.alpha,length.out = nxref)

  yref.col <- rep(yref.col,  length.out = nyref)
  yref.lty <- rep(yref.lty,  length.out = nyref)
  yref.lwd <- rep(yref.lwd,  length.out = nyref)
  yref.alpha<-rep(yref.alpha,length.out = nyref)

  if(length(yref))panel.abline(h = yref, col = yref.col, lty = yref.lty, lwd = yref.lwd, alpha = yref.alpha)
  if(length(xref))panel.abline(v = xref, col = xref.col, lty = xref.lty, xlwd = ref.lwd, alpha = xref.alpha)

  iso.aes <- list(col = 'black', lty = 'solid', lwd = 1, alpha = 1)
  if(is.list(iso)){
    iso.aes <- merge(iso.aes, iso)
    iso <- TRUE
  }
  if(iso)panel.abline(0, 1, col = iso.aes$col, lty = iso.aes$lty, lwd = iso.aes$lwd, alpha = iso.aes$alpha)
}

xpos <- function(loc, range = 0:1, lo = range[[1]], hi = range[[2]]){
  stopifnot(length(loc) %in% 1:2)
  if(length(loc) == 1) stopifnot(loc == as.integer(loc), loc < 10)
  l <- rep(c(.2,.5,.8),3)
  x <- if(length(loc) == 1) l[[loc]] else loc[[1]]
  stopifnot(x <= 1, x >= 0)
  # lo <- current.panel.limits()$xlim[[1]]
  # hi <- current.panel.limits()$xlim[[2]]
  xpos <- lo + x * (hi - lo)
  xpos
}

ypos <- function(loc, range = 0:1, lo = range[[1]], hi = range[[2]]){
  stopifnot(length(loc) %in% 1:2)
  if(length(loc) == 1) stopifnot(loc == as.integer(loc), loc < 10)
  l <- rep(c(.8,.5,.2),each = 3)
  y <- if(length(loc) == 1) l[[loc]] else loc[[2]]
  stopifnot(y <= 1, y >= 0)
  # lo <- current.panel.limits()$ylim[[1]]
  # hi <- current.panel.limits()$ylim[[2]]
  ypos <- lo + y * (hi - lo)
  ypos
}

#' Default Key
#'
#' Default key function for constructing scatterplot legends.
#'
#' @export
#' @return list, or possibly logical if gg is FALSE
#' @family metaplot
#' @family scatter
#' @family panel functions
#' @param groups name of the grouping variable
#' @param levels the (unique) levels of the grouping variable
#' @param points logical or alpha, same length as groups
#' @param lines logical or alpha, same length as groups
#' @param fill logical or alpha, same length as groups
#' @param space character: left, right, top, or bottom
#' @param gg logical: whether to to return a list of arguments for \code{\link[ggplot2]{theme}} instead of for \code{auto.key} as in \code{\link[lattice]{xyplot}}
#' @param type typically one of 'categorical','density', or 'scatter'
#' @param verbose generate messages describing process
#' @param ... ignored
#'
metaplot_key <- function(
  groups,
  levels,
  points = rep(FALSE, length.out = length(levels)),
  lines = rep(FALSE, length.out = length(levels)),
  fill = rep(FALSE, length.out = length(levels)),
  space = 'right',
  gg = FALSE,
  type = 'scatter',
  verbose = FALSE,
  ...
){
  if(verbose)cat('this is metaplot_key')
  nlev <- length(levels)
  stopifnot(space %in% c('left','right','top','bottom','none'))
  stopifnot(length(points) == nlev)
  stopifnot(length(levels) == nlev)
  stopifnot(length(type) == 1, is.character(type))
  if(type == 'categorical') lines = rep(FALSE, length.out = length(levels)) # coerce to default in this implementation
  key = list()
  if( gg) key$legend.direction <- 'vertical' # esp. for gg top bottom, overrides default to match lattice
  if( gg) key$legend.position <- space
  if(!gg) key$space <- space
  if(!gg) key$points <- any(as.logical(points))
  if(!gg) key$lines <- any(as.logical(lines))
  if(!gg) key$rectangles <- any(as.logical(fill))
  if(!gg & type == 'density') key$lines <- FALSE
  # for density plot, show only fill or lines
  # if(!gg) if(type == 'density'){
  #   showFill <- any(as.numeric(fill) > 0.00000001) # cf categorical nominal value of 0.000000001
  #   key$lines <- !showFill
  #   key$rectangles <- showFill
  # }
  extras <- list(...)
  extras$lwd <- NULL # (inflates point borders)
  nms <- names(extras)
  if(gg) nms <- intersect(nms, names(formals(ggplot2::theme)))
  for(i in nms) key[[i]] <- extras[[i]]
  # no key for imputed grouping
  if(nlev == 1 && groups == 'metaplot_groups')key <- if(gg)list(legend.position = 'none') else FALSE # no legend if one level
  key
}

