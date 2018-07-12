#' Boxplots
#'
#' @name boxplot
#' @keywords internal
NULL

#' Boxplot Function for Data Frame
#'
#' Boxplot for data.frame. Creates a boxplot using \code{boxplot_panel} by default.
#' @param x data.frame
#' @param yvar y variable
#' @param xvar x variable
#' @param facets optional conditioning variables
#' @param log whether to log transform numeric variable (auto-selected if NA)
#' @param crit if log is NA, log-transform if mean/median ratio for non-missing values is greater than this value
#' @param horizontal whether box/whisker axis should be horizontal (numeric x, categorical y); defaults TRUE if var[[2]] is numeric
#' @param scales passed to \code{\link[lattice]{xyplot}} (should be function(x = x, horizontal, log,...)) or \code{\link[ggplot2]{facet_grid}} or \code{\link[ggplot2]{facet_wrap}}
#' @param panel panel function
#' @param ref optional reference line(s) on numeric axis; can be function(x = x, var = con, ...) or NULL to suppress
#' @param ref.col color for reference line(s)
#' @param ref.lty line type for reference line(s)
#' @param ref.lwd line size for reference line(s)
#' @param ref.alpha transparency for reference line(s)
#' @param nobs whether to include the number of observations under the category label
#' @param na.rm whether to remove data points with one or more missing coordinates
#' @param ylab y axis label
#' @param xlab x axis label
#' @param numlab numeric axis label; can be function(x = x, var = numvar, log = ylog, ...)
#' @param catlab categorical axis label; can be function(x = x, var = catvar, ...)
#' @param aspect passed to \code{\link[lattice]{bwplot}} or ggplot; use 'fill', NA, or NULL to calculate automatically
#' @param as.table passed to \code{\link[lattice]{xyplot}}
#' @param main character, or a function of x, yvar, xvar, facets, and log
#' @param sub character, or a function of x, yvar, xvar, facets, and log
#' @param settings default parameter settings: a list from which matching elements are passed to lattice (as par.settings) or  to ggplot theme()  and facet_wrap() or facet_grid().  \code{ncol} and \code{nrow} are used as layout indices for lattice (for homology with facet_wrap).
#' @param padding numeric (will be recycled to length 4) giving plot margins in default units: top, right, bottom, left (in multiples of 5.5 points for ggplot)
#' @param reverse if y is categorical, present levels in reverse order (first at top)
#' @param pch special character for box median: passed to \code{\link[lattice]{panel.bwplot}}
#' @param notch whether to draw notched boxes: passed to \code{\link[lattice]{panel.bwplot}}
#' @param gg logical: whether to generate \code{ggplot} instead of \code{trellis}
#' @param ... passed arguments
#' @export
#' @importFrom rlang quos UQ
#' @importFrom graphics boxplot
#' @import lattice
#' @importFrom stats as.formula median cor loess.smooth density
#' @family mixedvariate plots
#' @family boxplot
#' @family metaplot
#' @examples
#' library(magrittr)
#' library(dplyr)
#' boxplot_data_frame(Theoph,'Subject','conc')
#' boxplot_data_frame(Theoph %>% filter(conc > 0),
#' 'conc','Subject', log = TRUE, ref = c(2,5),horizontal = FALSE)

boxplot_data_frame <- function(
  x,
  yvar,
  xvar,
  facets = NULL,
  log = metOption('metaplot_log_boxplot',FALSE),
  crit = metOption('metaplot_crit_boxplot',1.3),
  horizontal = metOption('metaplot_horizontal_boxplot',NULL),
  scales = metOption('metaplot_scales_boxplot',NULL),
  panel = metOption('metaplot_panel_boxplot',boxplot_panel),
  ref = metOption('metaplot_ref_x_boxplot',metaplot_ref),
  ref.col = metOption('metaplot_ref_col_boxplot','grey'),
  ref.lty = metOption('metaplot_ref_lty_boxplot','solid'),
  ref.lwd = metOption('metaplot_ref_lwd_boxplot',1),
  ref.alpha = metOption('metaplot_ref_alpha_boxplot',1),
  nobs = metOption('metaplot_nobs_boxplot',FALSE),
  na.rm = metOption('metaplot_narm_boxplot',TRUE),
  xlab = NULL,
  ylab = NULL,
  numlab = metOption('metaplot_numlab_boxplot',axislabel),
  catlab = metOption('metaplot_catlab_boxplot',axislabel),
  aspect = metOption('metaplot_aspect_boxplot',1),
  as.table = metOption('metaplot_astable_boxplot',TRUE),
  main = metOption('metaplot_main_boxplot',NULL),
  sub = metOption('metaplot_sub_boxplot',NULL),
  settings = metOption('metaplot_settings_boxplot',NULL),
  padding = metOption('metaplot_padding_boxplot', 1),
  reverse = metOption('metaplot_reverse_boxplot',TRUE),
  pch = metOption('metaplot_pch_boxplot','|'),
  notch = metOption('metaplot_notch_boxplot',FALSE),
  gg = metOption('metaplot_gg_boxplot',FALSE),
  ...
){
  settings <- as.list(settings)
  if(is.null(names(settings))) names(settings) <- character(0)
  aspect <- metaplot_aspect(aspect, gg)
  stopifnot(inherits(x, 'data.frame'))
  stopifnot(is.character(xvar))
  stopifnot(is.character(yvar))
  if(!is.null(facets))stopifnot(is.character(facets))
  stopifnot(is.numeric(padding))
  padding <- rep(padding, length.out = 4)
  par.settings <- settings[names(settings) %in% names(trellis.par.get())]
  par.settings <- parintegrate(par.settings, padding)
  if(gg)padding <- unit(padding * 5.5, 'pt')

  y <- x
  stopifnot(all(c(xvar,yvar,facets) %in% names(y)))

  xguide <- attr(x[[xvar]], 'guide')
  if(is.null(xguide)) xguide <- ''
  xguide <- as.character(xguide)

  if(is.null(horizontal)) horizontal <- is.numeric(x[[xvar]]) & !encoded(xguide)
  cat <- if(horizontal) yvar else xvar
  con <- if(horizontal) xvar else yvar

  if(is.character(ref)) ref <- match.fun(ref)
  if(is.function(ref)) ref <- ref(x = x, var = con, ...)
  ref <- as.numeric(ref)
  ref <- ref[is.defined(ref)]

  stopifnot(all(c(cat,con) %in% names(y)))
  if(na.rm){
    #y %<>% filter(is.defined(UQ(cat)) & is.defined(UQ(con))) # preserves attributes
    foo <- y
    y <- y[is.defined(y[[cat]]) & is.defined(y[[con]]),]
    for(col in names(foo))attributes(y[[col]]) <- attributes(foo[[col]])
    at <- attributes(foo)
    at$row.names <- NULL
    for(a in names(at)) attr(y,a) <- attr(foo,a)
  }
  ff <- character(0)
  if(!is.null(facets))ff <- paste(facets, collapse = ' + ')
  if(!is.null(facets))ff <- paste0('|',ff)
  formula <- as.formula(yvar %>% paste(sep = '~', xvar) %>% paste(ff))

  if(!is.null(facets)){
    for (i in seq_along(facets)) y[[facets[[i]]]] <- as_factor(y[[ facets[[i]] ]])
  }

  #formula <- as.formula(paste(sep = ' ~ ', yvar, xvar))
  if(!is.numeric(y[[con]]))stop(con, ' must be numeric')
  if(is.null(log)) log <- FALSE # same as default
  if(is.na(log)){
    if(any(y[[con]] <= 0, na.rm = TRUE)){
      log <- FALSE
    } else{
      log <- mean(y[[con]],na.rm = TRUE)/median(y[[con]],na.rm = TRUE) > crit
    }
  }

  bad <- !is.na(y[[con]]) & y[[con]] <= 0
  bad[is.na(bad)] <- FALSE
  if(log && any(bad)){
    warning('dropping ',sum(bad), ' non-positive records for log scale')
    y <- y[!bad,]
  }

  if(log & !gg){
    ref <- ref[ref > 0]
    if(length(ref)) ref <- log10(ref)
    if(!length(ref)) ref <- NULL
  }

  if(is.character(catlab)) catlab <- tryCatch(match.fun(catlab), error = function(e)catlab)
  if(is.function(catlab)) catlab <- catlab(x = y, var = cat, ...)
  if(is.character(numlab)) numlab <- tryCatch(match.fun(numlab), error = function(e)numlab)
  if(is.function(numlab)) numlab <- numlab(x = y, var = con, log = log, ...)

  if(is.null(ylab)) ylab <- if(horizontal) catlab else numlab
  if(is.null(xlab)) xlab <- if(horizontal) numlab else catlab

  # guide <- attr(y[[cat]], 'guide')
  # if(encoded(guide)){
  #   y[[cat]] <- decode(y[[cat]],encoding = if(length(decodes(guide)))guide else NULL)
  #   # y[[cat]] <- factor(y[[cat]],levels = rev(levels(y[[cat]])))
  # }
  # if(!is.factor(y[[cat]])) y[[cat]] <- factor(y[[cat]],levels = unique(y[[cat]]))
  y[[cat]] <- as_factor(y[[cat]])
  if(horizontal & reverse) y[[cat]] <- factor(y[[cat]],levels = rev(levels(y[[cat]])))
  if(nobs){
    lev <- levels(y[[cat]])
    num <- sapply(lev,function(l)sum(na.rm = TRUE, y[[cat]] == l))
    levels(y[[cat]]) <- paste(lev,num,sep = '\n')
  }
  if(is.null(scales) && gg) scales <- 'fixed'
  if(is.null(scales)) scales <- function(x = y, horizontal = horizontal, log = log, ...){
    s <- list(
      tck = c(1,0),
      alternating = FALSE,
      x = list(log = log,equispaced.log = FALSE)
    )
    if(!horizontal)s <- list(
      tck = c(1,0),
      alternating = FALSE,
      y = list(log = log,equispaced.log = FALSE)
    )
    s
  }
  if(is.character(scales) && !gg) scales <- match.fun(scales)
  if(!is.function(scales) && !gg) stop('scales must be NULL, or function(x, horizontal, log,...), or the name of a function')
  if(!gg) scales <- scales(x=x, horizontal = horizontal, log = log, ...)

  if(!is.null(main))if(is.function(main)) main <- main(x = x, yvar = yvar, xvar = xvar, facets = facets, log = log, ...)
  if(!is.null(sub))if(is.function(sub)) sub <- sub(x = x, yvar = yvar, xvar = xvar, facets = facets, log = log, ...)

  if(gg){
    plot <- ggplot(data = y, aes_string(cat, con))
    plot <- plot  +
      geom_boxplot(notch = notch) +
      xlab(catlab) +
      ylab(numlab) +
      ggtitle(main, subtitle = sub)

    # scale aesthetics
    panels <- nrow(unique(y[facets]))
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
      geom_hline(
        yintercept = ref,
        color = ref.col,
        linetype = ref.lty,
        size = ref.lwd,
        alpha = ref.alpha
      )
    theme_settings <- list(aspect.ratio = aspect, plot.margin = padding)
    theme_extra <- settings[names(settings) %in% names(formals(theme))]
    theme_settings <- merge(theme_settings, theme_extra)
    plot <- plot + do.call(theme, theme_settings)

    if(log) plot <- plot + scale_y_continuous(
      trans = log_trans(),
      breaks = base_breaks()
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
    if(length(facets) == 1) plot <- plot + do.call(facet_wrap, facet_args)
    if(length(facets) >  1) plot <- plot + do.call(facet_grid, facet_args)
    if(horizontal) plot <- plot + coord_flip()
    return(plot)
  }

  args <- list(
    formula,
    data = y,
    aspect = aspect,
    horizontal = horizontal,
    par.settings = par.settings,
    scales = scales,
    ylab = ylab,
    xlab = xlab,
    as.table = as.table,
    main = main,
    sub = sub,
    panel = panel,
    ref = ref,
    ref.col = ref.col,
    ref.lty = ref.lty,
    ref.lwd = ref.lwd,
    ref.alpha = ref.alpha,
    pch = pch,
    notch = notch
  )

  args <- c(args, list(...))
  if(all(c('ncol','nrow') %in% names(settings))){
    layout <- c(settings$ncol, settings$nrow)
    args <- c(args, list(layout = layout))
  }
  do.call(bwplot, args)
}

#' Panel Function for Metaplot Boxplot
#'
#' Panel function for metaplot boxplot. Calls, \code{\link[lattice]{panel.bwplot}} and plots reference lines if ref has length.
#' @export
#' @family panel functions
#' @family mixedvariate plots
#' @keywords internal
#' @param ref numeric
#' @param horizontal passed to \code{\link[lattice]{panel.bwplot}}
#' @param pch passed to \code{\link[lattice]{panel.bwplot}}
#' @param notch passed to \code{\link[lattice]{panel.bwplot}}
#' @param ref.col passed to \code{\link[lattice]{panel.abline}} as col
#' @param ref.lty passed to \code{\link[lattice]{panel.abline}} as lty
#' @param ref.lwd passed to \code{\link[lattice]{panel.abline}} as lwd
#' @param ref.alpha passed to \code{\link[lattice]{panel.abline}} as alpha
#'
boxplot_panel <- function(ref = NULL, horizontal,pch = '|',notch=FALSE, ref.col, ref.lty, ref.lwd, ref.alpha, ...){
  panel.bwplot(horizontal = horizontal, pch = pch, notch = notch, ...)
  if(length(ref)){
    if(horizontal) {
      panel.abline(v = ref, col = ref.col, lty = ref.lty, lwd = ref.lwd, alpha = ref.alpha)
    }else{
      panel.abline(h = ref, col = ref.col, lty = ref.lty, lwd = ref.lwd, alpha = ref.alpha)
    }
  }
}

#' Boxplot Method for Data Frame
#'
#' Boxplot for data.frame.  Parses arguments and generates the call: fun(x, yvar, xvar, facets, ...).
#' @param x data.frame
#' @param ... passed to fun
#' @param fun function that does the actual plotting
#' @param verbose generate messages describing process
#' @export
#' @importFrom rlang f_rhs
#' @family mixedvariate plots
#' @family boxplot
#' @family methods
#' @examples
#' library(dplyr)
#' library(magrittr)
#' Theoph %<>% mutate(site = ifelse(as.numeric(Subject) > 6, 'Site A','Site B'))
#' boxplot(Theoph,'Subject','conc')
#' boxplot(Theoph,Subject,conc)
#' boxplot(Theoph,Subject,conc, gg = T)
#' boxplot(Theoph,conc,Subject)
#' boxplot(Theoph,conc,Subject, gg = T)
#' boxplot(Theoph,conc,Subject,site)
#' boxplot(Theoph,conc,Subject,site, gg = T)
#' boxplot(Theoph,conc,Subject,site, gg = T, scales = 'free_x')
#' attr(Theoph,'title') <- 'Theophylline'
#' boxplot(Theoph, Subject, conc, main = function(x,...)attr(x,'title'))
#' boxplot(Theoph, Subject, conc, main = function(x,...)attr(x,'title'), gg = T)
#' boxplot(Theoph, Subject, conc, sub= function(x,...)attr(x,'title'))
#' boxplot(Theoph, Subject, conc, sub= function(x,...)attr(x,'title'), gg = T)
#' boxplot(Theoph %>% filter(conc > 0),Subject,conc, log = T)
#' boxplot(Theoph %>% filter(conc > 0),Subject,conc, log = T, gg = T)

boxplot.data.frame <- function(
  x,
  ...,
  fun = getOption('metaplot_box','boxplot_data_frame'),
  verbose = metOption('metaplot_verbose_boxplot',FALSE)
){
  args <- quos(...)
  args <- lapply(args,f_rhs)
  var <- args[names(args) == '']
  other <- args[names(args) != '']
  var <- sapply(var, as.character)
  ypos <- 1
  xpos <- 2
  yvar <- var[ypos]
  xvar <- var[xpos]
  more <- character(0)
  if(length(var) > xpos) more <- var[(xpos+1):length(var)]
  facets <- if(length(more)) more else NULL
  formal <- list(
    x = x,
    yvar = yvar,
    xvar = xvar,
    facets = facets
  )
  args <- c(formal, other)
  if(verbose){
    if(is.character(fun))message('calling ', fun) else message('calling fun')
  }
  do.call(fun, args)
}


