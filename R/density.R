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
#' @param ref reference line; can be function(x = x, var = xvar, ...)
#' @param ref.col color for reference line(s)
#' @param ref.lty type for reference line(s)
#' @param ref.alpha transparency for reference line(s)
#' @param log whether to log-transform x axis (auto-selected if NA)
#' @param crit if log is NA, log-transform if mean/median ratio for non-missing x is greater than this value (and no negative values)
#' @param aspect passed to \code{\link[lattice]{densityplot}}
#' @param scales  passed to \code{\link[lattice]{densityplot}}
#' @param panel  passed to \code{\link[lattice]{densityplot}}
#' @param key location of key (right, left, top, bottom) or something to pass to \code{\link[lattice]{xyplot}} (auto.key) or \code{\link[ggplot2]{theme}} as \code{legend.postion}
#' @param main character, or a function of x, xvar, groups, facets, and log
#' @param sub character, or a function of x, xvar, groups, facets, and log
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
#' densplot_data_frame(Theoph, 'conc', , 'Subject')
densplot_data_frame<- function(
  x,
  xvar,
  groups = NULL,
  facets = NULL,
  xlab = getOption('metaplot_lab',axislabel),
  ref = getOption('metaplot_ref',metaplot_ref),
  ref.col = getOption('metaplot_ref.col','grey'),
  ref.lty = getOption('metaplot_ref.lty','solid'),
  ref.alpha = getOption('metaplot_ref.alpha',1),
  log = getOption('metaplot_log',FALSE),
  crit = getOption('metaplot_crit',1.3),
  aspect = getOption('metaplot_aspect',1),
  scales = getOption('metaplot_dens.scales',NULL),
  panel = getOption('metaplot_dens.panel',dens_panel),
  key = getOption('metaplot_key','right'),
  main = getOption('metaplot_main',NULL),
  sub = getOption('metaplot_sub',NULL),
  gg = getOption('metaplot_gg',FALSE),
  ...
){
  stopifnot(inherits(x, 'data.frame'))
  stopifnot(length(xvar) == 1)
  stopifnot(is.character(xvar))
  if(is.null(log))log <- FALSE # same as default
  if(is.na(log)){
    if(any(x[[xvar]] <= 0, na.rm = TRUE)){
      log <- FALSE
    } else{
      log <- mean(x[[xvar]],na.rm = TRUE)/median(x[[xvar]],na.rm = TRUE) > crit
    }
  }
  if(log)if(any(x[[xvar]] <= 0, na.rm = TRUE)){
    warning(xvar,' must be positive for log scale')
    log <- FALSE
  }
  if(is.null(scales)) scales <- list(tck = c(1,0),x = list(log = log,equispaced.log = FALSE))
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

  #if(is.null(keycols))if(!is.null(groups))keycols <- min(3, length(unique(x[[groups]])))
  if(is.character(key))if(length(key == 1))if(key %in% c('left','right','top','bottom'))if(!gg)key <- list(space = key)
  #if(is.null(auto.key))if(!is.null(groups))if(length(unique(x[[groups]])) > 1) auto.key = list(columns = keycols,lines=TRUE)

  ff <- character(0)
  if(!is.null(facets))ff <- paste(facets, collapse = ' + ')
  if(!is.null(facets))ff <- paste0('|',ff)
  formula <- as.formula(paste0('~', xvar) %>% paste(ff))
  if(!is.null(facets)){
    for (i in seq_along(facets)) x[[facets[[i]]]] <- as_factor(x[[ facets[[i]] ]])
  }
  if(!is.null(main))if(is.function(main)) main <- main(x = x, xvar = xvar, groups = groups, facets = facets, log = log, ...)
  if(!is.null(sub))if(is.function(sub)) sub <- sub(x = x, xvar = xvar, groups = groups, facets = facets, log = log, ...)
  if(!is.null(groups)) {
    x[[groups]] <- as_factor(x[[groups]])
    if(!gg) groups <- as.formula(paste('~',groups))
  }

if(gg){
  # https://stackoverflow.com/questions/14255533/pretty-ticks-for-log-normal-scale-using-ggplot2-dynamic-not-manual
  base_breaks <- function(n = 10){
    function(x) {
      axisTicks(log(range(x, na.rm = TRUE)), log = TRUE, n = n)
    }
  }
  plot <- ggplot(data = x) +
  geom_density(mapping = aes_string(x = xvar,color = groups)) +
  xlab(xlab) +
  ggtitle(main, subtitle = sub)
  if(length(ref)) plot <- plot +
  geom_vline(
    xintercept = ref,
    color = ref.col,
    linetype = ref.lty,
    alpha = ref.alpha
  )
  plot <- plot +
  theme(aspect.ratio = aspect, legend.position = key)

 if(log) plot <- plot + scale_x_continuous(
   trans = log_trans(),
   breaks = base_breaks()
 )
  if(length(facets) == 1) plot <- plot + facet_wrap(facets[[1]])
  if(length(facets) >  1) plot <- plot +
    facet_grid(
      as.formula(
        paste(
          sep='~',
          facets[[1]],
          facets[[2]]
        )
      )
    )
  return(plot)
}

densityplot(
  formula,
  data = x,
  groups = groups,
  xlab = xlab,
  ref = ref,
  ref.col = ref.col,
  ref.lty = ref.lty,
  ref.alpha = ref.alpha,
  log = log,
  aspect = aspect,
  scales = scales,
  panel = panel,
  auto.key = key,
  main = main,
  sub = sub,
  ...
)
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
#' @param ref.alpha passed to \code{\link[lattice]{panel.abline}} as alpha
dens_panel <- function(ref = NULL, ref.col, ref.lty, ref.alpha, ...){
  panel.densityplot(...)
  if(length(ref))panel.abline(v = ref, col=ref.col, lty = ref.lty, alpha = ref.alpha)
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
#' @family methods
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

