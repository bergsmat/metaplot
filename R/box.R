#' Boxplots
#'
#' @name boxplot
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
#' @param horizontal whether box/whisker axis should be horizontal (numeric x, categorical y); defaults TRUE if (var[[2]] is numeric
#' @param scales passed to \code{\link[lattice]{xyplot}}; can be function(x = x, horizontal, log,...)
#' @param panel panel function
#' @param ref optional reference line(s) on numeric axis; can be function(x = x, var = con, ...)
#' @param ref.col color for reference line(s)
#' @param ref.lty line type for reference line(s)
#' @param ref.alpha transparency for reference line(s)
#' @param nobs whether to include the number of observations under the category label
#' @param na.rm whether to remove data points with one or more missing coordinates
#' @param ylab y axis label
#' @param xlab x axis label
#' @param numlab numeric axis label; can be function(x = x, var = numvar, log = ylog, ...)
#' @param catlab categorical axis label; can be function(x = x, var = catvar, ...)
#' @param aspect passed to \code{\link[lattice]{bwplot}}
#' @param main character, or a function of x, yvar, xvar, facets, and log
#' @param sub character, or a function of x, yvar, xvar, facets, and log
#' @param par.settings default parameter settings
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
  log = getOption('metaplot_log',FALSE),
  crit = getOption('metaplot_crit',1.3),
  horizontal = getOption('metaplot_horizontal',NULL),
  scales = getOption('metaplot_boxplot_scales',NULL),
  panel = getOption('metaplot_boxplot_panel',boxplot_panel),
  ref = getOption('metaplot_ref',metaplot_ref),
  ref.col = getOption('metaplot_ref.col','grey'),
  ref.lty = getOption('metaplot_ref.lty','solid'),
  ref.alpha = getOption('metaplot_ref.alpha',1),
  nobs = getOption('metaplot_nobs',FALSE),
  na.rm = getOption('metaplot_na.rm',TRUE),
  xlab = NULL,
  ylab = NULL,
  numlab = getOption('metaplot_lab',axislabel),
  catlab = getOption('metaplot_lab',axislabel),
  aspect = getOption('metaplot_aspect',1),
  main = getOption('metaplot_main',NULL),
  sub = getOption('metaplot_sub',NULL),
  par.settings = standard.theme('pdf',color = FALSE),
  reverse = getOption('metaplot_boxplot_reverse',TRUE),
  pch = getOption('metaplot_boxplot_pch','|'),
  notch = getOption('metaplot_boxplot_notch',FALSE),
  gg = getOption('metaplot_gg',FALSE),
  ...
){
  stopifnot(inherits(x, 'data.frame'))
  stopifnot(is.character(xvar))
  stopifnot(is.character(yvar))
  if(!is.null(facets))stopifnot(is.character(facets))
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
  if(log)if(any(y[[con]] <= 0, na.rm = TRUE)){
    warning(con, ' must be positive for log scale')
    log <- FALSE
  }
  if(log){
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
  if(is.null(scales)) scales <- function(x = y, horizontal = horizontal, log = log, ...){
    s <- list(
      tck = c(1,0),
      x = list(log = log,equispaced.log = FALSE)
    )
    if(!horizontal)s <- list(
      tck = c(1,0),
      y = list(log = log,equispaced.log = FALSE)
    )
    s
  }
  if(is.character(scales)) scales <- match.fun(scales)
  if(!is.function(scales)) stop('scales must be NULL, or function(x, horizontal, log,...), or the name of a function')
  scales <- scales(x=x, horizontal = horizontal, log = log, ...)

  if(!is.null(main))if(is.function(main)) main <- main(x = x, yvar = yvar, xvar = xvar, facets = facets, log = log, ...)
  if(!is.null(sub))if(is.function(sub)) sub <- sub(x = x, yvar = yvar, xvar = xvar, facets = facets, log = log, ...)

  if(gg)return(ggplot())
  bwplot(
    formula,
    data = y,
    aspect = aspect,
    horizontal = horizontal,
    par.settings = par.settings,
    scales = scales,
    ylab = ylab,
    xlab = xlab,
    main = main,
    sub = sub,
    panel = panel,
    ref = ref,
    ref.col = ref.col,
    ref.lty = ref.lty,
    ref.alpha = ref.alpha,
    pch = pch,
    notch = notch,
    ...
  )
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
#' @param ref.alpha passed to \code{\link[lattice]{panel.abline}} as alpha
#'
boxplot_panel <- function(ref = NULL, horizontal,pch = '|',notch=FALSE, ref.col, ref.lty, ref.alpha, ...){
  panel.bwplot(horizontal = horizontal, pch = pch, notch = notch, ...)
  if(length(ref)){
    if(horizontal) {
      panel.abline(v = ref, col = ref.col, lty = ref.lty, alpha = ref.alpha)
    }else{
      panel.abline(h = ref, col = ref.col, lty = ref.lty, alpha = ref.alpha)
    }
  }
}

#' Boxplot Method for Data Frame
#'
#' Boxplot for data.frame.  Parses arguments and generates the call: fun(x, yvar, xvar, facets, ...).
#' @param x data.frame
#' @param ... passed to fun
#' @param fun function that does the actual plotting
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
#' boxplot(Theoph,conc,Subject)
#' boxplot(Theoph,conc,Subject,site)
#' attr(Theoph,'title') <- 'Theophylline'
#' boxplot(Theoph, Subject, conc, main = function(x,...)attr(x,'title'))
#' boxplot(Theoph, Subject, conc, sub= function(x,...)attr(x,'title'))
boxplot.data.frame <- function(
  x,
  ...,
  fun = getOption('metaplot_box','boxplot_data_frame')
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
  do.call(fun, args)
}


