#' Boxplots
#'
#' @name boxplot
NULL

#' Boxplot Function for Data Frame
#'
#' Boxplot for data.frame. Uses standard evaluation.
#' @param x data.frame
#' @param yvar y variable
#' @param xvar x variable
#' @param facets optional conditioning variables
#' @param log whether to log transform numeric variable (auto-selected if NULL)
#' @param horizontal whether box/whisker axis should be horizontal (numeric x, categorical y); defaults TRUE if (var[[2]] is numeric
#' @param crit if log is NULL, log-transform if mean/median ratio for non-missing x is greater than this value
#' @param ref optional reference line on numeric axis
#' @param nobs whether to include the number of observations under the category label
#' @param na.rm whether to remove data points with one or more missing coordinates
#' @param ylab passed to \code{\link[lattice]{bwplot}}
#' @param xlab passed to \code{\link[lattice]{bwplot}}
#' @param aspect passed to \code{\link[lattice]{bwplot}}
#' @param main character, or a function of x, yvar, xvar, facets, and log
#' @param sub character, or a function of x, yvar, xvar, facets, and log
#' @param ... passed arguments
#' @export
#' @importFrom rlang quos UQ
#' @importFrom graphics boxplot
#' @import dplyr
#' @import lattice
#' @importFrom stats as.formula median cor loess.smooth density
#' @family mixedvariate plots
#' @family boxplot
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
  log = FALSE,
  horizontal = NULL,
  crit = 1.3,
  ref = NULL,
  nobs = FALSE,
  na.rm = TRUE,
  xlab = NULL,
  ylab = NULL,
  aspect = 1,
  main = getOption('metaplot_main',NULL),
  sub = getOption('metaplot_sub',NULL),
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
  stopifnot(all(c(cat,con) %in% names(y)))
  if(na.rm) y %<>% filter(is.defined(UQ(cat)) & is.defined(UQ(con))) # preserves attributes
  ff <- character(0)
  if(!is.null(facets))ff <- paste(facets, collapse = ' + ')
  if(!is.null(facets))ff <- paste0('|',ff)
  formula <- as.formula(yvar %>% paste(sep = '~', xvar) %>% paste(ff))

  if(!is.null(facets)){
    for (i in seq_along(facets)) y[[facets[[i]]]] <- ifcoded(y, facets[[i]])
  }

  #formula <- as.formula(paste(sep = ' ~ ', yvar, xvar))
  if(!is.numeric(y[[con]]))stop(con, ' must be numeric')
  if(is.null(log)){
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
  catlab <- axislabel(y,var = cat)
  conlab <- axislabel(y,var = con, log = log)
  if(is.null(ylab)) ylab <- if(horizontal) catlab else conlab
  if(is.null(xlab)) xlab <- if(horizontal) conlab else catlab

  guide <- attr(y[[cat]], 'guide')
  if(encoded(guide)){
    y[[cat]] <- decode(y[[cat]],encoding = guide)
    y[[cat]] <- factor(y[[cat]],levels = rev(levels(y[[cat]])))
  }
  if(!is.factor(y[[cat]])) y[[cat]] <- factor(y[[cat]],levels = unique(y[[cat]]))
  if(nobs){
    lev <- levels(y[[cat]])
    num <- sapply(lev,function(l)sum(na.rm = TRUE, y[[cat]] == l))
    levels(y[[cat]]) <- paste(lev,num,sep = '\n')
  }
  scales <- list(
    tck = c(1,0),
    x = list(log = log,equispaced.log = FALSE)
  )
  if(!horizontal)scales <- list(
    tck = c(1,0),
    y = list(log = log,equispaced.log = FALSE)
  )

  mypanel <- function(...){
      panel.bwplot(...)
    if(length(ref)){
      if(horizontal) {
        panel.abline(v = ref)
      }else{
        panel.abline(h = ref)
      }
    }
  }
  if(!is.null(main))if(is.function(main)) main <- main(x = x, yvar = yvar, xvar = xvar, facets = facets, log = log, ...)
  if(!is.null(sub))if(is.function(sub)) sub <- sub(x = x, yvar = yvar, xvar = xvar, facets = facets, log = log, ...)

  bwplot(
    formula,
    data = y,
    aspect = aspect,
    horizontal = horizontal,
    par.settings = standard.theme('pdf',color = FALSE),
    scales = scales,
    ylab = ylab,
    xlab = xlab,
    panel = mypanel,
    main = main,
    sub = sub,
    ...
  )
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
#' @family metaplot
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


