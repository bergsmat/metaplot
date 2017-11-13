#' Boxplots
#'
#' @name boxplot
NULL

#' Boxplot for Data Frame
#'
#' Boxplot for data.frame.
#' @param x data.frame
#' @param .y y axis item
#' @param .x x axis item
#' @param log whether to log transform continuous variable
#' @param horizontal whether box/whisker axis should be horizontal
#' @param main logical:whether to include title indicating x and y items; or a substitute title or NULL
#' @param crit if log is missing, log transform if mean/median ratio for non-missing x  is greater than this value
#' @param ref optional reference line on continuous axis
#' @param guide optional encoding for categories see \code{encode::encode}
#' @param nobs whether to include the number of observations under the category label
#' @param na.rm whether to remove data points with one or more missing coordinates
#' @param ... passed arguments
#' @export
#' @family bivariate functions
#' @rdname boxplot
boxplot.data.frame <- function(
  x,
  .y,
  .x,
  log,
  horizontal = TRUE,
  main = FALSE,
  crit = 1.3,
  ref = NULL,
  guide = NA_character_,
  nobs = FALSE,
  na.rm = TRUE,
  ...
){
  stopifnot(
    length(.x) == 1,
    length(.y) == 1
  )
  y <- x # %>% unfold(c(.y,.x))
  stopifnot(all(c(.x,.y) %in% names(y)))
  names(y)[names(y) ==.y] <- 'y_'
  names(y)[names(y) ==.x] <- 'x_'
  if(na.rm){
    bad <- is.na(y$y_) | is.na(y$x_)
    y <- y[!bad,,drop = FALSE]
  }
  formula <- 'y_ ~ x_'
  formula <- as.formula(formula)
  con <- if(horizontal) 'x_' else 'y_'
  cat <- if(horizontal) 'y_' else 'x_'
  if(missing(log)) log <- mean(y[[con]],na.rm = T)/median(y[[con]],na.rm = T) > crit
  if(any(y[[con]] <= 0,na.rm = T)) log <- FALSE
  #ylab <- axislabel(x,.y, log = FALSE )
  #xlab <- axislabel(x,.x, log = log )
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

  # if(is.null(ref)) if(ymeta$TYPEC %in% c('IIV','RESIDUAL') ) ref <- 0
  scales <- list(
    tck = c(1,0),
    x = list(log = log,equispaced.log = FALSE)
  )
  if(!horizontal)scales <- list(
    tck = c(1,0),
    y = list(log = log,equispaced.log = FALSE)
  )
  mn <- paste(sep = ' ~ ',.y,.x)
  mn <- list(mn, cex = 1, fontface = 1)
  if(is.logical(main)){
    if(main){
      main <- mn
    } else{
      main <- NULL
    }
  }
  bwplot(
    formula,
    data = y,
    aspect = 1,
    horizontal = horizontal,
    main = main,
    par.settings = standard.theme('pdf',color = FALSE),
    scales = scales,
    panel = function(...){
      panel.bwplot(...)
      if(length(ref)){
        if(horizontal) {
          panel.abline(v = ref)
        }else{
          panel.abline(h = ref)
        }
      }
    },
    ...
  )
}

#' Boxplot for Folded
#'
#' Boxplot for folded.
#' @import encode
#' @export
#' @family bivariate plots
#' @rdname boxplot
boxplot.folded <- function(
  x,
  .y,
  .x,
  log,
  horizontal,
  main = FALSE,
  crit = 1.3,
  ref = NULL,
  na.rm = TRUE,
  ...
){
  stopifnot(
    length(.x) == 1,
    length(.y) == 1
  )
  y <- x %>% unfold(UQS(c(.y,.x)))
  stopifnot(all(c(.x,.y) %in% names(y)))
  if(missing(horizontal)) horizontal <- continuous(x, .x)
  con <- if(horizontal) .x else .y
  cat <- if(horizontal) .y else .x
  if(missing(log)) log <- mean(y[[con]],na.rm = T)/median(y[[con]],na.rm = T) > crit
  if(any(y[[con]] <= 0,na.rm = T)) log <- FALSE
  ylab <- axislabel(x,.y, log = if(horizontal) FALSE else log )
  xlab <- axislabel(x,.x, log = if(horizontal) log else FALSE )
  guide <- guide(x,cat)
  boxplot(
    y,
    .y = .y,
    .x = .x,
    log = log,
    main = main,
    crit = crit,
    ref = ref,
    ylab = ylab,
    xlab = xlab,
    guide = guide,
    horizontal = horizontal,
    na.rm = na.rm,
    ...
  )
}
