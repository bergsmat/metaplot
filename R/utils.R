#' Generic Axis Label
#'
#' Generic axis label, with method for 'data.frame'.
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @family generic functions
#' @family axislabel
axislabel <- function(x,...)UseMethod('axislabel')

#' Axis Label for Data Frame
#'
#' Axis label for data.frame.
#' @param x data.frame
#' @param var item of interest
#' @param log whether this is for a log scale
#' @param ... passed arguments
#' @keywords internal
#' @family axislabel
#' @export
#' @import magrittr
#' @return character
#'
axislabel.data.frame <- function(x, var, log = FALSE, ...){
  #x <- x[x$VARIABLE == var & is.defined(x$META),,drop = FALSE]
  #lab <- unique(x$VALUE[x$META =='LABEL'])
  #guide <- unique(x$VALUE[x$META =='GUIDE'])
  lab <- attr(x[[var]], 'label')
  guide <- attr(x[[var]], 'guide')
  res <- var
  if(length(lab) == 1)
    if(is.defined(lab))
      res <- lab
  if(length(guide) == 1)
    if(!encoded(guide))
      if(is.defined(guide))
        if(nchar(guide)){
        guide <- paste0('(',guide,')')
        res <- paste(res,guide)
      }
  if(log) res <- paste0(res,'\n(log scale)')
  res
}


#' Upper Panel Function
#'
#' Upper panel function for corsplom(). Plots data with loess smooth.
#' @param x x values
#' @param y y values
#' @param col point color
#' @param loess.col loess color
#' @param loess.lty loess line type
#' @param loess.alpha loess alpha
#' @param ... passed arguments
#' @keywords internal
#' @export
#' @family panel functions
u.p = function(
  x,
  y,
  col,
  loess.col = getOption('metaplot_loess.col',col),
  loess.lty = getOption('metaplot_loess.lty','solid'),
  loess.alpha = getOption('metaplot_loess.alpha',1),
  ...
){
  panel.xyplot(x,y,col = col, ...)
  panel.loess(x,y,col = loess.col, lty = loess.lty, alpha = loess.alpha)
}

#' Lower Panel Function
#'
#' Lower panel function for corsplom(). Plots Pearson correlation coefficient.
#' @param x x values
#' @param y y values
#' @param ... passed arguments
#' @keywords internal
#' @export
#' @family panel functions
l.p = function(x, y, ...) {
  x1 <- range(x,na.rm = T)
  y1 <- range(y,na.rm = T)
  x0 <- min(x1)+(max(x1)-min(x1))/2
  y0 <- min(y1)+(max(y1)-min(y1))/2
  panel.text(x0 ,y0, labels = paste('r =',round(cor(x,y),2) ))
}

#' Diagonal Panel Function
#'
#' Diagonal panel function for corsplom(). Plots a density smooth against the corresponding axis from within the diagonal panel.  Plots a grey pin at each axis zero.
#' @param x numeric
#' @param varname variable name
#' @param .data copy of original dataset
#' @param diag.label label for the diagonal; can be a function of x, varname, .data
#' @param pin location for a pin (reference line) in the density region; can be a function of x, varname, .data
#' @param pin.col color of pin, if any
#' @param pin.alpha alpha transparency of pin
#' @param dens.col color for density region
#' @param dens.scale inflation factor for height of density smooth
#' @param dens.alpha alpha transparency for density region
#' @param ... passed arguments
#' @keywords internal
#' @export
#' @family panel functions
#' @seealso \code{\link{corsplom}}
my.diag.panel <- function(
  x,
  varname,
  .data,
  diag.label = getOption('metaplot_diag.label',diag_label),
  pin = getOption('metaplot_pin',diag_pin),
  pin.col = getOption('metaplot_pin.col','darkgrey'),
  pin.alpha = getOption('metaplot_pin.alpha',1),
  dens.col = getOption('metaplot_dens.col','grey'),
  dens.scale = getOption('metaplot_dens.scale',0.2),
  dens.alpha = getOption('metaplot_dens.alpha',0.5),
  ...
){
  # diag.label
  # pin
  d <- density(x)
  lim <- current.panel.limits()$x
  lo <- lim[[1]]
  hi <- lim[[2]]
  len <- hi - lo

  x1 <- d$x
  y1 <- d$y
  y1 <- y1 / max(y1,na.rm = TRUE)
  y1 <- y1 * len * dens.scale
  z1 <- hi - y1
  y1 <- y1 + lo
  lpolygon(
    x = x1,
    y = z1,
    col = dens.col,
    border = NA,
    alpha = dens.alpha
  )
  lpolygon(
    y = x1,
    x = y1,
    col = dens.col,
    border = NA,
    alpha = dens.alpha
  )

  if(is.character(pin))pin <- match.fun(pin)
  if(is.function(pin)) pin <- pin(x = x, varname = varname, .data = .data, ...)
  ref <- pin
  ref <- as.numeric(ref)
  ref <- ref[is.defined(ref)]
  if(length(ref)){
    x0 = ref
    x1 = ref
    y0 = rep(hi, length(ref))
    y1 = rep(hi - len * dens.scale, length(ref))
    lsegments(y0 = y0, y1 = y1, x0 = x0, x1 = x1, col = pin.col, alpha = pin.alpha)
    y0 = ref
    y1 = ref
    x0 = rep(lo, length(ref))
    x1 = rep(lo + len * dens.scale, length(ref))
    lsegments(x0 = x0, x1 = x1, y0 = y0,y1 = y1, col = pin.col ,alpha = pin.alpha)
  }

  if(is.character(diag.label))diag.label <- match.fun(diag.label)
  if(is.function(diag.label))diag.label <- diag.label(varname = varname, .data = .data, ...)

  diag.panel.splom(varname = diag.label, ...)
}

#' Calculate Pin Placement
#'
#' Calculates pin placement in the density region, inside margin of diagonal panels.
#' @export
#' @return numeric
#' @family panel functions
#' @family reference lines
#' @param x vector of data
#' @param varname name of vector in .data
#' @param .data original dataset, possibly with column attributes such as 'reference'
#' @param ... passed arguments
diag_pin <- function(x, varname, .data, ...)metaplot_ref(x = .data, var = varname, ...)

#' Calculate Reference Values
#'
#' Calculates reference values for x and y axes.
#' @export
#' @return numeric
#' @family panel functions
#' @family reference lines
#' @param x data.frame
#' @param var name of vector in x
#' @param ... ignored
metaplot_ref <- function(x, var, ...){
  ref <- attr(x[[var]],'reference')
  if(encoded(ref)) ref <- codes(ref)
  if(is.character(ref)) ref <- as.numeric(ref)
  ref <- ref[is.defined(ref)]
  ref
}

#' Calculate Panel Reference Values
#'
#' Calculates reference values for x and y axes at the panel level.
#' @export
#' @return numeric
#' @family panel functions
#' @family reference lines
#' @param a vector of interest
#' @param b vector for other axis
#' @param ... ignored
metapanel_ref <- function(a, b, ...){
  ref <- attr(a,'reference')
  if(encoded(ref)) ref <- codes(ref)
  if(is.character(ref)) ref <- as.numeric(ref)
  ref <- ref[is.defined(ref)]
  ref
}

#' Format a Diagonal Label
#'
#' Formats a diagonal label. Can return a simple column name, a column label (if attribute defined), a fractured column label (split on spaces), or a processed symbol (over-rides label).
#' @export
#' @return character
#' @family panel functions
#' @family formatters
#' @param varname character
#' @param .data data.frame
#' @param diag_label_simple logical: just return varname?
#' @param diag_label_split whether to substitute line breaks for spaces
#' @param diag_symbol_format function to process symbol attribute, if present
#' @param ... ignored
#'
diag_label <- function(varname, .data,
diag_label_simple = getOption('metaplot_diag_label_simple',FALSE),
diag_label_split = getOption('metaplot_diag_label_split',TRUE),
diag_symbol_format = getOption('metaplot_diag_symbol_format','wikisym2plotmath'),
...){
  stopifnot(length(varname) == 1)
  stopifnot(is.data.frame(.data))
  if(diag_label_simple) return(varname)
  label <- attr(.data[[varname]],'label')
  if(is.null(label))label <- ''
  label <- as.character(label)
  stopifnot(length(label) <= 1)
  i <- is.defined(label) & label != ''
  if(i)if(diag_label_split) label <- fracture(label)
  # best label complete
  symbol <- attr(.data[[varname]],'symbol')
  #if(is.null(symbol)) symbol <- ''
  symbol <- as.character(symbol)
  stopifnot(length(symbol) <= 1)
  symbol <- tryCatch(match.fun(diag_symbol_format)(symbol),error=function(e)symbol)
  # best symbol complete
  result <- varname
  if(length(label) & !is.na(label) & label != '') result <- label
  if(length(symbol)) result <- symbol
  result
}

is.defined <- function(x)!is.na(x)
parens <- function (x, ...)paste0("(", x, ")")
fracture <- function(x,sep='\n')gsub('\\s+',sep,x)

#' Convert Wiki Symbol to Plotmath
#'
#' Converts wiki symbol to plotmath.  A Wiki symbol is simple text with arbitrarily nested subscript (\code{_}) and superscript (\code{^}) groupings.  Use dot (\code{.}) to explicitly terminate a grouping, and use backslash-dot (\code{\.}) for a literal dot.  Examples: \code{V_c./F}. Trailing dots need not be supplied. Leading/trailing whitespace is removed. Tab character not allowed.
#'
#' @export
#' @return expression
#' @family formatters
#' @param x character
#' @param ... ignored
#' @importFrom dplyr recode
#' @examples
#' wikisym2plotmath('V_c./F')
#' wikisym2plotmath('AUC_ss')
#' wikisym2plotmath('C_max_ss')
#' wikisym2plotmath('var^\\eta_j')
wikisym2plotmath <- function(x,...){
  sapply(x, wikisym2plotmathOne,...)
}

wikisym2plotmathOne <- function(x,...){
  stopifnot(length(x) == 1)
  if(grepl('\t',x)) stop('tab character not allowed in wikisym')
  x <- sub('^\\s+','',x) # strip leading whitespace
  x <- sub('\\s+$','',x) # strip trailing whitespace
  x <- gsub('\\.', '\t',x,fixed = TRUE) # store literal dot as single character
  x <- strsplit(x,'')[[1]] # tokenize
  y <- character(0) # result accumulator
  b <- character(0) # closer stack
  while(length(x)){
    c <- x[1]
    x <- x[-1]
    t <- c # default
    if(c == '_'){
      t <- '['  # subscript initiator
      b <- append(b,']') # subscript closer
    }
    if(c == '^'){
      t <- '^{' # superscript initiator
      b <- append(b,'}') # superscript closer
    }
    if(c == '.'){
      t <- b[length(b)]
      b <- b[-length(b)] # drop from stack
    }
    if(c == '\t') t <- '.'  # literal dot

    # accumulate
    y <- paste0(y,t)
  }

  # all characters handled
  # empty closer stack
  b <- paste(b,collapse = '')
  y <- paste0(y,b)
  y <- as.expression(y)
  y
}

#' Execute Linear Model
#'
#' Executes a linear model, automatically choosing binomial family as necessary.
#' @param x x values
#' @param y y values
#' @param family gaussian by default, or binomial for all y either zero or 1
#' @param ... passed to \code{\link[stats]{glm}}
#' @return glm
#' @family regression functions
model <- function(x, y, family = if(all(y %in% 0:1,na.rm = TRUE)) 'binomial' else 'gaussian', ...){
  d <- data.frame(x=x,y=y)
  d <- d[order(d$x),]
  m <- glm(y~x,data=d,family=family) # elipses passed to glm.control, which chokes on unknown arguments, so not passing here.
  m
}

#' Calculate a Confidence Region
#'
#' Calculates a confidence region. \code{se.fit} from \code{\link[stats]{predict.glm}} is multiplied by \code{z} and added or subtracted from fits to give \code{hi} and \code{lo} columns in return value.  \code{z} is normal quantile for the one-tailed probablitity corresponding to \code{conf}, e.g. ~ 1.96 for \code{conf = 0.95}. If non-missing \code{y} is only 0 or 1, the model family is binomial and resulting confidence intervals are back-transformed using \code{\link[stats]{plogis}}.
#' @param x x values
#' @param y y values
#' @param family gaussian by default, or binomial for all y either zero or 1
#' @param length.out number of prediction points
#' @param conf width of confidence interval; logical TRUE defaults to 0.95
#' @importFrom stats qnorm predict glm plogis
#' @param ... passed to \code{\link{model}}
#' @return data.frame with x, y, hi, lo at 1000 points
#' @family regression functions
#' @seealso \url{https://stackoverflow.com/questions/14423325/confidence-intervals-for-predictions-from-logistic-regression}
#' @seealso \url{http://www.rnr.lsu.edu/bret/BretWebSiteDocs/GLMCI.pdf}
#' @seealso \url{https://stat.ethz.ch/pipermail/r-help/2010-September/254465.html}
#' @seealso \url{http://r.789695.n4.nabble.com/Confidence-Intervals-for-logistic-regression-td2315932.html}
region <- function(x, y, family = if(all(y %in% 0:1,na.rm = TRUE)) 'binomial' else 'gaussian', length.out = 1000, conf = 0.95, ...){
  if(is.logical(conf)){
    if(conf){
      conf <- 0.95
    } else{
      conf <- 0
    }
  }
  stopifnot(length(conf) == 1, is.numeric(conf), conf < 1, conf >= 0)
  tail <- 1 - conf  # e.g. 0.95 -> 0.05
  upper <- tail/2   # e.g. 0.025
  prob <- 1 - upper # e.g. 0.975
  z <- qnorm(prob)  # e.g. 1.96
  m <- model(x = x, y = y, family = family, ...)
  j <- seq(from=min(x),to=max(x),length.out=1000)
  f <- predict(m, se.fit=TRUE, newdata=data.frame(x=j),type='link')
  f <- data.frame(x=j,y=f$fit, se = f$se.fit)
  f$lo <- f$y - z * f$se
  f$hi <- f$y + z * f$se
  if(family=='binomial'){ # back-transform
    f <- within(f, y  <- plogis(y ))
    f <- within(f, lo <- plogis(lo))
    f <- within(f, hi <- plogis(hi))
  }
  f
}

#' Format GLM Statistics
#'
#' Formats GLM statistics.
#' @export
#' @param x x values
#' @param y y values
#' @param family regression family
#' @param ... other arguments
#' @importFrom stats coef glm plogis qnorm predict
#' @return character
#' @family regression functions
#' @seealso \code{\link{metapanel}}
#'
metastats <- function(x, y, family = if(all(y %in% 0:1,na.rm = TRUE)) 'binomial' else 'gaussian', ...){
  n <- paste('n =', length(x))
  m <- model(x, y, family, ...)
  p <- coef(summary(m))[,4]['x'] %>% signif(3)
  p <- paste('p =', p)
  r <- cor(x,y) %>% signif(3)
  r <- paste('r =',r)
  t <- paste(n,p,if(family=='gaussian') r else NULL,sep='\n')
  t
}

ifcoded <- function(x, var){
  guide <- attr(x[[var]],'guide')
  if(!encoded(guide)) return(x[[var]])
  decoded <- decode(x[[var]], encoding = guide)
  if(!any(is.na(decoded))) return(decoded)
  if(all(is.na(decoded)))return(decode(x[[var]]))
  x[[var]]
}
