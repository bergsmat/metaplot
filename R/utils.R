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
#' Axis label for data.frame. Substitutes label attribute if present for column name, and puts units if present in parentheses, trailing.  Puts 'log scale' in parentheses on a new line if log is TRUE.
#' @param x data.frame
#' @param var item of interest
#' @param log whether this is for a log scale
#' @param ... passed arguments
#' @keywords internal
#' @family axislabel
#' @family methods
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


#' Scatter Panel Function for Metaplot Corsplom
#'
#' Default upper panel function for corsplom_data_frame. Plots data with loess smooth.
#' @param x x values
#' @param y y values
#' @param col point color
#' @param smooth.col smooth color
#' @param smooth.lty smooth line type
#' @param smooth.lwd smooth line size
#' @param smooth.alpha smooth alpha
#' @param verbose generate messages describing process
#' @param ... passed arguments
#' @keywords internal
#' @export
#' @family panel functions
#' @family corsplom
corsplom_panel_scatter = function(
  x,
  y,
  col = metOption('point_col_corsplom_panel','#0080ff'),
  smooth.col = metOption('smooth_col_corsplom_panel',col),
  smooth.lty = metOption('smooth_lty_corspom_panel','solid'),
  smooth.lwd = metOption('smooth_lwd_corspom_panel',1),
  smooth.alpha = metOption('smooth_alpha_corsplom_panel',1),
  verbose = metOption('verbose_corsplom_panel'),
  ...
){
  if(verbose)cat('this is corsplom_panel_scatter calling panel.xyplot')
  panel.xyplot(x,y,col = col, ...)
  try(silent = TRUE, suppressWarnings(panel.loess(x,y,col = smooth.col, lty = smooth.lty, lwd = smooth.lwd, alpha = smooth.alpha)))
}

#' Correlation Panel Function for Metaplot Corsplom
#'
#' Default lower panel function for corsplom_data_frame. Plots Pearson correlation coefficient.
#' @param x x values
#' @param y y values
#' @param use passed to \code{\link[stats]{cor}}
#' @param verbose generate messages describing process
#' @param ... passed arguments
#' @keywords internal
#' @export
#' @family panel functions
#' @family corsplom
corsplom_panel_correlation = function(x, y, use = 'pairwise.complete.obs', verbose = FALSE,...) {
  if(verbose)cat('this is corsplom_panel_correlation calling panel.text')
  x1 <- range(x,na.rm = T)
  y1 <- range(y,na.rm = T)
  x0 <- min(x1)+(max(x1)-min(x1))/2
  y0 <- min(y1)+(max(y1)-min(y1))/2
  stat <- try(silent = TRUE, round(cor(x,y, use = use), 3))
  if(class(stat) == 'try-error') stat <- ''
  panel.text(x0 ,y0, labels = paste('r =',round(cor(x,y,use = use),3) ))
}

#' Diagonal Panel Function for Metaplot Corsplom
#'
#' Default diagonal panel function for corsplom_data_frame. Plots a density smooth against the corresponding axis from within the diagonal panel.  Plots a grey pin at each axis zero.
#' @param x numeric
#' @param varname variable name
#' @param .data copy of original dataset
#' @param diag.label label for the diagonal; can be a function of x, varname, .data
#' @param pin location for a pin (reference line) in the density region; can be a function of x, varname, .data
#' @param pin.col color of pin, if any
#' @param pin.alpha alpha transparency of pin
#' @param density whether to plot density polygons
#' @param dens.col color for density region
#' @param dens.scale inflation factor for height of density smooth
#' @param dens.alpha alpha transparency for density region
#' @param as.table diagonal arranged top-left to bottom-right
#' @param dens.up whether density plots should face the upper triangle (or lower, if FALSE)
#' @param verbose generate messages describing process
#' @param ... passed arguments
#' @keywords internal
#' @export
#' @family panel functions
#' @seealso \code{\link{corsplom}}
corsplom_panel_diagonal <- function(
  x,
  varname,
  .data,
  density = TRUE,
  diag.label = metOption('diag_label_corsplom_panel',diag_label),
  pin = metOption('pin_loc_corsplom_panel',diag_pin),
  pin.col = metOption('pin_col_corsplom_panel','darkgrey'),
  pin.alpha = metOption('pin_alpha_corsplom_panel',1),
  dens.col = metOption('dens_col_corsplom_panel','grey'),
  dens.scale = metOption('dens_scale_corsplom_panel',0.2),
  dens.alpha = metOption('dens_alpha_corsplom_panel',0.5),
  as.table = metOption('as.table_corsplom_panel', FALSE),
  dens.up = metOption('densup_corsplom_panel',TRUE),
  verbose = metOption('verbose_corsplom_panel',FALSE),
  ...
){
  if(verbose)cat('this is corsplom_panel_diagonal')
  #as.table <- as_table
  i <- match(varname,names(.data))
  ncol <- length(names(.data))

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

  mask <- c(top, right, bottom, left)
  names(mask) <- c('top','right','bottom','left')
  stopifnot(is.logical(density))
  density <- rep(density, length.out = 4)
  names(density) <- c('top','right','bottom','left')

  density <- mask & density

  #x <- as.character(mapping$x)
  #data$x <- data[[x]]
  #lim <- range(data$x,na.rm = TRUE)
  lim <- current.panel.limits()$x
  lo <- lim[[1]]
  hi <- lim[[2]]
  len <- hi - lo
  me <- hi/2 + lo/2
  d <- density(x,na.rm=TRUE, from = lo, to = hi)
  #d <- density(x,na.rm=TRUE)
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

  if(density[['top']])lpolygon(
    x = top$x1,
    y = top$y1,
    col = dens.col,
    border = NA,
    alpha = dens.alpha
  )
  if(density[['right']])lpolygon(
    x = right$x1,
    y = right$y1,
    col = dens.col,
    border = NA,
    alpha = dens.alpha
  )
  if(density[['bottom']])lpolygon(
    x = bottom$x1,
    y = bottom$y1,
    col = dens.col,
    border = NA,
    alpha = dens.alpha
  )
  if(density[['left']])lpolygon(
    x = left$x1,
    y = left$y1,
    col = dens.col,
    border = NA,
    alpha = dens.alpha
  )

  if(is.character(pin))pin <- match.fun(pin)
  if(is.function(pin)) pin <- pin(x = x, varname = varname, .data = .data, ...)
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

    # x0 = ref
    # x1 = ref
    # y0 = rep(hi, length(ref))
    # y1 = rep(hi - len * dens.scale, length(ref))
    if(density[['top']])lsegments(
      y0 = top$y0,
      y1 = top$y1,
      x0 = top$x0,
      x1 = top$x1,
      col = pin.col,
      alpha = pin.alpha
    )
    if(density[['right']])lsegments(
      y0 = right$y0,
      y1 = right$y1,
      x0 = right$x0,
      x1 = right$x1,
      col = pin.col,
      alpha = pin.alpha
    )
    if(density[['bottom']])lsegments(
      y0 = bottom$y0,
      y1 = bottom$y1,
      x0 = bottom$x0,
      x1 = bottom$x1,
      col = pin.col,
      alpha = pin.alpha
    )
    if(density[['left']])lsegments(
      y0 = left$y0,
      y1 = left$y1,
      x0 = left$x0,
      x1 = left$x1,
      col = pin.col,
      alpha = pin.alpha
    )
  }
  if(is.character(diag.label))diag.label <- match.fun(diag.label)
  if(is.function(diag.label))diag.label <- diag.label(varname = varname, .data = .data, ...)
  if(verbose)cat('calling diag.panel.splom')
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
#' Calculates reference values for x and y axes.  Coerces column attribute 'reference' to numeric: a single value or an encoding giving multiple numeric values (decodes are ignored).
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
  if(length(ref))ref <- ref[is.defined(ref)]
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
scatter_panel_ref <- function(a, b, ...){
  ref <- attr(a,'reference')
  if(encoded(ref)) ref <- codes(ref)
  if(is.character(ref)) ref <- as.numeric(ref)
  if(length(ref))ref <- ref[is.defined(ref)]
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
#' @param verbose generate messages describing process
#' @param ... ignored
#'
diag_label <- function(
  varname, .data,
  diag_label_simple = metOption('diag_label_simple',FALSE),
  diag_label_split = metOption('diag_label_split',TRUE),
  diag_symbol_format = metOption('diag_symbol_format','wikisym2plotmath'),
  verbose = metOption('verbose_diag_label', FALSE),
  ...
){
  if(verbose)cat('this is diag_label')
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
#' Converts wiki symbol to plotmath.  Vectorized version of \code{\link{wikisym2plotmath_}}.
#'
#' @export
#' @return expression
#' @family formatters
#' @param x character
#' @param ... ignored
wikisym2plotmath <- function(x,...){
  sapply(x, wikisym2plotmath_,...)
}

#' Convert One Wiki Symbol to Plotmath
#'
#' Converts one wiki symbol to plotmath.  A Wiki symbol is simple text with arbitrarily nested subscript (\code{_}) and superscript (\code{^}) groupings.  Use dot (\code{.}) to explicitly terminate a grouping, and use backslash-dot (\code{\.}) for a literal dot.  Examples: \code{V_c./F}. Trailing dots need not be supplied. Leading/trailing whitespace is removed. Tab character not allowed.
#'
#' @export
#' @return expression
#' @family formatters
#' @param x character
#' @param ... ignored
#' @aliases wikisym wikisymbol
#' @examples
#' wikisym2plotmath_('V_c./F')
#' wikisym2plotmath_('AUC_ss')
#' wikisym2plotmath_('C_max_ss')
#' wikisym2plotmath_('var^eta_j')
wikisym2plotmath_ <- function(x,...){
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
  b <- paste(rev(b),collapse = '')
  y <- paste0(y,b)
  y <- parse(text = y)
  y
}

#' Execute Linear Model
#'
#' Executes a linear model, automatically choosing binomial family as necessary.
#' @export
#' @keywords internal
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
#' @export
#' @keywords internal
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
#' Formats GLM statistics. Uses a gaussian family by default, or binomial family if all y are 0 or 1, to fit a general linear model.  Formats number of observations, p-value, and Pearson correlation coefficient into a string for printing.
#'
#' @export
#' @param x x values
#' @param y y values
#' @param family regression family
#' @param ... other arguments
#' @importFrom stats coef glm plogis qnorm predict
#' @return character
#' @family regression functions
#' @seealso \code{\link{scatter_panel}}
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

#' Coerce to Factor using Encoding if Present
#'
#' Coerces to factor, blending levels with encoding, if present. Vectors without encodings (or with empty encodings) acquire levels equal to \code{unique(x)} (notice that storage order controls presentation order). Vectors with non-empty encodings are decoded after harmonizing the encoding and the actual data. Factors with encodings defer to order and display value of the encoding as much as possible.  Missing levels are supplied.  Unused levels are removed. Other attributes beside 'class' and 'levels' are preserved.
#'
#' @export
#' @param x vector or factor
#' @return factor
#' @examples
#' library(magrittr)
#' foo <- c(1, 2, NA, 4, 5)
#' as_factor(foo)
#' as_factor(factor(foo))
#' as_factor(as.factor(foo))
#' as_factor(structure(foo, guide = '....'))
#' as_factor(structure(foo, guide = '//5//'))
#' as_factor(structure(foo, guide = '//5/bar//'))
#' as_factor(structure(foo, guide = '//5/bar//6/baz//'))
#' as_factor(structure(factor(foo), guide = '//5/bar//'))
#' as_factor(structure(factor(foo), guide = '//5/bar//')) %>% sort
#' as_factor(structure(factor(foo), guide = '....'))
#' as_factor(structure(factor(foo), guide = '//1/bar//5/bar//'))
#'
#'
as_factor <- function(x){
  at <- attributes(x)
  at[['guide']] <- NULL
  at[['class']] <- NULL
  at[['levels']] <- NULL
  guide <- attr(x,'guide') # may be NULL (not encoded)
  vals <- if(is.factor(x)) levels(x) else unique(x)
  vals <- vals[!is.na(vals)]
  if(is.null(guide)) guide <- encode(vals) # guide present
  if(!encoded(guide)) guide <- encode(vals) # guide encoded
  if(!length(decodes(guide))) guide <- encode(vals) # guide non-empty
  codes <- codes(guide)
  decodes <- decodes(guide)
  decodes[is.na(decodes)] <- codes[is.na(decodes)] # decodes fully defined
  extra <- setdiff(vals, codes) # values not captured by encoding
  codes <- c(codes, extra) # all possible values now recognized ...
  decodes <- c(decodes, extra) # ... and displayed as themselves
  encoding <- encode(codes, labels = decodes)
  x <- as.character(x)
  x <- decode(x, encoding = encoding)
  #x <- factor(x)
  for(a in names(at))attr(x,a) <- at[[a]]
  x
}

lattice_padding <- function()list(
  axis.components = list(
    left = list(
      pad2 = 1 # 2
    ),
    top = list(
      pad1 = 0, # 1,
      pad2 = 0  # 2
    ),
    right = list(
      pad1 = 0, # 1,
      pad2 = 0 # 2
    ),
    bottom = list(
      pad1 = 0.5, # 1,
      pad2 = 0  # 1
    )
  ),
  layout.heights = list(
    top.padding = .5, # 1,
    main.key.padding = .5, # 1,
    key.sub.padding = 0, # 1
    bottom.padding = 0.5 # 1
  ),
  layout.widths = list(
    left.padding = .5, # 1,
    ylab.axis.padding = 0, # 1,
    axis.key.padding = 0 #, # 1,
  )
)

parintegrate <- function(par.settings, padding){
  # res <- list()
  # if(!is.null(par.settings)) res <- par.settings
  stopifnot(length(padding) == 4)
  merge.list(
    par.settings,
    list(
      layout.heights = list(
        top.padding <- padding[[1]],
        bottom.padding <- padding[[3]]
      ),
      layout.widths = list(
        right.padding <- padding[[2]],
        left.padding <- padding[[4]]
      )
    )
  )
}

# https://stackoverflow.com/questions/14255533/pretty-ticks-for-log-normal-scale-using-ggplot2-dynamic-not-manual
base_breaks <- function(n = 10){
  function(x) {
    axisTicks(log(range(x, na.rm = TRUE)), log = TRUE, n = n)
  }
}
#' Get Metaplot Option with Partial Matching
#'
#' Gets a metaplot option value from  the named list \code{getOption('metaplot')}.
#' If an exact match is not found, trailing elements of x, separated by underscore,
#' are removed one at a time in search of a partial match. Thus 'ref.col' will match
#' for 'ref.col_dens' and 'ref.col_scatter' if neither of these is set (allowing
#' selective override). However, global' will never match 'global.col'.
#'
#' If x is missing a list of all metaplot options is returned.
#'
#' @param x a character string holding an option name
#' @param default the value returned if option is not set
#' @export
#' @seealso \code{\link{getOption}} \code{\link{setOption}}
#' @examples
#'
#' library(magrittr)
#' library(dplyr)
#' library(csv)

#' x <- as.csv(system.file(package = 'metaplot', 'extdata/theoph.csv'))
#' x %<>% pack
#'

#' multiplot(
#' x %>% metaplot(conc, gg = F),
#' x %>% metaplot(conc, time, gg = F),
#' x %>% metaplot(conc, arm, gg = F),
#' x %>% metaplot(conc, arm,  gg = T)
#' )
#'
#' # Add a reference line at 9 mg/L
#' x$conc %<>% structure(reference = 9)
#'
#' # Make the reference line green universally.
#' setOption(ref_col = 'green')
#'
#' # Make the reference line orange for density plots
#' setOption(ref_col_dens = 'orange')
#'
#' multiplot(
#' x %>% metaplot(conc, gg = F),
#' x %>% metaplot(conc, time, gg = F),
#' x %>% metaplot(conc, arm, gg = F),
#' x %>% metaplot(conc, arm,  gg = T)
#' )
#'
#' # Restore defaults
#' # setOption() # clears all metaplot options
#' setOption(ref_col = NULL)
#' setOption(ref_col_dens = NULL)


metOption <- function(x, default = NULL){
  mops <- getOption('metaplot',list())
  if(missing(x))return(mops)
  stopifnot(is.character(x))
  stopifnot(length(x) == 1)
  stopifnot(is.list(mops))
  if(is.null(names(mops))) return(default)
  while(grepl('_',x) && !any(names(mops) == x))x <- sub('_.*','',x)
  if(!any(names(mops) == x)) return(default)
  mops <- mops[names(mops) == x]
  mops <- rev(mops)
  return(mops[[1]])
}
#' Set or Reset Metaplot Options
#'
#' Sets an option value in the list \code{getOption('metaplot')}.
#' If invoked without named arguments, option 'metaplot' is set to NULL.
#' Setting an existing option moves it to the end of the list (breaks ties in \code{\link{metOption}}).
#'
#' @param ... any metaplot options can be defined, using \code{name = value}.
#' @return (invisible) character vector of option names that were set or unset
#' @export
#' @seealso \code{\link{metOption}}\code{\link{options}}
#' @examples
#' example(metOption)

setOption <- function(...){
  args <- list(...)
  nms <- names(args)
  mops <- getOption('metaplot', list())
  if(length(nms) == 0){
    options(metaplot = NULL)
    nms <- names(mops)
    if(!length(nms)) nms <- character(0)
    invisible(nms)
  }
  for(i in nms){
    mops[[i]] <- NULL
    mops[[i]] <- args[[i]]
  }
  options(metaplot = mops)
  invisible(nms)
}

metaplot_aspect <- function(aspect, gg){
  # metaplot default is 1
  # lattice default is 'fill'
  # ggplot default is NULL
  # cross-translate:
  empty <- FALSE
  if(is.null(aspect)) empty <- TRUE
  if(!is.null(aspect) && is.na(aspect)) empty <- TRUE
  if(!is.null(aspect) && !is.na(aspect) && aspect == 'fill') empty <- TRUE
  if(empty && !gg) aspect <- 'fill'
  if(empty && gg) aspect <- NULL
  aspect
}

#' Merge Two Lists
#'
#' Merges two lists. Every named element in the second argument is added recursively at the corresponding
#' position in the first argument by name, over-writing existing values as necessary.
#' Every un-named argument is added if there is no named argument to over-write.
#'
#' @param x a list (coerced if not)
#' @param y a list (coerced if not)
#' @param ... ignored
#' @export
#' @keywords internal
#' @examples
#' foo <- list(
#'   a = list(         # substituted by name
#'     col = 'red',    # substituted by name
#'     lty = 'dashed', # substituted by name
#'     alpha = 1,      # preserved
#'     8,              # preserved, since element 4 in replacement matches by name
#'     9               # substituted by position
#'   ),
#'   letters[8:10],    # preserved, siince elment 2 in replacement matches by name
#'   b = 3
#' )
#'
#' bar <- list(
#'   letters[11:13],  # ignored: position conflict with named element
#'   b = 2,           # substituted by name
#'   a = list(        # substituted by name
#'     'blue',        # ignored: position conflict with named element
#'     col = 'green', # substituted by name
#'     lty = 'solid', # substituted by name
#'     lwd = 2,       # added by name
#'     3,             # substituted by postion
#'     4,             # added by postion
#'     hue = 5        # added by name
#'   ),
#'   'baz'            # added by postion
#' )
#'
#' foo
#' bar
#' merge(foo,bar)
#' merge(list(1), list(2,foo = 3)) # 3 is assigned and named
#' merge(list(1), list(foo = 2,3)) # 3 is ignored since position 2 has been named by time of evaluation
#' merge(list(foo = 1), list(2,foo = 3)) # 2 ignored since pos. matches named argument; 3 overwrites
#' merge(list(foo = 1), list(2,3)) # 2 is ignored since position matches a named argument; 3 added

merge.list <- function(x, y, ...){
  x <- as.list(x) # in case method is invoked directly
  y <- as.list(y)
  if(length(y) == 0) return(x)
  index <- seq_along(y)
  ynms <- names(y)
  xnms <- names(x)
  if(is.null(ynms)) ynms <- rep('', length.out = length(y))
  if(is.null(xnms)) xnms <- rep('', length.out = length(x))
  # now we have y and ynms with same non-zero length, indexed by index
  #
  for(i in index){
    yn <- ynms[[i]] # could be ''
    byName <- yn != ''
    yi <- if(byName) y[[yn]] else y[[i]] # the candidate value, whether by name or position
    xi <- NULL # the target test value
    xn <- ''
    if(!byName && length(x) >= i){
      xn <- xnms[[i]]
      xi <- x[[i]]
    }
    if(byName && yn %in% xnms){
      xn <- yn
      xi <- x[[yn]]
    }
    if(byName && !yn %in% xnms){
      xnms <- c(xnms, yn) # will be true soon
    }

    # don't assign by position if doing so knocks out a named argument
    if(yn == '' && xn != ''){next}
    # otherwise, assign y[[i]] to x[[i]], recursively if necessary
    loc <- if(byName) yn else i
    if(is.list(xi) && is.list(yi)) {
      x[[loc]] <- merge.list(xi, yi)
    } else {
      #message(xi, ' becomes ', yi)
      x[[loc]] <- yi
    }
  }
  x
}




