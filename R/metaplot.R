globalVariables('groups_')
globalVariables('META')
globalVariables('VARIABLE')
globalVariables('VALUE')

#' Dens Generic
#'
#' Generic function for dens().
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
dens <- function(x,...)UseMethod('dens')

#' Plot Density for Data Frame
#'
#' Plot density for object of class 'data.frame'.
#' @param x folded
#' @param var item to plot
#' @param ref optional numeric
#' @param log logical; use log scale?
#' @param ... passed arguments
#' @export

dens.data.frame<- function(
  x,
  var,
  ref = NULL,
  log = FALSE,
  ...
){
  # d <- unfold(x,.y)
  v <- x[[var]]
  densityplot(
    v,
    aspect = 1,
    scales = list(tck = c(1,0),x = list(log = log,equispaced.log = FALSE)),
    panel = function(...){
      panel.densityplot(...)
      if(length(ref))panel.abline(v = ref)
    },
    ...
  )
}#' Plot Density for Folded
#'
#' Plot density for object of class 'folded'.
#' @param x folded
#' @param var item to plot
#' @param xref optional numeric
#' @param log logical; use log scale?
#' @param ... passed arguments
#' @export

dens.folded <- function(
  x,
  var,
  xref = NULL,
  log = FALSE,
  ...
){
  d <- unfold_(x,var=var)
  xlab = axislabel(x,var,log=log)
  dens(d, var = var, xref = xref, log = log, xlab=xlab, ...)
}

#' Scatterplot
#'
#' Scatterplot.
#'
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
scatter <- function(x,...)UseMethod('scatter')

#' Scatterplot for Data Frame
#'
#' Scatterplot for class 'data.frame'.
#' @param x data.frame
#' @param .y y axis item
#' @param .x x axis item
#' @param groups optional grouping item
#' @param ... passed arguments
#' @param ylog log transform y axis (guessed if missing)
#' @param xlog log transform x axis (guessed if missing)
#' @param yref reference line from y axis
#' @param xref reference line from x axis
#' @param ysmooth supply loess smooth of y on x
#' @param xsmooth supply loess smmoth of x on y
#' @param cols suggested columns for auto.key
#' @param density plot point density instead of points
#' @param iso use isometric axes with line of unity
#' @param main print Pearson correlation coefficient as title
#' @param group_codes append these to group values for display purposes
#' @param crit if ylog or xlog missing, log transform if mean/median ratio for non-missing values is greater than crit
#' @export
#' @import lattice
scatter.data.frame <- function(
  x,
  .y,
  .x,
  groups = NULL,
  ...,
  ylog = FALSE,
  xlog = FALSE,
  yref = NULL,
  xref = NULL,
  ysmooth = FALSE,
  xsmooth = FALSE,
  cols = 3,
  density = FALSE,
  iso = FALSE,
  main = TRUE,
  group_codes = NULL,
  crit = 1.3
){
  stopifnot(
    length(.x) == 1,
    length(.y) == 1,
    length(groups) <= 1
  )
  y <- x # %>% unfold(c(.y,.x,groups))
  stopifnot(all(c(.x,.y,groups) %in% names(y)))
  names(y)[names(y) ==.y] <- 'y_'
  names(y)[names(y) ==.x] <- 'x_'
  if(length(groups)){
    names(y)[names(y) == groups] <- 'groups_'
    gc <- group_codes
    if(length(gc) == 1){
      if(encoded(gc)){
        y$groups_ <- decode(y$groups_,gc)
      }else{
        y$groups_[is.defined(y$groups_)] <- paste(y$groups_[is.defined(y$groups_)],gc)
      }
    }
    cols = min(cols,length(unique(y$groups_[is.defined(y$groups_)])))
  }
  formula <- 'y_ ~ x_'
  formula <- as.formula(formula)
  auto.key <- if(length(groups)) list(columns = cols) else FALSE
  if(ylog %>% is.null) ylog <- all(na.rm = T,y$y_ > 0) && mean(y$y_,na.rm = T)/median(y$y_,na.rm = T) > crit
  if(xlog %>% is.null) xlog <- all(na.rm = T, y$x_ > 0) && mean(y$x_,na.rm = T)/median(y$x_,na.rm = T) > crit
  yscale = list(log = ylog,equispaced.log = FALSE)
  xscale = list(log = xlog,equispaced.log = FALSE)
  scales = list(y = yscale,x = xscale,tck = c(1,0))
  prepanel = if(iso) function(x,y,...){
    lim = c(min(x,y,na.rm = T),max(x,y,na.rm = T))
    list(
      xlim = lim,
      ylim = lim
    )
  } else NULL
  cor <- cor(use = 'pair',if(ylog) log(y$y_) else y$y_, if(xlog) log(y$x_) else y$x_)
  cor <- signif(cor,2)
  cor <- paste('R =',cor)
  cor <- parens(cor)
  mn <- paste(sep = ' ~ ',.y,.x)
  if(length(groups)) mn <- paste(mn,'by',groups)
  mn <- paste(mn,cor)
  metapanel <- function(x, y,...){
      if( length(groups))panel.superpose(x = x, y = y, ...)
      if(!length(groups)){
        if( density)panel.smoothScatter(x,y,...)
        if(!density)panel.xyplot(x,y,...)
      }
      foo <- loess.smooth(x,y)
      bar <- loess.smooth(y,x)
      if(ysmooth)try(panel.xyplot(foo$x,foo$y,col = 'black',lty = 'dashed',type = 'l'))
      if(xsmooth)try(panel.xyplot(bar$y,bar$x,col = 'black',lty = 'dashed',type = 'l'))
      if(length(yref))panel.abline(h = yref)
      if(length(xref))panel.abline(v = xref)
      if(iso)panel.abline(0,1)
    }
  xyplot(
    formula,
    data = y,
    groups = if(length(groups)) groups_ else NULL,
    auto.key = auto.key,
    aspect = 1,
    scales = scales,
    main = if(main) list(
      mn,
      cex = 1,
      fontface = 1
    ) else NULL,
    prepanel = prepanel,
    panel = metapanel,
    ...
  )
}
#' Scatterplot for Folded
#'
#' Scatterplot for class 'folded'.
#' @param x folded
#' @param .y y axis item
#' @param .x x axis item
#' @param groups optional grouping item
#' @param ... passed arguments
#' @param ylog log transform y axis (guessed if missing)
#' @param xlog log transform x axis (guessed if missing)
#' @param yref reference line from y axis
#' @param xref reference line from x axis
#' @param ysmooth supply loess smooth of y on x
#' @param xsmooth supply loess smmoth of x on y
#' @param cols suggested columns for auto.key
#' @param density plot point density instead of points
#' @param iso use isometric axes with line of unity
#' @param main print Pearson correlation coefficient as title
#' @param crit if ylog or xlog missing, log transform if mean/median ratio for non-missing values is greater than crit
#' @export
#' @import encode
#' @import lattice
scatter.folded <- function(
  x,
  .y,
  .x,
  groups = NULL,
  ...,
  ylog = FALSE,
  xlog = FALSE,
  yref = NULL,
  xref = NULL,
  ysmooth = FALSE,
  xsmooth = FALSE,
  cols = 3,
  density = FALSE,
  iso = FALSE,
  main = TRUE,
  crit = 1.3
){
  stopifnot(
    length(.x) == 1,
    length(.y) == 1,
    length(groups) <= 1
  )
  y <- x %>% unfold_(var = c(.y,.x,groups))
  stopifnot(all(c(.x,.y,groups) %in% names(y)))
  gc <- if(length(groups)) guide(x,groups) else NULL
  ylab <- axislabel(x,.y,ylog)
  xlab <- axislabel(x,.x,xlog)
  scatter(
    y,
    .y = .y,
    .x = .x,
    groups = groups,
    ylog = ylog,
    xlog = xlog,
    yref = yref,
    xref = xref,
    ysmooth = ysmooth,
    xsmooth = xsmooth,
    cols = cols,
    density = density,
    iso = iso,
    main = main,
    crit = crit,
    group_codes = gc,
    ylab = ylab,
    xlab = xlab,
    ...
  )
}

#' Generic Axis Label
#'
#' Generic axis label.
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
axislabel <- function(x,...)UseMethod('axislabel')

#' Axis Label for Folded
#'
#' Axis label for folded object.
#' @param x folded
#' @param var item of interest
#' @param log whether this is for a log scale
#' @param ... passed arguments
#' @export
#' @import magrittr
#' @return character
axislabel.folded <- function(x, var, log = FALSE, ...){
  x <- x[x$VARIABLE == var & is.defined(x$META),,drop = FALSE]
  lab <- unique(x$VALUE[x$META =='LABEL'])
  guide <- unique(x$VALUE[x$META =='GUIDE'])
  res <- var
  if(length(lab) == 1 & lab %>% is.defined)res <- lab
  if(length(guide) == 1 & !encoded(guide) & guide %>% is.defined){
    guide <- paste0('(',guide,')')
    res <- paste(res,guide)
  }
  if(log) res <- paste0(res,'\n(log)')
  res
}


#' Metaplot Generic
#'
#' Generic function for metaplot.
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
metaplot <- function(x,...)UseMethod('metaplot')

#' Metaplot Method for Folded
#'
#' Plots a folded object.
#' @param x folded
#' @param ... unquoted anonymous arguments are passed as character: var to metaplot_().
#' @import lazyeval
#' @seealso \code{\link{metaplot_.folded}}
#' @export
metaplot.folded <- function(x,...){
  args <- dots_capture(...)
  args <- lapply(args,f_rhs)
  var <- args[names(args) == '']
  other <- args[names(args) != '']
  var <- sapply(var, as.character)
  do.call(
    metaplot_,
    c(
      list(
        x = x,
        var = var
      ),
      other
    )
  )
}

#' Metaplot Generic, Standard Evaluation
#'
#' Generic metaplot function using standard evaluation.
#' @param x object
#' @param var character: quoted names of variables to plot
#' @param ... other arguments
#' @export
#' @keywords internal
metaplot_ <- function(x, ...)UseMethod('metaplot_')

#' Metaplot Method for Folded
#'
#' Metaplot method for folded.
#'
#' Try calling \code{\link{metaplot}} to create var from unquoted, anonymous arguments.
#'
#' What happens next depends on the number and type of items represented by var.
#'
#' A single argument representing a continuous variable is forwarded to \code{\link{dens}} to give a density plot.  Same for a single categorical, but this is unexpected.
#'
#' Two arguments of type continuous, categorical or categorical, continuous are forwarded to \code{link{boxplot.folded}} to give a boxplot (vertical or horizontal, respectively).
#'
#' Two arguments representing continuous variables give a scatterplot by means of \code{\link{scatter.folded}}.
#'
#' A third anonymous argument is unexpected if a preceding argument is categorical.
#'
#' A third, categorical argument following two continuous arguments is treated as a grouping variable.
#'
#' If there are three or more continuous arguments, a scatterplot matrix is created by means of \code{\link{corsplom.folded}}.  Additional categoricals will be ignored.
#'
#' Stratification, e.g. conditioning for trellis plots, is currently unimplemented.
#'
#' @param x folded
#' @param var character: names of items to plot
#' @param ... passed arguments
#' @import lazyeval
#' @importFrom graphics boxplot
#' @importFrom stats as.formula cor density loess.smooth median
#' @importFrom dplyr filter
#' @import fold
#' @export
metaplot_.folded = function(x, var, ...){
  x %<>% data.frame(stringsAsFactors = FALSE) # faster than grouped_df
  class(x) <- c('folded','data.frame')
  cont <- sapply(var,function(nm)continuous(x,nm))
  len <- length(var)
  args <- c(
    list(x = x),
    as.list(var),
    list(...)
  )
  if(!any(cont))stop('metaplot requires at least one continuous variable')
  if(len == 1) return(do.call(dens, args))
  if(sum(cont) > 2) return(do.call(corsplom_,args))
  # now have at least two var but no more than two continuous
  if(!cont[[1]] && !cont[[2]]) stop('metaplot requires at least one continuous variable')
  if( cont[[1]] &&  cont[[2]]) return(do.call(scatter,args))
  if(!cont[[1]] &&  cont[[2]]) return(do.call(boxplot,args))
  if( cont[[1]] && !cont[[2]]) return(do.call(boxplot,args))

}

#' Check if Something is Continuous
#'
#' Checks if something is continuous.
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
continuous <- function(x,...)UseMethod('continuous')

#' Check if Folded is Continuous
#'
#' Checks if folded is continuous with respect to \code{var}
#' @param x folded
#' @param var length-one character
#' @param ... passed arguments
#' @import dplyr
#' @import magrittr
#' @export
#' @keywords internal
continuous <- function(x,var, ...){
  stopifnot(length(var) == 1)
  is.number <- function(x)sum(is.defined(x)) == sum(is.defined(as.numeric(x)))
  val <- x %>% dplyr::filter(META %>% is.na) %>% dplyr::filter(VARIABLE == var) %$% VALUE
  if(length(val) ==0)stop('no values found for ',var)
  guide_var <- guide(x,var)
  enc <- if(length(guide_var)) encoded(guide_var) else FALSE
  cont <- val %>% is.number && !enc
  cont
}

is.defined <- function(x)!is.na(x)

#' Extract Guide
#'
#' Extracts guide.
#' @param x object
#' @param ... passed arguments
#' @export
guide <- function(x,...)UseMethod('guide')

#' Extract Guide for Folded
#'
#' Extracts guide for class folded, given a variable.
#' @param x folded
#' @param var length-one character
#' @param ... ignored arguments
#' @return length-one character, possibly NA
#' @export
guide.folded <- function(x,var,...){
  stopifnot(length(var) == 1)
  y <- x[is.defined(x$META) & x$META =='GUIDE' & x$VARIABLE == var,'VALUE']
  y <- unique(y) #
  if(length(y) > 1)stop('conflicting guides for ', var)
  if(length(y) == 0) y <- NA_character_
  y
}

#' Extract Label
#'
#' Extracts label.
#' @param x object
#' @param ... passed arguments
#' @export
label <- function(x,...)UseMethod('label')

#' Extract Label for Folded
#'
#' Extracts label for class folded, given a variable.
#' @param x folded
#' @param var length-one character
#' @param ... ignored arguments
#' @return length-one character, possibly NA
#' @export
label.folded <- function(x,var, ...){
  stopifnot(length(var) == 1)
  y <- x[is.defined(x$META) & x$META =='LABEL' & x$VARIABLE == var,'VALUE']
  y <- unique(y)
  if(length(y) > 1)stop('conflicting guides for ', var)
  if(length(y) == 0) y <- NA_character_
  y
}


parens <- function (x, ...)paste0("(", x, ")")


#' Boxplot for Data Frame
#'
#' Boxplot for data.frame.
#' @param x data.frame
#' @param .y y axis item
#' @param .x x axis item
#' @param log whether to log transform continuous variable
#' @param horizontal whether box/whisker axis should be horizontal
#' @param main whether to include title indicating x and y items
#' @param crit if log is missing, log transform if mean/median ratio for non-missing x  is greater than this value
#' @param ref optional reference line on continuous axis
#' @param guide optional encoding for categories see \code{encode::encode}
#' @param ... passed arguments
#' @export
boxplot.data.frame <- function(
  x,
  .y,
  .x,
  log,
  horizontal = TRUE,
  main = TRUE,
  crit = 1.3,
  ref = NULL,
  guide = NA_character_,
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
  # if(is.null(ref)) if(ymeta$TYPEC %in% c('IIV','RESIDUAL') ) ref <- 0
  scales <- list(
    tck = c(1,0),
    x = list(log = log,equispaced.log = FALSE)
  )
  mn <- paste(sep = ' ~ ',.y,.x)
  bwplot(
    formula,
    data = y,
    aspect = 1,
    horizontal = horizontal,
    main = if(main) list(
      mn,
      cex = 1,
      fontface = 1
    ) else NULL,
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
#' Boxplot for folded object.
#' @param x folded
#' @param .y y axis item
#' @param .x x axis item
#' @param log whether to log transform continuous variable
#' @param horizontal if missing, will be set to TRUE if .x is continuous
#' @param main whether to include title indicating x and y items.
#' @param crit if log is missing, log transform if mean/median ratio for non-missing x  is greater than this value
#' @param ref optional reference line on continuous axis
#' @param ... passed arguments
#' @import encode
#' @export
boxplot.folded <- function(
  x,
  .y,
  .x,
  log,
  horizontal,
  main = TRUE,
  crit = 1.3,
  ref = NULL,
  ...
){
  stopifnot(
    length(.x) == 1,
    length(.y) == 1
  )
  y <- x %>% unfold_(var=c(.y,.x))
  stopifnot(all(c(.x,.y) %in% names(y)))
  if(missing(horizontal)) horizontal <- continuous(x, .x)
  con <- if(horizontal) .x else .y
  cat <- if(horizontal) .y else .x
  if(missing(log)) log <- mean(y[[con]],na.rm = T)/median(y[[con]],na.rm = T) > crit
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
    ...
  )
}

#' Upper Panel Function
#'
#' Upper panel function for corsplom(). Plots data with loess smooth.
#' @param x x values
#' @param y y values
#' @param col point color
#' @param loess loess color
#' @param ... passed arguments
#' @export
u.p = function(x,y,col = 'black', loess = 'red',...){
  panel.xyplot(x,y,col = col, ...)
  panel.loess(x,y,col = loess)
}

#' Lower Panel Function
#'
#' Lower panel function for corsplom(). Plots Pearson correlation coefficient.
#' @param x x values
#' @param y y values
#' @param ... passed arguments
#' @export
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
#' @param denscale inflation factor for height of density smooth
#' @param ... passed arguments
#' @export
my.diag.panel <- function(x, denscale = 0.2,...){
  d <- density(x)
  lim <- current.panel.limits()$x
  lo <- lim[[1]]
  hi <- lim[[2]]
  len <- hi - lo

  x1 <- d$x
  y1 <- d$y
  y1 <- y1 / max(y1,na.rm = TRUE)
  y1 <- y1 * len * denscale
  z1 <- hi - y1
  y1 <- y1 + lo
  lpolygon(
    x = x1,
    y = z1,
    col = 'gray',
    border = NA,
    alpha = 0.5
  )
  lpolygon(
    y = x1,
    x = y1,
    col = 'gray',
    border = NA,
    alpha = 0.5
  )
  lsegments(x0 = lo,x1 = lo + len * denscale,y0 = 0,y1 = 0,col = 'darkgray',alpha = 0.5)
  lsegments(y0 = hi,y1 = hi - len * denscale,x0 = 0,x1 = 0,col = 'darkgray',alpha = 0.5)
  diag.panel.splom(...)
}

#' Scatterplot Matrix with Correlations, Generic.
#'
#' Generic function for scatterplot matrix with correlations.
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
corsplom <- function(x,...)UseMethod('corsplom')

#' Scatterplot Matrix with Correlations, Generic for Standard Evaluation
#'
#' Generic function for scatterplot matrix with correlations.
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
corsplom_ <- function(x,...)UseMethod('corsplom_')


#' Scatterplot Matrix with Correlations for data.frame, Standard Evaluation

#' Creates a scatterplot matrix with correlations in lower panel, by default.
#' @param x data.frame
#' @param upper.panel passed to splom
#' @param lower.panel passed to splom
#' @param pscales passed to splom
#' @param xlab passed to splom
#' @param varname.cex passed to splom
#' @param diag.panel passed to splom
#' @param split break diagonal names on white space
#' @param ... passed arguments
corsplom.data.frame <- function(
  x,
  upper.panel = u.p,
  lower.panel= l.p,
  pscales= 0,
  xlab = '',
  varname.cex = 0.5,
  diag.panel = my.diag.panel,
  split = TRUE,
  ...
){
  if(split) names(x) <- fracture(names(x))
  splom(
  x,
  upper.panel = upper.panel,
  lower.panel = lower.panel,
  pscales = pscales,
  xlab = xlab,
  varname.cex = varname.cex,
  diag.panel = diag.panel,
  ...
)
}


fracture <- function(x,sep='\n')gsub('\\s+',sep,x)


#' Scatterplot Matrix with Correlations for Folded, Standard Evaluation

#' Creates a scatterplot matrix with correlations for class 'folded'. Categoricals in \code{var} are currently ignored.
#' @param x folded
#' @param ... unnamed arguments indicating variables to plot, and named arguments passed to corsplom()
#' @export
#' @seealso corsplom.data.frame
#' @import lattice
corsplom_.folded <- function(x, ...){
  var <- dots_capture(...)
  var <- lapply(var, f_rhs)
  anonymous <- var[names(var) == '']
  anonymous <- sapply(anonymous,as.character)
  named <- var[names(var) != '']
  x %<>% filter(VARIABLE %in% anonymous)
  cont <- sapply(anonymous,function(nm)continuous(x,nm))
  anonymous <- anonymous[cont]
  meta <- x %>% filter(!is.na(META))
  data <- x %>% filter(is.na(META))
  data %<>% unfold_(var = anonymous, ...)
  data %<>% ungroup %>% select_(.dots=anonymous)
  for(nm in names(data)){
    y <- label(meta,nm)
    if(!is.na(y))names(data)[names(data) == nm] <- y
  }
  this <- list(x=data)
  out <- c(this,named)
  do.call(corsplom,out)
}

#' Scatterplot Matrix with Correlations for Folded, Non Standard Evaluation

#' Creates a scatterplot matrix with correlations for class 'folded'. Categoricals in \code{var} are currently ignored.
#' @param x folded
#' @param ... passed arguments
#' @export
#' @seealso \code{\link{corsplom_.folded}} \code{\link{corsplom.data.frame}}
#' @import lattice
corsplom.folded <- function(x, ...){
  args <- dots_capture(...)
  args <- lapply(args, f_rhs)
  var <- args[names(args) == '']
  other <- args[names(args) != '']
  var <- sapply(var,as.character)
  do.call(
    corsplom_.folded,
    c(
      list(
        x = x,
        var = var
      ),
      list(...)
    )
  )
}

# indplot <- function(x,...)UseMethod('indplot')
# indplot.meta <- function(x,  ..., by = 'USUBJID'){
#   # currently cannot support second-level metadata
#   terms <- list(...) %>% unlist
#   .x <- rev(terms)[[1]]
#   .y <- terms %>% setdiff(.x)
#   key <- names(x) %>% setdiff(c('VARIABLE','META','VALUE'))
#   x %<>% filter(!(VARIABLE %in% META)) # hack
#   y <- x %>% unfold
#   y %<>% gather_(value_col = .y[[1]],key_col = 'MOMENT',gather_cols = .y)
#   y %<>% fold(group_by = c(key,'MOMENT'))
#   y$by <- y[[by]]
#   for(i in unique(y[[by]])){
#     z <- y %>% filter((META %>% is.defined) | (by == i))
#     z$by <- NULL
#     z %>% plot(.y[[1]],.x,...,groups = 'MOMENT')
#   }
# }

#' Make Individual Plots
#'
#' Makes individual plots
#' @param x object
#' @param ... passed arguments
#' @export
indiplot <- function(x,...)UseMethod('indiplot')

#' Make Individual Plots from Numeric
#'
#' Makes individual plots from numeric
#' @inheritParams indiplot
#' @export
indiplot.numeric <- function(x,...)indiplot(as.character(x),...)


#' Make Individual Plots from Character
#'
#' Makes individual plots from character by coercing to 'folded' (i.e. calling fold() ).
#' @inheritParams indiplot
#' @export
indiplot.character <- function(x, ...)indiplot(fold(x),...)

#' Make Individual Plots from Folded
#'
#' Makes individual plots from folded
#' @param x folded
#' @param cols character: up to 4 columns, x first
#' @param filepath path for pdf output (or NULL)
#' @export

# indiplot.folded <- function(
#   x,
#   filepath = file.path(getOption('project'),x,paste0(x,'-ind.pdf')),
#   cols,
#   ...
# ){
#   # x %<>% as.pharmval already meta
#   x %<>% filter(VARIABLE %in% cols) #c('UPA','PRED','IPRE','TAD'))  #### PROJECT SPECIFIC
#   x %<>% unfold
#   x %>% head %>% data.frame
#
#   ## NEEDS ABSTRACTION
#   x %<>% filter(is.defined(IPRE) & is.defined(TAD) & is.defined(UPA))
#   x %<>% select(
#     USUBJID,DATETIME,UPA,OBS_GUIDE = UPA_GUIDE,
#     OBS_LABEL = UPA_LABEL,IPRE,TAD,TAD_GUIDE,TAD_LABEL
#   )
#   x %<>% gather(GROUP,OBS,UPA,IPRE)
#
#   lapply(x,function(col)sum(is.na(col)))
#   x %>% filter(is.na(OBS)) %>% head %>% data.frame
#   x %>% filter(USUBJID == '3083-101-002.101001001') %>% data.frame
#   x %<>% mutate(GROUP = factor(GROUP,levels = c('UPA','IPRE'),labels = c('observation','individual prediction')))
#   if(length(filepath))pdf(filepath)
#   x %>%
#     xyplot(
#       data = .,
#       OBS ~ TAD|USUBJID,
#       layout = c(1,1),
#       scales = list(
#         relation = 'free',
#         y = list(
#           log = T,equispaced.log = FALSE
#         )),
#       aspect = 1,
#       auto.key = T,
#       xlab = paste(x$TAD_LABEL,parens(x$TAD_GUIDE))[[1]],
#       ylab = paste(x$OBS_LABEL,parens(x$OBS_GUIDE))[[1]],
#       groups = GROUP,
#       as.table = T,
#       panel = panel.superpose,
#       panel.groups = function(group.number,type,x,y,...){
#         type <- 'p'
#         if(group.number == 2) type <- 'l'
#         d <- data.frame(x = x,y = y)
#         d <- d[d$x %>% is.defined & d$y %>% is.defined,]
#         d$lag <- lag(x)
#         d$node <- with(d, is.na(lag) | x < lag)
#         d$interval <- cumsum(d$node)
#         for(i in unique(d$interval))panel.xyplot(
#           type = type,
#           x = d$x[d$interval ==i],
#           y = d$y[d$interval ==i],
#           ...
#         )
#       }
#     ) %>% print
#   if(length(filepath))dev.off()
#   if(length(filepath))filepath else invisible(NULL)
# }
#
# diagnostics <- function(
#   x,
#   filepath = file.path(getOption('project'),x,paste0(x,'.pdf')),
#   cov = character(0),
#   ...
# ){
#   filepath
#   if(length(filepath))pdf(filepath)
#   x %<>% fold
#   eta <- unique(x$VARIABLE[x$VARIABLE %contains% '^ETA'])
#   x %>% plot('UPA','PRED',iso = T,ysmooth = T) %>% print
#   x %>% plot('UPA','IPRE',iso = T,ysmooth = T)%>% print
#   x %>% plot('CWRESI','TIME',yref = 0,ysmooth = T)%>% print
#   x %>% plot('CIWRESI','TIME',yref = 0,ysmooth = T)%>% print
#   x %>% plot('CWRESI','PRED',yref = 0,ysmooth = T)%>% print
#   x %>% plot('CIWRESI','IPRE',yref = 0,ysmooth = T)%>% print
#   x %>% plot('CIWRESI','TAD',yref = 0,ysmooth = T)%>% print
#   for(i in eta)x %>% plot(i,xref = 0) %>% print
#   for(i in eta){
#     for(j in cov){
#       x %>% plot(i,j,yref = 0,ysmooth = T,bref = 0) %>% print
#     }
#   }
#   x %>% indiplot(filepath = NULL)
#   if(length(filepath))dev.off()
#   if(length(filepath))filepath else invisible(NULL)
# }


