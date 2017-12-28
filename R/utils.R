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
#' @param loess loess color
#' @param ... passed arguments
#' @keywords internal
#' @export
#' @family panel functions
u.p = function(x,y, col, loess = col,...){
  panel.xyplot(x,y,col = col, ...)
  panel.loess(x,y,col = loess)
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
#' @param denscale inflation factor for height of density smooth
#' @param ... passed arguments
#' @keywords internal
#' @export
#' @family panel functions
#' @seealso \code{\link{corsplom}}
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

is.defined <- function(x)!is.na(x)
parens <- function (x, ...)paste0("(", x, ")")
fracture <- function(x,sep='\n')gsub('\\s+',sep,x)

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
