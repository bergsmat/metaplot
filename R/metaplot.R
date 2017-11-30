globalVariables('groups_')
globalVariables('META')
globalVariables('VARIABLE')
globalVariables('VALUE')
globalVariables('collapse')
globalVariables('panel_')

#' Metaplot
#'
#' Creates a metaplot.
#' @param x object
#' @param ... passed arguments
#' @export
#' @family generic functions
#' @family metaplot
metaplot <- function(x,...)UseMethod('metaplot')

#' Categorical Plot
#'
#' Categorical plot.  Currently unimplemented. Returns a named vector indicating whether anonymous arguments were detected as numeric or categorical.
#'
#' @param x data.frame
#' @param ... other arguments
#' @family categorical
#' @family metaplot
#' @return character
#' @export
#' @importFrom rlang quos
#'
categorical <- function(x,...){
  args <- quos(...)
  args <- lapply(args,f_rhs)
  vars <- args[names(args) == '']
  other <- args[names(args) != '']
  vars <- sapply(vars, as.character)
  stopifnot(all(vars %in% names(x)))
  num <- with(x, sapply(vars, is.numeric))

  guide <- with(x, lapply(vars, attr, 'guide'))
  guide[is.null(guide)] <- ''
  stopifnot(all(sapply(guide,length) <= 1))
  guide <- as.character(guide)

  label <- with(x, lapply(vars, attr, 'label'))
  label[is.null(label)] <- ''
  stopifnot(all(sapply(label,length) <= 1))
  label <- as.character(label)

  encoded <- with(x, sapply(vars, encoded))
  num[encoded] <- FALSE
  num <- ifelse(num,'numeric','categorical')
  names(num) <- vars
  warning('categorical() is currently unimplemented')
  num
}

#' Create Metaplot from Folded
#'
#' Creates a plot from folded.  Packs metadata into attributes and calls next method.
#'

#' @param x object
#' @param ... passed arguments
#' @family metaplot
#' @family univariate plots
#' @family bivariate plots
#' @family multivariate plots
#' @importFrom graphics boxplot
#' @importFrom stats as.formula cor density loess.smooth median
#' @importFrom dplyr filter
#' @import fold
#' @export

metaplot.folded <- function(x, ...)metaplot(unpack(x,...),...)
#' Create Metaplot for Data Frame.
#'
#' Metaplot creates univariate, bivariate, or multivariate plots depending on the number and types of variables represented by the anonymous arguments.  Types are either numeric (NUM, e.g. real, integer) or categorical (CAT, e.g. factor, character).  A variable stored as numeric that nonetheless has an \code{\link[encode]{encoded}} \code{guide} attribute will be treated as categorical.
#'
#' Design your plot by specifying y variables (optional), the x variable, the groups variable (optional) and the conditioning variables (optional).
#'
#' The single groups variable, if any, is the first categorical in the third position or later. An earlier categorical gives a bivariate plot, e.g. horizontal boxplot (first position) or vertical boxplot (second postion) causing groups and facets to be ignored.
#'
#' The x variable is the last variable before groups, if present.
#'
#' The y variables are those before x. If none, the result is univariate. If one, the result is typically a boxplot or scatterplot, depending on x. Several numeric y followed by a numeric x are treated as multivariate (scatterplot matrix).  But if all y have the same \code{guide} attribute and it is different from that for x, the result is bivariate (i.e, an \code{overlay} scatterplot).
#'
#' The groups argument is only relevant for bivariate plots with one y.  For multiple y (overlay), the sources of y are the implied groups: any trailing categorical arguments are treated as facets. To specify facets without specifying groups, let groups be empty, e.g., \code{(metaplot(y, x, , facet1, facet2))}.
#'
#' Template designs follow; substitute behaviors by setting global options (see argument list).
#'
#'\itemize{
#' \item{NUM:}{ univariate (densityplot) }
#'
#' \item{CAT:}{ categorical (unimplemented) }
#'
#' \item{NUM, CAT:}{mixedvariate (vertical boxplot)}
#'
#' \item{CAT, NUM:}{mixedvariate (horizontal boxplot)}
#'
#' \item{NUM, NUM:}{bivariate (scatterplot)}
#'
#'\item{CAT, CAT:}{ categorical (unimplemented)}
#'
#' \item{NUM, NUM, CAT:}{ grouped bivariate (grouped scatterplot)}
#'
#' \item{NUM, NUM,, CAT:}{ non-grouped bivariate with one facet}
#'
#' \item{NUM, NUM, CAT, CAT:}{ grouped bivariate with one facet}
#'
#' \item{NUM, NUM, CAT, CAT, CAT:}{ grouped bivariate with two facets}
#'
#' \item{NUM, NUM, NUM:}{ multivariate, or grouped bivariate for \code{overlay}}
#'
#' \item{NUM, NUM, NUM, CAT}{ multivariate, or faceted bivariate for \code{overlay}}
#'
#' \item{NUM, NUM, NUM, CAT, CAT}{ multivariate, or bivariate with two facets for \code{overlay}}
#'
#'}
#'
#' @param x object
#' @param univariate function for univariate arguments
#' @param mixedvariate function for bivariate combinations of numeric and categoral arguments
#' @param bivariate function for arguments that resolve to two numerics (see rules)
#' @param multivariate function for more than two numeric arguments
#' @param categorical function for categorical arguments
#' @param ... passed arguments
#' @import encode
#' @family metaplot
#' @family univariate plots
#' @family bivariate plots
#' @family multivariate plots
#' @family categorical plots
#' @export
#' @importFrom rlang quos
#' @examples
#' \dontrun{
#' library(magrittr)
#' library(dplyr)
#' library(csv)
#' library(nlme)
#' x <- Theoph
#'
#' # mixed effects model
#' m1 <- nlme(
#'   conc ~ SSfol(Dose, Time, lKe, lKa, lCl),
#'   data = x,
#'   fixed = lKe + lKa + lCl ~ 1,
#'   random = lKe + lKa + lCl ~ 1
#' )
#'
#' # some numeric and categorical properties
#' x %<>% mutate(arm = ifelse(as.numeric(as.character(Subject)) %% 2 == 0, 1, 2))
#' x %<>% mutate(site = ifelse(as.numeric(as.character(Subject)) < 7, 1, 2))
#' x %<>% mutate(pred = predict(m1,level = 0))
#' x %<>% mutate(ipred = predict(m1))
#' x %<>% mutate(res = residuals(m1))
#' x %<>% mutate(sres = residuals(m1, type = 'pearson'))
#' r <- ranef(m1)
#' r$Subject <- rownames(r)
#' x %<>% left_join(r)

#' # metadata
#' attr(x$Subject,'label') <- 'subject identifier'
#' attr(x$Wt,'label') <- 'subject weight'
#' attr(x$Dose,'label') <- 'theophylline dose'
#' attr(x$Time,'label') <- 'time since dose administration'
#' attr(x$conc,'label') <- 'theophylline concentration'
#' attr(x$arm,'label') <- 'trial arm'
#' attr(x$site,'label') <- 'investigational site'
#' attr(x$pred,'label') <- 'population-predicted concentration'
#' attr(x$ipred,'label') <- 'individual-predicted conentration'
#' attr(x$res,'label') <- 'residuals'
#' attr(x$sres,'label') <- 'standardized residuals'
#' attr(x$lKe,'label') <- 'natural log of elimination rate constant'
#' attr(x$lKa,'label') <- 'natural log of absorption rate constant'
#' attr(x$lCl,'label') <- 'natural log of clearance'

#' attr(x$Subject,'guide') <- '....'
#' attr(x$Wt,'guide') <- 'kg'
#' attr(x$Dose,'guide') <- 'mg/kg'
#' attr(x$Time,'guide') <- 'h'
#' attr(x$conc,'guide') <- 'mg/L'
#' attr(x$arm,'guide') <- '//1/Arm A//2/Arm B//'
#' attr(x$site,'guide') <- '//1/Site 1//2/Site 2//'
#' attr(x$pred,'guide') <- 'mg/L'
#' attr(x$ipred,'guide') <- 'mg/L'
#' x %>% unpack %>% as.csv('theoph.csv')
#' }
#'
#' library(magrittr)
#' library(dplyr)
#' library(csv)

#' x <- as.csv(system.file(package = 'metaplot', 'extdata/theoph.csv'))
#' x %<>% pack


#' # sample plots
#' x %>% metaplot(conc)
#'#x %>% metaplot(site)
#' x %>% metaplot(Wt, arm)
#' x %>% metaplot(arm, Wt)
#' x %>% metaplot(conc, Time)
#'#x %>% metaplot(arm, site)
#' x %>% metaplot(conc, Time, Subject)
#' x %>% metaplot(conc, Time, , Subject)
#' x %>% metaplot(conc, Time, Subject, site)
#' x %>% metaplot(conc, Time, Subject, site, arm)
#' x %>% metaplot(lKe, lKa, lCl)
#' x %>% metaplot(conc, ipred, Time)
#' x %>% metaplot(conc, ipred, Time, Subject)
#' x %>% metaplot(conc, ipred, Time, site, arm)
#' x %>% metaplot(res, conc, yref = 0, ysmooth = T, conf = T, grid = T, loc = 1)
#'
#'\dontshow{
#' y <- x
#' y[] <- lapply(y, as.character)
#' y[] <- lapply(y, as.numeric)
#' y$arm <- as.factor(y$arm)
#' y$site <- as.factor(y$site)
#' y$Subject <- as.factor(y$Subject)
#' y %>% metaplot(conc)
#'#y %>% metaplot(site)
#' y %>% metaplot(Wt, arm)
#' y %>% metaplot(arm, Wt)
#' y %>% metaplot(conc, Time)
#'#y %>% metaplot(arm, site)
#' y %>% metaplot(conc, Time, Subject)
#' y %>% metaplot(conc, Time, , Subject)
#' y %>% metaplot(conc, Time, Subject, site)
#' y %>% metaplot(conc, Time, Subject, site, arm)
#' y %>% metaplot(lKe, lKa, lCl)
#' y %>% scatter(conc, ipred, Time)
#' y %>% scatter(conc, ipred, Time, Subject)
#' y %>% scatter(conc, ipred, Time, site, arm)
#'}
#'
metaplot.data.frame <- function(
  x,
  ...,
  univariate   = getOption('metaplot_univariate','dens'),
  mixedvariate = getOption('metaplot_mixedvariate','boxplot'),
  bivariate    = getOption('metaplot_bivariate','scatter'),
  multivariate = getOption('metaplot_multivariate','corsplom'),
  categorical  = getOption('metaplot_categorical','categorical')
){
  args <- quos(...)
  args <- lapply(args,f_rhs)
  vars <- args[names(args) == '']
  other <- args[names(args) != '']
  vars <- sapply(vars, as.character)
  # now x, vars, and other are passable
  args <- c(list(x = x),vars,other)
  # where to pass them depends only on properties of prime variables
  # prime is all y, if present, and x
  # prime is defined as all vars before groups or facets, if present
  # non-prime start with the first missing or categorical in position 3 or later
  # since groups may be missing, checking properties may fail
  # discard non-prime
  missing <- match('',vars)
  if(is.defined(missing)) vars <- vars[seq(length.out = missing -1)]

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

  # groups, if present, is non-missing. (may actually be a facet)
  pos <- seq_along(num)
  can <- !num & pos > 2
  grp <- match(TRUE, can)
  if(is.defined(grp)) num <- num[seq(length.out = grp - 1)]

  # now all num corresponds to prime vars (y if any, x)
  # the last of these is x

  stopifnot(length(num) > 0)
  if(length(num) == 1 & !num[[1]])return(do.call(match.fun(categorical), args))
  if(length(num) == 1 & num[[1]]) return(do.call(match.fun(univariate), args))
  # now length num is at least 2
  if(xor(num[[1]],num[[2]])) return(do.call(match.fun(mixedvariate),args))
  if(!num[[1]] && !num[[2]]) return(do.call(match.fun(categorical),args))
  # now there are no categoricals in the first two positions
  # that means there are two numerics in first two positions
  # that means there is at least one y and at least one x
  # find x.
  xpos <- length(num)
  ypos <- 1:(xpos - 1)
  yguide <- guide[ypos]
  xguide <- guide[xpos]
  # we only need to choose between multivariate and bivariate.
  # if all yguide identical and distinct from xguide, we collapse to bivariate overlay.
  overlay <- FALSE
  if(length(unique(yguide)) == 1 &  paste(yguide[[1]]) != paste(xguide) ) overlay <- TRUE
  if(length(ypos) > 1 & !overlay) return(do.call(match.fun(multivariate),args))
  # now univariate, mixedvariate, and multivariate plots have been dismissed.
  # only bivariate remains
  return(do.call(match.fun(bivariate),args))
}


