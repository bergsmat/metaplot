globalVariables('groups_')
globalVariables('META')
globalVariables('VARIABLE')
globalVariables('VALUE')
globalVariables('collapse')
globalVariables('panel_')

#' Metaplot
#'
#' Metaplot creates univariate, bivariate, or multivariate plots depending on the number and types of variables represented by the anonymous arguments.  Types are either numeric (NUM, e.g. real, integer) or categorical (CAT, e.g. factor, character).  A variable stored as numeric that nonetheless has an \code{\link[encode]{encode}}d \code{guide} attribute will be treated as categorical. Mnemonic: \code{ x \%>\% metaplot(yvars, xvar, groupvar, facets)} where arguments are unquoted column names, and only xvar is required.  Column attributes \code{label}, \code{guide}, \code{reference}, and \code{symbol} modify the behavior of the default handlers.
#'
#' Design your plot by specifying y variables (optional), the x variable, the groups variable (optional) and the conditioning variables (i.e., facets, optional).
#'
#' The single groups variable, if any, is the first categorical in the third position or later. An earlier categorical gives a "mixed" bivariate plot, e.g. horizontal boxplot (first position) or vertical boxplot (second postion).
#'
#' The x variable is the last variable before groups, if present.
#'
#' The y variables are those before x. If none, the result is univariate. If one, the result is typically a boxplot or scatterplot, depending on x. Several numeric y followed by a numeric x are treated as multivariate (scatterplot matrix).  But if all y have the same \code{guide} attribute and it is different from that for x, the result is bivariate (i.e, an \code{overlay} scatterplot).
#'
#' Wherever a groups argument is meaningful, it may be missing.  This allows specification of facets in the absence of groups, e.g., \code{(metaplot(y, x, , facet1, facet2))}.  For multiple y (overlay), the sources of y are the implied groups: any trailing categorical arguments are treated as facets.
#'
#' Template designs follow; substitute behaviors by setting global options (see argument list).
#'
#'\itemize{
#' \item{NUM:}{ univariate (densityplot) }
#'
#' \item{CAT:}{ categorical (unimplemented) }
#'
#'\item{CAT, CAT:}{ categorical (unimplemented)}
#'
#' \item{NUM, CAT:}{ mixedvariate (vertical boxplot)}
#'
#' \item{CAT, NUM:}{ mixedvariate (horizontal boxplot)}
#'
#' \item{CAT, NUM, CAT:}{ mixedvariate with one facet}
#'
#' \item{NUM, NUM:}{ bivariate (scatterplot)}
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
#' Variable attributes may be supplied by conventional means;  \code{\link{pack}} and \code{\link{unpack}} support storing and retrieving scalar column attributes.  The following scalar attributes are currently supported.
#' \itemize{
#' \item{label:}{ A variable descriptor.  If present, panel functions will use label to create informative axis labels. See \code{\link{axislabel}}. }
#' \item{guide:}{ Units for a numeric variable, or an encoding (scalar string giving codes and possibly decodes) for a categorical item.  If present, units will be used to inform the corresponding axis label (\code{\link{axislabel}}).  If present, codes will be used to impose sort order on categorical variables.  If present, decodes will be used as substitutes for stored values when presenting categorical labels, legends, and facet names. For more on encodings, see \code{\link[encode]{encode}}.}
#' \item{reference:}{ Some variables have values to which they can be compared.  For example, residual error is often expected to be centered at zero. Default panel functions plot corresponding reference lines if this attribute is present. See for example \code{\link{dens_panel}}.}
#' \item{symbol:}{ Variable names are useful for programming, and variable labels are useful as axis labels.  A symbol can be more formal than a variable name and more compact than a label.  For example, \code{\link{diag_label}} will use variable names as labels for the diagonal panels of a scatterplot matrix; but it will prefer labels, if available; and will prefer symbols most of all. Markup rules for symbols are given in \code{\link{wikisym2plotmathOne}}.}
#'}

#'
#' @param x object
#' @param ... passed arguments
#' @export
#' @family generic functions
#' @family metaplot
#' @examples
#'
#' library(magrittr)
#' library(dplyr)
#' library(csv)

#' x <- as.csv(system.file(package = 'metaplot', 'extdata/theoph.csv'))
#' x %<>% pack


#' # sample plots
#' x %>% metaplot(sres)
#'#x %>% metaplot(site)
#' x %>% metaplot(Wt, arm)
#' x %>% densplot(Wt, arm)
#' x %>% metaplot(arm, Wt)
#' x %>% metaplot(Wt, arm, site, ref = 70)
#' x %>% metaplot(Wt, site, arm, ref = 70)
#' x %>% metaplot(conc, Time)
#' x %>% metaplot(conc, Time, panel = panel.smoothScatter)
#'#x %>% metaplot(arm, site)
#' x %>% metaplot(conc, Time, Subject)
#' x %>% metaplot(conc, Time, , Subject)
#' x %>% metaplot(conc, Time, Subject, site)
#' x %>% metaplot(conc, Time, Subject, site, arm)
#' x %>% metaplot(lKe, lKa, lCl)
#' x %>% metaplot(
#'   lKe, lKa, lCl,
#'   col = 'black',loess.col = 'red', pin.col = 'red',
#'   dens.col='blue',dens.alpha = 0.1
#' )
#' x %>% metaplot(conc, pred, ipred, Time)
#' x %>% metaplot(conc, pred, ipred, Time, Subject)
#' x %>% metaplot(conc, pred, ipred, Time, Subject,
#' colors = c('black','blue','magenta'),
#' points = c(0.9,0, 0.4),
#' lines = c(F,T,T))
#' x %>% metaplot(conc, ipred, Time, site, arm)
#' x %>% metaplot(res, conc, yref = 0, ysmooth = T, conf = T, grid = T, loc = 1)
#' x %>% metaplot(res, conc, arm, ysmooth = T, conf = T )
#' x %>% metaplot(res, conc, arm, ysmooth = T, conf = T, global = T, refcol = 'red')
#'
metaplot <- function(x,...)UseMethod('metaplot')


#' Create Metaplot for Data Frame.
#'
#' Creates a metaplot for class 'data.frame'.  Implements a rule to decided whether to make a density plot, a boxplot, a scatter plot, or a scatterplot matrix, given the supplied column names.
#'
#' @param x object
#' @param univariate function for univariate arguments
#' @param mixedvariate function for bivariate combinations of numeric and categoral arguments
#' @param bivariate function for arguments that resolve to two numerics (see rules)
#' @param multivariate function for more than two numeric arguments
#' @param categorical function for categorical arguments
#' @param ... passed arguments
#' @import encode
#' @family methods
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
#' x %<>% mutate(pred = predict(m1,level = 0) %>% signif(4))
#' x %<>% mutate(ipred = predict(m1) %>% signif(4))
#' x %<>% mutate(res = residuals(m1) %>% signif(4))
#' x %<>% mutate(sres = residuals(m1, type = 'pearson') %>% signif(4))
#' r <- ranef(m1) %>% signif(4)
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
#' attr(x$ipred,'label') <- 'individual-predicted concentration'
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
#'
#' attr(x$lKe,'reference') <- 0
#' attr(x$lKa,'reference') <- 0
#' attr(x$lCl,'reference') <- 0
#' attr(x$res,'reference') <- 0
#' attr(x$sres,'reference') <- '//-1.96//1.96//'
#'
#' attr(x$Subject,'symbol') <- 'ID_i'
#' attr(x$Wt,'symbol') <- 'W_i'
#' attr(x$Dose,'symbol') <- 'A_i'
#' attr(x$Time,'symbol') <- 't_i,j'
#' attr(x$conc,'symbol') <- 'C_i,j'
#' attr(x$arm,'symbol') <- 'Arm_i'
#' attr(x$site,'symbol') <- 'Site_i'
#' attr(x$pred,'symbol') <- 'C_pred_p'
#' attr(x$ipred,'symbol') <- 'C_pred_i'
#' attr(x$res,'symbol') <- '\\epsilon'
#' attr(x$sres,'symbol') <- '\\epsilon_st'
#' attr(x$lKe,'symbol') <- 'ln(K_e.)'
#' attr(x$lKa,'symbol') <- 'ln(K_a.)'
#' attr(x$lCl,'symbol') <- 'ln(Cl_c./F)'
#'
#'
#' x %>% unpack %>% as.csv('theoph.csv')
#' }

#'\dontshow{
#'\dontrun{
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
#' y %>% metaplot(arm, Wt,site)
#' y %>% metaplot(conc, Time)
#'#y %>% metaplot(arm, site)
#' y %>% metaplot(conc, Time, Subject)
#' y %>% metaplot(conc, Time, , Subject)
#' y %>% metaplot(conc, Time, Subject, site)
#' y %>% metaplot(conc, Time, Subject, site, arm)
#' y %>% metaplot(lKe, lKa, lCl)
#' y %>% scatter(conc, ipred, Time)
#' y %>% scatter(conc, ipred, Time, Subject)
#' x %>% metaplot(conc, ipred, Time, Subject, colors = 'black', points = c(T,F), lines = c(F,T))
#' y %>% scatter(conc, ipred, Time, site, arm)
#' y %<>% mutate(Time = ifelse(Time > 15, NA, Time))
#' y %>% scatter(conc, ipred, Time, site, arm)
#'}}
#'
metaplot.data.frame <- function(
  x,
  ...,
  univariate   = getOption('metaplot_univariate','densplot'),
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


