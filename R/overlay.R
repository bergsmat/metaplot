#' Create an Overlay Plot
#'
#' Creates an Overlay Plot. Generic, with methods for classes 'folded' and 'data.frame'
#' @export
#' @param x object
#' @param ... passed arguments
#' @seealso \code{\link{overlay.folded}}
#' @seealso \code{\link{overlay.data.frame}}
overlay <- function(x,...)UseMethod('overlay')

#' Create an Overlay Plot for Folded
#'
#' Creates an overlay plot for class 'folded'.  Continuous items are plotted vs last continuous item, conditioned (facetted) by up to two categorical items.
#' @export
#' @param x folded. See \code{\link[fold]{fold}}.
#' @param ... unquoted names of items to plot
#' @param ncol number of panel columns if only one conditioning variable
#' @param nrow number of panel rows if only one conditioning variable
#' @param color color for points and lines, recycled as necessary
#' @param points logical: whether to plot points, recycled as necessary
#' @param line character: line types ('none','solid','dashed', etc.  See \code{\link[ggplot2]{geom_line}}). Recycled as necessary
#' @param ylab y axis label to override default
#' @param xlab x axis label to override default
#' @param ylog whether to log-transform y axis data
#' @param xlog whether to log-transform x axis data
#' @import ggplot2
#' @return a list of ggplot, possibly length one
#' @seealso \code{\link{overlay.data.frame}}
overlay.folded <- function(
  x,
  ...,
  ncol = 4,
  nrow = 4,
  color = 'black',
  points = TRUE,
  line = 'solid',
  ylab = NULL,
  xlab = NULL,
  ylog = FALSE,
  xlog = FALSE
){
  var <- quos(...)
  var <- lapply(var, f_rhs)
  item <- var[names(var) == '']
  item <- sapply(item,as.character)
  named <- var[names(var) != '']
  x %<>% filter(VARIABLE %in% item)
  cont <- sapply(item,function(nm)continuous(x,nm))
  cat <- !cont
  abscissa <- rev(item[cont])[[1]]
  ordinate <- setdiff(item[cont], abscissa)
  if(is.null(ylab)){
    ylab <- sapply(ordinate,function(o)axislabel(x,var = o, log = ylog))
    ylab <- ylab[!duplicated(ylab)]
    ylab <- paste(ylab, collapse = '\n')
  }
  if(is.null(xlab)) xlab <- axislabel(x,var = abscissa)
  data <- x %>% filter(is.na(META))
  data %<>% unfold(UQS(item))
  data %<>% ungroup %>% select(item) # ungroup should not be necessary
  for(i in item)if(!continuous(x,i))data[[i]] <- factor(data[[i]])

  this <- list(x=data)
  addl <- list(
    ncol = ncol,
    nrow = nrow,
    color = color,
    points = points,
    line = line,
    ylab = ylab,
    xlab = xlab,
    ylog = ylog,
    xlog = xlog
  )
  out <- c(this,named,addl)
  do.call(overlay,out)
}

#' Create an Overlay Plot for Data Frame
#'
#' Creates an overlay plot for class 'data.frame'. Continuous items are plotted vs last continuous item in \code{names(x)}, conditioned (facetted) by up to two categorical items.
#' @export
#' @param x folded. See \code{\link[fold]{fold}}.
#' @param ncol number of panel columns if only one conditioning variable
#' @param nrow number of panel rows if only one conditioning variable
#' @param color color for points and lines, recycled as necessary
#' @param points logical: whether to plot points, recycled as necessary
#' @param line character: line types ('none','solid','dashed', etc.  See \code{\link[ggplot2]{geom_line}}). Recycled as necessary
#' @param ylab y axis label to override default
#' @param xlab x axis label to override default
#' @param ylog whether to log-transform y axis data
#' @param xlog whether to log-transform x axis data
#' @param ... currently passed to \code{\link[ggplot2]{facet_wrap}} or \code{\link[ggplot2]{facet_grid}}
#' @return a ggplot, or list of ggplot as required to support panel_by
#' @seealso \code{\link{overlay.folded}}
overlay.data.frame <- function(
  x,
  ncol = 4,
  nrow = 4,
  color = 'black',
  points = TRUE,
  line = 'solid',
  ylab = NULL,
  xlab = NULL,
  ylog = FALSE,
  xlog = FALSE,
  ...
){
  cont <- sapply(x,is.numeric)
  cat <- !cont
  abscissa <- rev(names(x)[cont])[[1]]
  ordinate <- setdiff(names(x)[cont], abscissa)
  panel_by <- names(x)[cat]
  trend <- data.frame(ordinate = ordinate,stringsAsFactors = FALSE)
  trend$color = rep(color, length.out = nrow(trend))
  trend$points = rep(points, length.out = nrow(trend))
  trend$line = rep(line, length.out = nrow(trend))
  if(xlog) x[abscissa] <- log(x[abscissa])
  if(ylog) for(o in ordinate) x[o] <- log(x[o])
  x$panel_ <- ''
  if(sum(cat) == 1) x$panel_ <- x[,cat]
  if(sum(cat) > 1)  x$panel_ <- do.call(paste,c(x[cat & cumsum(cat) <= 2],list(sep = '@')))
  panel <- unique(x$panel_)
  dex <- seq_along(panel)
  count = nrow * ncol # for facet_wrap
  num_plots <- ceiling(length(panel) / count)
  if(length(cat) != 1) num_plots <- 1 # only 1 plot for 2-way or 0-way facetting
  out <- list()
  for(i in seq_len(num_plots)){
    max_this <- i * count
    max_prev <- (i - 1) * count
    these <- seq(from = max_prev + 1, to = max_this)
    okay <- intersect(these, dex)
    chosen <- panel[okay]
    if(sum(cat) != 1) chosen <- panel # 2-way or zero-way facetting
    slice <- x %>% filter(panel_ %in% chosen)
    plt <- ggplot(data = slice, aes_string(x = abscissa, y = trend[1,'ordinate']))
    for(o in rownames(trend)){
      ord <- trend[o,'ordinate']
      pts <- trend[o,'points']
      lty <- trend[o,'line']
      col <- trend[o,'color']
      if(pts)           plt <- plt + geom_point(aes_string(y = ord), color = col)
      if(lty != 'none') plt <- plt + geom_line (aes_string(y = ord), color = col, lty = lty)
    }
    facet <- names(cat)[cat & cumsum(cat) <=2]
    if(sum(cat) == 1) plt <- plt + facet_wrap(facet, ncol = ncol, nrow = nrow,...)
    if(sum(cat)  > 1) plt <- plt + facet_grid(facet, ...)
    plt <- plt  +  ylab(ylab) + xlab(xlab)
    out[[i]] <- plt
  }
  out
}
