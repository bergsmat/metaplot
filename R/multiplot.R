#' Arrange Multiple Trellis or GG Plots in a Grid
#'
#' Arranges mutiple trellis plots or ggplots in a grid, automatically choosing number of rows and columns.  By default, number of rows is one less than or equal to the number of columns.
#'
#' @export
#' @param ... trellis objects
#' @param nrow number of rows of plots
#' @param ncol number of columns of plots
#' @importFrom gridExtra grid.arrange
#' @examples
#' library(lattice)
#' a <- xyplot(
#' conc ~ Time,
#' xlab=NULL,
#' ylab = NULL,
#' Theoph,
#' aspect = 1,
#' scales=list(draw=FALSE)
#' )
#' multiplot(a,a,a,a,a,a)
#' multiplot(a,a,a,a,a,a,a)
#' multiplot(a,a,a,a,a,a,a,a)
#' multiplot(a,a,a,a,a,a,a,a,a)
#' multiplot(a,a,a,a,a,a,a,a,a,a)
#' multiplot(a,a,a,a,a,a,a,a, nrow = 2)
#' multiplot(a,a,a,a,a,a,a,a, ncol = 4)
#' multiplot(a,a,a,a,a,a,a,a, ncol = 2)
#' multiplot(a,a,a,a,a,a,a,a, ncol = 4, nrow = 3)
multiplot <- function(..., nrow = NULL, ncol = NULL){
  x <- list(...)
  len <- length(x)
  nms <- seq_along(x)
  class <- unique(sapply(x,class))
  if(!is.null(class))stopifnot(sapply(x,inherits,'trellis') | sapply(x,inherits,'ggplot'))
  #gg <- inherits(x[[1]],'ggplot')
  root <- sqrt(len)
  if(!is.null(nrow)){
    if(is.null(ncol)) {
      ncol <- ceiling(len/nrow)
    }else {
      stopifnot(nrow * ncol >= len)
    }
  }
  # now ncol is defined if nrow is defined.
  if(is.null(ncol)) ncol <- ceiling(root)
  # now ncol is defined for sure
  if(is.null(nrow)) nrow <- ceiling(len/ncol)
  # now nrow is defined for sure

  #if(gg)return(do.call(grid.arrange, c(x,list(ncol = ncol))))
  return(do.call(grid.arrange, c(x,list(ncol = ncol))))
#
#   y <- expand.grid(
#     run = seq_len(ncol),
#     rise = seq_len(nrow)
#   )
#   stopifnot(nrow(y) >= len)
#   y <- y[nms,]
#   printOne <- function(z,x,y,nx,ny,more)print(z,split=c(x,y,nx,ny),more = more)
#   for(i in nms)printOne(
#     z = x[[i]],
#     x = y[i,'run'],
#     y = y[i,'rise'],
#     nx = ncol,
#     ny = nrow,
#     more = !(i == len)
#   )
}
