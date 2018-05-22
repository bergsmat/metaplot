#' Pack Something
#'
#' Pack Something.  Generic, with method for data.frame.
#'
#' @export
#' @param x object
#' @param ... other arguments
#' @family generic functions
#' @family pack
pack <- function(x,...)UseMethod('pack')


#' Capture Scalar Column Metadata as Column Attributes
#'
#' Captures scalar column metadata (row values) as column attributes.  Excises rows with non-missing values of \code{meta}, converting column values to column attributes. Afterward, column classes are re-optimized using default behavior of \code{read.table}. It is an error if \code{meta} is not in \code{names(x)}.
#'
#' @param x data.frame
#' @param meta column in x giving names of attributes
#' @param as.is passed to \code{\link[utils]{type.convert}}
#' @param attributes preserve non-standard attributes (ignores names, row.names, class)
#' @param ... ignored arguments
#' @export
#' @importFrom utils type.convert
#' @return data.frame
#' @family pack
#' @family methods
#' @examples
#' foo <- data.frame(head(Theoph))
#' attr(foo$Subject, 'label') <-  'subject identifier'
#' attr(foo$Wt, 'label') <-  'weight'
#' attr(foo$Dose, 'label') <-  'dose'
#' attr(foo$Time, 'label') <-  'time'
#' attr(foo$conc, 'label') <-  'concentration'
#' attr(foo$Subject, 'guide') <-  '////'
#' attr(foo$Wt, 'guide') <-  'kg'
#' attr(foo$Dose, 'guide') <-  'mg/kg'
#' attr(foo$Time, 'guide') <-  'h'
#' attr(foo$conc, 'guide') <-  'mg/L'
#' unpack(foo, pos = 1)
#' unpack(foo, pos = 2)
#' unpack(foo, pos = 3)
#' unpack(foo, pos = 4)
#' bar <- unpack(foo)
#' pack(bar)
#' attributes(pack(bar)$Subject)
pack.data.frame <- function(x, meta = getOption('meta','meta'), as.is = TRUE, attributes = TRUE, ...){
  stopifnot(meta %in% names(x))
  a <- attributes(x)
  a$row.names <- NULL
  a$names <- NULL
  a$class <- NULL
  i <- x[[meta]]
  y <- x[!is.na(i),]
  x <- x[is.na(i),]
  x[[meta]] <- NULL
  if(nrow(y) == 0) return(x)
  # have at least one non-missing value of meta
  # now that meta is excised, refigure column classes
  x[] <- lapply(x,type.convert, as.is = as.is)
  # distribute metadata
  y$meta <- as.character(y$meta)
  if(any(duplicated(y$meta)))stop('found duplicate metadata names')
  for(attr in y$meta){
    for(col in names(x)){
      val <- y[y$meta == attr, col]
      if(!is.na(val)) attr(x[[col]], attr) <- val
    }
  }
  if(attributes)for(at in names(a))attr(x,at) <- a[[at]]
  x
}

#' Unpack Something
#'
#' Unpack Something.  Generic, with method for data.frame.
#'
#' @family pack
#' @family generic functions
#' @export
#' @param x object
#' @param ... other arguments
unpack <- function(x,...)UseMethod('unpack')


#' Express Scalar Column Attributes as Column Metadata
#'
#' Expresses scalar column attributes as column metadata (row values).  Column with name \code{meta} is created to hold names of attributes, if any. A transposed table (sorted by attribute name) of scalar column attribute values (coerced to character) is bound to the existing data.frame (the attributes themselves are removed from columns).  Bind position is controlled by \code{position} such that the intersection of new rows and column occurs in the corresponding corner, numbered clockwise from top-left. Resulting column classes are character. It is an error if \code{meta} is already in \code{names(x)}.
#'
#' @param x data.frame
#' @param meta column in result giving names of attributes
#' @param position 1 (top-left), 2 (top-right), 3 (bottom-right), or 4 (bottom-left)
#' @param ignore character: attributes to ignore
#' @param ... ignored arguments
#' @export
#' @return data.frame
#' @family pack
#' @family methods
#' @importFrom dplyr bind_rows bind_cols
#' @return data.frame with all columns of class character
unpack.data.frame <- function(x, meta = getOption('meta','meta'), position = 1L, ignore = c('class','levels'), ...){
  stopifnot(length(position) == 1)
  stopifnot(length(meta) == 1)
  stopifnot(!meta %in% names(x))
  stopifnot(position %in% 1:4)
  stopifnot(is.character(ignore))
  y <- data.frame(x[0,,drop=FALSE],stringsAsFactors = FALSE)
  y[] <- lapply(y, as.character)
  y <- data.frame(t(y),stringsAsFactors = FALSE) # transpose
  for(col in names(x)){
    attr <- attributes(x[[col]])
    for(name in setdiff(names(attr),ignore)){
      val <- attr[[name]]
      if(length(val) == 1) {
        attr[[name]] <- NULL
        val <- as.character(val)
        y[col,name] <- val
      }
    }
    attributes(x[[col]]) <- attr
  }
  y <- data.frame(t(y), stringsAsFactors = FALSE) # back-transpose
  y[[meta]] <- row.names(y)
  x[] <- lapply(x, as.character)
  if(position %in% 1:2){ # meta rows top
    x <- bind_rows(y,x)
  } else{
    x <- bind_rows(x,y)
  }
  if(position %in% c(1,4)) x <- x[,union(meta, names(y))] #  meta col first
  x
}

