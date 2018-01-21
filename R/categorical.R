
#' Categorical Plot
#'
#' Categorical Plot. Generic, with method for 'data.frame'.
#' @export
#' @param x object of dispatch
#' @param ... passed arugments
#' @family generic functions
#' @family categorical
categorical <- function(x, ...)UseMethod('categorical')

#' Categorical Method for Data Frame
#'
#' Categorical method for 'data.frame'.  Currently unimplemented. Returns a named vector indicating whether anonymous arguments were detected as numeric or categorical.
#'
#' @param x data.frame
#' @param ... other arguments
#' @family categorical
#' @family methods
#' @return character
#' @export
#' @importFrom rlang quos
#'
categorical.data.frame <- function(x,...){
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
