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
