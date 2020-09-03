## ---------------------------------------------------------
##                NON-EXPORTED HELPERS
## ---------------------------------------------------------

## MAPS
.map_chr     <- function(x, fun, ...) vapply(X = x, FUN = fun, FUN.VALUE = character(1), ...)
.map_int     <- function(x, fun, ...) vapply(X = x, FUN = fun, FUN.VALUE = integer(1), ...)
.map_dbl     <- function(x, fun, ...) vapply(X = x, FUN = fun, FUN.VALUE = numeric(1), ...)
.map_lgl     <- function(x, fun, ...) vapply(X = x, FUN = fun, FUN.VALUE = logical(1), ...)
.map_lst     <- function(x, fun, ...) vapply(X = x, FUN = fun, FUN.VALUE = list(), ...)
neq_null     <- function(x) !is.null(x)

## STRINGS
str_rem <- function(s, pos) {
  # Vectorized removal of substrings
  # s: character vector
  .map_chr(strsplit(s, ""), function(x) {
    paste0(x[-pos], collapse = "")
  })
}

# TODO: Test if length(x) > 1L ? And throw an error?
.split_chars <- function(x) unlist(strsplit(x, ""))


## MISC
push         <- function(l, el, name = NULL) {
  # TODO: if el is a named list, we must take this into account
  c(l, structure(list(el), names = name))
}
