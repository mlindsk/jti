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

possible_chars <- function(n) {
  chars <- c(letters, LETTERS, 0:9)
  if (n > length(chars)) stop("One or more variables have more than 62 levels.")
  return(chars[1:n])
}

## ---------------------------------------------------------
##                     EXPORTED HELPERS
## ---------------------------------------------------------

## #' Convert discrete values into a single character representation
## #'
## #' Convert all values in a data frame or matrix of characters to a single character representation
## #'
## #' @param x Data frame or matrix of characters
## #' @examples
## #' d <- data.frame(x = c("11", "2"), y = c("2", "11"))
## #' to_chars(d)
## #' @export
## to_chars <- function(x) {
##   # Implicitly assumes that no columns has more than length(chars) = 62 unique levels
##   # Consider saving the olde levels so we can retrive them again easily later
##   stop("deprectated. Use char_frame!") 
##   apply(x, 2, function(z) {
##     f <- as.factor(z)
##     levels(f) <- possible_chars(length(levels(f)))
##     as.character(f)
##   })
## }


## #' To come
## #'
## #' To come
## #'
## #' @param x Named list of arrays
## #' @examples
## #' # TBA
## #' @export
## dimnames_to_chars <- function(x) {
##   stop("deprectated. Use char_frame!") 
##   lapply(x, function(y) {
##     dimnames(y) <- lapply(dimnames(y), function(z) {
##       possible_chars(length(z))
##     })
##     y
##   })
## }

