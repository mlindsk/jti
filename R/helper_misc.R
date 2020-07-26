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

## SETS
neq_empt_chr <- function(x) !identical(x, character(0))
neq_empt_num <- function(x) !identical(x, numeric(0))
neq_empt_int <- function(x) !identical(x, integer(0))
neq_empt_lst <- function(x) !identical(x, list())
neq_null     <- function(x) !is.null(x)
'%ni%'       <- Negate('%in%')
push         <- function(l, el, name = NULL) {
  # TODO: if el is a named list, we must take this into account
  c(l, structure(list(el), names = name))
}


## GRAPHS
as_adj_lst <- function(A) {
  Delta <- colnames(A)
  out <- lapply(seq_along(Delta), function(r) {
    Delta[as.logical(A[, r])]
  })
  names(out) <- Delta
  out
}

is_decomposable <- function(adj) {
  m <- try(mcs(adj), silent = TRUE)
  if( inherits(m, "list") ) return(TRUE)
    else return(FALSE)
}


## MISC
is_named_list <- function(x) {
  if (is.null(names(x))) return(FALSE)
  if ("" %in% names(x)) {
    return(FALSE) 
  } else {
    return(TRUE)
  }
}

only_chars <- function(A) {
  all(apply(A, 2L, function(x) {
    nc <- nchar(paste0(x, collapse = ""))
    nc == nrow(A)
  }))
}

possible_chars <- function(n) {
  chars <- c(letters, LETTERS, 0:9)
  if (n > length(chars)) stop("One or more variables have more than 62 levels.")
  return(chars[1:n])
}

## ---------------------------------------------------------
##                     EXPORTED HELPERS
## ---------------------------------------------------------

#' Convert discrete values into a single character representation
#'
#' Convert all values in a data frame or matrix of characters to a single character representation
#'
#' @param x Data frame or matrix of characters
#' @examples
#' d <- data.frame(x = c("11", "2"), y = c("2", "11"))
#' to_chars(d)
#' @export
to_chars <- function(x) {
  # Implicitly assumes that no columns has more than length(chars) = 62 unique levels
  # Consider saving the olde levels so we can retrive them again easily later
  apply(x, 2, function(z) {
    f <- as.factor(z)
    levels(f) <- possible_chars(length(levels(f)))
    as.character(f)
  })
}


#' To come
#'
#' To come
#'
#' @param x Named list of arrays
#' @examples
#' # TBA
#' @export
dimnames_to_chars <- function(x) {
  lapply(x, function(y) {
    dimnames(y) <- lapply(dimnames(y), function(z) {
      possible_chars(length(z))
    })
    y
  })
}
