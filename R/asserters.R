## SETS
neq_empt_chr <- function(x) !identical(x, character(0))
neq_empt_num <- function(x) !identical(x, numeric(0))
neq_empt_int <- function(x) !identical(x, integer(0))
neq_empt_lst <- function(x) !identical(x, list())
neq_null     <- function(x) !is.null(x)
'%ni%'       <- Negate('%in%')

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

only_chars <- function(A) UseMethod("only_chars")

only_chars.data.frame <- function(A) {
  all(apply(A, 2L, function(x) {
    nc <- nchar(paste0(x, collapse = ""))
    nc == nrow(A)
  }))
}

only_chars.array <- function(A) {
  if (inherits(A, "matrix")) {
    if (!is_named_list(dimnames(A))) { # non-array-like matrix
      return(only_chars.data.frame(as.data.frame(A)))
    }
  }
  all(.map_lgl(dimnames(A), function(x) {
    sum(nchar(x)) == length(x)
  }))
}

only_chars.matrix <- only_chars.array

only_chars.table  <- only_chars.array

is_character_frame <- function(x) {
  stopifnot(is.matrix(x) || is.data.frame(x))
  all(.map_lgl(x, is.character))
}
