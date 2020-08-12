#' Sparse table
#'
#' A sparse contingency table representation of a data frame or matrix.
#'
#' @param x Data frame or matrix of characters
#' @param validate Logical. If TRUE, it checks whether or not the values of all variables
#' in \code{A} are constrained to a single character. If not, an error is produced. 
#' @seealso \code{\link{to_cpt}}
#' @details The reason for the values to be constrained to a single character is due to
#' an increase in performance.
#' @examples
#' sptable(as.matrix(asia[, 1:3]))
#' @export
sptable <- function(x, validate = TRUE) {
  x <- if (inherits(x, "data.frame")) as.matrix(x) else x
  if (validate) {
    if (is.null(colnames(x))) stop("x must have column names")
    if (!is_character_frame(x)) stop("x can only have character variables")    
  }
  sptab <- sptab_(x, validate)
  class(sptab) <- c("sptable", class(sptab))
  return(sptab)
}

#' Conditional probability table
#'
#' Returns a sparse conditional probability table conditioned on variables in \code{b}.
#'
#' @param x A sparse table obtained from \code{sptable}
#' @param y Character vector with variables to condition on
#' @details If \code{y} is \code{NULL}, \code{x} is just converted to a \code{sptable} with no conditioning variables, i.e. the marginal table.
#' @return A \code{sptable} object
#' @seealso \code{\link{sptable}}
#' @examples
#' sp  <- sptable(as.matrix(asia[, 1:3]))
#' psp <- to_cpt(sp, c("S", "T"))
#' sum(psp) # Since (S,T) have 4 configurations
#' @export
to_cpt <- function(x, y = NULL) UseMethod("to_cpt")

#' @rdname to_cpt
#' @export
to_cpt.sptable <- function(x, y = NULL) {

  if (is.null(y) || !neq_empt_chr(y)) {
    sum_x <- sum(x)
    parr <- list2env(eapply(x, function(x_) x_ / sum_x))
    class(parr) <- class(x)
    attr(parr, "vars") <- attr(x, "vars")
    return(parr)
  }

  if (any(y %ni% attr(x, "vars"))) stop("some elements in y are not in x")
  
  pos  <- match(y, attr(x, "vars"))
  conf <- .find_cond_configs(x, pos)
  conditional_list <- split(names(conf), conf)
  
  parr <- list2env(as.list(unlist(unname(lapply(conditional_list, function(e) {
    xe <- x[e]
    sum_xe <- sum(xe)
    eapply(xe, function(xe_) xe_ / sum_xe)
  })))))
  
  attr(parr, "vars") <- attr(x, "vars")
  class(parr) <- class(x)
  return(parr)
}


## ## # Tell CMD check from what namespace we want head
## head <- utils::head

## #' Head
## #' 
## #' @param x sptable
## #' @param ... for S3 compatability
## #' @examples
## #' sp <- sptable(asia[, 1:3])
## #' head(sp)
## #' @rdname head.sptable
## #' @export
## head.sptable <- function(x, ...) {
##   args  <- list(...)
##   n <- if ("n" %in% names(args)) args[["n"]] else 10L
##   vars  <- attr(x, "vars")
##   nchr  <- sum(.map_int(vars, function(s) nchar(s))) + length(vars) + nchar("Vars:") - 1
##   N     <- length(x)
##   cells <- names(x)
##   cat("", paste0(rep("-", nchr), collapse = ""), "\n")
##   cat(" Vars:", paste0(vars, collapse = "-"), "\n")
##   cat("", paste0(rep("-", nchr), collapse = ""), "\n")
##   cells_n <- if (N < n) cells else cells[1:n]
##   for (cell in cells_n) {
##     cat(cell, ":", x[[cell]], "\n")
##   }
##   invisible(NULL)
## }

#' Print
#' 
#' @param x sptable
#' @param ... For S3 compatability.
#' @examples
#' sp <- sptable(asia[, 1:3])
#' print(sp)
#' @export
print.sptable <- function(x, ...) {
  vars  <- attr(x, "vars")
  nchr  <- sum(.map_int(vars, function(s) nchar(s))) + length(vars) + nchar("Vars:") - 1
  N     <- length(x)
  cells <- names(x)
  cat("", paste0(rep("-", nchr), collapse = ""), "\n")
  cat(" Vars:", paste0(vars, collapse = "-"), "\n")
  cat("", paste0(rep("-", nchr), collapse = ""), "\n")
  if (inherits(x, "unity_sptable")) {
    cat(" <unity_stable> \n")
  } else {
    for (cell in cells) {
      cat(cell, ":", x[[cell]], "\n")
    }
  }
  invisible(NULL)
}

# export?
# set_value: x[["el"]] <- value
# - how to set multiple values?


#' Subset sptable

#' @param x sptable
#' @param i character vector
#' @param result 'sptable' or 'value'. The former returns a new
#' sptable and the latter returns a vector of counts.
#' @examples
#' sp <- sptable(as.matrix(asia[, 1:3]))
#' sp[c("yny", "nyy")]
#' sp[c("yny", "nyy"), "value"]
#' @export
`[.sptable` <- function(x, i, result = "sptable") {
  # result: 'sptable' or 'value'
  if (!is.character(i)) stop("an object of result 'sptable' is only subsettable via characters")
  if (result %ni% c("sptable", "value")) stop("result must be 'sptable' or 'value'")
  y <- mget(i, x, ifnotfound = 0L) 
  if (result == "sptable") {
    out <- list2env(y)
    class(out) <- class(x)
    attr(out, "vars") <- attr(x, "vars")
    return(out)
  } else {
    return(unlist(y))
  }
}


#' Vector-like operations on sptables

#' @param x sptable
#' @param ... For S3 compatability.
#' @examples
#' sp <- sptable(as.matrix(asia[, 1:3]))
#' print(sp)
#' sum(sp) # = nrow(asia)
#' min(sp)
#' max(sp)
#' which_min(sp)
#' which_max(sp)

#' @rdname vec-ops
#' @export
sum.sptable <- function(x, ...) {
  sum(x[ls(envir = x), "value"])
}

#' @rdname vec-ops
#' @export
max.sptable <- function(x, ...) {
  max(x[ls(envir = x), "value"])
}

#' @rdname vec-ops
#' @export
min.sptable <- function(x, ...) {
  min(x[ls(envir = x), "value"])
}

#' @rdname vec-ops
#' @export
which_min <- function(x) UseMethod("which_min")

#' @rdname vec-ops
#' @export
which_min.sptable <- function(x) {
  xvec <- x[ls(envir = x), "value"]
  names(which(xvec == min(xvec)))
}

#' @rdname vec-ops
#' @export
which_max <- function(x) UseMethod("which_max")

#' @rdname vec-ops
#' @export
which_max.sptable <- function(x) {
  xvec <- x[ls(envir = x), "value"]
  names(which(xvec == max(xvec)))
}
