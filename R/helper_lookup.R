#' Character frame
#'
#' Convert a data frame or matrix into a new representation where all values
#' are a single character.
#'
#' @param x data.frame or matrix
#' @details \code{sptable} objects only allow for single
#' character-representations and \code{char_frame} helps achieve this.
#' Using \code{lookup} in a \code{char_frame} yields a lookup table
#' such that new and old values can be compared and looked up. To find
#' a particular value use \code{find.}
#' @examples
#'
#' # Note: asia is already on correct form, so
#' # no need to use char_frame in principle
#' 
#' ca <- char_frame(asia[, 1:3])
#' sptable(as.matrix(ca))
#'
#' # retrieve the old dimnames:
#' lu <- lookup(ca)
#'
#' # find a particular combination
#' find(lu, c(A = "y", S = "y", T = "n"))
#' @export
char_frame <- function(x) UseMethod("char_frame")

#' @rdname char_frame
#' @export
char_frame.data.frame <- function(x) {
  # Implicitly assumes that no columns has more than 62 unique levels

  if (!is_character_frame(x)) stop("some columns in x are not of type character")
  
  is_matrix <- inherits(x, "matrix")

  if (is_matrix) x <- as.data.frame(x, stringsAsFactors = FALSE)

  old_vals <- lapply(x, unique)
  new_vals <- lapply(old_vals, function(z) structure(possible_chars(length(z)), names = z))

  x[] <- lapply(colnames(x), function(z) {
    xz <- as.factor(x[, z])
    levels(xz) <- new_vals[[z]][match(levels(xz), names(new_vals[[z]]))]
    as.character(xz)
  })

  x <- if (is_matrix) as.matrix(x) else x
  class(new_vals) <- c("lookup", class(new_vals))
  structure(x, lookup = new_vals, class = c("char_frame", class(x)))
}

char_frame.matrix <- char_frame.data.frame


#' Lookup
#'
#' @param x lookup object returned from char_frame or char_array
#' @export
lookup <- function(x) UseMethod("lookup")

#' @rdname lookup
#' @export
lookup.char_frame <- function(x) attr(x, "lookup")

#' @rdname lookup
#' @export
lookup.char_array <- lookup.char_frame

#' Find
#'
#' @param x lookup object returned from char_frame or char_array
#' @param y named vector
#' @export
find <- function(x, y) UseMethod("find")

#' @rdname find
#' @export
find.lookup <- function(x, y) {
  # x: lookup 
  # y: c(var1 =  val1, var2 = val2, ...)

  if (!(all(names(y) %in% names(x)))) stop("one or more names of y are not valid")

  y_sub <- x[names(y)]
  cnt <- 1L

  lapply(y_sub, function(ys) {
    out <- ys[y[cnt]]
    if (is.na(out)) stop(glue::glue("'{y[cnt]}' is not a value in variable '{names(y[cnt])}'"))
    cnt <<- cnt + 1L
    out
  })
}


#' Character array
#'
#' Convert an array-like object into a new representation where all values
#' are a single character.
#'
#' @param x array, matrix or table
#' @details \code{sptable} objects only allow for single
#' character-representations and \code{char_frame} helps achieve this 
#' @examples
#' 
#' ca <- char_array(HairEyeColor)
#'
#' # retrieve the old dimnames:
#' lu <- lookup(ca)
#'
#' # find a particular combination
#' find(lu, c(Eye = "Brown", Sex = "Male", Hair = "Red"))
#' @export
char_array <- function(x) UseMethod("char_array")

char_array.array <- function(x) {
  # x: an array-like type. See .allowed_cpt_classes()
  
  # TODO: Make a vanilla print method that does not print the lookup object.

  old_names <- dimnames(x)
  
  if (inherits(x, "matrix")) {
    if (!is_named_list(old_names)) {
      # TODO: Just check for is.null(old_names[[1]]) ?
      return(char_frame.matrix(x))
    }
  }
 
  dimnames(x) <- lapply(dimnames(x), function(y) {possible_chars(length(y))})

  lu <- mapply(
    function(a, b) structure(a, names = b),
    dimnames(x),
    old_names,
    SIMPLIFY = FALSE
  )
  
  class(lu) <- c("lookup", class(lu))
  attr(x, "lookup") <- lu
  
  structure(x, lookup = lu, class = c("char_array", class(x)))
}

#' @rdname char_array
#' @export
char_array.matrix <- char_array.array

#' @rdname char_array
#' @export
char_array.table  <- char_array.array

#' Subset lookup
#'
#' @param x lookup object
#' @param i integer vector
#' @export
`[.lookup` <- function(x, i) {
  structure(NextMethod(x[i]), class = class(x))
}


#' Subset char frame
#'
#' @param x \code{char_frame} object
#' @param i First index for subsetting. Behaves as an ordinary data frame
#' @param j Second index for subsetting. Behaves as an ordinary data frame
#' @param ... For S3 compatability
#' @param drop If TRUE the result is coerced to the lowest possible dimension
#' @export
`[.char_frame` <- function(x, i, j, ..., drop = FALSE) {
  lu <- lookup(x)[j]
  structure(NextMethod(x[i, j, ..., drop = FALSE]), lookup = lu)
}

## d <- data.frame(
##   x = sample(c("This", "is", "x"), 10,TRUE),
##   y = sample(c("Liver", "pool"), 10,TRUE),
##   z = sample(c("Frej", "lev"), 10,TRUE),
##   stringsAsFactors = FALSE
## )

## x <- char_frame(as.matrix(d))
## y <- char_array(asia2[[4]])

## lu <- attr(x, "lookup")
## find(lu, c(x = "This", y = "Liver"))


## lx <- lookup(x)
## ly <- lookup(y)

## find(ly, c(smoke = "yes", lung = "no"))
