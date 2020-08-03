char_frame <- function(x) UseMethod("char_frame")

char_frame.data.frame <- function(x) {
  # Implicitly assumes that no columns has more than 62 unique levels

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

lookup <- function(x) UseMethod("lookup")

lookup.char_frame <- function(x) attr(x, "lookup")

lookup.char_array <- lookup.char_frame

find <- function(x, y) UseMethod("find")

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

char_array.matrix <- char_array.array

char_array.table  <- char_array.array


`[.char_frame` <- function(x, i, j, ...) {
  lu <- lookup(x)[j]
  structure(NextMethod(x[i, j, ...]), lookup = lu)
}

`[.lookup` <- function(x, i) {
  structure(NextMethod(x[i]), class = class(x))
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
