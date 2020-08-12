#' Convert an array-like object to sptable
#'
#' @param x array-like type
#' @param validate Boolean. Check if \code{x} has valid dimnames
#' @examples
#' ca <- char_array(asia2[[4]])
#' sp <- as_sptable(ca)
#' sp
#' 
#' # char_array is used to turn the array into correct form, i.e.
#' # correct length of dimnames
#'
#' # retrieve the old dimnames:
#' lu <- lookup(ca)
#'
#' # find a particular combination
#' x <- find(lu, c(lung = "yes", smoke = "no"))
#' sp[paste(unlist(x), collapse = "")]          # Preserving sptable class
#' sp[paste(unlist(x), collapse = ""), "value"] # returning the value
#' @export
as_sptable <- function(x, validate = TRUE) UseMethod("as_sptable")

#' @rdname as_sptable
#' @export
as_sptable.array <- function(x, validate = TRUE)  {

  if (inherits(x, "sptable")) return(x)

  if (validate) {
    if (!is_named_list(dimnames(x))) {
      msg_allowed <- paste(.allowed_cpt_classes(), collapse = ", ")
      msg <- paste(
        "x must be a _named_ array-like object. Allowed classes are:",
        msg_allowed
      )
      stop(msg)
    }
    if (!only_chars(x)) {
      stop("cells are only allowed to be a single char. Use 'char_array' on x")
    }
  }

  all_comb <- expand.grid(dimnames(x), stringsAsFactors = FALSE)
  all_comb <- apply(all_comb, 1L, paste0, collapse = "")
  x_vec <- as.vector(x)

  # NOTE: We rely on all_comb and x_vec to conform (names correspond to values)
  non_zero_cells <- which(x_vec != 0)
  non_zero_names <- all_comb[non_zero_cells]

  spt <- new.env()
  for (k in seq_along(non_zero_cells)) {
    spt[[non_zero_names[k]]] <- x_vec[non_zero_cells[k]]
  }

  attr(spt, "vars") <- names(dimnames(x))
  class(spt) <- c("sptable", class(spt))
  
  return(spt)
}

#' @rdname as_sptable
#' @export
as_sptable.matrix <- as_sptable.array


#' @rdname as_sptable
#' @export
as_sptable.table <- as_sptable.array


#' Convert sptable to array-like object
#'
#' @param x \code{sptable}
#' @param dim_names A named list of named vectors. See examples.
#' @examples
#' d  <- asia[, 1:3]
#' # dm <- lapply(d, function(x) structure(unique(x), names = toupper(unique(x))))
#' # sp <- sptable(d)
#' # as_array(sp, dm)
#' @export
as_array <- function(x, dim_names) UseMethod("as_array")

as_array.sptable <- function(x, dim_names) {
  
  if (!is_named_list(dim_names)) stop("dim_names is not a named list.", call. = FALSE)
  
  dim_true_names <- lapply(dim_names, names)
  dim_char_names <- lapply(dim_names, unname)
  
  arr     <- array(0L, .map_int(dim_names, length), dim_char_names)
  arr_vec <- unlist(as.list(x))
  
  split_vec <- structure(strsplit(names(arr_vec), ""), names = names(arr_vec))
  
  for (k in seq_along(split_vec)) {
    arr[matrix(split_vec[[k]], 1L)] <- arr_vec[k]
  }
  
  dimnames(arr) <- dim_true_names
  arr
}
