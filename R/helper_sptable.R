## ---------------------------------------------------------
##                NON-EXPORTED HELPERS
## ---------------------------------------------------------
.find_cond_configs <- function(x, pos) {
  # x  : sptable
  # pos: the position of the conditional variables

  # TODO: Should we test for variablenames containing "@"?
  skeleton <- paste(rep("@", nchar(names(x)[1])))
  .map_chr(names(x), function(s) {
    sk <- skeleton
    s_pos_val <- .map_chr(pos, function(l) substr(s, l, l))
    sk[pos] <- s_pos_val
    paste(gsub("@", "", sk), collapse = "")
  })
}

.reposition_names <- function(x, pos) {
  # x : named list
  structure(x, names =.map_chr(names(x), function(y) {
    paste(.split_chars(y)[pos], collapse = "")
  }))
}

.set_as_sptable <- function(x) {
  structure(x, class = c("sptable", class(x)))
}


.allowed_cpt_classes <- function() {
  c(.map_chr(utils::methods("as_sptable"), function(generic) sub("as_sptable.", "", generic)), "sptable")  
}


merge_two_unities <- function(x, y) {
    vx  <- attr(x, "vars")
    vy  <- attr(y, "vars")
    sep_var     <- intersect(vx, vy)
    sep_lvl_x   <- attr(x, "lvls")[sep_var]
    sep_lvl_y   <- attr(y, "lvls")[sep_var]
    sep_lvl_new <- mapply(function(x, y) {
      if (length(x) <= length(y)) return(x) else return(y)
    }, sep_lvl_x, sep_lvl_y, SIMPLIFY = FALSE)
    attr(x, "lvls")[sep_var] <- sep_lvl_new
    res_var <- setdiff(vy, vx)
    res_lvl <- attr(y, "lvls")[res_var]
    for (k in 1:length(res_lvl)) {
      attr(x, "lvls") <- push(attr(x, "lvls"), res_lvl[[k]], names(res_lvl)[k])
    }
    attr(x, "vars") <- c(attr(x, "vars"), res_var)
    return(x)
}

merge_unity <- function(x, y, op = "*", validate = TRUE, ...) {

  stopifnot(op %in% c("*", "/", "+", "-"))

  if (inherits(x, "unity_table") && inherits(y, "unity_table")) {
    return(merge_two_unities(x, y))
  }

  if (inherits(x, "unity_table")) {
    # assumming that x is sptable and y is unity_table
    tmp <- x
    x <- y
    y <- tmp
  }
  
  vx  <- attr(x, "vars")
  vy  <- attr(y, "vars")
  sep <- intersect(vx, vy)

  posx <- match(sep, vx)
  # The if statement ensures correctness even if no vars in common
  cfx <- if (neq_empt_int(posx)) .find_cond_configs(x, posx) else x
  y_res_lvl  <- if (neq_empt_int(posx)) attr(y, "lvls")[-which(vy %in% sep)] else attr(y, "lvls")
  y_res_comb <- expand.grid(y_res_lvl, stringsAsFactors = FALSE)
  y_res_comb <- apply(y_res_comb, 1L, paste0, collapse = "")
  
  spt <- unlist(lapply(names(cfx), function(string) {
    structure(rep(x[string], length(y_res_comb)), names = paste(string, y_res_comb, sep = ""))
  }))
  
  attr(spt, "vars") <- c(vx, setdiff(vy, vx))
  class(spt) <- c("sptable", class(spt))
  return(spt)
}

## ---------------------------------------------------------
##                   EXPORTED HELPERS
## ---------------------------------------------------------

#' @export
unity_table <- function(vars, lvls) {
  spt <- numeric(0)
  attr(spt, "vars") <- vars
  attr(spt, "lvls") <- lvls
  class(spt) <- c("unity_table","sptable", class(spt))
  spt
}

#' @export
`[<-.sptable` <- function(x, i, value) {
  NextMethod()
}

#' @export
`[.sptable` <- function(x, i) {
  # TODO: This is critical - the second condition is expensive
  #  - replace it with  ?
  if (is.character(i) && !all(i %in% names(x))) return(0L)
  out <- structure(.set_as_sptable(NextMethod()) , vars = attr(x, "vars"))
  # if (anyNA(out)) stop("some elements are not in x")
  return(out)
}

#' Print Sparse Table
#'
#' A print method for \code{sptable} and \code{slice_sptable} objects
#'
#' @param x A \code{sptable} object
#' @param ... Not used (for S3 compatability)
#' @export
print.sptable <- function(x, ...) {
  vars      <- attr(x, "vars")
  nchr  <- sum(.map_int(vars, function(s) nchar(s))) + length(vars) + nchar("Vars:") - 1
  N     <- length(x)
  cells <- names(x)
  cat("", paste0(rep("-", nchr), collapse = ""), "\n")
  cat(" Vars:", paste0(vars, collapse = "-"), "\n")
  cat("", paste0(rep("-", nchr), collapse = ""), "\n")
  if (inherits(x, "unity_table")) {
    cat(" <unity_table> \n")
  } else {
    for (k in seq_along(cells)) {
      cat(cells[k], ":", x[k], "\n")
    }    
  }

  invisible(x)
}

#' Array to Sparse Table
#'
#' Convert conditional probability table of array-type to a sparse representation
#'
#' @param x An \code{array}-like object
#' @param validate Logical. Check if x has proper dimnames.
#' @details Use only validate = FALSE if you are positve that the structure is correct.
#' @examples
#'
#'
#' # Convert a one-dimensional array
#'
#' dmx <- list(X = c("a", "b"))
#' x   <- array(c(1, 2), dimnames = dmx)
#' as_sptable(x)
#'
#' # Convert a mutli-dimensional array
#' 
#' dmy <- list(Y = c("a", "b"), Z = c("c", "d"), W = c("e", "f"))
#' y   <- array(c(1,0,3,0,0,2,1,0), c(2,2,2), dimnames = dmy)
#' as_sptable(y)
#'
#' # Convert a data frame
#'
#' as_sptable(table(asia[, 2:3]))
#' 
#'
#' @export
as_sptable <- function(x, validate = TRUE) UseMethod("as_sptable")

#' @rdname as_sptable
#' @export
as_sptable.array <- function(x, validate = TRUE)  {

  if (inherits(x, "sptable")) return(x)
  
  if (validate) {
    if (!is_named_list(dimnames(x))) {
      msg_allowed <- paste(.allowed_cpt_classes(), collapse = ", ")
      msg <- paste("x must be a _named_ array-like object. Allowed classes are:", msg_allowed)
      stop(msg)
    }
    # TODO: Test for allowed chars in the future and dont convert automatically
    dimnames(x) <- lapply(dimnames(x), function(x) possible_chars(length(x)))  
  }

  # TODO: Use an iterator instead of copying!
  # - also, can we do: which(x != 0)?
  cond_comb  <- expand.grid(dimnames(x), stringsAsFactors = FALSE)
  spt        <- c()
  cell_names <- c()
  
  for (k in 1:nrow(cond_comb)) {
    cc  <- unlist(cond_comb[k, ])
    val <- x[matrix(cc, 1L)]
    if (val != 0) {
      spt <- c(spt, val)
      cell_names <- c(cell_names, paste(cc, collapse = ""))
    }
  }

  spt <- structure(spt, names = cell_names)
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
