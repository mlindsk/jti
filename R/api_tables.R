## ---------------------------------------------------------
##                NON-EXPORTED HELPERS
## ---------------------------------------------------------
find_cond_configs <- function(x, pos) {
  # x  : sptable
  # pos: the position of the conditional variables
  skeleton <- paste(rep("@", nchar(names(x)[1])))
  .map_chr(names(x), function(s) {
    sk <- skeleton
    s_pos_val <- .map_chr(pos, function(l) substr(s, l, l))
    sk[pos] <- s_pos_val
    paste(gsub("@", "", sk), collapse = "")
  })
}

reposition_names <- function(x, pos) {
  # x : named list
  structure(x, names =.map_chr(names(x), function(y) {
    paste(.split_chars(y)[pos], collapse = "")
  }))
}

.set_as_sptable <- function(x) {
  structure(x, class = c("sptable", class(x)))
}


## ---------------------------------------------------------
##                 EXPORTED FUNCTIONS
## ---------------------------------------------------------
#' Sparse table
#'
#' Returns a sparse contingency table for the variables in \code{x} as a vector .
#'
#' @param x Character matrix
#' @seealso \code{\link{parray}}
#' @examples
#' sptable(as.matrix(asia[, 1:2]))
#' @export
sptable <- function(x) {
  stopifnot(is.matrix(x))
  sptab <- sptab_(x)
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
#' psp <- parray(sp, c("S", "T"))
#' sum(psp) # Since (S,T) have 4 configurations
#' @export
parray <- function(x, y = NULL) {
  stopifnot(inherits(x, "sptable"))
  if (is.null(y)) {
    xs <- x / sum(x)
    class(xs) <- class(x)
    return(xs)
  }
  pos  <- match(y, attr(x, "vars"))
  conf <- find_cond_configs(x, pos)
  conditional_list <- split(names(conf), conf)
  parr <- unlist(unname(lapply(conditional_list, function(e) {
    x[e] / sum(x[e])
  })))
  attr(parr, "vars") <- attr(x, "vars")
  class(parr) <- class(x)
  return(parr)
}

#' Merge sparse tables
#'
#' Multiplication or division of two sparse tables
#' 
#' @param x A \code{sptable} object
#' @param y A \code{sptable} object
#' @param op Either "*" (multiplication) or "/" (division)
#' @param ... Not used. For S3 compatability
#' @examples
#' x <- sptable(as.matrix(asia[, 3:5]))
#' y <- sptable(as.matrix(asia[, 4:6]))
#' x
#' y
#' merge(x, y, "/")
#' @export
merge.sptable <- function(x, y, op = "*", ...) {
  
  stopifnot(op %in% c("*", "/"))
  
  vx     <- attr(x, "vars")
  vy     <- attr(y, "vars")
  sep    <- intersect(vx, vy)
  
  # If no variables in common it is easy
  if (!neq_empt_chr(sep)) {
    spt <- lapply(seq_along(x), function(i) {
      xi <- x[i]
      structure(do.call(op, list(y, xi)),
        names = paste(names(y), names(x[i]), sep = "")
      )
    })
    spt <- unlist(spt)
    attr(spt, "vars") <- c(vy, vx)
    class(spt) <- c("sptable", class(spt))
    return(spt)
  }

  posx   <- match(sep, vx)
  posy   <- match(sep, vy)

  cfx    <- find_cond_configs(x, posx)
  cfy    <- find_cond_configs(y, posy)

  scfx   <- split(names(cfx), cfx)
  scfy   <- split(names(cfy), cfy)

  # No need for repositioning if leng(sep) > 1 (they must agree then).
  if (length(sep) > 1L) {
    posx_sep <- structure(seq_along(posx), names = vx[posx])
    sposy    <- sort(posy)
    posy_sep <- structure(seq_along(sposy), names = vy[sort(sposy)])
    posy_new <- posy_sep[names(posx_sep)]
    scfy     <- reposition_names(scfx, posy_new)
  }
  
  # Those not in sc_sep are structural zeroes!
  sc_sep  <- intersect(names(scfx), names(scfy))
  scfx    <- scfx[sc_sep]
  scfy    <- scfy[sc_sep]
   
  spt <- lapply(sc_sep, function(z) {

    scfx_z <- scfx[[z]]
    scfy_z <- scfy[[z]]

    xz    <- x[scfx_z]
    yz    <- y[scfy_z]

    res    <- vector("double", length = length(xz) * length(yz))
    res_names <- vector("character", length = length(xz) * length(yz))

    iter <- 1L
    for (i in seq_along(xz)) {

      for (j in seq_along(yz)) {
        xzi <- xz[i]
        yzj <- xz[j]

        yzj_name  <- names(yzj)
        new_name  <- paste0(names(xzi), str_rem(yzj_name, posy), collapse = "")

        res[iter] <- if (isTRUE(all.equal(yzj, 0))) 0 else do.call(op, list(xzi, yzj))
        res_names[iter] <- new_name
        iter <- iter + 1L
      }
    }
    structure(res, names = res_names)
  })

  
  spt <- unlist(spt)
  
  attr(spt, "vars") <- c(vx, setdiff(vy, vx))
  class(spt) <- c("sptable", class(spt))
  
  return(spt)
}

#' Marginalize
#'
#' @param p A \code{sptable} object
#' @param s The variables to marginalize out
#' @param flow Either "sum" or "max" where "max" is most useful in connection to the internals of  junction tree algorithm.
#' @examples
#' p <- sptable(as.matrix(asia[, 3:5]))
#' p
#' marginalize(p, "L")
#' @export
marginalize <- function(p, s, flow = "sum") UseMethod("marginalize")

#' @rdname marginalize
#' @export
marginalize.sptable <- function(p, s, flow = "sum") {

  if (flow %ni% c("sum", "max")) stop("flow must be 'sum' or 'max'")
  
  v <- attr(p, "vars")
  if (any(is.na(match(s, v)))) stop("Some variables in s are not in p")

  marg_vars <- setdiff(v, s)
  pos <- match(marg_vars, v)

  cf  <- find_cond_configs(p, pos)
  scf <- split(names(cf), cf)

  spt <- lapply(scf, function(e) {
    if (flow == "sum") sum(p[e]) else max(p[e])
  })
  
  spt <- unlist(spt)
  attr(spt, "vars") <- marg_vars
  class(spt) <- c("sptable", class(spt))
  return(spt)
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
  vars_cond <- attr(x, "vars_cond")
  nchr  <- sum(.map_int(vars, function(s) nchar(s))) + length(vars) + nchar("Vars:") - 1
  N     <- length(x)
  cells <- names(x)
  cat("", paste0(rep("-", nchr), collapse = ""), "\n")
  if (inherits(x, "slice")) {
    cat("", paste(paste(names(vars_cond), "= "), vars_cond, sep = "", collapse = ", "), "\n")    
  }
  cat(" Vars:", paste0(vars, collapse = "-"), "\n")
  cat("", paste0(rep("-", nchr), collapse = ""), "\n")
  for (k in seq_along(cells)) {
    cat(cells[k], ":", x[k], "\n")
  }
  invisible(x)
}

#' @export
`[<-.sptable` <- function(x, i, value) {
  NextMethod()
}

#' @export
`[.sptable` <- function(x, i) {
  if (is.character(i) && !all(i %in% names(x))) return(0L)
  structure(.set_as_sptable(NextMethod()) , vars = attr(x, "vars"))
}


## library(dplyr)
## d <- as.matrix(tgp_dat[, 7:50])
## colnames(d) <- letters[1:44]

## sp <- sptable(d[, 1:30])
## p  <- parray(sp)
## m  <- marginalize(p, letters[1:10], sum)
## sum(m)

## p1 <- sptable(d[, 1, drop = FALSE])
## p <- sptable(d[, 1:2, drop = FALSE])
## marginalize(p, c("b"), max)
## mp <- merge(parray(p1), parray(p2), "*")
## merge(parray(p2), mp)

## p <- parray(sptable(d[, 1:5]))
## marginalize(p, c("c", "e"), max)
## slice_sptable(p, c("A" = 5))

## p1 <- sptable(d[, c(2,1,3), drop = FALSE])
## p2 <- sptable(d[, 1:5, drop = FALSE])
## merge(p1, p2)



## #' Slice of a sparse table
## #'
## #' Returns the b-slice of a conditional sparse contingency table for the variables in \code{x} as a vector.
## #'
## #' @param x A \code{sptable} object
## #' @param b A named vector of indicies for which the variables are fixed
## #' @description The names of \code{b} are the fixed values of the variables corresponding to the indicies
## #' @seealso \code{\link{sptable}}
## #' @examples
## #' sp <- sptable(as.matrix(digits[, 1:3]))
## #' # print(sp)
## #' slice_sptable(sp, c(e = 3))
## #' @export
## slice_sptable <- function(x, b) {
##   stopifnot(inherits(x, "sptable"))
##   sl <- n_b(x, b)
##   class(sl) <- c("slice", class(x))
##   return(sl)
## }



## #' Conditional sparse table
## #'
## #' Returns a conditional sparse contingency table conditioned on variables in \code{b}.
## #'
## #' @param x A sparse table obtained from \code{sptable}
## #' @param y Character vector with variables to condition on
## #' @details If \code{y} is \code{NULL}, \code{x} is just converted to a \code{csptable} with no conditioning variables, i.e. the marginal table.
## #' @seealso \code{\link{sptable}}, \code{\link{slice_sptable}}
## #' @examples
## #' sp <- sptable(as.matrix(digits[, 1:3]))
## #' sp
## #' length(sp)
## #' csp <- csptable(sp, c("V3"))
## #' csp
## #' length(csp)
## #' @export
## csptable <- function(x, y) {
##   # x : sptable
##   # y : conditional variables
##   if (is.null(y)) {
##     cspt <- structure(list(x), names = "marg")
##     class(cspt) <- c("csptable", class(cspt))
##     return(cspt)
##   }
  
##   # check if y is in the names of x
##   if (!all(y %in% attr(x, "vars"))) stop("Some names in 'y' are not in 'x'")
  
##   pos     <- match(y, attr(x, "vars"))
##   configs <- find_cond_configs(x, pos)
##   cspt <- lapply(configs, function(z) {
##     b  <- structure(pos, names = .split_chars(z))
##     sl <- slice_sptable(x, b)
##     return(sl)
##   })
##   cspt <- structure(cspt, names = configs, pos = pos)
##   class(cspt) <- c("csptable", class(cspt))
##   cspt
## }


## #' Print method for conditional sparse tables
## #'
## #' @param x A \code{csptable} object
## #' @param ... Not used (for S3 compatability)
## #' @export
## print.csptable <- function(x, ...) {
##   for (i in seq_along(x)) {print(x[[i]]); cat("\n")}
## }
