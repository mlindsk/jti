## library(igraph)

## munin <- readRDS("../inst/extdata/munin.rds")

## cpts <- lapply(munin[1:50], function(x) {
##   arr <- as(x$prob, "array")
##   if (length(dim(arr)) == 1L) { # The onedimensional ones are not named
##     dimnames(arr) <- structure(dimnames(arr), names = x$node)
##   }
##   arr
## }) 

## cpts <- dimnames_to_single_chars(cpts)
## cl   <- cpt_list(cpts)
## cp   <- compile(cl)
## j    <- jt(cp, propagate = FALSE)
## plot(j, vertex.size = 0.1, vertex.label = NA)

## j <- send_messages(j); plot(j)






## ---------------------------------------------------------
##              NEW FUNCTIONS ON TRIAL
## ---------------------------------------------------------

#' @export
as_env.sptable <- function(x) {
  structure(list2env(as.list(x)), "vars" = attr(x, "vars"), class = c("sptable_env", class(x)))
}


#' @export
print.sptable_env <- function(x, ...) {
  vars  <- attr(x, "vars")
  nchr  <- sum(.map_int(vars, function(s) nchar(s))) + length(vars) + nchar("Vars:") - 1
  N     <- length(x)
  cells <- names(x)
  cat("", paste0(rep("-", nchr), collapse = ""), "\n")
  cat(" Vars:", paste0(vars, collapse = "-"), "\n")
  cat("", paste0(rep("-", nchr), collapse = ""), "\n")
  for (cell in cells) {
    cat(cell, ":", x[[cell]], "\n")
  }
  invisible(NULL)
}


#' @export
marginalize.sptable_env <- function(p, s, flow = "sum") {

  if (flow %ni% c("sum", "max")) stop("flow must be 'sum' or 'max'")
  
  v <- attr(p, "vars")
  if (any(is.na(match(s, v)))) stop("Some variables in s are not in p")

  marg_vars <- setdiff(v, s)
  pos <- match(marg_vars, v)

  cf  <- .find_cond_configs(p, pos)
  scf <- split(names(cf), cf)

  spt <- lapply(scf, function(e) {
    if (flow == "sum") sum(unlist(mget(e, envir = p))) else max(p[[e]])
  })
  
  spt <- unlist(spt)
  attr(spt, "vars") <- marg_vars
  class(spt) <- c("sptable_env", class(spt))
  return(spt)
}

# Q: Return either a new sptable or value(s) in one function as this?
#' @export
`[.sptable_env` <- function(x, i, result = "sptable_env") {
  # result: 'sptable_env' or 'value'
  if (!is.character(i)) stop("an object of result 'sptable_env' is only subsettable via characters")
  if (result %ni% c("sptable_env", "value")) stop("result must be 'sptable_env' or 'value'")
  y <- mget(i, x, ifnotfound = 0L) 
  if (result == "sptable_env") {
    out <- list2env(y)
    class(out) <- class(x)
    attr(out, "vars") <- attr(x, "vars")
    return(out)
  } else {
    return(unlist(y))
  }
}


#' @export
`[<-.sptable_env` <- function(x, i, value) {
  NextMethod()
}
