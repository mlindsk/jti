deep_copy_env <- function(x) structure(list2env(as.list(x)), class = class(x), vars = attr(x, "vars"))

## ---------------------------------------------------------
##              NEW FUNCTIONS ON TRIAL
## ---------------------------------------------------------
#' @export
merge.sptable_env <- function(x, y, op = "*", validate = TRUE, ...) {

  stopifnot(op %in% c("*", "/", "+", "-"))
  
  if (inherits(x, "unity_table") || inherits(y, "unity_table")) {
    return(merge_unity(x, y))
  }
  
  if (validate) {
    nvars_x <- length(attr(x, "vars"))
    nvars_y <- length(attr(y, "vars"))
    msg1 <- "One or more names of" 
    msg2 <- "has nchar(name) > 1 which is not allowed."
    for (name in names(x)) if (nchar(name) > nvars_x) stop(paste(msg1, "x", msg2))
    for (name in names(y)) if (nchar(name) > nvars_y) stop(paste(msg1, "y", msg2))
  }

  # Interchange x and y such that x is shortest gives better performance in vectorization.
  if (length(x) > length(y)) {
    tmp <- deep_copy_env(y)
    y <- x
    x <- tmp
    rm(tmp)
  }
  
  vx  <- attr(x, "vars")
  vy  <- attr(y, "vars")
  sep <- intersect(vx, vy)

  # If no variables in common it is easy
  if (!neq_empt_chr(sep)) {
    # TODO: Put this into a function: merge_tables_with_empty_separator
    
    spt <- unlist(lapply(names(x), function(nx_) {
      .names <- paste0(nx_, names(y))
      .vals  <- do.call(op, list(x[nx_, "value"], y[names(y), "value"]))
      structure(.vals, names = .names)
    }))

    spt <- list2env(as.list(spt))
    attr(spt, "vars") <- c(vx, vy)
    class(spt) <- c("sptable_env", class(spt))
    return(spt)
  }

  posx <- match(sep, vx)
  posy <- match(sep, vy)
  
  cfx  <- .find_cond_configs(x, posx)
  cfy  <- .find_cond_configs(y, posy)
  
  scfx <- split(names(cfx), cfx)
  scfy <- split(names(cfy), cfy)

  # We need to align the tables to have identical names in order to merge correctly
  # - (if !(length(sep) > 1) names in scfx and scfy must agree!
  if (length(sep) > 1L && !identical(names(scfx), names(scfy))) {
    # TODO: put this into a function
    posx_sep    <- structure(seq_along(posx), names = vx[posx])
    posy_sorted <- sort(posy)
    posy_sep    <- structure(seq_along(posy_sorted), names = vy[posy_sorted])
    posy_new    <- posy_sep[names(posx_sep)]
    stop("reposition not implemented for sptable_env yet. fix")
    scfy        <- .reposition_names(scfy, posy_new)
  }

  # Those not in sc_sep are structural zeroes!
  sc_sep <- intersect(names(scfx), names(scfy))
  scfx   <- scfx[sc_sep]
  scfy   <- scfy[sc_sep]

  spt <- lapply(sc_sep, function(z) {

    scfx_z <- scfx[[z]]
    scfy_z <- scfy[[z]]
    
    xz <- x[scfx_z]
    yz <- y[scfy_z]
    
    xn_ <- names(xz)
    yn_ <- names(yz)
    yn_rem <- str_rem(names(yz), posy)
    
    unlist(lapply(xn_, function(z) {
      .names <- paste0(z, yn_rem)
      .vals  <- do.call(op, list(xz[z, "value"], yz[yn_, "value"]))
      structure(.vals, names = .names)
    }))
  })

  spt <- list2env(as.list(unlist(spt, recursive = FALSE)))
  attr(spt, "vars") <- c(vx, setdiff(vy, vx))
  class(spt) <- c("sptable_env", class(spt))
  return(spt)
}


#' @export
marginalize.sptable_env <- function(p, s, flow = "sum") {

  if (flow %ni% c("sum", "max")) stop("flow must be 'sum' or 'max'")

  v <- attr(p, "vars")
  if (anyNA(match(s, v))) stop("Some variables in s are not in p")

  marg_vars <- setdiff(v, s)
  pos <- match(marg_vars, v)

  cf  <- .find_cond_configs(p, pos)
  scf <- split(names(cf), cf)

  spt <- unlist(lapply(scf, function(e) {
    # if (flow == "sum") sum(unlist(mget(e, envir = p))) else max(unlist(mget(e, envir = p)))
    if (flow == "sum") sum(p[e]) else max(p[e])
  }))
  
  attr(spt, "vars") <- marg_vars
  class(spt) <- c("sptable_env", class(spt))
  return(spt)
}

#' @rdname as_sptable
#' @export
as_parray.sptable_env <- function(x, y = NULL) {

  if (is.null(y)) {
    sum_x <- sum(x)
    parr <- list2env(eapply(x, function(x_) x_ / sum_x))
    class(parr) <- class(x)
    attr(parr, "vars") <- attr(x, "vars")
    return(parr)
  }

  pos  <- match(y, attr(x, "vars"))
  conf <- .find_cond_configs(x, pos)
  conditional_list <- split(names(conf), conf)

  parr <- list2env(unlist(unname(lapply(conditional_list, function(e) {
    xe <- x[e]
    sum_xe <- sum(xe)
    eapply(xe, function(xe_) xe_ / sum_xe)
  }))))
  
  attr(parr, "vars") <- attr(x, "vars")
  class(parr) <- class(x)
  return(parr)
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

head.sptable_env <- function(x, n = 10) {
  # TODO: DONT JUST COPY THE PRINT FUNCTION! MAKE IT SHORTER!
  vars  <- attr(x, "vars")
  nchr  <- sum(.map_int(vars, function(s) nchar(s))) + length(vars) + nchar("Vars:") - 1
  N     <- length(x)
  cells <- names(x)
  cat("", paste0(rep("-", nchr), collapse = ""), "\n")
  cat(" Vars:", paste0(vars, collapse = "-"), "\n")
  cat("", paste0(rep("-", nchr), collapse = ""), "\n")
  cells_n <- if (N < n) cells else cells[1:n]
  for (cell in cells_n) {
    cat(cell, ":", x[[cell]], "\n")
  }
  invisible(NULL)
}

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

#'' export
# set_value: x[["el"]] <- value

# - how to set multiple values?


#' @export
sum.sptable_env <- function(x, ...) {
  sum(x[ls(envir = x), "value"])
}

#' @export
max.sptable_env <- function(x, ...) {
  max(x[ls(envir = x), "value"])
}

#' @export
min.sptable_env <- function(x, ...) {
  min(x[ls(envir = x), "value"])
}

#' @export
as_env.sptable <- function(x) {
  warning("obsolete")
  .class <- c("sptable_env", "environment")
  structure(list2env(as.list(x)), "vars" = attr(x, "vars"), class = .class)
}


#' @export
as_sptable_env <- function(x, validate = TRUE) UseMethod("as_sptable")

#' @rdname as_sptable_env
#' @export
as_sptable_env.array <- function(x, validate = TRUE)  {

  if (inherits(x, "sptable_env")) return(x)

  if (validate) {
    if (!is_named_list(dimnames(x))) {
      msg_allowed <- paste(.allowed_cpt_classes(), collapse = ", ")
      msg <- paste(
        "x must be a _named_ array-like object. Allowed classes are:",
        msg_allowed
      )
      stop(msg)
    }
    # TODO: Test for allowed chars in the future and dont convert automatically
    dimnames(x) <- lapply(dimnames(x), function(x) possible_chars(length(x)))  
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
  class(spt) <- c("sptable_env", class(spt))
  
  return(spt)
}

#' @rdname as_sptable_env
#' @export
as_sptable_env.matrix <- as_sptable_env.array


#' @rdname as_sptable_env
#' @export
as_sptable_env.table <- as_sptable_env.array
