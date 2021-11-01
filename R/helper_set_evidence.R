set_evidence_pot <- function(x, evidence, inc) {
  # x: list of (sparse) tables
  for (k in seq_along(x)) {

    pot_k <- names(x[[k]])
    es_in_ck <- which(names(evidence) %in% pot_k)

    if (neq_empt_int(es_in_ck)) {

      e <- evidence[es_in_ck]

      if (inherits(x[[k]], "sparta_unity")) {
        new_names <- setdiff(names(x[[k]]), names(e))
        x[[k]] <- sparta::sparta_unity_struct(sparta::dim_names(x[[k]])[new_names])
        next
      }
      
      conform  <- length(pot_k) > length(e)
      m <- if (conform) {
        try(sparta::slice(x[[k]], e, drop = TRUE), silent = TRUE)  # possibly a sparta_unity
      } else {
        try(sparta::slice(x[[k]], e, drop = FALSE), silent = TRUE)
      }

      if (inherits(m, "try-error")) {
        new_names <- setdiff(names(x[[k]]), names(e))
        m <- sparta::sparta_unity_struct(sparta::dim_names(x[[k]])[new_names])
        inc$inc <- TRUE
      }
      x[[k]] <- m
    }
  }
  return(x)
}

set_evidence_cpt <- function(x, evidence, inc, eps) {
  # x: a cpt_list object
  for (k in seq_along(x)) {

    child   <- names(x)[k]
    parents <- attr(x, "parents")[[child]]
    family  <- c(child, parents)

    if (inherits(x[[k]], "sparta_unity")) next

    e             <- evidence[which(names(evidence) %in% family)]
    es_in_child   <- which(names(e) %in% child)
    es_in_parents <- which(names(e) %in% parents)
    es_in_family  <- c(es_in_child, es_in_parents) 

    if (neq_empt_int(es_in_family)) {
      
      # parent evidence
      if (neq_empt_int(es_in_parents)) {
        m <- try(sparta::slice(x[[k]], e[es_in_parents], drop = TRUE), silent = TRUE)  # possibly a sparta_unity
        if (inherits(m, "try-error")) {
          inc$inc <- TRUE
          new_dim_names <- sparta::dim_names(x[[k]])[setdiff(names(x[[k]]), names(e[es_in_parents]))]
          sp_child <- length(sparta::dim_names(x[[k]])[[child]])
          x[[k]] <- sparta::sparta_unity_struct(new_dim_names, rank = 1/sp_child)
          next
        }
        x[[k]] <- m
      }

      # child evidence
      if (neq_empt_int(es_in_child)) {
        m <- if (length(names(x[[k]])) > 1) {
          try(sparta::slice(x[[k]], e[es_in_child], drop = TRUE), silent = TRUE)
        } else {
          try(sparta::slice(x[[k]], e[es_in_child], drop = FALSE), silent = TRUE)
        }

        # epsilon-smoothing
        if (inherits(m, "try-error")) {
          inc$inc <- TRUE
          new_dim_names <- sparta::dim_names(x[[k]])[setdiff(names(x[[k]]), names(e[es_in_child]))]
          x[[k]] <- sparta::sparta_unity_struct(new_dim_names, eps[child])
          next
        }
        x[[k]] <- m
      }
      
    } # end es_in_family
  } # end for loop
  
  return(x)
}


#' Enter Evidence 
#'
#' Enter evidence into a the junction tree object that has not been propagated
#'
#' @param x A junction tree object, \code{jt}.
#' @param evidence A named vector. The names are the variabes and the elements
#' are the evidence.
#' @param initialize_cpts \code{TRUE} if the CPTs should be initialized and then
#' create the clique potentials. Only relevant on objects returned from \code{compile}.
#' @examples
#' # See the 'jt' function
#' @seealso \code{\link{jt}}, \code{\link{mpe}}
#' @export
set_evidence <- function(x, evidence, initialize_cpts = TRUE) UseMethod("set_evidence")

#' @rdname set_evidence
#' @export
set_evidence.jt <- function(x, evidence, initialize_cpts = FALSE) {
  if (attr(x, "propagated") != "no") {
    stop(
      "Evidence can only be entered into a junction tree, ",
      "that has not begun propagation.",
      call. = FALSE
    )
  }
  
  if (!valid_evidence(attr(x, "dim_names"), evidence)) {
    stop("Evidence is not on correct form", call. = FALSE)
  }


  # TODO: WE ONLY NEED TO FLAG THIS NOW, THE CODE WORKS NOW!
  # i.e.; we can't trust evidence inserted on pot level!

  
  inc <- new.env()
  inc$inc <- FALSE # TODO: shouldnt it be attr(x, "inc")?
  # TODO: If inc here, the evidence cannot be trusted. Flag this.
  x$charge$C <- set_evidence_pot(x$charge$C, evidence, inc)
  attr(x, "evidence") <- c(attr(x, "evidence"), evidence)
  attr(x, "inconsistencies") <- inc$inc
  return(x)
}

#' @rdname set_evidence
#' @export
set_evidence.charge <- function(x, evidence, initialize_cpts = TRUE) {

  if (!valid_evidence(attr(x, "dim_names"), evidence)) {
    stop("Evidence is not on correct form", call. = FALSE)
  }

  inc     <- new.env()
  inc$inc <- FALSE # TODO: shouldnt it be attr(x, "inc")?
  init    <- attr(x, "cpts_initialized")

  if (init) {
    # TODO: If inc here, the evidence cannot be trusted. Flag this.
    x$charge$C <- set_evidence_pot(x$charge$C, evidence, inc)
  } else {
    # TODO: If init here, we approximate the evidence
    x$charge$cpts <- set_evidence_cpt(x$charge$cpts, evidence, inc, attr(x, "eps"))  
  }
  
  attr(x, "evidence") <- c(attr(x, "evidence"), evidence)
  attr(x, "inconsistencies") <- inc$inc

  if (initialize_cpts && !init) {
    x$charge <- new_charge_cpt(x$charge$cpts, x$cliques, x$charge$parents)
    attr(x, "cpts_initialized") <- TRUE
    x
  } else {
    x
  }
}

#' Initialize
#'
#' Initialization of CPTs
#'
#' @param x A compiled object.
#' @details Multiply the CPTs and allocate them to clique potentials.
#' @export
initialize <- function(x) UseMethod("initialize")

#' @rdname initialize
#' @export
initialize.charge <- function(x) {
  attr(x, "cpts_initialized") <- TRUE
  x$charge <-structure(new_charge_cpt(x$charge$cpts, x$cliques, x$charge$parents), initialized = TRUE)
  x
}
