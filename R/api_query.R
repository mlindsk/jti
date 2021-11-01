#' Query probabilities
#'
#' Get probabilities from a junction tree object
#'
#' @param x A junction tree object, \code{jt}.
#' @param nodes The nodes for which the probability is desired
#' @param type Either 'marginal' or 'joint'
#' @examples
#' # See the 'jt' function
#' @seealso \code{\link{jt}}, \code{\link{mpe}}
#' @export
query_belief <- function(x, nodes, type = "marginal") UseMethod("query_belief")


#' @rdname query_belief
#' @export
query_belief.jt <- function(x, nodes, type = "marginal") {

  check_query(
    attr(x, "propagate"),
    type,
    has_inconsistencies(x),
    attr(x, "flow"),
    nodes,
    attr(x, "evidence"),
    get_clique_root(x)
  )
  
  node_lst <- if (type == "joint") {
    list(nodes)
  } else {
    as.list(nodes)
  }

  .query <- lapply(node_lst, function(z) {

    # TODO: Split this into two functions?:
    # .query_collect(x, z) and .query_full(x, z)
    
    if (attr(x, "propagate") == "collect") {
      root_idx    <- get_clique_root_idx(x)
      C_idx_names <- names(x$charge$C[[root_idx]])

      z_is_unity  <- length(z) == 1L && z %ni% C_idx_names
      if (z_is_unity) {
        dnz  <- dim_names(x)[z]
        ldnz <- length(dnz[[1]])
        vals <- rep(1 / ldnz, ldnz)
        return(structure(array(vals, dimnames = dnz), is_unity = TRUE))
      }
      
      marg_out <- setdiff(C_idx_names, z)
      return(sparta::marg(x$charge$C[[root_idx]], marg_out))
    }

    in_which_cliques <- .map_lgl(x$cliques, function(clq) all(z %in% clq))
    
    if (!any(in_which_cliques) && type == "joint") {
      stop("The function does not, support out-of-clique ",
        "queries, i.e. nodes that belong to different cliques. ",
        "Use plot(x) or get_cliques(x) to see ",
        "the cliques of the junction tree. ",
        "Alternatively, use the `joint_vars` ",
        "argument in the compilation process.",
        call. = FALSE
      )
    }

    index_in_which_cliques <- which(in_which_cliques)
    statespace_of_possible_cliques <- .map_dbl(x$charge$C[in_which_cliques], ncol)
    idx <- index_in_which_cliques[which.min(statespace_of_possible_cliques)]
    C_idx_names <- names(x$charge$C[[idx]])

    z_is_unity <- length(z) == 1L && z %ni% C_idx_names
    if (z_is_unity) {
      dnz  <- dim_names(x)[z]
      ldnz <- length(dnz[[1]])
      vals <- rep(1 / ldnz, ldnz)
      return(structure(array(vals, dimnames = dnz), is_unity = TRUE))
    }

    marg_out <- setdiff(C_idx_names, z)
    return(sparta::marg(x$charge$C[[idx]], marg_out))
  })

  if (type == "joint") {
    sparta::as_array(.query[[1]])
  } else {
    out <- lapply(.query, function(z) {
      if (!is.null(attr(z, "is_unity"))) structure(z, is_unity = NULL) else sparta::as_array(z)
    })
    return(structure(out, names = nodes))
  }
}
