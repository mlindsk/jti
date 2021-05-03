thin_triang <- function(x, fill_edges) {
  # Algorithm II from
  # 'Thinning a Triangulation of a BN to Create a Minimal Triangulation'

  # x: triangulated adjacency matrix
  # fill_edges: list of fill-ins

  if (!neq_empt_lst(fill_edges)) return(list(new_graph = x, fill_edges = fill_edges))
  
  while (TRUE) {

    rm_idx <- c()

    for (k in seq_along(fill_edges)) {
      e <- fill_edges[[k]]
      e1 <- e[1]
      e2 <- e[2]
      nei1  <- which(x[, e1] == 1L)
      nei2  <- which(x[, e2] == 1L)
      nei   <- intersect(nei1, nei2)
      x_nei <- x[nei, nei]
      nn    <- ncol(x_nei)

      if (is.null(nn)) {
        is_complete <- TRUE
      } else {
        is_complete <- sum(x_nei) == nn * (nn - 1)   
      }

      if (is_complete) {
        e <- fill_edges[[k]]
        x[e[1], e[2]] <- 0L
        x[e[2], e[1]] <- 0L
        rm_idx        <- c(rm_idx, k)
      }
    }

    if (is.null(rm_idx)) {
      return(list(new_graph = x, fill_edges = fill_edges))
    }
    
    fill_edges <- fill_edges[-rm_idx]
  }
}


.triang <- function(obj, thin = FALSE) {
  eg  <- elim_game(obj)
  if (thin) {
    return(thin_triang(eg[["new_graph"]], eg[["fill_edges"]])[["new_graph"]])
  } else {
    return(eg[["new_graph"]])
  }
}


#' Triangulate a Bayesian network
#'
#' Given a list of CPTs, this function finds a triangulation
#'
#' @param x An object returned from \code{cpt_list}
#' @param joint_vars A vector of variables for which we require them
#' to be in the same clique. Edges between all these variables are added
#' to the moralized graph.
#' @param tri The optimization strategy used for triangulation. Either
#' one of 'min_nei', 'min_fill', 'min_sp', 'evidence', 'minimal'
#' @param evidence_nodes Character vector. TODO: More details
#' @export
triangulate <- function(x,
                        joint_vars = NULL,
                        tri = "min_fill",
                        evidence_nodes = character(0)
                        ) {
  UseMethod("triangulate")
}


#' @rdname triangulate
#' @export
triangulate.cpt_list <- function(x,
                                 joint_vars = NULL,
                                 tri = "min_fill",
                                 evidence_nodes = character(0)
                                 ) {

  if (tri %ni% c("min_nei", "min_fill", "min_sp", "minimal", "evidence")) {
    stop(
      "tri must be one of min_nei, min_fill, min_sp, alpha, minimal, evidence",
      call. = FALSE
    )
  }
  
  g       <- attr(x, "graph")
  parents <- attr(x, "parents")

  gm      <- moralize_igraph(g, parents)
  if (!is.null(joint_vars)) gm <- add_joint_vars_igraph(gm, joint_vars)

  # if sparse = TRUE, the run time explodes
  M  <- igraph::as_adjacency_matrix(gm, sparse = FALSE)
  
  tri_obj <- switch(tri,
    "min_nei"  = new_min_nei_triang(M),
    "min_fill" = new_min_fill_triang(M),
    "minimal"  = new_min_fill_triang(M),
    "min_sp"   = new_min_sp_triang(M, .map_int(attr(x, "dim_names"), length)),
    "evidence" = new_evidence_triang(M, evidence_nodes)
  )
  
  eg <- elim_game(tri_obj)
  if (inherits(tri_obj, "minimal")) {
    thin_eg <- thin_triang(eg[["new_graph"]], eg[["fill_edges"]])
    eg[["new_graph"]]  <- thin_eg[["new_graph"]]
    eg[["fill_edges"]] <- thin_eg[["fill_edges"]]
  }

  cliques_    <- rip(as_adj_lst(eg[["new_graph"]]))$C

  statespace_ <- .map_dbl(cliques_, function(clique) {
    prod(.map_int(dim_names(x)[clique], length))
  })  
  
  list(
    new_graph  = eg[["new_graph"]],
    fill_edges = lapply(eg[["fill_edges"]], function(e) names(x)[e]),
    alpha      = names(x)[eg[["alpha"]]],
    cliques    = cliques_,
    statespace = statespace_
  )
}
