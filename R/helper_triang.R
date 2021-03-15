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


triang <- function(obj) {

  # if (inherits(obj, "sparse_triang")) {
  #   stop("not yet implemented")
  # }

  eg  <- elim_game(obj)
  
  if (inherits(obj, "minimal_triang")) {
    return(thin_triang(eg[["new_graph"]], eg[["fill_edges"]])[["new_graph"]])
  } else {
    return(eg[["new_graph"]])
  }
}
