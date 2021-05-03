# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#              NEW TRIANGULATION CONSTRUCTORS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

new_min_nei_triang <- function(x) {
  structure(list(x = x), class = c("min_nei_triang", "list"))
}

new_min_fill_triang <- function(x) {
  structure(list(x = x), class = c("min_fill_triang", "list"))
}

new_min_sp_triang <- function(x, nlvls) {
  structure(list(x = x, nlvls =  nlvls[dimnames(x)[[1]]]), class = c("min_sp_triang", "list"))
}

# new_evidence_triang <- function(x, evidence_nodes) {
#   # Note: Evidence nodes must be ordered according to their priority
#   bools <- vector("logical", length = ncol(x))
#   if (neq_empt_chr(evidence_nodes)) {
#     # Test if evidence_are valid
#     stopifnot(all(evidence_nodes %in% colnames(x)))
#     bools[match(evidence_nodes, colnames(x))] <- TRUE
#   }
#   structure(list(x = x, x_nei_lookup = x, evidence_nodes = bools), class = c("evidence_triang", "list"))
# }

new_evidence_triang <- function(x, evidence_nodes) {
  # Note: Evidence nodes must be ordered according to their priority
  bools <- vector("logical", length = ncol(x))
  if (neq_empt_chr(evidence_nodes)) {
    # Test if evidence_are valid
    stopifnot(all(evidence_nodes %in% colnames(x)))
    bools[match(evidence_nodes, colnames(x))] <- TRUE
  }
  structure(list(x = x, evidence_nodes = bools), class = c("evidence_triang", "list"))
}


new_node_to_eliminate_min_nei <- function(x) {
  # Minimum-neibor/minimum size elimination:
  # Minimizing the number of fillins by choosing vertices that
  # are hopefully simplicial
  lookup_order <- as.integer(names(sort(structure(apply(x, 2L, sum), names = 1:ncol(x)))))
  new_node_idx <- lookup_order[1L]
  nei     <- x[, new_node_idx, drop = TRUE]
  nei_idx <- unname(which(nei == 1L))
  x_nei   <- x[nei_idx, nei_idx, drop = FALSE]

  nn        <- ncol(x_nei)
  max_edges <- nn * (nn - 1)
  nei_complete <- sum(x_nei) == max_edges
  
  return(list(x_nei = x_nei, nei_idx = nei_idx, nei_complete = nei_complete, a = new_node_idx))
}

new_node_to_eliminate_min_fill <- function(x) {
  # Find the optimal node to eliminate
  new_node <- integer(0)
  nei_idx  <- integer(0)
  crit     <- Inf
  
  for (k in 1:ncol(x)) {
    nei_idx_k <- which(x[, k] == 1L)

    all_edges <- length(nei_idx_k) * (length(nei_idx_k) - 1L) / 2
    existing_edges <- sum(x[nei_idx_k, nei_idx_k]) / 2L
    crit_k <- all_edges - existing_edges
    
    if (crit_k < crit) {
      crit     <- crit_k
      new_node <- k
      nei_idx  <- nei_idx_k
    }
  }

  x_nei    <- x[nei_idx, nei_idx, drop = FALSE]
  nei_complete <- sum(x_nei) == length(nei_idx) * (length(nei_idx) - 1)
  return(list(x_nei = x_nei, nei_idx = nei_idx, nei_complete = nei_complete, a = new_node))
}

new_node_to_eliminate_min_sp <- function(x, nlvls) {
  # Find the optimal node to eliminate
  new_node <- integer(0)
  nei_idx  <- integer(0)
  crit     <- Inf

  for (k in 1:ncol(x)) {
    nei_idx_k <- which(x[, k] == 1L)
    family_k  <- c(k, nei_idx_k)
    crit_k    <- prod(nlvls[family_k])
    if (crit_k < crit) {
      crit     <- crit_k
      new_node <- k
      nei_idx  <- nei_idx_k
    }
  }

  x_nei    <- x[nei_idx, nei_idx, drop = FALSE]
  nei_complete <- sum(x_nei) == length(nei_idx) * (length(nei_idx) - 1)
  return(list(x_nei = x_nei, nei_idx = nei_idx, nei_complete = nei_complete, a = new_node))
}


# new_node_to_eliminate_evidence <- function(x, x_nei_lookup, evidence_nodes) {

#   n_new_fills <- .map_dbl(1:ncol(x), function(k) {
#     nei_idx_k <- which(x[, k] == 1L)
#     all_edges <- length(nei_idx_k) * (length(nei_idx_k) - 1L) / 2
#     existing_edges <- sum(x[nei_idx_k, nei_idx_k]) / 2L
#     all_edges - existing_edges
#   })
  
#   F_ <- which(n_new_fills == min(n_new_fills))
#   E  <- which(evidence_nodes == TRUE)
#   H  <- setdiff(F_, E)

#   new_node <- if (neq_empt_int(H)) {
#     if (neq_empt_int(E)) {
#       H_neis   <- lapply(E, function(e) which(H %in% which(x_nei_lookup[, e] == 1L)))
#       # H_neis   <- lapply(E, function(e) which(F_ %in% which(x_nei_lookup[, e] == 1L)))
#       H_neis   <- Filter(neq_empt_int, H_neis)
#       new_idx <- if (neq_empt_lst(H_neis)) {
#         H_neis[[1]][1]
#       } else {
#         1L
#       }
#       H[new_idx]
#     } else { # E empty
#       F_[1]
#     }
#   } else {   # H empty
#     F_[1]
#   }
  
#   nei_idx      <- which(x[, new_node] == 1L)
#   x_nei        <- x[nei_idx, nei_idx, drop = FALSE]
#   nei_complete <- sum(x_nei) == length(nei_idx) * (length(nei_idx) - 1)

#   return(list(x_nei = x_nei, nei_idx = nei_idx, nei_complete = nei_complete, a = new_node))
# }


new_node_to_eliminate_evidence <- function(x, evidence_nodes) {

  n_new_fills <- .map_dbl(1:ncol(x), function(k) {
    nei_idx_k <- which(x[, k] == 1L)
    all_edges <- length(nei_idx_k) * (length(nei_idx_k) - 1L) / 2
    existing_edges <- sum(x[nei_idx_k, nei_idx_k]) / 2L
    all_edges - existing_edges
  })
  
  F_ <- which(n_new_fills == min(n_new_fills))
  E  <- which(evidence_nodes == TRUE)
  H  <- setdiff(F_, E)
  W  <- intersect(F_, E)
  
  new_node <- if (neq_empt_int(E)) {
    if (neq_empt_int(H)) {
      neis <- lapply(E, function(e) which(H %in% which(x[, e] == 1L)))
      neis <- Filter(neq_empt_int, neis)
      if (neq_empt_lst(neis)) H[neis[[1]][1]] else F_[1]      
    } else {
      neis <- lapply(E, function(e) which(W %in% which(x[, e] == 1L)))
      neis <- Filter(neq_empt_int, neis)
      if (neq_empt_lst(neis)) W[neis[[length(neis)]][1]] else F_[1]      
    }

  } else {
    F_[1]
  }
  
  nei_idx      <- which(x[, new_node] == 1L)
  x_nei        <- x[nei_idx, nei_idx, drop = FALSE]
  nei_complete <- sum(x_nei) == length(nei_idx) * (length(nei_idx) - 1)

  return(list(x_nei = x_nei, nei_idx = nei_idx, nei_complete = nei_complete, a = new_node))
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#               ELIMINATION GAME ENGINE
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
elim_game <- function(obj) {
  # x: adjacency matrix

  x <- obj$x

  # Save the input graph
  y <- x

  # triangulation set
  fill_edges <- list()

  # elimination ordering
  alpha      <- vector("integer", length = ncol(x))
  alpha_iter <- 1L
  
  x_orig_col_idx <- 1:ncol(x)

  while (ncol(x) > 1L) {

    X <- if (inherits(obj, "min_nei_triang")) {
      new_node_to_eliminate_min_nei(x)
    } else if (inherits(obj, "min_fill_triang")) {
      new_node_to_eliminate_min_fill(x)
    } else if (inherits(obj, "min_sp_triang")) {
      new_node_to_eliminate_min_sp(x, obj$nlvls)
    } else if (inherits(obj, "evidence_triang")) {
      out <- new_node_to_eliminate_evidence(x, obj$evidence_nodes)
      # Eliminate corresponing idx in evidence nodes and update neighborhood of evidence nodes
      obj$evidence_nodes <- obj$evidence_nodes[-out$a]
      # obj$x_nei_lookup   <- obj$x_nei_lookup[-out$a, -out$a, drop = FALSE]
      out
    }
    
    xa        <- X$a
    nc        <- X$nei_complete
    
    if (!nc) { # append new fill_edges
      x_nei   <- X$x_nei
      nei_idx <- X$nei_idx
      nn      <- ncol(x_nei)

      for (k in 1:(nn-1)) {
        x_nei_k <- x_nei[, k, drop = TRUE]
        non_adj <- nei_idx[which(x_nei_k[(k+1):nn] == 0L) + k]
        fills   <- lapply(non_adj, function(a) c(nei_idx[k], a))
        
        for (fill in fills) {
          # convert to original indices:
          fill_orig    <- x_orig_col_idx[fill]
          fill_edges   <- push(fill_edges, fill_orig)

          x[fill[1], fill[2]] <- 1L
          x[fill[2], fill[1]] <- 1L

          y[fill_orig[1], fill_orig[2]] <- 1L
          y[fill_orig[2], fill_orig[1]] <- 1L
          
        }
      }
    }

    alpha[alpha_iter] <- x_orig_col_idx[xa]
    alpha_iter        <- alpha_iter + 1L
    
    x                 <- x[-xa, -xa, drop = FALSE]
    x_orig_col_idx    <- x_orig_col_idx[-xa]
  }

   # should only ontain 1 value now
  alpha[length(alpha)] <- x_orig_col_idx[1L]
  
  list(
    new_graph  = y,
    fill_edges = fill_edges,
    alpha      = alpha
  )
}
