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

new_alpha_triang <- function(x, alpha) {
  stopifnot(identical(sort(alpha), 1:ncol(x)))
  structure(list(x = x, alpha = alpha), class = c("alpha_triang", "list"))
}

new_minimal_triang <- function(x) {
    structure(list(x = x), class = c("minimal_triang", "list"))
}

# new_sparse_triang can be found in tmp_helper_sparse_triang.

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#          HELPERS TO FIND NEXT ELIMINATION NODE 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# new_node_to_eliminate_sparse <- function(spt, x) {
#   # x: The submatrix during elimination
#   nnzc <- .map_dbl(1:ncol(x), function(k) {

#     new_node <- k
#     nei_idx <- unname(which(x[, new_node] == 1L))
#     nei_x   <- x[nei_idx, nei_idx]
#     length(nei_idx)

#     # which messages and which non_allocated can be put into (new_node, nei_idx)
#     new_prime_chr <- colnames(x)[c(new_node, nei_idx)]

#     na_cpts_idx <- .map_lgl(spt$tmp_potentials$non_allocated_cpts, function(cpt) {
#       all(names(cpt) %in% new_prime_chr)
#     })

#     msg_cpts_idx <- .map_lgl(spt$tmp_potentials$flawed_root_msg, function(cpt) {
#       all(names(cpt) %in% new_prime_chr)
#     })

#     na_cpts  <- spt$tmp_potentials$non_allocated_cpts[na_cpts_idx]
#     msg_cpts <- spt$tmp_potentials$flawed_root_msg[msg_cpts_idx]
#     all_cpts <- c(na_cpts, msg_cpts)

#     statespace_vars <- spt$dns[unique(unlist(lapply(all_cpts, names)))]
#     statespace <- prod(.map_int(statespace_vars, length))
#     sparsity_  <- prod(.map_dbl(all_cpts, function(x) {
#       sparta::sparsity(x)
#     }))

#     # TODO: TRY TO MULTIPLY THE NUMBER OF VARIABLES IN THE NEW CLIQUE!
#     # Estimate of number of non-zero elements in the product
#     sparsity_ * statespace * length(statespace_vars)

#     # Sanity check
#     # Reduce(sparta::mult, all_cpts)
#   })

#   new_node     <- which.min(nnzc)
#   nei_idx      <- unname(which(x[, new_node] == 1L))
#   x_nei        <- x[nei_idx, nei_idx]
#   nei_complete <- sum(x_nei) == length(nei_idx) * (length(nei_idx) - 1)
  
#   return(list(x_nei = x_nei, nei_idx = nei_idx, nei_complete = nei_complete, a = new_node))
# }


new_node_to_eliminate_alpha <- function(x, alpha, x_orig_col_idx) {
  new_node <- match(alpha[1L], x_orig_col_idx)
  nei      <- x[, new_node, drop = TRUE]
  nei_idx  <- unname(which(nei == 1L))
  x_nei    <- x[nei_idx, nei_idx, drop = FALSE]
  nei_complete <- sum(x_nei) == length(nei_idx) * (length(nei_idx) - 1)
  return(list(x_nei = x_nei, nei_idx = nei_idx, nei_complete = nei_complete, a = new_node))
}

new_node_to_eliminate_min_nei <- function(x) {
  # Minimum-neibor/minimum size elimination:
  # Minimizing the number of fillins by choosing vertices that
  # hopefully simplicial
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


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#               ELIMINATION GAME ENGINE
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
elim_game <- function(obj) {
  # x: adjacency matrix

  # x <- if (inherits(obj, "sparse_triang")) {
  #   obj$flawed_root_graph
  # } else {
  #   obj$x
  # }

  x <- obj$x

  # Save the input graph
  y <- x

  # triangulation set
  fill_edges <- list()

  x_orig_col_idx <- 1:ncol(x)

  while (ncol(x) > 1L) {

    X <- if (inherits(obj, "min_nei_triang")) {
      new_node_to_eliminate_min_nei(x)
    } else if (inherits(obj, "min_fill_triang")) {
      new_node_to_eliminate_min_fill(x)
    } else if (inherits(obj, "min_sp_triang")) {
      new_node_to_eliminate_min_sp(x, obj$nlvls)
    } else if (inherits(obj, "alpha_triang")) {
      new_node_to_eliminate_alpha(x, obj$alpha, x_orig_col_idx)
    } else if (inherits(obj, "minimal_triang")) {
      new_node_to_eliminate_min_nei(x)
    }
    
    xa        <- X$a
    nc        <- X$nei_complete
    
    if (!nc) { # append new fill_edges

      x_nei   <- X$x_nei
      nei_idx <- X$nei_idx
      nn      <- ncol(x_nei)

      # if (nn > 1) { # Already complete then!
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
    # }

    x <- x[-xa, -xa, drop = FALSE]
    x_orig_col_idx <- x_orig_col_idx[-xa]

    # TODO: Let all new_<>_triang constructors be environments - alpha can be mainted inside then
    if (inherits(obj, "alpha_triang")) obj$alpha <- obj$alpha[-1L]
  }

  list(
    new_graph  = y,
    fill_edges = fill_edges
  )
}
