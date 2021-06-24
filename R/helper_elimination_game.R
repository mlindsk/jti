# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                TRIANGULATION CONSTRUCTORS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# NOTE:

# The four arguments
# ------------------
# - current_nei_mat
# - current_nei_idx
# - is_nei_complete
# - new_node_idx   

# _must_ be part of any new structure,
# should be placed as the "last" arguments and
# should be initialized as NULL. These variables are updated
# in each iteration of the elimination game.
# NOTE: It dosent matter when we use/implement the new_base_triang below

# The argument
# ------------
# - x
# is the input matrix and should be placed first.

# New arguments
# -------------
# must be inserted between x and
# the four arguments described above. See e.g.
# new_min_efill_triang which has further three
# arguments: nlvls, pmf_evidence and find_simplicial.

# TODO: Use this new_base_triang to reduce the code
# new_base_triang <- function(x) {
#   structure(
#     list(
#       x               = x,
#       current_nei_mat = NULL,
#       current_nei_idx = NULL,
#       is_nei_complete = NULL,
#       new_node_idx    = NULL
#     ),
#     class = c("list")
#   )
# }

new_min_fill_triang <- function(x) {
  structure(
    list(
      x               = x,
      current_nei_mat = NULL,
      current_nei_idx = NULL,
      is_nei_complete = NULL,
      new_node_idx    = NULL
    ),
    class = c("min_fill_triang", "list")
  )
}

new_min_rfill_triang <- function(x) {
  structure(
    list(
      x               = x,
      current_nei_mat = NULL,
      current_nei_idx = NULL,
      is_nei_complete = NULL,
      new_node_idx    = NULL
    ),
    class = c("min_rfill_triang", "list")
  )
}



new_min_efill_triang <- function(x, nlvls, pmf_evidence) {
  if (neq_empt_int(nlvls)) {
    # TODO: check if pmf and nlvls agree!
  }
  structure(
    list(
      x                = x,
      nlvls            = nlvls[dimnames(x)[[1]]],
      pmf_evidence     = pmf_evidence,
      find_simplicials = TRUE,
      current_nei_mat  = NULL,
      current_nei_idx  = NULL,
      is_nei_complete  = NULL,
      new_node_idx     = NULL      
    ),
    class = c("min_efill_triang", "list")
  )
}

new_min_sfill_triang <- function(x, nlvls, pmf_evidence) {
  structure(
    list(
      x                = x,
      nlvls            = nlvls[dimnames(x)[[1]]],
      find_simplicials = TRUE,
      current_nei_mat  = NULL,
      current_nei_idx  = NULL,
      is_nei_complete  = NULL,
      new_node_idx     = NULL      
    ),
    class = c("min_sfill_triang", "list")
  )
}

new_min_rsfill_triang <- function(x, nlvls, pmf_evidence) {
  structure(
    list(
      x                = x,
      nlvls            = nlvls[dimnames(x)[[1]]],
      find_simplicials = TRUE,
      current_nei_mat  = NULL,
      current_nei_idx  = NULL,
      is_nei_complete  = NULL,
      new_node_idx     = NULL      
    ),
    class = c("min_rsfill_triang", "list")
  )
}

new_min_sp_triang <- function(x, nlvls) {
  structure(
    list(
      x               = x,
      nlvls           = nlvls[dimnames(x)[[1]]],
      current_nei_mat = NULL,
      current_nei_idx = NULL,
      is_nei_complete = NULL,
      new_node_idx    = NULL      
    ),
    class = c("min_sp_triang", "list")
  )
}

new_min_esp_triang <- function(x, nlvls, pmf_evidence) {
  structure(
    list(
      x                = x,
      nlvls            = nlvls[dimnames(x)[[1]]],
      pmf_evidence     = pmf_evidence,
      find_simplicials = TRUE,
      current_nei_mat  = NULL,
      current_nei_idx  = NULL,
      is_nei_complete  = NULL,
      new_node_idx     = NULL      
    ),
    class = c("min_esp_triang", "list")
  )
}

new_min_nei_triang <- function(x) {
  structure(
    list(
      x               = x,
      current_nei_mat = NULL,
      current_nei_idx = NULL,
      is_nei_complete = NULL,
      new_node_idx    = NULL
    ),
    class = c("min_nei_triang", "list")
  )
}

new_minimal_triang <- function(x) {
  obj <- new_min_fill_triang(x)
  class(obj) <- c("minimal_triang", class(obj))
  obj
}

new_alpha_triang <- function(x, alpha) {
  structure(
    list(
      x               = x,
      alpha           = alpha,
      current_nei_mat = NULL,
      current_nei_idx = NULL,
      is_nei_complete = NULL,
      new_node_idx    = NULL      
    ),
    class = c("alpha_triang", "list")
  )
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                UPDATING FUNCTIONS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
new_node_to_eliminate <- function(obj) UseMethod("new_node_to_eliminate")

new_node_to_eliminate.min_nei_triang <- function(obj) {
  x               <- obj$x
  new_node_idx    <- integer(0)
  current_nei_idx <- integer(0)

  simplicial_nodes_idx <- which(.map_dbl(1:ncol(x), function(k) {
    current_nei_idx_k <- which(x[, k] == 1L)
    all_edges <- length(current_nei_idx_k) * (length(current_nei_idx_k) - 1L) / 2
    existing_edges <- sum(x[current_nei_idx_k, current_nei_idx_k]) / 2L
    all_edges - existing_edges
  }) == 0)

  if (neq_empt_int(simplicial_nodes_idx)) {
    # Eliminate simplicial node
    new_node_idx    <- simplicial_nodes_idx[1L]
    current_nei_idx <- which(x[, new_node_idx] == 1L)
  } else {
    lookup_order    <- as.integer(names(sort(structure(apply(x, 2L, sum), names = 1:ncol(x)))))
    new_node_idx    <- lookup_order[1L]
    nei             <- x[, new_node_idx, drop = TRUE]
    current_nei_idx <- unname(which(nei == 1L))    
  }
  
  current_nei_mat <- x[current_nei_idx, current_nei_idx, drop = FALSE]
  obj$current_nei_mat <- current_nei_mat
  obj$current_nei_idx <- current_nei_idx
  obj$is_nei_complete <- sum(current_nei_mat) == length(current_nei_idx) * (length(current_nei_idx) - 1)
  obj$new_node_idx    <- new_node_idx
  
  return(obj)
}

new_node_to_eliminate.min_fill_triang <- function(obj) {
  x               <- obj$x
  # new_node_idx    <- integer(0)
  # current_nei_idx <- integer(0)
  # edges_to_add    <- Inf

  # for (k in 1:ncol(obj$x)) {
  #   current_nei_idx_k <- which(x[, k] == 1L)
  #   all_edges         <- length(current_nei_idx_k) * (length(current_nei_idx_k) - 1L) / 2
  #   existing_edges    <- sum(x[current_nei_idx_k, current_nei_idx_k]) / 2L
  #   edges_to_add_k    <- all_edges - existing_edges
    
  #   if (edges_to_add_k < edges_to_add) {
  #     edges_to_add    <- edges_to_add_k
  #     new_node_idx    <- k
  #     current_nei_idx <- current_nei_idx_k
  #   }
  # }

  min_fills <- .map_dbl(1:ncol(x), function(k) {
    current_nei_idx_k <- which(x[, k] == 1L)
    all_edges <- length(current_nei_idx_k) * (length(current_nei_idx_k) - 1L) / 2
    existing_edges <- sum(x[current_nei_idx_k, current_nei_idx_k]) / 2L
    all_edges - existing_edges
  })

  candidate_nodes_idx <- which(min_fills == min(min_fills))
  new_node_idx        <- candidate_nodes_idx[1] # candidate_nodes_idx[length(candidate_nodes_idx)]
  current_nei_idx     <- which(x[, new_node_idx, drop = TRUE] == 1L)

  current_nei_mat     <- obj$x[current_nei_idx, current_nei_idx, drop = FALSE]  
  obj$current_nei_mat <- current_nei_mat
  obj$current_nei_idx <- current_nei_idx
  obj$is_nei_complete <- sum(current_nei_mat) == length(current_nei_idx) * (length(current_nei_idx) - 1)
  obj$new_node_idx    <- new_node_idx

  return(obj)
}

new_node_to_eliminate.min_rfill_triang <- function(obj) {
  x                   <- obj$x

  min_fills <- .map_dbl(1:ncol(x), function(k) {
    current_nei_idx_k <- which(x[, k] == 1L)
    all_edges         <- length(current_nei_idx_k) * (length(current_nei_idx_k) - 1L) / 2
    existing_edges    <- sum(x[current_nei_idx_k, current_nei_idx_k]) / 2L
    all_edges - existing_edges    
  })

  candidates          <- which(min_fills == min(min_fills))
  new_node_idx        <- sample(candidates, 1L)
  current_nei_idx     <- which(x[, new_node_idx, drop = TRUE] == 1L)
  current_nei_mat     <- obj$x[current_nei_idx, current_nei_idx, drop = FALSE]
  obj$current_nei_idx <- current_nei_idx
  obj$current_nei_mat <- current_nei_mat
  obj$is_nei_complete <- sum(current_nei_mat) == length(current_nei_idx) * (length(current_nei_idx) - 1)
  obj$new_node_idx    <- new_node_idx

  return(obj)
}

new_node_to_eliminate.min_efill_triang <- function(obj) {
  x               <- obj$x
  new_node_idx    <- integer(0)
  current_nei_idx <- integer(0)
  nlvls           <- obj$nlvls

  min_fills <- .map_dbl(1:ncol(x), function(k) {
    current_nei_idx_k <- which(x[, k] == 1L)
    all_edges <- length(current_nei_idx_k) * (length(current_nei_idx_k) - 1L) / 2
    existing_edges <- sum(x[current_nei_idx_k, current_nei_idx_k]) / 2L
    all_edges - existing_edges
  })

  min_ <- min(min_fills)
  candidate_nodes_idx <- which(min_fills == min_)

  if (min_ == 0) {
    new_node_idx    <- candidate_nodes_idx[1]
    current_nei_idx <- which(x[, new_node_idx, drop = TRUE] == 1L)
  } else {
    pmf <- obj$pmf_evidence
    evidence_nodes <- names(pmf)
    nodes <- colnames(x)
    expected_statespace <- Inf

    for (k in candidate_nodes_idx) {
      current_nei_idx_k   <- which(x[, k] == 1L)
      family_k            <- nodes[c(k, current_nei_idx_k)]
      evidence_nodes_k    <- intersect(evidence_nodes, family_k)
      remaining_nodes_k   <- setdiff(family_k, evidence_nodes)
      prod_sp_remaining_k <- prod(nlvls[remaining_nodes_k])
      
      if (neq_empt_chr(evidence_nodes_k)) {
        sp_evidence_k <- nlvls[evidence_nodes_k]
        pmf_k         <- pmf[evidence_nodes_k]
        pmf_k_lst     <- lapply(pmf_k, function(p) c(p, 1-p)) # pres/absc (0/1) = 1/2 here

        # Calculate expected statespace
        evidence_state_space <- expand.grid(replicate(length(pmf_k), c(1, 2), FALSE), stringsAsFactors = FALSE)
        expected_sum_k <- sum(apply(evidence_state_space, 1L, function(state) {
          state_prob <- .map_dbl(seq_along(pmf_k_lst), function(k) pmf_k_lst[[k]][state[k]])
          prod(sp_evidence_k^c(state-1)) * prod(state_prob)
        }))

        expected_statespace_k <- expected_sum_k * prod_sp_remaining_k
        if (expected_statespace_k < expected_statespace) {
          expected_statespace <- expected_statespace_k
          current_nei_idx <- current_nei_idx_k
          new_node_idx    <- k
        }
      } else {
        if (prod_sp_remaining_k < expected_statespace) {
          expected_statespace <- prod_sp_remaining_k
          current_nei_idx <- current_nei_idx_k
          new_node_idx    <- k
        }
      }
    }
  }
  
  current_nei_mat     <- obj$x[current_nei_idx, current_nei_idx, drop = FALSE]
  obj$current_nei_mat <- current_nei_mat
  obj$current_nei_idx <- current_nei_idx
  obj$is_nei_complete <- sum(current_nei_mat) == length(current_nei_idx) * (length(current_nei_idx) - 1)
  obj$new_node_idx    <- new_node_idx
  obj$nlvls           <- nlvls[-new_node_idx]

  return(obj)
}

new_node_to_eliminate.min_sfill_triang <- function(obj) {
  x               <- obj$x
  nlvls           <- obj$nlvls
  new_node_idx    <- integer(0)
  current_nei_idx <- integer(0)
  min_sp          <- Inf

  min_fills <- .map_dbl(1:ncol(x), function(k) {
    current_nei_idx_k <- which(x[, k] == 1L)
    all_edges <- length(current_nei_idx_k) * (length(current_nei_idx_k) - 1L) / 2
    existing_edges <- sum(x[current_nei_idx_k, current_nei_idx_k]) / 2L
    all_edges - existing_edges
  })

  min_ <- min(min_fills)
  candidate_nodes_idx <- which(min_fills == min_)

  if (min_ == 0) {
    new_node_idx    <- candidate_nodes_idx[1]
    current_nei_idx <- which(x[, new_node_idx, drop = TRUE] == 1L)
  } else {
    for (k in candidate_nodes_idx) {
      current_nei_idx_k <- which(x[, k] == 1L)
      family_k <- c(k, current_nei_idx_k)
      min_sp_k <- prod(nlvls[family_k])
      if (min_sp_k < min_sp) {
        min_sp          <- min_sp_k
        new_node_idx    <- k
        current_nei_idx <- current_nei_idx_k
      }
    }
  }
  
  current_nei_mat     <- obj$x[current_nei_idx, current_nei_idx, drop = FALSE]
  obj$current_nei_mat <- current_nei_mat
  obj$current_nei_idx <- current_nei_idx
  obj$is_nei_complete <- sum(current_nei_mat) == length(current_nei_idx) * (length(current_nei_idx) - 1)
  obj$new_node_idx    <- new_node_idx
  obj$nlvls           <- nlvls[-new_node_idx]

  return(obj)
}

new_node_to_eliminate.min_rsfill_triang <- function(obj) {
  x               <- obj$x
  nlvls           <- obj$nlvls
  new_node_idx    <- integer(0)
  current_nei_idx <- integer(0)

  min_fills <- .map_dbl(1:ncol(x), function(k) {
    current_nei_idx_k <- which(x[, k] == 1L)
    all_edges <- length(current_nei_idx_k) * (length(current_nei_idx_k) - 1L) / 2
    existing_edges <- sum(x[current_nei_idx_k, current_nei_idx_k]) / 2L
    all_edges - existing_edges
  })

  min_ <- min(min_fills)
  candidate_nodes_idx <- which(min_fills == min_)

  if (min_ == 0) {
    new_node_idx    <- candidate_nodes_idx[1]
    current_nei_idx <- which(x[, new_node_idx, drop = TRUE] == 1L)
  } else {
    all_min_sp <- .map_dbl(candidate_nodes_idx, function(k) {
      current_nei_idx_k <- which(x[, k] == 1L)
      family_k <- c(k, current_nei_idx_k)
      prod(nlvls[family_k])
    })
    mins <- which(all_min_sp == min(all_min_sp))
    new_node_idx <- candidate_nodes_idx[sample(mins, 1)]
    current_nei_idx <- which(x[, new_node_idx, drop = TRUE] == 1L)
  }

  current_nei_mat     <- obj$x[current_nei_idx, current_nei_idx, drop = FALSE]
  obj$current_nei_mat <- current_nei_mat
  obj$current_nei_idx <- current_nei_idx
  obj$is_nei_complete <- sum(current_nei_mat) == length(current_nei_idx) * (length(current_nei_idx) - 1)
  obj$new_node_idx    <- new_node_idx
  obj$nlvls           <- nlvls[-new_node_idx]

  return(obj)
}
  
new_node_to_eliminate.min_sp_triang <- function(obj) {
  x               <- obj$x
  nlvls           <- obj$nlvls
  new_node_idx    <- integer(0)
  current_nei_idx <- integer(0)
  min_sp          <- Inf

  simplicial_nodes_idx <- which(.map_dbl(1:ncol(x), function(k) {
    current_nei_idx_k <- which(x[, k] == 1L)
    all_edges <- length(current_nei_idx_k) * (length(current_nei_idx_k) - 1L) / 2
    existing_edges <- sum(x[current_nei_idx_k, current_nei_idx_k]) / 2L
    all_edges - existing_edges
  }) == 0)

  if (neq_empt_int(simplicial_nodes_idx)) {
    # Eliminate simplicial node
    new_node_idx    <- simplicial_nodes_idx[1L]
    current_nei_idx <- which(x[, new_node_idx] == 1L)
  } else {
    for (k in 1:ncol(x)) {
      current_nei_idx_k <- which(x[, k] == 1L)
      family_k <- c(k, current_nei_idx_k)
      min_sp_k <- prod(nlvls[family_k])
      if (min_sp_k < min_sp) {
        min_sp          <- min_sp_k
        new_node_idx    <- k
        current_nei_idx <- current_nei_idx_k
      }
    }
  }

  current_nei_mat     <- obj$x[current_nei_idx, current_nei_idx, drop = FALSE]
  obj$current_nei_mat <- current_nei_mat
  obj$current_nei_idx <- current_nei_idx
  obj$is_nei_complete <- sum(current_nei_mat) == length(current_nei_idx) * (length(current_nei_idx) - 1)
  obj$new_node_idx    <- new_node_idx
  obj$nlvls           <- nlvls[-new_node_idx]

  return(obj)
}

new_node_to_eliminate.min_esp_triang <- function(obj) {
  x               <- obj$x
  new_node_idx    <- integer(0)
  current_nei_idx <- integer(0)
  nlvls           <- obj$nlvls

  simplicial_nodes_idx <- which(.map_dbl(1:ncol(x), function(k) {
      current_nei_idx_k <- which(x[, k] == 1L)
      all_edges <- length(current_nei_idx_k) * (length(current_nei_idx_k) - 1L) / 2
      existing_edges <- sum(x[current_nei_idx_k, current_nei_idx_k]) / 2L
      all_edges - existing_edges
    }) == 0)

  if (neq_empt_int(simplicial_nodes_idx)) {
    # Eliminate simplicial node
    new_node_idx    <- simplicial_nodes_idx[1L]
    current_nei_idx <- current_nei_idx_k <- which(x[, new_node_idx] == 1L)
  } else {
    candidate_nodes_idx <- 1:ncol(x)
    pmf <- obj$pmf_evidence
    evidence_nodes <- names(pmf)
    nodes <- colnames(x)
    expected_statespace <- Inf

    for (k in candidate_nodes_idx) {
      current_nei_idx_k   <- which(x[, k] == 1L)
      family_k            <- nodes[c(k, current_nei_idx_k)]
      evidence_nodes_k    <- intersect(evidence_nodes, family_k)
      remaining_nodes_k   <- setdiff(family_k, evidence_nodes)
      prod_sp_remaining_k <- prod(nlvls[remaining_nodes_k])
      
      if (neq_empt_chr(evidence_nodes_k)) {
        sp_evidence_k <- nlvls[evidence_nodes_k]
        pmf_k         <- pmf[evidence_nodes_k]
        pmf_k_lst     <- lapply(pmf_k, function(p) c(p, 1-p)) # pres/absc (0/1) = 1/2 here

        # Calculate expected statespace
        evidence_state_space <- expand.grid(replicate(length(pmf_k), c(1, 2), FALSE), stringsAsFactors = FALSE)
        expected_sum_k <- sum(apply(evidence_state_space, 1L, function(state){
          state_prob <- .map_dbl(seq_along(pmf_k_lst), function(k) pmf_k_lst[[k]][state[k]])
          prod(sp_evidence_k^c(state-1)) * prod(state_prob)
        }))

        expected_statespace_k <- expected_sum_k * prod_sp_remaining_k
        if (expected_statespace_k < expected_statespace) {
          expected_statespace <- expected_statespace_k
          current_nei_idx <- current_nei_idx_k
          new_node_idx <- k
        }
      } else {
        if (prod_sp_remaining_k < expected_statespace) {
          expected_statespace <- prod_sp_remaining_k
          current_nei_idx <- current_nei_idx_k
          new_node_idx <- k
        }
      }
    }
  }
  
  current_nei_mat     <- obj$x[current_nei_idx, current_nei_idx, drop = FALSE]
  obj$current_nei_mat <- current_nei_mat
  obj$current_nei_idx <- current_nei_idx
  obj$is_nei_complete <- sum(current_nei_mat) == length(current_nei_idx) * (length(current_nei_idx) - 1)
  obj$new_node_idx    <- new_node_idx
  obj$nlvls           <- nlvls[-new_node_idx]

  return(obj)
}


new_node_to_eliminate.alpha_triang <- function(obj) {
  new_node_idx        <- match(obj$alpha[1L], colnames(obj$x))
  current_nei_idx     <- which(obj$x[, new_node_idx] == 1L)
  current_nei_mat     <- obj$x[current_nei_idx, current_nei_idx, drop = FALSE]
  obj$current_nei_mat <- current_nei_mat
  obj$current_nei_idx <- current_nei_idx
  obj$is_nei_complete <- sum(current_nei_mat) == length(current_nei_idx) * (length(current_nei_idx) - 1)
  obj$new_node_idx    <- new_node_idx
  obj$alpha           <- obj$alpha[-1L]
  return(obj)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#               ELIMINATION GAME ENGINE
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
elim_game <- function(obj) {
  # Save the input graph
  y <- obj$x

  # triangulation set
  fill_edges <- list()

  # elimination ordering
  alpha      <- vector("integer", length = ncol(obj$x))
  alpha_iter <- 1L

  # transformation of current variable indices in a subgraph to original indices
  x_orig_col_idx <- 1:ncol(y)

  while (ncol(obj$x) > 1L) {

    obj <- new_node_to_eliminate(obj)
    
    if (!obj$is_nei_complete) { # append new fill_edges
      nn <- ncol(obj$current_nei_mat)

      for (k in 1:(nn-1)) {
        current_nei_mat_k <- obj$current_nei_mat[, k, drop = TRUE]
        non_adj <- obj$current_nei_idx[which(current_nei_mat_k[(k+1):nn] == 0L) + k]
        fills   <- lapply(non_adj, function(a) c(obj$current_nei_idx[k], a))
        
        for (fill in fills) {
          # convert to original indices:
          fill_orig    <- x_orig_col_idx[fill]
          fill_edges   <- push(fill_edges, fill_orig)

          obj$x[fill[1], fill[2]] <- 1L
          obj$x[fill[2], fill[1]] <- 1L

          y[fill_orig[1], fill_orig[2]] <- 1L
          y[fill_orig[2], fill_orig[1]] <- 1L
          
        }
      }
    }

    alpha[alpha_iter] <- x_orig_col_idx[obj$new_node_idx]
    alpha_iter        <- alpha_iter + 1L
    x_orig_col_idx    <- x_orig_col_idx[-obj$new_node_idx]
    obj$x             <- obj$x[-obj$new_node_idx, -obj$new_node_idx, drop = FALSE]
  }

   # should only ontain 1 value now
  alpha[length(alpha)] <- x_orig_col_idx[1L]
  
  list(
    new_graph  = y,
    fill_edges = fill_edges,
    alpha      = alpha
  )
}
