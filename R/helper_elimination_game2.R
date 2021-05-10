# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                TRIANGULATION CONSTRUCTORS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# NOTE:

# The four arguments
# - current_nei_mat
# - current_nei_idx
# - is_nei_complete
# - new_node_idx   

# _must_ be part of any new structure and 
# must be placed as the "last" arguments. They
# should be initialized as NULL. These variables are updated
# in each iteration of the elimination game.

# The argument
# - x
# is the input matrix and must be placed first.

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

new_evidence_triang <- function(x, pmf_evidence, nlvls) {
  if (neq_empt_int(nlvls)) {
    stopifnot(all(names(nlvls) %in% colnames(x)))
    # TODO: check if pmf and nlvls agree!
  }
  structure(
    list(
      x               = x,
      pmf_evidence    = pmf_evidence,
      nlvls           = nlvls[dimnames(x)[[1]]],
      current_nei_mat = NULL,
      current_nei_idx = NULL,
      is_nei_complete = NULL,
      new_node_idx    = NULL      
    ),
    class = c("evidence_triang", "list")
  )
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                UPDATING FUNCTIONS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
new_node_to_eliminate <- function(obj) UseMethod("new_node_to_eliminate")

new_node_to_eliminate.min_nei_triang <- function(obj) {
  # Minimum-neibor/minimum size elimination:
  # Minimizing the number of fillins by choosing vertices that
  # are hopefully simplicial
  # browser()
  x               <- obj$x
  lookup_order    <- as.integer(names(sort(structure(apply(x, 2L, sum), names = 1:ncol(x)))))
  new_node_idx    <- lookup_order[1L]
  nei             <- x[, new_node_idx, drop = TRUE]
  current_nei_idx <- unname(which(nei == 1L))
  current_nei_mat <- x[current_nei_idx, current_nei_idx, drop = FALSE]

  nn        <- ncol(current_nei_mat)
  max_edges <- nn * (nn - 1)
  is_nei_complete <- sum(current_nei_mat) == max_edges

  obj$current_nei_mat <- current_nei_mat
  obj$current_nei_idx <- current_nei_idx
  obj$is_nei_complete <- is_nei_complete
  obj$new_node_idx    <- new_node_idx
  
  return(obj)
}

new_node_to_eliminate.min_fill_triang <- function(obj) {
  x               <- obj$x
  new_node_idx    <- integer(0)
  current_nei_idx <- integer(0)
  edges_to_add    <- Inf

  for (k in 1:ncol(obj$x)) {
    current_nei_idx_k <- which(x[, k] == 1L)
    all_edges         <- length(current_nei_idx_k) * (length(current_nei_idx_k) - 1L) / 2
    existing_edges    <- sum(x[current_nei_idx_k, current_nei_idx_k]) / 2L
    edges_to_add_k    <- all_edges - existing_edges
    
    if (edges_to_add_k < edges_to_add) {
      edges_to_add    <- edges_to_add_k
      new_node_idx    <- k
      current_nei_idx <- current_nei_idx_k
    }
  }

  current_nei_mat     <- obj$x[current_nei_idx, current_nei_idx, drop = FALSE]
  
  obj$current_nei_mat <- current_nei_mat
  obj$current_nei_idx <- current_nei_idx
  obj$is_nei_complete <- sum(current_nei_mat) == length(current_nei_idx) * (length(current_nei_idx) - 1)
  obj$new_node_idx    <- new_node_idx

  return(obj)
}

new_node_to_eliminate.min_sp_triang <- function(obj) {
  x               <- obj$x
  nlvls           <- obj$nlvs
  new_node_idx    <- integer(0)
  current_nei_idx <- integer(0)
  min_sp          <- Inf

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

  current_nei_mat     <- obj$x[current_nei_idx, current_nei_idx, drop = FALSE]
  
  obj$current_nei_mat <- current_nei_mat
  obj$current_nei_idx <- current_nei_idx
  obj$is_nei_complete <- sum(current_nei_mat) == length(current_nei_idx) * (length(current_nei_idx) - 1)
  obj$new_node_idx    <- new_node_idx
  obj$nlvls           <- nlvls[-new_node_idx]

  return(obj)
}

# new_node_to_eliminate.evidence_triang <- function(obj) {
#   # TODO: INPUT SHOULD JUST BE x and obj and return obj

#   n_new_fills <- .map_dbl(1:ncol(x), function(k) {
#     current_nei_idx_k <- which(x[, k] == 1L)
#     all_edges <- length(current_nei_idx_k) * (length(current_nei_idx_k) - 1L) / 2
#     existing_edges <- sum(x[current_nei_idx_k, current_nei_idx_k]) / 2L
#     all_edges - existing_edges
#   })

#   # n_new_fills <- .map_dbl(1:ncol(x), function(k) {
#   #   current_nei_idx_k <- which(x[, k] == 1L)
#   #   family_k  <- c(k, current_nei_idx_k)
#   #   prod(nlvls[family_k])
#   # })

#   F_ <- which(n_new_fills == min(n_new_fills))

#   # TODO: For each candidate in F_, eliminate the one
#   # for wich the new clique has the smalleste expected statespace taking
#   # into account the evidence variables

#   # This expectation requires the dimension names? We must loop over all
#   # configurations of the evidence nodes.

#   # We can utilize the more sophisticated MRF for the evidence nodes here

  
#   current_nei_idx      <- which(x[, new_node] == 1L)
#   current_nei_mat        <- x[current_nei_idx, current_nei_idx, drop = FALSE]
#   is_nei_complete <- sum(current_nei_mat) == length(current_nei_idx) * (length(current_nei_idx) - 1)

#   return(list(current_nei_mat = current_nei_mat, current_nei_idx = current_nei_idx, is_nei_complete = is_nei_complete, a = new_node))
# }


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

    # browser()
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
