find_simplicial_and_neighbors <- function(x) {

  # Minimizing the number of elements to investigate
  lookup_order <- as.integer(names(sort(structure(apply(x, 2L, sum), names = 1:ncol(x)))))

  for (k in lookup_order) {
    nei       <- x[, k, drop = TRUE]
    nei_idx   <- unname(which(nei == 1L))
    x_nei     <- x[nei_idx, nei_idx, drop = FALSE]
    nn        <- ncol(x_nei)
    max_edges <- nn * (nn - 1)
    nei_complete <- sum(x_nei) == max_edges
    if (nei_complete) {
      return(list(x_nei = x_nei, nei_idx = nei_idx, nei_complete = nei_complete, a = k))
    }
  }

  min_nei_idx <- lookup_order[1L]

  # -----------------
  # IDX <- min(length(lookup_order), 3)
  # min_nei_idx <- lookup_order[IDX]
  # -----------------
  
  nei     <- x[, min_nei_idx, drop = TRUE]
  nei_idx <- unname(which(nei == 1L))
  x_nei   <- x[nei_idx, nei_idx, drop = FALSE]

  # ------------------------------------------
  # nn        <- ncol(x_nei)
  # max_edges <- nn * (nn - 1)
  # nei_complete <- sum(x_nei) == max_edges
  # -------------------------------------------

  # --------------------------------------------------------------------------------------------
   # return(list(x_nei = x_nei, nei_idx = nei_idx, nei_complete = nei_complete, a = min_nei_idx))
  # --------------------------------------------------------------------------------------------
  
  return(list(x_nei = x_nei, nei_idx = nei_idx, nei_complete = FALSE, a = min_nei_idx))
}

elim_game <- function(x) {
  # x: adjacency matrix

  # Save the input graph
  y <- x
  
  # triangulation set
  fill_edges <- list()
  fill_set   <- list()

  alpha <- vector("integer")
  x_orig_col_idx <- 1:ncol(x)
  
  while (ncol(x) > 1L) {

    X  <- find_simplicial_and_neighbors(x)
    xa <- X$a
    alpha_new <- x_orig_col_idx[xa]
    alpha <- c(alpha, alpha_new)
    nc <- X$nei_complete

    if (!nc) { # append new fill_edges

      tmp_fill_set <- list()
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
          tmp_fill_set <- push(tmp_fill_set, fill_orig)

          x[fill[1], fill[2]] <- 1L
          x[fill[2], fill[1]] <- 1L

          y[fill_orig[1], fill_orig[2]] <- 1L
          y[fill_orig[2], fill_orig[1]] <- 1L
          
        }
      }
      fill_set <- push(fill_set, tmp_fill_set)
    }

    x <- x[-xa, -xa, drop = FALSE]
    x_orig_col_idx <- x_orig_col_idx[-xa]
  }

  if (neq_empt_lst(fill_edges)) {
    names(fill_edges) = 1:length(fill_edges)
  } else {
    fill_edges <- NULL
    fill_set   <- NULL
  }
  
  list(
    fill_edges = fill_edges,
    fill_set   = unname(fill_set),
    new_graph  = y
  )
}

tprime_index <- function(rprime, graph) {
  .map_lgl(rprime, function(e) {
    nei_e1  <- which(graph[, e[1], drop = TRUE] == 1L)
    nei_e2  <- which(graph[, e[2], drop = TRUE] == 1L)
    nei_int <- intersect(nei_e1, nei_e2)
    N       <- length(nei_int)
    # is common neiborhood complete?
    sum(graph[nei_int, nei_int]) == N * (N - 1)
  })
}

rprime_index <- function(t, r) {
  .map_lgl(t, function(te) {
    any(.map_lgl(r, function(re) {
      # TODO: Optimize by stopping early
      length(intersect(te, re)) == 1L
    }))
  })
}

mint <- function(triang, graph, R = triang, start = TRUE, cnt = 1L) {
  if (start) {
    if (is.null(triang)) return(list(fill_edges = unname(triang), new_graph = graph))
  }
  Rp_idx <- if (start) rep(TRUE, length(triang)) else rprime_index(triang, R)
  Rp     <- triang[Rp_idx]
  Tp_idx <- tprime_index(Rp, graph)
  Tp     <- Rp[Tp_idx]
  if (neq_empt_lst(unname(Tp))) {
    for (e in Tp) {
      graph[e[1], e[2]] <- 0L
      graph[e[2], e[1]] <- 0L
    }
    Tnew <- triang[-match(names(Tp), names(triang))]
    return(mint(Tnew, graph, Tp, FALSE, cnt + 1L))
  } else {
    return(list(fill_edges = unname(triang), new_graph = graph))
  }
}

is_minimal <- function(triang, graph) {
  !any(.map_lgl(triang, function(e) {
    g <- graph
    g[e[1], e[2]] <- 0L
    g[e[2], e[1]] <- 0L
    is_decomposable(as_adj_lst(g))
  }))
}


## library(igraph)

## el <- matrix(c(
## "A", "T",
## "T", "E",
## "S", "L",
## "S", "B",
## "L", "E",
## "E", "X",
## "E", "D",
## "B", "D"),
##  nc = 2,
##  byrow = TRUE
## )

## g <- igraph::graph_from_edgelist(el, FALSE)
## x <- igraph::as_adjacency_matrix(g)
## plot(g)
## eg <- elim_game(x)
## plot(igraph::graph_from_adjacency_matrix(eg$new_graph, "undirected"))
## m  <- mint(eg$fill_edges, eg$new_graph)
## is_minimal(m[[1]], m[[2]])
## plot(igraph::graph_from_adjacency_matrix(m$new_graph, "undirected"))

