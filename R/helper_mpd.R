cliques_mat_int_ <- function(mat, prime_ints = NULL) {
  # prime_ints are the true indices of mat columns if considered as a submatrix
  # of some larger matrix
  # if NULL no conversion is made (as in new_mpd)

  dimnames(mat)[[1]] <- 1:nrow(mat)
  dimnames(mat)[[2]] <- dimnames(mat)[[1]]
  lapply(rip(as_adj_lst(mat), "")$C, function(x) {
    if (is.null(prime_ints)) {
      return(as.integer(x))
    } else {
      # Convert to original indices
      return(prime_ints[as.integer(x)])
    }
  })
}


#' Maximal Prime Decomposition
#'
#' Find the maximal prime decomposition and its associated junction tree
#'
#' @param x Either a neighbor matrix or a \code{cpt_list} object
#' @param save_graph Logical indicating if the moralized graph should be kept.
#' Useful when \code{x} is a \code{cpt_list} object.
#' @return
#'
#' - \code{prime_ints}: a list with the prime components,
#' - \code{flawed}: indicating which prime components that are triangulated
#' - \code{jt_collect}: the MPD junction tree prepared for collecting
#' 
#' @examples
#'
#' library(igraph)
#' el <- matrix(c(
#' "A", "T",
#' "T", "E",
#' "S", "L",
#' "S", "B",
#' "L", "E",
#' "E", "X",
#' "E", "D",
#' "B", "D"),
#'  nc = 2,
#'  byrow = TRUE
#' )
#' 
#' g <- igraph::graph_from_edgelist(el, directed = FALSE)
#' A <- igraph::as_adjacency_matrix(g, sparse = FALSE)
#' mpd(A)


#' @rdname mpd
#' @export
mpd <- function(x, save_graph = TRUE) UseMethod("mpd")

#' @rdname mpd
#' @export
mpd.matrix <- function(x, save_graph = TRUE) new_mpd(x)

#' @rdname mpd
#' @export
mpd.cpt_list <- function(x, save_graph = TRUE) {
  g       <- attr(x, "graph")
  parents <- attr(x, "parents")
  gm      <- moralize_igraph(get_graph(x), parents)
  M  <- igraph::as_adjacency_matrix(gm, sparse = FALSE)
  if (save_graph) {
    out <- new_mpd(M)
    out$graph <- M
    return(out)
  } else {
    return(new_mpd(M))
  }
}

new_mpd <- function(graph) {
  # graph: undirected adjacency matrix

  # Find minimal triangulation and construct junction tree
  eg  <- elim_game(new_min_fill_triang(graph))
  gmt <- thin_triang(eg[[1]], eg[[2]])[[1]]
  ## dimnames(gmt) <- lapply(dimnames(gmt), function(x) 1:nrow(gmt))
  ## adj_lst_int   <- as_adj_lst(gmt)
  ## cliques_int   <- lapply(rip(adj_lst_int, "")$C, as.integer)
  cliques_int <- cliques_mat_int_(gmt, NULL)
  n_cliques   <- length(cliques_int)

  # Vector of flags that indicate if a prime is flawed or not
  flawed <- rep(FALSE, n_cliques)

  rjt_minimal <- rooted_junction_tree(cliques_int)
  jt_minimal  <- rjt_minimal[[1]] + rjt_minimal[[2]]
  jt_inwards  <- rjt_minimal[[1]]
  jt_collect  <- jt_inwards

  index_vector_jt    <- 1:n_cliques
  index_vector_prune <- 1:n_cliques

  # Note: jt_inwards is the inwards junction tree used for pruning/merging
  #       That is, jt_inwards is the 1x1 matrix of 0L at the end.
  #       jt_collect is the resulting MPD junction tree for collecting.
  #
  #       The index vectors are used to align true indices between
  #       jt_inwards and jt_collect.

  # Propagate through the mpd tree to find incomplete separators
  while (n_cliques > 1L) {

    lvs <- leaves_jt(jt_inwards)
    par <- parents_jt(jt_inwards, lvs)
    to_merge <- list()

    for (k in seq_along(lvs)) {

      # Convert index in jt_inwards to true index in jt_collect
      i <- index_vector_prune[lvs[k]]
      j <- index_vector_prune[par[[k]]]

      # Find the true clique index in jt_collect
      ci_idx <- match(i, index_vector_jt)
      cj_idx <- match(j, index_vector_jt)
      
      ci <- cliques_int[[ci_idx]]
      cj <- cliques_int[[cj_idx]]
      sep <- intersect(ci, cj)
      sep_sub_mat <- try(graph[sep, sep, drop = FALSE])
      
      N <- ncol(sep_sub_mat)
      is_complete <- sum(sep_sub_mat) == N * (N - 1)

      # Is the separator complete in the (moral) graph
      if (!is_complete) {
        to_merge <- push(to_merge, c(ci_idx, cj_idx))
      }
    }

    # Merge those cliques for which their separator
    # is incomplete in the moral graph. The new clique
    # index is j and i gets deleted
    for (pair in to_merge) {
      i <- pair[1] 
      j <- pair[2]
      
      col_i    <- jt_collect[, i]
      col_j    <- jt_collect[, j]
      col_j[i] <- 0L
      col_i[j] <- 0L

      jt_collect[, j]  <- col_j + col_i
      cliques_int[[j]] <- union(cliques_int[[j]], cliques_int[[i]])
      flawed[j] <- TRUE
    }

    # Delete those leaves that are merged
    del_idx <- .map_int(to_merge, "[", 1)
    if (neq_empt_int(del_idx)) {
      jt_collect           <- jt_collect[-del_idx, -del_idx, drop = FALSE]
      cliques_int[del_idx] <- NULL
      index_vector_jt      <- index_vector_jt[-del_idx]
      flawed               <- flawed[-del_idx] 
    }

    # Prune: delete the leave nodes
    jt_inwards          <- jt_inwards[-lvs, -lvs, drop = FALSE]
    n_cliques           <- ncol(jt_inwards)
    index_vector_prune  <- index_vector_prune[-lvs]
  }

  list(
    primes_int   = cliques_int,
    flawed       = flawed,
    jt_collect   = jt_collect
  )
}

