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


.triang <- function(obj) {
  eg  <- elim_game(obj)
  if (inherits(obj, "minimal_triang")) {
    return(thin_triang(eg[["new_graph"]], eg[["fill_edges"]])[["new_graph"]])
  } else {
    return(eg[["new_graph"]])
  }    
}


#' Triangulate a Bayesian network
#'
#' Given a list of CPTs, this function finds a triangulation
#'
#' @inheritParams compile
#' @param perm Logical. If \code{TRUE} the moral graph is permuted
#' @param mpd_based Logical. True if the triangulation should be performed on a maximal
#' peime decomposition
#' @export
triangulate <- function(x,
                        root_node    = "",
                        joint_vars   = NULL,
                        tri          = "min_fill",
                        pmf_evidence = NULL,
                        alpha        = NULL,
                        perm         = FALSE,
                        mpd_based    = FALSE
                        ) {
  UseMethod("triangulate")
}

#' @rdname triangulate
#' @export
triangulate.cpt_list <- function(x,
                                 root_node    = "",                                 
                                 joint_vars   = NULL,
                                 tri          = "min_fill",
                                 pmf_evidence = NULL,
                                 alpha        = NULL,
                                 perm         = FALSE,
                                 mpd_based    = FALSE
                                 ) {

  check_params_compile(tri, pmf_evidence, alpha, names(x), root_node)
  
  g       <- attr(x, "graph")
  parents <- attr(x, "parents")
  gm      <- moralize_igraph(g, parents)

  if (!is.null(joint_vars)) gm <- add_joint_vars_igraph(gm, joint_vars)

  # if sparse = TRUE, the run time explodes
  M <- igraph::as_adjacency_matrix(gm, sparse = FALSE)

  if (perm) {
    perm_ <- sample(1:ncol(M), ncol(M))
    M <- M[perm_, perm_]
  }

  if (mpd_based) {

    mpdx <- mpd(x)
    flawed_primes_int <- mpdx$primes_in[mpdx$flawed]

    fill_edges <- unlist(lapply(flawed_primes_int, function(fp_int) {
      Mfp <- mpdx$graph[fp_int, fp_int]
      if (perm) {
        perm_ <- sample(1:ncol(Mfp), ncol(Mfp))
        mpdx$graph[perm_, perm_]
      }
      tri_obj <- new_triang(tri, Mfp, .map_int(dim_names(x), length), pmf_evidence, alpha)
      eg <- elim_game(tri_obj)
      if (inherits(tri_obj, "minimal")) {
        thin_eg <- thin_triang(eg[["new_graph"]], eg[["fill_edges"]])
        eg[["new_graph"]]  <- thin_eg[["new_graph"]]
        eg[["fill_edges"]] <- thin_eg[["fill_edges"]]
      }

      lapply(eg[["fill_edges"]], function(edge) {
        colnames(eg[["new_graph"]])[edge]
      })
      
    }), recursive = F)

    alpha <- names(x)
    mat_tri <- M
    
    for (fill in fill_edges) {
      mat_tri[fill[1], fill[2]] <- 1L
      mat_tri[fill[2], fill[1]] <- 1L
    }
    
  } else {

    tri_obj <- new_triang(tri, M, .map_int(dim_names(x), length), pmf_evidence, alpha)
    eg <- elim_game(tri_obj)

    if (inherits(tri_obj, "minimal")) {
      thin_eg <- thin_triang(eg[["new_graph"]], eg[["fill_edges"]])
      eg[["new_graph"]]  <- thin_eg[["new_graph"]]
      eg[["fill_edges"]] <- thin_eg[["fill_edges"]]
    }
    
    fill_edges  <- lapply(eg[["fill_edges"]], function(e) names(x)[e])
    alpha       <- names(x)[eg[["alpha"]]]
    mat_tri     <- eg[["new_graph"]]
  }
  
  adj_lst_tri <- as_adj_lst(mat_tri)
  
  # construct cliques and statespace
  rip_        <- rip(adj_lst_tri, start_node = root_node, check = TRUE)
  cliques_    <- structure(rip_$C, names = paste("C", 1:length(rip_$C), sep = ""))
  statespace_ <- .map_dbl(cliques_, function(clique) {
    prod(.map_int(dim_names(x)[clique], length))
  })

  # construct junction tree
  dimnames(mat_tri) <- lapply(dimnames(mat_tri), function(x) 1:nrow(mat_tri))
  adj_lst_int       <- as_adj_lst(mat_tri)
  root_node_int     <- ifelse(root_node != "", as.character(match(root_node, names(adj_lst_tri))), "")
  cliques_int       <- lapply(rip(adj_lst_int)$C, as.integer)
  rjt               <- rooted_junction_tree(cliques_int)

  structure(
    list(
      new_graph             = mat_tri,
      fill_edges            = fill_edges,
      alpha                 = alpha,
      cliques               = cliques_,
      statespace            = statespace_,
      dim_names             = dim_names(x),
      junction_tree_collect = rjt$collect,
      clique_root           = rjt$clique_root
    ),
    class = c("triangulation", "list")
  )
}
