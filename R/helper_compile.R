as_undirected_igraph <- function(g) igraph::as.undirected(g)

parents_igraph <- function(g) {
  # out: named list where a name is the child and elements are parents
  A  <- igraph::as_adjacency_matrix(g)
  cn <- colnames(A)
  if (is.null(cn)) {
    stop("The vertices in the igraph object must have names")
  }
  apply(A, 2, function(x) {
    names(which(x == 1L))
  })
}

parents_cpt_list <- function(x) {
  parents <- structure(lapply(seq_along(x), function(i) {
    child   <- names(x)[i]
    setdiff(names(attr(x[[i]], "dim_names")), child)
  }), names  = names(x))
}


graph_from_cpt_list <- function(x) {
  pairs <- lapply(seq_along(x), function(i) {
    child <- names(x)[i]
    parents <- setdiff(names(attr(x[[i]], "dim_names")), child)
    as.matrix(expand.grid(parents, child, stringsAsFactors = FALSE))
  })
  el <- do.call(rbind, pairs)
  igraph::graph_from_edgelist(el)
}

moralize_igraph <- function(g, parents) {
  g <- igraph::as.undirected(g)
  for (p in parents) {
    if (length(p) > 1) {
      pairs <- utils::combn(p, 2,  simplify = FALSE)
      for (ps in pairs) {
        if (!igraph::are_adjacent(g, ps[1], ps[2])) {
          g <- g + igraph::edge(ps[1], ps[2]) 
        }
      }
    }
  }
  g
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#               gRain TRIANGULATION
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## triangulate_igraph2 <- function(g, nlvls) {
##   gu    <- igraph::as_adjacency_matrix(igraph::as.undirected(g))
##   nlvls <- nlvls[dimnames(gu)[[2]]]
##   tg    <- gRbase::triang(gu, control = list(method="mcwh", nLevels = nlvls))
##   igraph::graph_from_adjacency_matrix(tg, "undirected")
## }

## adjacency_list_from_moralization_and_triangulation_igraph2 <- function(g, par, nlvls) {
##   g <- moralize_igraph(g, par)
##   g <- triangulate_igraph2(g, nlvls)
##   as_adj_lst(igraph::as_adjacency_matrix(g))
## }

triangulate_igraph <- function(g) {
  igraph::is.chordal(g, fillin = FALSE, newgraph = TRUE)$newgraph
}


moralize_and_triangulate_igraph <- function(g, par) {
  triangulate_igraph(moralize_igraph(g, par))
}

construct_cliques_and_parents <- function(adj, root_node = "") {
  rip_ <- rip(adj, start_node = root_node, check = FALSE)
  cliques <- rip_$C
  names(cliques) <- paste("C", 1:length(cliques), sep = "")
  return(list(cliques = cliques, parents = rip_$P))
}

construct_cliques_int <- function(adj_mat) {
  # cliques_int is needed to construct the junction tree in new_jt -> new_schedule
  dimnames(adj_mat) <- lapply(dimnames(adj_mat), function(x) 1:nrow(adj_mat))
  adj_lst_int       <- as_adj_lst(adj_mat)
  cliques_int       <- rip(adj_lst_int)$C
  lapply(cliques_int, as.integer)
}

moralize_triangulate_and_get_parents_and_cliques <- function(g, is_dag, root_node, pca) {
  if (is_dag) {
    pca[["parents"]] <- parents_igraph(g)
    gmt     <- moralize_and_triangulate_igraph(g, pca[["parents"]])
    adj_mat <- igraph::as_adjacency_matrix(gmt)
    adj_lst <- as_adj_lst(adj_mat)
    cp      <- construct_cliques_and_parents(adj_lst, root_node)
  } else {
    adj_mat <- igraph::as_adjacency_matrix(g)
    adj_lst <- as_adj_lst(adj_mat)
    cp      <- construct_cliques_and_parents(adj_lst, root_node)
    pca[["parents"]] <- cp$parents
  }
  pca[["cliques"]]     <- cp$cliques
  pca[["cliques_int"]] <- construct_cliques_int(adj_mat)
  NULL
}
