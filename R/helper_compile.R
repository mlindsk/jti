parents_igraph <- function(g) {
  # out: named list where a name is the child and elements are parents
  stopifnot(igraph::is_directed(g), igraph::is_dag(g))
  Ag <- igraph::as_adjacency_matrix(g)
  cn <- colnames(Ag)
  if (is.null(cn)) {
    stop("The vertices in the igraph object must have names")
  }
  apply(Ag, 2, function(x) {
    names(which(x == 1L))
  })
}

moralize_igraph <- function(g, parents) {
  for (p in parents) {
    if (length(p) > 1) {
      pairs <- utils::combn(p, 2,  simplify = FALSE)
      for (ps in pairs) {
        if (!igraph::are.connected(g, ps[1], ps[2])) {
          g <- g + igraph::edge(ps[1], ps[2]) 
        }
      }
    }
  }
  return(g)
}

triangulate_igraph <- function(g) {
  igraph::is.chordal(g, fillin = FALSE, newgraph = TRUE)$newgraph
}

as_undirected_igraph <- function(g) igraph::as.undirected(g)

adjacency_list_from_moralization_and_triangulation_igraph <- function(g, par) {
  g <- moralize_igraph(g, par)
  g <- igraph::as.undirected(g)
  g <- triangulate_igraph(g)
  as_adj_lst(igraph::as_adjacency_matrix(g))
}

construct_cliques_and_parents <- function(adj, root_node = "") {
  rip_ <- rip(adj, root_node, check = FALSE)
  cliques <- rip_$C
  names(cliques) <- paste("C", 1:length(cliques), sep = "")
  # parents is only used if compile.data.frame is used
  return(list(cliques = cliques, parents = rip_$P))
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

adjacency_list_from_graph <- function(g, pe) {
  adj <- if (igraph::is.igraph(g)) {
    pe$parents <- parents_igraph(g)
    adjacency_list_from_moralization_and_triangulation_igraph(g, pe$parents)
  } else if (inherits(g, "gengraph")) {
    g$G_adj
  } else {
    stopifnot(is_decomposable(g))
    g
  }
}
