#' Conditional probability list
#'
#' A check and conversion of cpts to be used in the junction tree algorithm
#'
#' @param x A named list with cpts in form of array-like object(s).
#' The name must be the child.
#' @examples
#' print(asia2)
#' cpt_list(asia2)
#' @export
cpt_list <- function(x) { 
  # x: list of cpts with dimnames
    
  if (!is_named_list(x)) {
    stop("x must be a named list of cpts. A name should be the name of the corresponding child node.")
  }

  dim_names <- list()
  
  y <- lapply(seq_along(x), function(i) {
    l <- x[[i]]
    class_allowed <- any(.map_lgl(sparta::allowed_class_to_sparta(), function(x) inherits(l, x)))
    if (!class_allowed) stop("one ore more elements in x is not an array-like object")
    spar <- sparta::as_sparta(l)
    dim_names <<- push(dim_names, attr(spar, "dim_names"))
    spar
  })

  dim_names <- unlist(dim_names, FALSE)
  dim_names <- dim_names[unique(names(dim_names))]
  
  structure(y, names = names(x), dim_names = dim_names, class = c("cpt_list", class(x)))
}


#' Compile information
#'
#' Compiled objects are used as building blocks for junction tree inference
#'
#' @param x Either a \code{data.frame} or an object returned from \code{cpt_list}
#' @param g Either a directed acyclic graph (DAG) as an igraph object or a
#' decomposable graph obtained from \code{ess::fit_graph} or an
#' adjacency list - a named list where an element is a character vector defining
#' the neigbors of the element. If \code{x} is a \code{cpt_list},
#' \code{g} must be \code{NULL}. The procdure then deduce the graph
#' from the conditional probability tables that was inputtet in \code{cpt_list}
#' @param root_node A node for which we require it to live in the root
#' clique (the first clique)
#' @param save_graph Logical indicating if the graph induced by the cpts
#' from the cpt_list should be saved. It has no effect when x is a
#' \code{data.frame}. Use \code{dag} to obtain the graph
#' @export
compile <- function(x, g = NULL, root_node = "", save_graph = FALSE) UseMethod("compile")

#' @rdname compile
#' @export
compile.cpt_list <- function(x, g = NULL, root_node = "", save_graph = FALSE) {
  # x is validated in cpt_list

  if (!is.null(g)) stop("g must be 'NULL'")

  g <- graph_from_cpt_list(x)
  if (!igraph::is_dag(g)) stop("The cpts does not induce an acyclic graph.")

  parents <- parents_cpt_list(x)
    
  gmt     <- moralize_and_triangulate_igraph(g, parents)
  adj_mat <- igraph::as_adjacency_matrix(gmt)
  adj_lst <- as_adj_lst(adj_mat)

  # cliques_int is needed to construct the junction tree in new_jt -> new_schedule
  dimnames(adj_mat) <- lapply(dimnames(adj_mat), function(x) 1:nrow(adj_mat))
  adj_lst_int       <- as_adj_lst(adj_mat)
  cliques_int       <- rip(adj_lst_int)$C
  cliques_int       <- lapply(cliques_int, as.integer)

  cliques <- construct_cliques_and_parents(adj_lst, root_node)$cliques
  charge  <- new_charge(x, cliques, parents)
  out     <- structure(
    list(charge = charge, cliques = cliques),
    root_node   = root_node,
    dim_names   = attr(x, "dim_names"),
    cliques_int = cliques_int,
    class       = c("charge", "list")
  )
  if (save_graph) attr(out, "graph") <- g
  out
}

#' @rdname compile
#' @export
compile.data.frame <- function(x, g, root_node = "", save_graph = FALSE) {

  # TODO: Make a vanilla print method

  if (!igraph::is.igraph(g)) stop("g must be an igraph object")

  is_dag <- igraph::is_dag(g)
  
  if (!is_dag) {
    if (!igraph::is_chordal(g)$chordal) {
      stop("undirected graphs must be be decomposable")
    }
  }

  pc <- new.env()
  moralize_triangulate_and_get_parents_and_cliques(g, is_dag, root_node, pc)

  dim_names <- lapply(x, unique)
  attr(x, "dim_names") <- dim_names

  charge  <- new_charge(x, pc[["cliques"]], pc[["parents"]])

  out <- structure(
    list(charge = charge, cliques = pc[["cliques"]]),
    root_node   = root_node,
    dim_names   = dim_names,
    cliques_int = pc[["cliques_int"]],
    class       = c("charge", "list")
  )

  if (save_graph) attr(out, "graph") <- g
  out
}

#' DAG
#'
#' Retrieve the DAG from a compiled object
#'
#' @param x A compiled object
#' 
#' @export
dag.charge <- function(x) attr(x, "graph")

## #' @export
## triangulate.igraph <- function(g) triangulate_igraph(g)

## #' @export
## moralize.igraph <- function(g) moralize_igraph(g, parents_igraph(g))



## compile2.cpt_list <- function(x, g = NULL, root_node = "", save_graph = FALSE) {
##   # x is validated in cpt_list
##   if (!is.null(g)) stop("g must be 'NULL'")
##   g       <- graph_from_cpt_list(x)
##   parents <- parents_igraph(g)
##   nlvls   <- .map_int(attr(x, "dim_names"), length)
##   adj     <- adjacency_list_from_moralization_and_triangulation_igraph2(g, parents, nlvls)
##   cliques <- construct_cliques_and_parents(adj, root_node)$cliques
##   charge  <- new_charge(x, cliques, parents)
##   out     <- structure(
##     list(charge = charge, cliques = cliques),
##     root_node = root_node,
##     dim_names = attr(x, "dim_names"),
##     class = c("charge", "list")
##   )
##   if (save_graph) attr(out, "graph") <- g
##   out
## }
